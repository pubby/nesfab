#include "ir_algo.hpp"
#include <iostream> // TODO

#include "ir.hpp"

thread_local std::vector<cfg_algo_d> cfg_algo_pool;
thread_local std::vector<cfg_ht> postorder;
thread_local std::vector<cfg_ht> preorder;
thread_local std::vector<cfg_ht> loop_headers;

////////////////////////////////////////
// order
////////////////////////////////////////

static void _visit_order(cfg_ht h)
{
    auto& u = algo(h);

    u.preorder_i = preorder.size();
    preorder.push_back(h);

    for(unsigned i = 0; i < h->output_size(); ++i)
    {
        cfg_ht succ = h->output(i);
        auto& succ_u = algo(succ);

        if(succ_u.preorder_i == UNVISITED)
            _visit_order(succ);
    }
    u.postorder_i = postorder.size();
    postorder.push_back(h);
}

// This does a basic depth-first traversal of the graph.
void build_order(ir_t const& ir)
{
    cfg_algo_pool.resize(cfg_pool::array_size());

    for(auto& algo : cfg_algo_pool)
    {
        algo.preorder_i = UNVISITED;
        algo.postorder_i = UNVISITED;
    }

    preorder.clear();
    postorder.clear();

    preorder.reserve(ir.cfg_size());
    postorder.reserve(ir.cfg_size());

    _visit_order(ir.root);

    assert(preorder.empty() || preorder.front() == ir.root);
    assert(postorder.empty() || postorder.back() == ir.root);
}

////////////////////////////////////////
// loops
////////////////////////////////////////

// Adds a loop header to 'node'.
// Nodes can have multiple loop headers, but only the immediate header is 
// stored per node. 
// This function weaves the new header in, adjusting the immediate headers
// until everything nests nicely.
static void _tag_loop_header(cfg_ht node, cfg_ht header)
{
    if(node == header || !header)
        return;

    while(cfg_ht iloop_header = algo(node).iloop_header)
    {
        if(iloop_header == header)
            return;

        auto& header_u = algo(header);
        auto& iloop_header_u = algo(iloop_header);

        // 'iloop_header' should always be in the DFS path:
        assert(iloop_header_u.preorder_i != UNVISITED
               && iloop_header_u.postorder_i == UNVISITED);

        // The new header should already be traversed:
        assert(header_u.preorder_i != UNVISITED);

        if(header_u.postorder_i != UNVISITED // If header's in the path
           && iloop_header_u.preorder_i < header_u.preorder_i)
           // And if 'iloop_header' comes before 'header' in the path.
        {
            algo(node).iloop_header = header;
            node = header;
            header = iloop_header;
        }
        else
            node = iloop_header;
    }

    algo(node).iloop_header = header;
}

// Paper: A New Algorithm for Identifying Loops in Decompilation
// By Tao Wei, Jian Mao, Wei Zou, Yu Chen 
static cfg_ht _visit_loops(cfg_ht node)
{
    auto& u = algo(node);

    u.preorder_i = preorder.size(); // Marks as traversed.
    preorder.push_back(node);

    unsigned const output_size = node->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        cfg_ht succ = node->output(i);
        auto& succ_u = algo(succ);

        if(succ_u.preorder_i == UNVISITED) // If 'succ' hasn't been traversed.
            _tag_loop_header(node, _visit_loops(succ));
        else if(succ_u.postorder_i == UNVISITED) // Is back edge?
        {
            if(!succ_u.is_loop_header)
            { 
                // Create a new header
                loop_headers.push_back(succ);
                succ_u.is_loop_header = true;
            }
            _tag_loop_header(node, succ);
        }
        else if(cfg_ht header = succ_u.iloop_header)
        {
            auto& header_u = algo(header);
            if(header_u.postorder_i == UNVISITED) // Is back edge?
            {
                assert(header_u.preorder_i != UNVISITED);
                _tag_loop_header(node, header);
            }
            else
            {
                // We've found a re-entry point.

                unsigned const out_i = i;
                unsigned const in_i = node->output_edge(i).index;

                if(out_i >= sizeof_bits<decltype(u.reentry_out)>)
                    throw std::runtime_error("CFG node has too many outputs.");
                if(in_i >= sizeof_bits<decltype(u.reentry_in)>)
                    throw std::runtime_error("CFG node has too many inputs.");

                u.reentry_out |= (1 << out_i);
                succ_u.reentry_in |= (1 << in_i);

                header->set_flags(FLAG_IRREDUCIBLE);

                // Travel up the iloop header tree until either finding
                // a loop header that exists inside the current DFS path,
                // or until we run out of headers to check.
                while(header_u.iloop_header)
                {
                    header = header_u.iloop_header;
                    // Check if 'header' is in the current DFS path:
                    if(algo(header).postorder_i == UNVISITED)
                    {
                        assert(algo(header).preorder_i != UNVISITED);
                        _tag_loop_header(node, header);
                        break;
                    }

                    header->set_flags(FLAG_IRREDUCIBLE);
                }
            }
        }
    }

    u.postorder_i = postorder.size();
    postorder.push_back(node);

    return u.iloop_header;
}

void build_loops_and_order(ir_t& ir)
{
    cfg_algo_pool.resize(cfg_pool::array_size());

    for(auto& u : cfg_algo_pool)
    {
        u.preorder_i = UNVISITED;
        u.postorder_i = UNVISITED;
        u.iloop_header = {};
        u.is_loop_header = false;
        u.reentry_in = 0;
        u.reentry_out = 0;
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        cfg_it->clear_flags(FLAG_IRREDUCIBLE);

    preorder.clear();
    postorder.clear();

    preorder.reserve(ir.cfg_size());
    postorder.reserve(ir.cfg_size());

    loop_headers.clear();

    _visit_loops(ir.root);

    assert(preorder.empty() || preorder.front() == ir.root);
    assert(postorder.empty() || postorder.back() == ir.root);
}

cfg_ht this_loop_header(cfg_ht h)
{
    assert(h);
    auto& d = algo(h);
    if(d.is_loop_header)
        return h;
    return d.iloop_header;
}

unsigned loop_depth(cfg_ht cfg)
{
    unsigned depth = 0;

    assert(cfg);

    if(!algo(cfg).is_loop_header)
        cfg = algo(cfg).iloop_header;

    for(; cfg; cfg = algo(cfg).iloop_header)
        ++depth;

    return depth;
}

unsigned edge_depth(cfg_ht cfg, cfg_ht output)
{
    assert(cfg);
    unsigned const depth = loop_depth(cfg);
    unsigned const output_depth = loop_depth(output);
    if(depth && depth == output_depth && !loop_is_parent_of(this_loop_header(output), cfg))
        return depth - 1;
    return std::min(depth, output_depth);
}

std::uint64_t depth_exp(std::uint64_t depth, std::uint64_t scale, std::uint64_t max_shifts)
{
    return 1ull << std::min<std::uint64_t>(depth * scale, max_shifts);
}

bool loop_is_parent_of(cfg_ht loop_header, cfg_ht node)
{
    assert(loop_header && algo(loop_header).is_loop_header);

    for(node = this_loop_header(node); node; node = algo(node).iloop_header)
        if(node == loop_header)
            return true;

    return false;
}

////////////////////////////////////////
// dominance
////////////////////////////////////////

bool dominates(cfg_ht a, cfg_ht b)
{
    assert(b);
    while(b != a && algo(b).postorder_i < algo(a).postorder_i)
    {
        b = algo(b).idom;
        assert(b);
    }
    assert((b == a) == (dom_intersect(a, b) == a));
    return b == a;
}

cfg_ht dom_intersect(cfg_ht a, cfg_ht b)
{
    assert(a && b);

    while(a != b)
    {
        if(algo(a).postorder_i < algo(b).postorder_i)
            a = algo(a).idom;
        if(algo(a).postorder_i > algo(b).postorder_i)
            b = algo(b).idom;
        assert(a && b);
    }

    return a;
}

// Finds the immediate dominator of every cfg node.
// 
// Paper: A Simple, Fast Dominance Algorithm
// By Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
void build_dominators_from_order(ir_t& ir)
{
    for(auto& algo : cfg_algo_pool)
        algo.idom = {};

    bool changed;
    do
    {
        changed = false;

        // Reverse postorder, but skip start node.
        for(auto it = postorder.rbegin()+1; it < postorder.rend(); ++it)
        {
            cfg_ht h = *it;
            assert(h != ir.root);

            cfg_ht new_idom = {};

            unsigned const input_size = h->input_size();
            for(std::size_t i = 0; i < input_size; ++i)
            {
                cfg_ht pred = h->input(i);
                if(pred == ir.root || algo(pred).idom)
                    new_idom = new_idom ? dom_intersect(new_idom, pred) : pred;
            }

            if(new_idom != algo(h).idom)
            {
                algo(h).idom = new_idom;
                changed = true;
            }
        }
    }
    while(changed);
}

////////////////////////////////////////
// other stuff
////////////////////////////////////////

static void _toposort_visit(ssa_ht node, ssa_ht*& vec_end)
{
    if(node->get_mark() == MARK_PERMANENT)
        return;

    assert(node->get_mark() != MARK_TEMPORARY);
    assert((node->set_mark(MARK_TEMPORARY), true));

    unsigned const input_size = node->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t input = node->input(i);
        if(input.holds_ref() && input->cfg_node() == node->cfg_node())
            _toposort_visit(input.handle(), vec_end);
    }

    if(node->in_daisy())
        if(ssa_ht prev = node.prev())
            _toposort_visit(prev, vec_end);

    node->set_mark(MARK_PERMANENT);
    *(vec_end++) = node;
}

void toposort_cfg_node(cfg_ht cfg_node, ssa_ht* vec)
{
    for(ssa_ht ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
    {
        // Phi nodes always come first.
        if(ssa_it->op() == SSA_phi)
        {
            *(vec++) = ssa_it;
            ssa_it->set_mark(MARK_PERMANENT);
        }
        else
            ssa_it->set_mark(MARK_NONE);
    }

    for(ssa_ht ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->in_daisy() && ssa_it != cfg_node->last_daisy())
            continue;

        unsigned const output_size = ssa_it->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_ht output = ssa_it->output(i);
            if(output->cfg_node() == cfg_node && output->op() != SSA_phi)
                goto not_leaf;
        }
        _toposort_visit(ssa_it, vec);
    not_leaf:;
    }
}
