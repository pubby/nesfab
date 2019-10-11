#include "ir.hpp"

#include <iostream> // TODO: remove

#include "flat/small_set.hpp"

#include "format.hpp"

static constexpr unsigned UNVISITED = -1u;

////////////////////////////////////////
// ssa_node_t                         //
////////////////////////////////////////

void ssa_node_t::link_append_input(ssa_value_t value)
{
    ssa_forward_edge_t edge = { value };
    if(value.is_ptr())
    {
        edge.index = value->output_vec.size();
        value->output_vec.push_back({ this, input_vec.size() });
    }
    input_vec.push_back(edge);
}

void ssa_node_t::remove_inputs_output(unsigned i)
{
    assert(i < input_vec.size());
    if(input_vec[i].node.is_ptr())
    {
        assert(input_vec[i].node.ptr());
        ssa_node_t& from_node = *input_vec[i].node;
        unsigned const from_i = input_vec[i].index;

        // Remove the output edge that leads to our input on 'i'.
        from_node.output_vec.back().input().index = from_i;
        std::swap(from_node.output_vec[from_i], from_node.output_vec.back());
        from_node.output_vec.pop_back();
    }
}

void ssa_node_t::link_remove_input(unsigned i)
{
    assert(i < input_vec.size());

    // First deal with the node we're receiving input along 'i' from.
    remove_inputs_output(i);

    // Remove the input edge on 'i'.
    if(ssa_reverse_edge_t* o = input_vec.back().output())
        o->index = i;
    std::swap(input_vec[i], input_vec.back());
    input_vec.pop_back();
}

void ssa_node_t::link_change_input(unsigned i, ssa_value_t new_value)
{
    assert(i < input_vec.size());

    // First deal with the node we're receiving input along 'i' from.
    remove_inputs_output(i);

    // Now change our input.
    ssa_forward_edge_t edge = { new_value };
    if(new_value.is_ptr())
    {
        assert(new_value.ptr());
        ssa_node_t& from_node = *new_value;
        edge.index = from_node.output_vec.size();

        // Add the new output entry.
        from_node.output_vec.push_back({ this, i });
    }
    input_vec[i] = edge;
}

void ssa_node_t::link_clear_input()
{
    for(std::size_t i = 0; i < input_vec.size(); ++i)
        remove_inputs_output(i);
    input_vec.clear();
}

////////////////////////////////////////
// cfg_node_t                         //
////////////////////////////////////////

void cfg_node_t::create()
{
    assert(ssa_list.empty());
    assert(input_vec.empty());
    assert(output_vec.empty());
    exit = nullptr;
}

void cfg_node_t::destroy(ir_t& ir)
{
    input_vec = input_vec_t();
    output_vec = output_vec_t();

    // Delete all owned SSA nodes.
    ssa_foreach([&ir](ssa_node_t& ssa_node)
    {
        ssa_node.destroy();
        ir.ssa_pool.free(ssa_node);
    });
    ssa_list.clear();
}

void cfg_node_t::remove_ssa(ir_t& ir, ssa_node_t& ssa_node)
{
    assert(&ssa_node.cfg_node() == this);
    if(exit == &ssa_node)
        exit = nullptr;
    ssa_list.erase(ssa_node);
    ssa_node.destroy();
    ir.ssa_pool.free(ssa_node);
}

void cfg_node_t::build_resize_output(unsigned i)
{
    assert(output_vec.empty());
    output_vec.resize(i);
}

void cfg_node_t::build_set_output(unsigned i, cfg_node_t& new_node)
{
    assert(i < output_vec.size());
    assert(output_vec[i].node == nullptr);

    new_node.input_vec.push_back({ this, i });
    output_vec[i] = { &new_node, new_node.input_vec.size() };
}

void cfg_node_t::link_append_output(cfg_node_t& node)
{
    cfg_reverse_edge_t edge = { &node, node.input_vec.size() };
    node.input_vec.push_back({ this, output_vec.size() });
    output_vec.push_back(edge);
}

void cfg_node_t::remove_outputs_input(unsigned i)
{
    assert(i < output_vec.size());
    assert(output_vec[i].node);

    cfg_node_t* to_node = output_vec[i].node;
    unsigned const to_i = output_vec[i].index;

    // Remove the output edge that leads to our input on 'i'.
    to_node->input_vec.back().output().index = to_i;

    // Update all phi nodes
    to_node->ssa_foreach([to_i](ssa_node_t& ssa_node)
    {
        if(ssa_node.op() == SSA_phi)
            ssa_node.link_remove_input(to_i);
    });

    std::swap(to_node->input_vec[to_i], to_node->input_vec.back());
    to_node->input_vec.pop_back();
}

void cfg_node_t::link_remove_output(unsigned i)
{
    assert(i < output_vec.size());

    // First deal with the node we're passing outputs along 'i' from.
    remove_outputs_input(i);

    // Remove the output edge on 'i'.
    assert(output_vec.back().node);
    output_vec.back().input().index = i;
    std::swap(output_vec[i], output_vec.back());
    output_vec.pop_back();
}

void cfg_node_t::link_change_output(unsigned i, cfg_node_t& new_node)
{
    assert(i < output_vec.size());

    // First deal with the node we're passing outputs along 'i' from.
    remove_outputs_input(i);

    // Now change our output.
    new_node.input_vec.push_back({ this, i });
    output_vec[i] = { &new_node, new_node.input_vec.size() };
}

void cfg_node_t::link_clear_output()
{
    for(std::size_t i = 0; i < output_vec.size(); ++i)
        remove_outputs_input(i);
    output_vec.clear();
}

/* TODO: remove?
void cfg_node_t::link_insert_out(unsigned i, cfg_node_t& node)
{
    assert(out.size() > i);
    out[i] = &node;
    node.in.push_back({ this, i });
}
*/

bool cfg_node_t::dominates(cfg_node_t const& node) const
{
    if(node.postorder_i > postorder_i)
        return false;
    if(this == &node)
        return true;
    assert(node.idom->postorder_i > node.postorder_i);
    return dominates(*node.idom);
}

////////////////////////////////////////
// ir_t                               //
////////////////////////////////////////

cfg_node_t& ir_t::emplace_cfg()
{
    cfg_node_t& node = cfg_pool.alloc();
    node.create();
    cfg_list.insert(node);
    return node;
}

void ir_t::remove_cfg(cfg_node_t& cfg_node)
{
    cfg_list.erase(cfg_node);
    if(exit == &cfg_node)
        exit = nullptr;
    cfg_node.destroy(*this);
    cfg_pool.free(cfg_node);
}

void ir_t::clear()
{
    cfg_foreach([this](cfg_node_t& cfg_node) { cfg_node.destroy(*this); });
    cfg_pool.clear();
    ssa_pool.clear();
    root = nullptr;
    exit = nullptr;
    preorder.clear();
    postorder.clear();
}

void ir_t::finish_construction()
{
#ifndef NDEBUG
    cfg_foreach([](cfg_node_t& cfg_node)
    { 
        assert(cfg_node.exit); 
        cfg_node.ssa_foreach([&](ssa_node_t& ssa_node)
        { 
            assert(&ssa_node.cfg_node() == &cfg_node);
            if(ssa_node.op() == SSA_phi)
                assert(ssa_node.input_size() == cfg_node.input_size());
        });
    });
#endif

    build_order();
    build_dominators();
    build_loops(); // TODO: combine with order.
}

void ir_t::build_order()
{
    // This is a basic depth-first traversal of the graph.

    // 'preorder_i' will temporarily be used to track nodes that are visited,
    // and 'postorder_i' will temporarily be used to track nodes that
    // are currently in the recursion stack.
    cfg_foreach([](cfg_node_t& node) 
    { 
        node.preorder_i = UNVISITED; 
        node.postorder_i = UNVISITED; 
    });

    preorder.clear();
    preorder.reserve(cfg_pool.size());

    postorder.clear();
    postorder.reserve(cfg_pool.size());

    visit_order(*root);

    assert(preorder.empty() || preorder.front() == root);
    assert(postorder.empty() || postorder.back() == root);
}

void ir_t::visit_order(cfg_node_t& node)
{
    node.preorder_i = preorder.size();
    preorder.push_back(&node);
    for(unsigned i = 0; i < node.output_size(); ++i)
    {
        cfg_node_t& succ = node.output(i);
        if(succ.preorder_i == UNVISITED)
            visit_order(succ);
    }
    node.postorder_i = postorder.size();
    postorder.push_back(&node);
}

static cfg_node_t* best_idom(cfg_node_t const& node, cfg_node_t* root)
{
    if(node.input_size() == 0)
        return nullptr;
    cfg_node_t* new_idom = &node.input(0);
    for(std::size_t j = 1; j < node.input_size(); ++j)
    {
        cfg_node_t* pred = &node.input(j);
        while(pred != new_idom)
        {
            if(pred->postorder_i < new_idom->postorder_i)
                pred = pred->idom ? pred->idom : root;
            else 
                new_idom = new_idom->idom ? new_idom->idom : root;
        }
    }
    return new_idom;
}

// Finds the immediate dominator of every cfg node.
// 
// Paper: A Simple, Fast Dominance Algorithm
// By Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
void ir_t::build_dominators()
{
    cfg_foreach([](cfg_node_t& node){ node.idom = nullptr; });

    for(bool changed = true; changed;)
    {
        changed = false;

        // Reverse postorder, but skip start node.
        for(auto it = postorder.rbegin()+1; it < postorder.rend(); ++it)
        {
            cfg_node_t& node = **it; assert(&node != root);
            cfg_node_t* new_idom = best_idom(node, root);
            if(new_idom != node.idom)
            {
                node.idom = new_idom;
                changed = true;
            }
        }
    }
}

void ir_t::build_loops()
{
    cfg_foreach([](cfg_node_t& node) 
    { 
        node.iloop_header = nullptr;
        node.preorder_i = UNVISITED; 
        node.postorder_i = UNVISITED; 
    });

    preorder.clear();
    preorder.reserve(cfg_pool.size());

    postorder.clear();
    postorder.reserve(cfg_pool.size());

    visit_loops(*root);
}

// Paper: A New Algorithm for Identifying Loops in Decompilation
// By Tao Wei, Jian Mao, Wei Zou, Yu Chen 
cfg_node_t* ir_t::visit_loops(cfg_node_t& node)
{
    node.preorder_i = preorder.size();
    preorder.push_back(&node);

    for(unsigned i = 0; i < node.output_size(); ++i)
    {
        cfg_node_t& succ = node.output(i);
        if(succ.preorder_i == UNVISITED)
            tag_loop_header(&node, visit_loops(succ));
        else if(succ.postorder_i == UNVISITED) // Is back edge?
        {
            //loop_headers.push_back(succ); TODO
            tag_loop_header(&node, &succ);
        }
        else if(cfg_node_t* header = succ.iloop_header)
        {
            if(header->postorder_i == UNVISITED) // Is back edge?
                tag_loop_header(&node, header);
            else
            {
                // We've found a re-entry point.
                goto mark_reentry;
                while((header = header->iloop_header))
                {
                    if(header->postorder_i == UNVISITED) // Is back edge?
                    {
                        tag_loop_header(&node, header);
                        break;
                    }
                mark_reentry:
                    ;
                    //header->loop_entrances.push_back(&node); TODO
                }
            }
        }
    }

    node.postorder_i = postorder.size();
    postorder.push_back(&node);

    return node.iloop_header;
}

void ir_t::tag_loop_header(cfg_node_t* node, cfg_node_t* header)
{
    if(node == header || !header)
        return;

    while(cfg_node_t* iheader = node->iloop_header)
    {
        if(iheader == header)
            return;

        if(iheader->preorder_i < header->preorder_i)
        {
            node->iloop_header = header;
            node = header;
            header = iheader;
        }
        else
            node = iheader;
    }

    node->iloop_header = header;
}

std::ostream& ir_t::gv_ssa(std::ostream& o)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";

    cfg_foreach([&](cfg_node_t& cfg_node) 
    {
        o << "subgraph cluster_" << cfg_node.gv_id() << " {\n";
        o << "  style=filled;\n";
        o << "  color=lightgrey;\n";
        o << "  node [style=filled color=white];\n";
        cfg_node.ssa_foreach([&](ssa_node_t& ssa_node)
        {
            o << "  " << ssa_node.gv_id() << ";\n";

            for(unsigned i = 0; i < ssa_node.input_size(); ++i)
            {
                ssa_value_t input = ssa_node.input(i);
                if(input.is_const())
                    o << "  const_" << ssa_node.gv_id() << '_' << i << ";\n";
            }
        });
        o << "  " << cfg_node.gv_id() << ";\n"; 
        o << "}\n";
    });

    cfg_foreach([&](cfg_node_t& cfg_node) 
    {
        o << cfg_node.gv_id() << " [label=\"(ENTRY)\"];\n"; 
    });

    ssa_foreach([&](ssa_node_t& ssa_node) 
    {
        o << ssa_node.gv_id() << " [label=\"" << to_string(ssa_node.op());
        o << " " << ssa_node.type();
        if(&ssa_node == ssa_node.cfg_node().exit)
            o << " (EXIT)";
        o << "\"];\n"; 
    });

    cfg_foreach([&](cfg_node_t& cfg_node) 
    {
        for(unsigned i = 0; i < cfg_node.output_size(); ++i)
        {
            cfg_node_t& succ = cfg_node.output(i);
            o << cfg_node.exit->gv_id() << " -> " << succ.gv_id();
            o << "[penwidth=2 color=red";
            if(cfg_node.exit->op() == SSA_if)
                o << " label=\"" << (i ? "TRUE" : "FALSE") << "\"";
            o << "];\n";
        }
    });

    ssa_foreach([&](ssa_node_t& ssa_node) 
    {
        for(unsigned i = 0; i < ssa_node.input_size(); ++i)
        {
            ssa_value_t input = ssa_node.input(i);
            if(input.is_const())
            {
                o << "const_" << ssa_node.gv_id() << '_' << i;
                o << " -> " << ssa_node.gv_id() << ";\n";
                o << "const_" << ssa_node.gv_id() << '_' << i;
                o << " [label=\"" << to_double(input.fixed());
                o << "\" shape=box];\n";
            }
            else
                o << input->gv_id() << " -> " << ssa_node.gv_id() << ";\n";
        }
    });

    o << "}\n";
    return o;
}

std::ostream& ir_t::gv_cfg(std::ostream& o)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";

    cfg_foreach([&](cfg_node_t& cfg_node) 
    {
        o << cfg_node.gv_id();
        o << " [label=\"" << cfg_node.preorder_i;
        if(&cfg_node == root)
            o << " (ROOT)";
        if(&cfg_node == exit)
            o << " (EXIT)";
        if(cfg_node.iloop_header)
            o << " (LOOP: " << cfg_node.iloop_header->preorder_i << ")";
        o << "\"];\n"; 
    });

    cfg_foreach([&](cfg_node_t& cfg_node) 
    {
        for(unsigned i = 0; i < cfg_node.output_size(); ++i)
        {
            cfg_node_t& succ = cfg_node.output(i);
            o << cfg_node.gv_id() << " -> " << succ.gv_id() << ";\n";
        }

        /* TODO
        for(cfg_node_t* entrance : cfg_node.loop_entrances)
        {
            o << entrance->gv_id() << " -> " << cfg_node.gv_id();
            o << " [color=\"blue\", constraint=false];\n";
        }
        */

        if(cfg_node.idom)
        {
            o << cfg_node.gv_id() << " -> " << cfg_node.idom->gv_id();
            o << " [color=\"red\", constraint=false];\n";
        }

        /*
        if(cfg_node.iloop_header)
        {
            o << cfg_node.gv_id() << " -> " << cfg_node.iloop_header->gv_id();
            o << " [color=\"blue\", constraint=false];\n";
        }
        */
    });

    o << "}\n";
    return o;
}

