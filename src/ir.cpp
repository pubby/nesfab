#include "ir.hpp"

#include <iostream> // TODO: remove

#include "flat/small_set.hpp"

#include "format.hpp"

static constexpr unsigned UNVISITED = -1u;

bool ssa_node_t::has_side_effects() const
{
    // TODO! TODO! TODO!
    return op == SSA_return;
    //assert(inputs.size() > 0);
    //assert(inputs[0].is_ptr());
    //return inputs[0].op != SSA_fence;
}

void ssa_node_t::remove_out(ssa_reverse_edge_t output)
{
    auto it = std::find(out.begin(), out.end(), output);
    assert(it != out.end());
    std::swap(*it, out.back());
    out.pop_back();
}

// Changes the input array while keeping 'users' pointers valid.
void ssa_node_t::link_change_input(unsigned i, ssa_value_t new_value)
{
    assert(i < in.size());

    // Remove user entry.
    if(in[i].is_ptr())
        in[i]->remove_out({ this, i });

    // Add the new user entry.
    if(new_value.is_ptr())
        new_value->out.push_back({ this, i });

    // Actually change the input.
    in[i] = new_value;
}

void ssa_node_t::link_append_input(ssa_value_t value)
{
    if(value.is_ptr())
        value->out.push_back({ this, in.size() });
    in.push_back(value);
}

void ssa_node_t::link_clear_in()
{
    for(unsigned i = 0; i < in.size(); ++i)
        if(in[i].is_ptr())
            in[i]->remove_out({ this, i });
    in.clear();
}

void cfg_node_t::remove_in(cfg_forward_edge_t input)
{
    assert(in.size() > 0);
    auto it = std::find(in.begin(), in.end(), input);
    assert(it != in.end());
    std::size_t index = it - in.begin();
    std::swap(*it, in.back());
    in.pop_back();
    for(ssa_node_t* ssa_node : ssa_nodes)
    {
        assert(ssa_node->cfg_node == this);
        if(ssa_node->op == SSA_phi)
        {
            assert(ssa_node->in.size() > 0);
            std::swap(ssa_node->in[index], ssa_node->in.back());
            ssa_node->in.pop_back();
            assert(ssa_node->in.size() == in.size());
        }
    }
}

void cfg_node_t::link_remove_in(cfg_forward_edge_t input)
{
    // Remove the 'in'.
    remove_in(input);

    // Remove the 'out'.
    assert(input.index < input.node->out.size());
    unsigned const swap_i = input.node->out.size() - 1;
    if(swap_i != input.index)
    {
        cfg_node_t*& swap_node = input.node->out[swap_i];
        auto it = std::find(swap_node->in.begin(), swap_node->in.end(),
                            cfg_forward_edge_t{ input.node, swap_i });
        assert(it != swap_node->in.end());
        it->index = input.index;
        std::swap(input.out(), swap_node);
    }
    input.node->out.pop_back();
}

void cfg_node_t::link_remove_out(unsigned out_i)
{
    assert(out_i < out.size());
    assert(out[out_i]);
    out[out_i]->link_remove_in({ this, out_i });
}

void cfg_node_t::link_insert_out(unsigned i, cfg_node_t& node)
{
    assert(out.size() > i);
    out[i] = &node;
    node.in.push_back({ this, i });
}

bool cfg_node_t::dominates(cfg_node_t const& node) const
{
    if(node.postorder_i > postorder_i)
        return false;
    if(this == &node)
        return true;
    assert(node.idom->postorder_i > node.postorder_i);
    return dominates(*node.idom);
}

void ir_t::clear()
{
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
    cfg_pool.foreach([](cfg_node_t& node)
    { 
        assert(node.exit); 
        for(ssa_node_t* ssa_node : node.ssa_nodes)
        {
            assert(ssa_node);
            assert(ssa_node->cfg_node == &node);
        }
    });
    ssa_pool.foreach([](ssa_node_t& node)
    { 
        assert(node.cfg_node);
        if(node.op == SSA_phi)
            assert(node.in.size() == node.cfg_node->in.size());
    });
#endif

    build_order();
    build_users();
    build_dominators();
    build_loops(); // TODO: combine with order.
}

void ir_t::build_order()
{
    // This is a basic depth-first traversal of the graph.

    // 'preorder_i' will temporarily be used to track nodes that are visited,
    // and 'postorder_i' will temporarily be used to track nodes that
    // are currently in the recursion stack.
    cfg_pool.foreach([](cfg_node_t& node) 
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
    for(unsigned i = 0; i < node.out.size(); ++i)
    {
        cfg_node_t* succ = node.out[i];
        if(succ->preorder_i == UNVISITED)
        {
            node.out_edge_types[i] = FORWARD_EDGE;
            visit_order(*succ);
        }
        else if(succ->postorder_i == UNVISITED)
            node.out_edge_types[i] = BACK_EDGE;
        else
            node.out_edge_types[i] = CROSS_EDGE;
    }
    node.postorder_i = postorder.size();
    postorder.push_back(&node);
}

void ir_t::build_users()
{
    ssa_pool.foreach([](ssa_node_t& node) { node.out.clear(); });
    ssa_pool.foreach([](ssa_node_t& node)
    {
        for(unsigned i = 0; i < node.in.size(); ++i)
        {
            ssa_value_t input = node.in[i];
            if(input.is_ptr())
                input->out.push_back({ &node, i });
        }
    });
}

// Finds the immediate dominator of every cfg node.
// 
// Paper: A Simple, Fast Dominance Algorithm
// By Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
void ir_t::build_dominators()
{
    cfg_pool.foreach([](cfg_node_t& node){ node.idom = nullptr; });

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

cfg_node_t* best_idom(cfg_node_t const& node, cfg_node_t* root)
{
    if(node.in.empty())
        return nullptr;
    cfg_node_t* new_idom = node.in[0].node; assert(new_idom);
    for(std::size_t j = 1; j < node.in.size(); ++j)
    {
        cfg_node_t* pred = node.in[j].node; assert(pred);
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

void ir_t::build_loops()
{
    cfg_pool.foreach([](cfg_node_t& node) 
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

    for(cfg_node_t* succ : node.out)
    {
        if(succ->preorder_i == UNVISITED)
            tag_loop_header(&node, visit_loops(*succ));
        else if(succ->postorder_i == UNVISITED) // Is back edge?
        {
            //loop_headers.push_back(succ); TODO
            tag_loop_header(&node, succ);
        }
        else if(cfg_node_t* header = succ->iloop_header)
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

    cfg_pool.foreach([&](cfg_node_t& cfg_node) 
    {
        o << "subgraph cluster_" << cfg_node.gv_id() << " {\n";
        o << "  style=filled;\n";
        o << "  color=lightgrey;\n";
        o << "  node [style=filled color=white];\n";
        for(ssa_node_t* ssa_node : cfg_node.ssa_nodes)
        {
            o << "  " << ssa_node->gv_id() << ";\n";

            for(unsigned i = 0; i < ssa_node->in.size(); ++i)
            {
                ssa_value_t input = ssa_node->in[i];
                if(input.is_const())
                    o << "  const_" << ssa_node->gv_id() << '_' << i << ";\n";
            }
        }
        o << "  " << cfg_node.gv_id() << ";\n"; 
        o << "}\n";
    });

    cfg_pool.foreach([&](cfg_node_t& cfg_node) 
    {
        o << cfg_node.gv_id() << " [label=\"(ENTRY)\"];\n"; 
    });

    ssa_pool.foreach([&](ssa_node_t& ssa_node) 
    {
        o << ssa_node.gv_id() << " [label=\"" << to_string(ssa_node.op);
        o << " " << ssa_node.type;
        if(&ssa_node == ssa_node.cfg_node->exit)
            o << " (EXIT)";
        o << "\"];\n"; 
    });

    cfg_pool.foreach([&](cfg_node_t& cfg_node) 
    {
        for(unsigned i = 0; i < cfg_node.out.size(); ++i)
        {
            cfg_node_t* succ = cfg_node.out[i];
            o << cfg_node.exit->gv_id() << " -> " << succ->gv_id();
            o << "[penwidth=2 color=red";
            if(cfg_node.exit->op == SSA_if)
                o << " label=\"" << (i ? "TRUE" : "FALSE") << "\"";
            o << "];\n";
        }
    });

    ssa_pool.foreach([&](ssa_node_t& ssa_node) 
    {
        for(unsigned i = 0; i < ssa_node.in.size(); ++i)
        {
            ssa_value_t input = ssa_node.in[i];
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

    cfg_pool.foreach([&](cfg_node_t& cfg_node) 
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

    cfg_pool.foreach([&](cfg_node_t& cfg_node) 
    {
        for(cfg_node_t* succ : cfg_node.out)
            o << cfg_node.gv_id() << " -> " << succ->gv_id() << ";\n";

        for(cfg_node_t* entrance : cfg_node.loop_entrances)
        {
            o << entrance->gv_id() << " -> " << cfg_node.gv_id();
            o << " [color=\"blue\", constraint=false];\n";
        }

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

