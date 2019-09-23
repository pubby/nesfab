#include "ir.hpp"

#include <iostream>

#include "flat/small_set.hpp"

static constexpr unsigned UNVISITED = -1u;

std::string_view to_string(ssa_op_t op)
{
    using namespace std::literals;
    switch(op)
    {
#define SSA_DEF(x, ...) case SSA_##x: return #x##sv;
#include "ssa.inc"
#undef SSA_DEF
    default: return "???"sv;
    }
}

std::ostream& operator<<(std::ostream& o, ssa_op_t node_type)
{
    o << to_string(node_type);
    return o;
}

bool ssa_node_t::has_side_effects() const
{
    // TODO! TODO! TODO!
    return op == SSA_return;
    //assert(inputs.size() > 0);
    //assert(inputs[0].is_ptr());
    //return inputs[0].op != SSA_fence;
}

void ssa_node_t::link_remove_out(ssa_output_t output)
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
        in[i]->link_remove_out({ this, i });

    // Add the new user entry.
    if(new_value.is_ptr())
        new_value->out.push_back({ this, i });

    // Actually change the input.
    in[i] = new_value;
}

void ssa_node_t::link_clear_in()
{
    for(unsigned i = 0; i < in.size(); ++i)
        if(in[i].is_ptr())
            in[i]->link_remove_out({ this, i });
    in.clear();
}

void cfg_node_t::link_insert_out(unsigned i, cfg_node_t& node)
{
    assert(!out[i]);
    out[i] = &node;
    node.in.push_back({ this, i });
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
    //build_order();
    build_users();
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

    visit_order(*cfg_root);

    assert(preorder.empty() || preorder.front() == root);
    assert(postorder.empty() || postorder.back() == root);
}

unsigned scheduler_t::visit_order(cfg_node_t& node)
{
    node.num_descendents = 0;
    node.preorder_i = preorder.size();
    preorder.push_back(&node);
    for(int i = 0; i < 2; ++i)
    if(cfg_node_t* succ = node.succs[i])
    {
        if(succ->preorder_i == UNVISITED)
        {
            node.num_descendents += visit_order(*succ);
            node.succ_edge_types[i] = FORWARD_EDGE;
        }
        else if(succ->postorder_i == UNVISITED)
            node.succ_edge_types[i] = BACK_EDGE;
        else
            node.succ_edge_types[i] = CROSS_EDGE;
    }
    node.postorder_i = postorder.size();
    postorder.push_back(&node);
    return node.num_descendents;
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

std::ostream& ir_t::gv(std::ostream& o)
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
        if(&ssa_node == ssa_node.cfg_node->exit)
            o << " (EXIT)";
        o << "\"];\n"; 
    });

    cfg_pool.foreach([&](cfg_node_t& cfg_node) 
    {
        for(unsigned i = 0; i < cfg_node.out.size(); ++i)
        if(cfg_node_t* succ = cfg_node.out[i])
        {
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
                o << " [label=\"" << input.whole() << "\" shape=box];\n";
            }
            else
                o << input->gv_id() << " -> " << ssa_node.gv_id() << ";\n";
        }
    });

    o << "}\n";
    return o;
}

// TODO:
/*
std::string ir_t::handle_name(ssa_handle_t handle) const
{
    return fmt("[%]", std::to_string(handle.value));
}

void ir_t::debug_print(std::ostream& out) const
{


    auto print_cfg_node = [&](cfg_node_t& cfg_node)
    {
        out << "NODE " << cfg_node.index << ' ';

        switch(cfg_node.edge_type)
        {
        case CFG_return:
            std::cout << "return\n";
            break;
        case CFG_forward_jump:
            assert(cfg_node.succ[0]);
            std::cout << "jump " << cfg_node.succ[0]->index << '\n';
            break;
        case CFG_conditional:
            std::cout << "conditional " << cfg_node.succ[0]->index << ' '
                                        << cfg_node.succ[1]->index << '\n';
            break;
        case CFG_loop:
            std::cout << "loop " << cfg_node.succ[0]->index << ' '
                                 << cfg_node.succ[1]->index << '\n';
            break;
        default:
            break;
        }
        for(unsigned i = 0; i < cfg_node.ssa.size(); ++i)
        {
            ssa_node_t const& ssa_node = cfg_node.ssa[i];

            out << handle_name({ &cfg_node, i }) << " <- "
                << op_name(ssa_node.op) << ' ' << ssa_node.value;

            for(ssa_handle_t arg : ssa_node.args)
                out << ' ' << handle_name(arg);

            out << '\n';
        }
    };

    print_cfg_node(*special_cfg_node);
    for(cfg_node_t* cfg_node : cfg)
        print_cfg_node(*cfg_node);
}
*/
