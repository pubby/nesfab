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

ssa_node_t* ssa_node_t::block()
{
    if(control->op == SSA_block)
        return this;
    return control->block();
}

void ssa_node_t::alloc_input(ir_t& ir, std::size_t size)
{
    input = ir.value_pool.alloc(input_size = size);
}

void ssa_node_t::set_input(
    ir_t& ir, ssa_value_t const* begin, ssa_value_t const* end)
{
    input_size = end - begin;
    input = ir.value_pool.insert(begin, end);
}

void ssa_node_t::set_input(
    ir_t& ir, ssa_node_t* const* begin, ssa_node_t* const* end)
{
    input = ir.value_pool.alloc(input_size = end - begin);
    std::copy(begin, end, input);
}

void ir_t::clear()
{
    ssa_pool.clear();
    order.clear();
    root = nullptr;
    exit = nullptr;
}
void ir_t::finish_construction()
{
    build_order();
    build_users();
}

void ir_t::build_order()
{
    assert(exit);
    order.clear();
    ssa_pool.foreach([](ssa_node_t& node) { node.order_i = UNVISITED; });
    visit_order(*exit);
}

void ir_t::visit_order(ssa_node_t& node)
{
    if(node.order_i != UNVISITED)
        return;
    node.order_i = order.size();
    order.push_back(&node);
    for(unsigned i = 0; i < node.input_size; ++i)
        if(node.input[i].is_ptr())
            visit_order(*node.input[i]);
}

void ir_t::build_users()
{
    for(ssa_node_t* node : order)
    {
        node->users_size = 0;
        node->users_capacity = 0;
    }

    for(ssa_node_t* node : order)
    {
        for(unsigned i = 0; i < node->input_size; ++i)
            if(node->input[i].is_ptr())
                node->input[i]->users_capacity += 1;
    }

    for(ssa_node_t* node : order)
        node->users = usage_pool.alloc(node->users_capacity);

    for(ssa_node_t* node : order)
    {
        for(unsigned i = 0; i < node->input_size; ++i)
        {
            if(node->input[i].is_ptr())
            {
                ssa_node_t& pred = *node->input[i];
                pred.users[pred.users_size++] = { node, i };
            }
        }
    }
}

std::ostream& ir_t::gv(std::ostream& o)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";

    ssa_pool.foreach([&](ssa_node_t& node) 
    {
        o << node.id() << "[label=\"" << to_string(node.op) << "\"];\n"; 
    });

    ssa_pool.foreach([&](ssa_node_t& node) 
    {
        if(node.op != SSA_block)
        {
            o << node.id() << " -> " << node.control->id();
            o << " [color=\"red\"];\n";
        }

        for(unsigned i = 0; i < node.input_size; ++i)
        {
            if(node.input[i].is_const())
                o << node.id() << " -> c" << node.input[i].whole() << '\n';
            else
                o << node.id() << " -> " << node.input[i]->id() << '\n';
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
