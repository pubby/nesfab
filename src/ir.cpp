#include "ir.hpp"

#include <iostream>

#include "flat/small_set.hpp"

std::string_view op_name(ssa_op_t op)
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

void ir_t::clear()
{
    ssa.clear();
    input_vec.clear();
}

std::ostream& ir_t::gv(std::ostream& o)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";
    for(unsigned i = 0; i < ssa.size(); ++i)
        o << i << "[label=\"" << op_name(ssa[i].op) << "\"];\n";

    for(unsigned i = 0; i < ssa.size(); ++i)
    {
        if(ssa[i].op != SSA_cfg_region || ssa[i].control_h.value != i)
        {
            o << i << " -> " << ssa[i].control_h.value;
            o << " [color=\"red\"];\n";
        }

        for(unsigned j = 0; j < ssa[i].input_size; ++j)
        {
            ssa_handle_t input = ssa[i].input(*this)[j];
            if(ssa_is_const(input))
                o << i << " -> c" << ssa_extract_const(input) << '\n';
            else
                o << i << " -> " << input.value << '\n';
        }
    }
    o << "}\n";
    return o;
}

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
