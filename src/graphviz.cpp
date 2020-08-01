#include "graphviz.hpp"

#include <string>

#include "globals.hpp"
#include "ir.hpp"

static std::string gv_id(ssa_ht h) { return "ssa_" + std::to_string(h.index); }
static std::string gv_id(cfg_ht h) { return "cfg_" + std::to_string(h.index); }

void graphviz_ssa(std::ostream& o, ir_t const& ir)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        o << "subgraph cluster_" << gv_id(cfg_it) << " {\n";
        o << "  style=filled;\n";
        o << "  color=lightgrey;\n";
        o << "  node [style=filled color=white];\n";
        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            o << "  " << gv_id(ssa_it) << ";\n";

            for(unsigned i = 0; i < ssa_it->input_size(); ++i)
            {
                ssa_value_t input = ssa_it->input(i);
                if(input.is_const())
                    o << "  const_" << gv_id(ssa_it) << '_' << i << ";\n";
            }
        }
        o << "  " << gv_id(cfg_it) << ";\n"; 
        o << "}\n";
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        o << gv_id(cfg_it) << " [label=\"" << cfg_it.index << " (ENTRY)\"];\n";

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() == SSA_fence)
        {
            o << gv_id(ssa_it) << " [label=\"\" color=black shape=insulator];\n";
            continue;
        }
        o << gv_id(ssa_it) << " [label=\"" << to_string(ssa_it->op());
        o << " " << ssa_it->type();
        if(ssa_it == ssa_it->cfg_node()->exit)
            o << " (EXIT)";
        o << "\"";

        if(ssa_it->op() == SSA_fn_call)
            o << " shape=invhouse";

        o << "];\n"; 
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        for(unsigned i = 0; i < cfg_it->output_size(); ++i)
        {
            cfg_ht succ = cfg_it->output(i);
            if(cfg_it->exit)
                o << gv_id(cfg_it->exit);
            else
                o << gv_id(cfg_it);
            o << " -> " << gv_id(succ);
            o << "[penwidth=3 color=red arrowsize=2";
            if(cfg_it->exit && cfg_it->exit->op() == SSA_if)
                o << " label=\"" << (i ? "TRUE" : "FALSE") << "\"";
            o << "];\n";
        }
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        for(unsigned i = 0; i < ssa_it->input_size(); ++i)
        {
            ssa_value_t input = ssa_it->input(i);
            if(input.is_const())
            {
                if((ssa_it->op() == SSA_read_global
                    || ssa_it->op() == SSA_write_global
                    || ssa_it->op() == SSA_fn_call)
                   && i == 1
                   && input.ptr<global_t>())
                {
                    o << "const_" << gv_id(ssa_it) << '_' << i;
                    o << " [label=\"{";
                    o << input.ptr<global_t>()->name.view();
                    o << "}\" shape=box];\n";
                }
                else
                {
                    o << "const_" << gv_id(ssa_it) << '_' << i;
                    o << " [label=\"" << to_double(input.fixed());
                    o << "\" shape=box];\n";
                }

                o << "const_" << gv_id(ssa_it) << '_' << i;
                o << " -> " << gv_id(ssa_it) << "[";

            }
            else
            {
                o << gv_id(input.handle()) << " -> " << gv_id(ssa_it) << "[";
            }
            o << " fontcolor=lime fontsize=10 ";
            o << " headlabel=\"" << i << "\"];\n";
        }
    }

    o << "}\n";
}

void graphviz_cfg(std::ostream& o, ir_t const& ir)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        o << gv_id(cfg_it);
        o << " [label=\"" << cfg_it.index;
        if(cfg_it == ir.root)
            o << " (ROOT)";
        if(cfg_it == ir.exit)
            o << " (EXIT)";
        o << "\"];\n"; 
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        for(unsigned i = 0; i < cfg_it->output_size(); ++i)
        {
            cfg_ht succ = cfg_it->output(i);
            o << gv_id(cfg_it) << " -> " << gv_id(succ) << ";\n";
        }
    }

    o << "}\n";
}


