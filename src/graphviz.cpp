#include "graphviz.hpp"

#include <iostream>
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
        o << gv_id(ssa_it) << " [label=\"" << to_string(ssa_it->op());
        o << " " << ssa_it->type();
        if(ssa_it == ssa_it->cfg_node()->last_daisy())
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
            if(cfg_it->last_daisy())
                o << gv_id(cfg_it->last_daisy());
            else
                o << gv_id(cfg_it);
            o << " -> " << gv_id(succ);
            o << "[penwidth=3 color=red arrowsize=2";
            if(cfg_it->last_daisy() && cfg_it->last_daisy()->op() == SSA_if)
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
                if(ssa_it->op() == SSA_fn_call && i == 0)
                {
                    o << "const_" << gv_id(ssa_it) << '_' << i;
                    o << " [label=\"{";
                    o << input.ptr<global_t>()->name;
                    o << "}\" shape=box];\n";
                }
                else if(input.is_locator())
                {
                    o << "const_" << gv_id(ssa_it) << '_' << i;
                    o << " [label=\"{";
                    o << input.locator();
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

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); 
        ssa_it->test_flags(FLAG_DAISY); ++ssa_it)
    {
        ssa_ht next = ssa_it.next();
        if(!next || !next->test_flags(FLAG_DAISY))
            continue;
        o << gv_id(ssa_it) << " -> " << gv_id(next);
        o << " [color=blue, style=dashed];\n";
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


