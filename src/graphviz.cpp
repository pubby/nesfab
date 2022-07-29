#include "graphviz.hpp"

#include <iostream>
#include <string>

#include "globals.hpp"
#include "ir.hpp"
#include "format.hpp"

static std::string gv_id(ssa_ht h) { return "ssa_" + std::to_string(h.id); }
static std::string gv_id(cfg_ht h) { return "cfg_" + std::to_string(h.id); }

static std::string gv_input_id(ssa_ht owner, unsigned input_i) 
{ 
    ssa_value_t input = owner->input(input_i);
    if(input.is_const())
        return fmt("const_%_%", gv_id(owner), input_i);
    else if(input.holds_ref())
    {
        if(input->cfg_node() == owner->cfg_node())
            return gv_id(input.handle());
        else
            return fmt("input_%_%", gv_id(owner), gv_id(input.handle()));
    }
    return gv_id(input.handle());
}

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
                o << gv_input_id(ssa_it, i) << ";\n";
        }
        o << "  " << gv_id(cfg_it) << ";\n"; 
        o << "}\n";
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        o << gv_id(cfg_it) << " [label=\"" << cfg_it.id << " (ENTRY)\"];\n";

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        o << gv_id(ssa_it) << " [label=\"(" << ssa_it.id << ") ";
        o << to_string(ssa_it->op());
        o << " " << ssa_it->type();
        if(ssa_it == ssa_it->cfg_node()->last_daisy())
            o << " (EXIT)";
        o << "\"";

        if(fn_like(ssa_it->op()))
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
                if(fn_like(ssa_it->op()) && i == 0)
                {
                    o << gv_input_id(ssa_it, i);
                    o << " [label=\"{";
                    o << input.locator().fn()->global.name;
                    o << "}\" shape=box];\n";
                }
                else if(input.is_locator())
                {
                    o << gv_input_id(ssa_it, i);
                    o << " [label=\"{";
                    o << input.locator();
                    o << "}\" shape=box];\n";
                }
                else
                {
                    o << gv_input_id(ssa_it, i);
                    o << " [label=\"" << to_double(input.fixed()) << " " << to_string(input.num_type_name());
                    o << "\" shape=box];\n";
                }

            }
            else if(input.holds_ref() && input->cfg_node() != cfg_it)
            {
                o << gv_input_id(ssa_it, i);
                o << " [label=\"(" << input.handle().id;
                o << ")\" shape=diamond];\n";
            }

            o << gv_input_id(ssa_it, i) << " -> " << gv_id(ssa_it) << "[";
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
        o << " [label=\"" << cfg_it.id;
        if(cfg_it == ir.root)
            o << " (ROOT)";
        o << "\"];\n"; 
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        for(unsigned i = 0; i < cfg_it->output_size(); ++i)
        {
            cfg_ht succ = cfg_it->output(i);
            o << gv_id(cfg_it) << " -> " << gv_id(succ) << "[";
            switch(i)
            {
            case 0: o << " color=blue "; break;
            case 1: o << " color=red "; break;
            default: break;
            }

            switch(cfg_it->output_edge(i).index)
            {
            case 0: o << " arrowhead=empty "; break;
            case 1: o << " arrowhead=open "; break;
            case 3: o << " arrowhead=dot"; break;
            case 4: o << " arrowhead=box "; break;
            default: break;
            }

            o << "]\n";
        }
    }

    o << "}\n";
}


