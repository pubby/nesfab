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

            if(ssa_flags(ssa_it->op()) & SSAF_WRITE_GLOBALS)
            {
                unsigned const begin = write_globals_begin(ssa_it->op());

                for(unsigned i = 0; i < begin; ++i)
                    o << gv_input_id(ssa_it, i) << ";\n";

                unsigned const input_size = ssa_it->input_size();
                assert((input_size - begin) % 2 == 0);
                for(unsigned i = begin; i < input_size; i += 2)
                    o << gv_input_id(ssa_it, i) << ";\n";
            }
            else for(unsigned i = 0; i < ssa_it->input_size(); ++i)
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
        if(fn_set_ht fn_set = get_fn_set(*ssa_it))
            o << " " << fn_set->global.name;
        else if(fn_ht fn = get_fn(*ssa_it))
            o << " " << fn->global.name;
        else
            o << " " << ssa_it->type();
        if(ssa_it->in_daisy())
            o << " (DAISY)";
        if(ssa_it == ssa_it->cfg_node()->last_daisy())
            o << " (EXIT)";
        o << "\"";

        if(direct_fn(ssa_it->op()) || ssa_it->op() == SSA_fn_ptr_call)
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
            if(cfg_it->last_daisy())
            {
                auto const& branch = *cfg_it->last_daisy();
                if(branch.op() == SSA_if)
                    o << " label=\"" << (i ? "TRUE" : "FALSE") << "\"";
                else if(branch.op() == SSA_switch_full)
                    o << " label=\"" << branch.input(i+1).whole() << " [" << i << "]\"";
                else if(branch.op() == SSA_switch_partial)
                {
                    if(i == 0)
                        o << " label=\"default [" << i << "]\"";
                    else
                        o << " label=\"" << branch.input(i).whole() << " [" << i << "]\"";
                }
            }
            o << "];\n";
        }
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        auto const write_input = [&](unsigned i, ssa_value_t input, locator_t loc = {})
        {
            if(!input)
                return;

            if(input.is_const())
            {
                if(ssa_it->op() == SSA_fn_ptr_call && i == 0)
                {
                    o << gv_input_id(ssa_it, i);
                    o << " [label=\"{";
                    o << input.locator().fn_set()->global.name;
                    o << " (" << input.locator().data() << ")";
                    if(loc)
                        o << "\\n(" << loc << ")";
                    o << "}\" shape=box];\n";
                }
                else if(direct_fn(ssa_it->op()) && i == 0)
                {
                    o << gv_input_id(ssa_it, i);
                    o << " [label=\"{";
                    o << input.locator().fn()->global.name;
                    o << " (" << input.locator().data() << ")";
                    if(loc)
                        o << "\\n(" << loc << ")";
                    o << "}\" shape=box];\n";
                }
                else if(input.is_locator())
                {
                    o << gv_input_id(ssa_it, i);
                    o << " [label=\"{";
                    o << input.locator();
                    if(loc)
                        o << "\\n(" << loc << ")";
                    o << "}\" shape=box];\n";
                }
                else
                {
                    o << gv_input_id(ssa_it, i);
                    o << " [label=\"" << to_double(input.fixed()) << " " << to_string(input.num_type_name());
                    if((ssa_it->op() == SSA_multi_lt || ssa_it->op() == SSA_multi_lte) && i < 2)
                        o << " {" << to_string(type_name_t(input.whole())) << "}";
                    if(loc)
                        o << "\\n(" << loc << ")";
                    o << "\" shape=box];\n";
                }

            }
            else if(input.holds_ref() && input->cfg_node() != cfg_it)
            {
                o << gv_input_id(ssa_it, i);
                o << " [label=\"(" << input.handle().id << ")";
                if(loc)
                {
                    o << "\\n(" << loc << ")";
                    o << "\" shape=box];\n";
                }
                else
                    o << "\" shape=diamond];\n";
            }

            o << gv_input_id(ssa_it, i) << " -> " << gv_id(ssa_it) << "[";
            o << " fontcolor=lime fontsize=10 ";
            o << " headlabel=\"" << i << "\"];\n";
        };

        if(ssa_flags(ssa_it->op()) & SSAF_WRITE_GLOBALS)
        {
            unsigned const begin = write_globals_begin(ssa_it->op());

            for(unsigned i = 0; i < begin; ++i)
            {
                ssa_value_t input = ssa_it->input(i);
                write_input(i, input);
            }

            unsigned const input_size = ssa_it->input_size();
            assert((input_size - begin) % 2 == 0);
            for(unsigned i = begin; i < input_size; i += 2)
            {
                ssa_value_t input = ssa_it->input(i);
                locator_t loc = ssa_it->input(i+1).locator();
                write_input(i, input, loc);
            }
        }
        else 
        {
            for(unsigned i = 0; i < ssa_it->input_size(); ++i)
            {
                ssa_value_t input = ssa_it->input(i);
                write_input(i, input);
            }
        }
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); 
        ssa_it && ssa_it->test_flags(FLAG_DAISY); ++ssa_it)
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


