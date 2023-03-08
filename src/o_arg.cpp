#include "o_arg.hpp"

#include "globals.hpp"
#include "ir.hpp"

bool o_remove_unused_arguments(log_t* log, ir_t& ir, fn_t const& fn, bool byteified)
{
    bool changed = false;

    for(cfg_node_t const& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it; ++ssa_it)
    {
        if(!fn_like(ssa_it->op()))
            continue;

        fn_ht const called_h = get_fn(*ssa_it);
        fn_t const& called = *called_h;

        // If the called fn is a mode, it may not have been compiled yet.
        // Thus, we can't check if it uses the argument.
        if(called.fclass == FN_MODE)
            continue;

        passert(called.global.compiled(), fn.global.name, called.global.name);

        for(unsigned i = write_globals_begin(ssa_it->op()); i < ssa_it->input_size();)
        {
            locator_t const loc = ssa_it->input(i+1).locator();
            if(loc.lclass() == LOC_ARG && loc.fn() == called_h
               && (!called.lvars().seen_arg(loc.arg())
                   || (byteified && called.lvars().index(loc) < 0)))
            {
                dprint(log, "REMOVE_UNUSED_ARGUMENTS_PRUNE", loc);

                // Prune this arg:
                ssa_it->link_remove_input(i+1);
                ssa_it->link_remove_input(i);
                changed = true;
            }
            else
                i += 2;
        }
    }

    return changed;
}

