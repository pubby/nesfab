#include "o_locator.hpp"

#include "globals.hpp"
#include "ir.hpp"

bool o_optimize_locators(log_t* log, ir_t& ir)
{
    bool changed = false;

    for(cfg_node_t const& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it; ++ssa_it)
    {
        unsigned input_size = ssa_it->input_size();

        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t const input = ssa_it->input(i);

            if(!input.is_locator())
                continue;

            locator_t loc = input.locator();
            if(loc.is() == IS_BANK)
                loc.set_offset(0); // Banks ignore offsets.

            switch(loc.lclass())
            {
            default: 
                break;

            case LOC_GCONST:
                if(loc.is() == IS_PTR && loc.byteified() 
                   && mod_test(loc.const_()->mods(), MOD_align) && loc.const_()->type().array_length() >= 256)
                {
                    ssa_it->link_change_input(i, ssa_value_t(0u, TYPE_U));
                    changed = true;
                    continue;
                }
                break;


            case LOC_NAMED_LABEL:
                // Named label banks should map to the same bank locator:
                if(loc.is() == IS_BANK)
                {
                    global_ht const g = loc.global();
                    if(g->gclass() == GLOBAL_CONST)
                        loc = locator_t::gconst(g->handle<const_ht>()).with_is(IS_BANK);
                }
                break;
            }

            if(loc != input.locator())
            {
                ssa_it->link_change_input(i, loc);
                changed = true;
            }
        }
    }

    return changed;
}

