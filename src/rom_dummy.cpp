#include "rom_dummy.hpp"

#include <iostream>

#include "rom.hpp"
#include "locator.hpp"
#include "group.hpp"

void gen_rom_dummies()
{
    for(group_ht g : group_ht::handles())
    {
        if(!g->dummy_required())
            continue;

        loc_vec_t vec = { locator_t::data_bank(g) };
        rom_array_ht a = rom_array_t::make(std::move(vec), false, false, ROMR_NORMAL, g->data_handle());
        a->mark_emits();
        g->set_dummy(a);
    }
}
