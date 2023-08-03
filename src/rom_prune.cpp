#include "rom_prune.hpp"

#include "rom.hpp"
#include "runtime.hpp"
#include "lt.hpp"
#include <iostream>

static void rom_mark_emits(rom_data_ht data);

static void locator_mark_emits(locator_t loc)
{
    if(rom_data_ht h = loc.rom_data())
        ::rom_mark_emits(h); // Recurse

    if(loc.lclass() == LOC_LT_EXPR)
    {
        lt_value_t& value = *loc.lt();

        if(!value.prune_processed)
        {
            value.prune_processed = true;
            value.for_each_locator(locator_mark_emits);
        }
    }
}

static void rom_mark_emits(rom_data_ht data)
{
    if(data.get()->emits())
        return;

    data.get()->mark_emits();
    assert(data.get()->emits());

    data.for_each_locator(locator_mark_emits);
}

void prune_rom_data()
{
    // Recursively mark rom_data as being emitted, starting from the runtime rom.
    for(runtime_rom_name_t rtrom = {}; rtrom < NUM_RTROM; rtrom = runtime_rom_name_t(rtrom + 1))
        if(rom_data_ht data = runtime_data(rtrom))
            rom_mark_emits(data);
}
