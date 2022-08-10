#include "rom_prune.hpp"

#include "rom.hpp"
#include "runtime.hpp"

static void rom_mark_emits(rom_data_ht data)
{
    if(data.get()->emits())
        return;

    data.get()->mark_emits();
        
    data.for_each_locator([](locator_t loc)
    {
        if(rom_data_ht h = loc.rom_data())
            ::rom_mark_emits(h); // Recurse
    });
}

void prune_rom_data()
{
    // Recursively mark rom_data as being emitted, starting from the runtime rom.
    for(runtime_rom_name_t rtrom = {}; rtrom < NUM_RTROM; rtrom = runtime_rom_name_t(rtrom + 1))
        if(rom_data_ht data = runtime_data(rtrom))
            rom_mark_emits(data);
}
