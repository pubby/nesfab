#include "rom_prune.hpp"

#include "rom.hpp"
#include "static_addr.hpp"

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
    // Recursively mark rom_data as being emitted, starting from the static rom.
    for(static_rom_name_t srom = {}; srom < NUM_SROM; srom = static_rom_name_t(srom + 1))
        if(rom_data_ht data = static_data(srom))
            rom_mark_emits(data);
}
