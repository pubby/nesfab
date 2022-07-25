#include "rom.hpp"

#include "globals.hpp"
#include "asm_proc.hpp"

rom_alloc_ht::rom_alloc_ht(rom_static_ht h) { assign(h); }
rom_alloc_ht::rom_alloc_ht(rom_many_ht h) { assign(h); }
rom_alloc_ht::rom_alloc_ht(rom_once_ht h) { assign(h); }

void rom_alloc_ht::assign(rom_static_ht h) { assign(ROM_STATIC, h.value); }
void rom_alloc_ht::assign(rom_many_ht h) { assign(ROM_MANY, h.value); }
void rom_alloc_ht::assign(rom_once_ht h) { assign(ROM_ONCE, h.value); }

rom_alloc_t* rom_alloc_ht::get() const
{
    switch(rclass())
    {
    default: return nullptr;
    case ROM_MANY: return &rom_vector<rom_many_t>[handle()];
    case ROM_ONCE: return &rom_vector<rom_once_t>[handle()];
    }
}

int rom_alloc_ht::first_bank() const
{
    switch(rclass())
    {
    default: 
        return -1;
    case ROM_STATIC:
        return 0;
    case ROM_MANY: 
        return rom_vector<rom_many_t>[handle()].in_banks.lowest_bit_set(); // This returns -1 on error
    case ROM_ONCE: 
        if(!rom_vector<rom_once_t>[handle()].span)
            return -1;
        return rom_vector<rom_once_t>[handle()].bank;
    }
}

std::size_t rom_data_size(rom_data_t const& data)
{
    if(auto const* v = std::get_if<std::vector<locator_t> const*>(&data))
        return (*v)->size();
    else if(auto const* p = std::get_if<asm_proc_t*>(&data))
        return (*p)->size();
    return 0;
}
