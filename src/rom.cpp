#include "rom.hpp"

#include "globals.hpp"
#include "asm_proc.hpp"
#include "rom_array.hpp"

rom_alloc_ht::rom_alloc_ht(rom_static_ht h) { assign(ROMA_STATIC, h.id); }
rom_alloc_ht::rom_alloc_ht(rom_many_ht h) { assign(ROMA_MANY, h.id); }
rom_alloc_ht::rom_alloc_ht(rom_once_ht h) { assign(ROMA_ONCE, h.id); }

rom_alloc_t* rom_alloc_ht::get() const
{
    switch(rclass())
    {
    default: return nullptr;
    case ROMA_STATIC: return rom_static_ht{handle()}.operator->();
    case ROMA_MANY: return rom_many_ht{handle()}.operator->();
    case ROMA_ONCE: return rom_once_ht{handle()}.operator->();
    }
}

int rom_alloc_ht::first_bank() const
{
    switch(rclass())
    {
    default: 
        return -1;
    case ROMA_STATIC:
        return 0;
    case ROMA_MANY: 
        return rom_many_ht{handle()}->in_banks.lowest_bit_set(); // This returns -1 on error
    case ROMA_ONCE: 
        if(!rom_once_ht{handle()}->span)
            return -1;
        return rom_once_ht{handle()}->bank;
    }
}

unsigned rom_data_ht::max_size() const
{
    switch(rclass())
    {
    default: 
        return 0;
    case ROMD_ARRAY:
        return rom_array_t::get({ handle() }).data.size();
    case ROMD_PROC:
        return rom_proc_ht{ handle() }->max_size();
    }
}
