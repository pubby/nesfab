#include "mlb.hpp"

#include "robin/map.hpp"

#include "decl.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "compiler_error.hpp"
#include "options.hpp"
#include "ram.hpp"
#include "rom.hpp"
#include "debug_print.hpp"
#include "hex.hpp"

void print_mlb(std::ostream& o)
{
    auto const fix_addr = [](unsigned addr, unsigned bank) -> unsigned
    {
        return addr - 0x8000 + (bank * 0x8000);
    };

    for(fn_t const& fn : fn_ht::values())
    {
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(auto a = fn.rom_proc()->get_alloc(romv_t(romv)))
            {
                span_t const span = a.get()->span;

                if(!span)
                    continue;

                a.for_each_bank([&](unsigned bank)
                {
                    unsigned const begin = fix_addr(span.addr, bank);

                    o << fmt("NesPrgRom:%:%@%_%:\n", 
                             hex_string(begin, 6),
                             fn.global.name, bank, romv);

                    locator_t const linked = locator_t::fn(fn.handle()).link(romv_t(romv), fn.handle(), -1);
                    
                    if(linked.lclass() != LOC_ADDR)
                        return;

                    unsigned addr = linked.data() + linked.offset();
                    assert(addr >= span.addr && addr < span.end());
                    addr = fix_addr(addr, bank);

                    if(addr == begin)
                        return;

                    o << fmt("NesPrgRom:%:%@%_%_entry:\n", 
                             hex_string(addr, 6),
                             fn.global.name, bank, romv);
                });
            }
        }
    }

    for(const_t const& c : const_ht::values())
    {
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(!c.rom_array())
                continue;

            if(auto a = c.rom_array()->get_alloc(romv_t(romv)))
            {
                span_t const span = a.get()->span;
                if(!span)
                    continue;

                a.for_each_bank([&](unsigned bank)
                {
                    unsigned const begin = fix_addr(span.addr,  bank);
                    unsigned const end   = fix_addr(span.end(), bank);

                    o << fmt("NesPrgRom:%-%:%@%_%:\n", 
                             hex_string(begin, 6), hex_string(end, 6),
                             c.global.name, bank, romv);
                });
            }
        }
    }

    for(unsigned i = 0; i < NUM_RTROM; ++i)
    {
        runtime_rom_name_t const rt = runtime_rom_name_t(i);

        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            span_t const span = runtime_span(rt, romv_t(romv));
            if(!span)
                continue;
            
            for(unsigned bank = 0; bank < mapper().num_32k_banks; ++bank)
            {
                unsigned const begin = fix_addr(span.addr,  bank);
                unsigned const end   = fix_addr(span.end(), bank);

                o << fmt("NesPrgRom:%-%:runtime_%@%_%:\n", 
                         hex_string(begin, 6), hex_string(end, 6),
                         to_string(rt), bank, romv);
            }
        }
    }

    rh::batman_map<unsigned, std::string> ram_map;

    auto const add_ram = [&](unsigned addr, std::string const& name, unsigned i)
    {
        auto& string = ram_map[addr];
        if(string.empty())
            string += fmt("%@%", name, i);
        else
            string += fmt("__%@%", name, i);
    };

    auto const write_gvar = [&](gvar_t const& v)
    {
        unsigned i = 0;
        v.for_each_locator([&](locator_t loc)
        { 
            if(span_t span = loc.gmember()->span(loc.atom()))
                add_ram(span.addr, v.global.name, i++);
        });
    };

    for(gvar_t const& v : gvar_ht::values())
        write_gvar(v);

    for(unsigned i = 0; i < NUM_RTRAM; ++i)
    {
        runtime_ram_name_t const rt = runtime_ram_name_t(i);

        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
            if(span_t span = runtime_span(rt, romv_t(romv)))
                add_ram(span.addr, fmt("runtime_%", to_string(rt)), romv);
    }

    for(auto const& pair : ram_map)
        o << fmt("R:%:%:\n", hex_string(pair.first, 4), pair.second);
}
