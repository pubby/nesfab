#include "rom_link.hpp"

#include <stdexcept>

#include "rom.hpp"
#include "format.hpp"
#include "locator.hpp"
#include "options.hpp"
#include "asm_proc.hpp"
#include "static_addr.hpp"
#include "rom_array.hpp"

static void write_linked(std::vector<locator_t> const& vec, int bank, 
                         std::uint8_t* const start)
{
    std::uint8_t* at = start;

    for(locator_t loc : vec)
    {
        loc = loc.link({}, bank);

        if(!is_const(loc.lclass()))
            throw std::runtime_error(fmt("Unable to link locator %", loc));

        std::uint16_t data = loc.data() + loc.offset();

        if(loc.high())
            data >>= 8;

        *at++ = data;
    }
}

std::vector<std::uint8_t> write_rom(std::uint8_t default_fill)
{
    std::size_t const header_size = mapper().ines_header_size();
    std::size_t const chr_rom_size = mapper().num_8k_chr_rom * 0x2000;
    std::size_t const prg_rom_size = mapper().num_32k_banks * 0x8000;
    std::size_t const total_size = header_size + chr_rom_size + prg_rom_size;

    std::size_t const header_start = 0;
    std::size_t const chr_rom_start = header_start + header_size;
    std::size_t const prg_rom_start = chr_rom_start + chr_rom_size;

    std::vector<std::uint8_t> rom(total_size, default_fill);

    write_ines_header(rom.data() + header_start, mapper());

    // TODO: write chr_rom
    assert(chr_rom_size == 0);

    auto const write_ptr = [&](span_t span, unsigned bank) -> std::uint8_t*
    {
        return rom.data() + prg_rom_start + bank * 0x8000 + span.addr - mapper().rom_span().addr;
    };

    /* TODO
    for(unsigned i = 0; i < NUM_SROM; ++i)
    {
        auto const srom = static_rom_name_t(i);
        if(!static_span(srom))
            continue;

        auto data = static_data(srom);
        if(auto const* v = std::get_if<std::vector<locator_t> const*>(&data))
        {
            assert(*v);
            for(unsigned bank = 0; bank < mapper().num_32k_banks; ++bank)
                write_linked(**v, bank, write_ptr(static_span(srom), bank));
        }
        else if(auto const* p = std::get_if<asm_proc_t*>(&data))
        {
            assert(*p);
            asm_proc_t& proc = **p;
            proc.link(); // Link without specifying bank
            proc.relocate(static_span(srom).addr);

            for(unsigned bank = 0; bank < mapper().num_32k_banks; ++bank)
                proc.write_bytes(write_ptr(static_span(srom), bank), bank);
        }
    }
    */
    assert(false);

    asm_proc_t asm_proc;

    for(rom_once_t const& once : rom_vector<rom_once_t>)
    {
        assert(once.span);

        if(once.data.rclass() == ROMD_ARRAY)
        {
            auto const& rom_array = rom_array_t::get({ once.data.handle() });
            write_linked(rom_array.data, once.bank, write_ptr(once.span, once.bank));
        }
        else if(once.data.rclass() == ROMD_PROC)
        {
            // We're copying the proc here.
            // This is slower than necessary, but safer to code.
            asm_proc = rom_proc_ht{ once.data.handle() }->asm_proc();

            asm_proc.link(once.bank);
            asm_proc.relocate(once.span.addr);
            asm_proc.write_bytes(write_ptr(once.span, once.bank), once.bank);
        }
    }

    /* TODO
    for(rom_many_t const& many : rom_vector<rom_many_t>)
    {
        assert(many.span);

        if(auto const* v = std::get_if<std::vector<locator_t> const*>(&many.data))
        {
            many.in_banks.for_each([&](unsigned bank)
            {
                write_linked(**v, bank, write_ptr(many.span, bank));
            });
        }
        else if(auto const* p = std::get_if<asm_proc_t*>(&many.data))
        {
            asm_proc_t& proc = **p;
            proc.link(); // Link without specifying bank
            proc.relocate(many.span.addr);

            many.in_banks.for_each([&](unsigned bank)
            {
                proc.write_bytes(write_ptr(many.span, bank), bank);
            });
        }
    }
    */

    return rom;
}
