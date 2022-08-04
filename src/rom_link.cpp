#include "rom_link.hpp"

#include <stdexcept>
#include <iostream> // TODO

#include "rom.hpp"
#include "format.hpp"
#include "locator.hpp"
#include "options.hpp"
#include "asm_proc.hpp"
#include "static_addr.hpp"

static void write_linked(std::vector<locator_t> const& vec, int bank, 
                         std::uint8_t* const start)
{
    std::uint8_t* at = start;

    for(locator_t loc : vec)
    {
        loc = loc.link({}, bank);

        if(!is_const(loc.lclass()))
            throw std::runtime_error(fmt("Unable to link locator %", loc));
        std::cout << loc << std::endl;
        assert(loc.is_immediate());
        assert(!loc.offset());

        std::uint16_t data = loc.data();

        if(loc.is() == IS_HI)
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

    // Scratch pad proc used in 'write':
    asm_proc_t asm_proc;

    auto const calc_addr = [&](span_t span, unsigned bank) -> std::uint8_t*
    {
        return rom.data() + prg_rom_start + bank * 0x8000 + span.addr - mapper().rom_span().addr;
    };

    auto const write = [&](auto const& alloc)
    {
        alloc.data.visit([&](rom_array_ht rom_array)
        {
            alloc.for_each_bank([&](unsigned bank)
            {
                write_linked(rom_array->data(), bank, calc_addr(alloc.span, bank));
            });
        }, 
        [&](rom_proc_ht rom_proc)
        {
            // We're copying the proc here.
            // This is slower than necessary, but safer to code.
            asm_proc = rom_proc->asm_proc();

            asm_proc.link(alloc.only_bank());
            asm_proc.relocate(alloc.span.addr);

            asm_proc.write_assembly(std::cerr);

            alloc.for_each_bank([&](unsigned bank)
            {
                asm_proc.write_bytes(calc_addr(alloc.span, bank), bank);
            });
        });
    };

    for(rom_static_t const& static_ : rom_static_ht::values())
        write(static_);
    for(rom_once_t const& once : rom_once_ht::values())
        write(once);
    for(rom_many_t const& many : rom_many_ht::values())
        write(many);

    return rom;
}
