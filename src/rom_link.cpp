#include "rom_link.hpp"

#include <stdexcept>
#include <iostream> // TODO

#include "rom.hpp"
#include "format.hpp"
#include "locator.hpp"
#include "options.hpp"
#include "asm_proc.hpp"
#include "runtime.hpp"
#include "globals.hpp"
#include "compiler_error.hpp"

static void write_linked(
    std::vector<locator_t> const& vec, romv_t romv, int bank, 
    std::uint8_t* const start)
{
    std::uint8_t* at = start;

    std::size_t const size = vec.size();
    for(std::size_t i = 0; i < size; ++i)
    {
        locator_t const loc = vec[i].link(romv, {}, bank);

        if(!is_const(loc.lclass()))
            throw std::runtime_error(fmt("Unable to link locator %", loc));
        //std::cout << loc << std::endl;
        //passert(loc.is_immediate(), loc);
        assert(!loc.offset());

        std::uint16_t data = loc.data();

        if(loc.is() == IS_PTR_HI)
            data >>= 8;

        *at++ = data;
    }
}

std::vector<std::uint8_t> write_rom(std::uint8_t default_fill)
{
    std::size_t const header_size = mapper().ines_header_size();
    std::size_t const prg_rom_size = mapper().num_32k_banks * 0x8000;
    std::size_t const chr_rom_size = mapper().num_8k_chr_rom * 0x2000;
    std::size_t const total_size = header_size + chr_rom_size + prg_rom_size;

    std::size_t const header_start = 0;
    std::size_t const prg_rom_start = header_start + header_size;
    std::size_t const chr_rom_start = prg_rom_start + prg_rom_size;

    std::vector<std::uint8_t> rom(total_size, default_fill);

    write_ines_header(rom.data() + header_start, mapper());

    // Scratch pad proc used in 'write':
    asm_proc_t asm_proc;

    auto const file_addr = [&](span_t span, unsigned bank) -> std::uint8_t*
    {
        return rom.data() + prg_rom_start + bank * 0x8000 + span.addr - mapper().rom_span().addr;
    };

    auto const write = [&](auto const& alloc)
    {
        alloc.data.visit([&](rom_array_ht rom_array)
        {
            alloc.for_each_bank([&](unsigned bank)
            {
                write_linked(rom_array->data(), alloc.romv, bank, file_addr(alloc.span, bank));
            });
        }, 
        [&](rom_proc_ht rom_proc)
        {
            // We're copying the proc here.
            // This is slower than necessary, but safer to code.
            asm_proc = rom_proc->asm_proc();

            //asm_proc.write_assembly(std::cout, alloc.romv);

            asm_proc.link(alloc.romv, alloc.only_bank());
            asm_proc.relocate(locator_t::addr(alloc.span.addr));

            //asm_proc.write_assembly(std::cout, alloc.romv);

            alloc.for_each_bank([&](unsigned bank)
            {
                asm_proc.write_bytes(file_addr(alloc.span, bank), alloc.romv, bank);
            });
        });
    };

    for(rom_static_t const& static_ : rom_static_ht::values())
        write(static_);
    for(rom_once_t const& once : rom_once_ht::values())
        write(once);
    for(rom_many_t const& many : rom_many_ht::values())
        write(many);

    if(chr_rom_size)
    {
        if(!global_t::chrrom() || global_t::chrrom()->gclass() != GLOBAL_CONST)
            throw compiler_error_t(fmt_error(fmt("Mapper % requires chrrom, but none was defined.", mapper().name())));

        const_t const& chrrom = global_t::chrrom()->impl<const_t>();
        rom_array_ht const rom_array = chrrom.rom_array();
        assert(rom_array);
        std::size_t const size = rom_array->data().size();

        if(size > chr_rom_size)
        {
            compiler_error(chrrom.global.pstring(), 
                fmt("chrrom of size % is greater than the mapper's expected size of %.", 
                    size, chr_rom_size));
        }
        else if(size < chr_rom_size)
        {
            compiler_warning(chrrom.global.pstring(), 
                fmt("chrrom of size % is smaller the mapper's expected size of %.", 
                    size, chr_rom_size));
        }

        write_linked(rom_array->data(), ROMV_MODE, 0, rom.data() + chr_rom_start);
    }
    else if(global_t::chrrom())
        compiler_warning(global_t::chrrom()->pstring(), fmt("Mapper % ignores chrrom. Data will not appear in ROM.", mapper().name()));

    return rom;
}
