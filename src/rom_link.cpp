#include "rom_link.hpp"

#include <stdexcept>

#include "rom.hpp"
#include "format.hpp"
#include "locator.hpp"
#include "options.hpp"
#include "asm_proc.hpp"
#include "runtime.hpp"
#include "globals.hpp"
#include "compiler_error.hpp"

void link_variables_optimize()
{
    for(rom_proc_t& rom_proc : rom_proc_ht::values())
    {
        romv_for_each(rom_proc.desired_romv(), [&](romv_t romv)
        {
            asm_proc_t asm_proc = rom_proc.asm_proc();
            asm_proc.link_variables(romv);
            if(asm_proc.fn && !asm_proc.fn->iasm)
            {
                asm_proc.late_optimize();
                asm_proc.build_label_offsets();
            }
            rom_proc.assign(std::move(asm_proc), romv);
        });
    }
}

static void write_linked(
    std::vector<locator_t> const& vec, romv_t romv, int bank, 
    std::uint8_t* const start)
{
    passert(bank < mapper().num_banks, bank);
    std::uint8_t* at = start;

    std::size_t const size = vec.size();

    for(std::size_t i = 0; i < size; ++i)
        *at++ = linked_to_rom(vec[i].link(romv, {}, bank));
}

std::vector<std::uint8_t> write_rom(std::uint8_t default_fill)
{
    std::size_t const header_size = mapper().ines_header_size();
    std::size_t const prg_rom_size = mapper().prg_size();
    std::size_t const chr_rom_size = mapper().num_8k_chr_rom * 0x2000;
    std::size_t const total_size = header_size + chr_rom_size + prg_rom_size;

    std::size_t const header_start = 0;
    std::size_t const prg_rom_start = header_start + header_size;
    std::size_t const chr_rom_start = prg_rom_start + prg_rom_size;

    std::vector<std::uint8_t> rom(total_size, default_fill);

    write_ines_header(rom.data() + header_start, mapper());

    auto const file_addr = [&](span_t span, unsigned bank) -> std::uint8_t*
    {
        passert(bank < mapper().num_banks, bank);
        return rom.data() + prg_rom_start + bank * mapper().bank_size() + span.addr - mapper().bank_span(bank).addr;
    };

    auto const write = [&](auto const& alloc, bool stat = false)
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
            auto& asm_proc = rom_proc->asm_proc(alloc.romv);

            asm_proc.link(alloc.romv, alloc.only_bank());
            asm_proc.relocate(locator_t::addr(alloc.span.addr));

            if(asm_proc.fn)
            {
                if(auto* os = asm_proc.fn->info_stream())
                {
                    *os << "\nLINK:\n";
                    asm_proc.write_assembly(*os, alloc.romv);
                }
            }

            alloc.for_each_bank([&](unsigned bank)
            {
                asm_proc.write_bytes(file_addr(alloc.span, bank), alloc.romv, bank);
            });
        });
    };

    for(rom_static_t const& static_ : rom_static_ht::values())
        write(static_, true);
    for(rom_once_t const& once : rom_once_ht::values())
        write(once);
    for(rom_many_t const& many : rom_many_ht::values())
        write(many);

    if(auto addr = mapper().this_bank_addr())
    {
        for(unsigned bank = 0; bank < mapper().num_switched_prg_banks(); ++bank)
            *file_addr({ addr, 1 }, bank) = (bank << bank_shift()) + bank_add();
    }

    if(chr_rom_size)
    {
        if(!global_t::chrrom() || global_t::chrrom()->gclass() != GLOBAL_CONST)
            compiler_warning(fmt("Mapper % requires chrrom, but none was defined.", mapper().name()));
        else
        {
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
    }
    else if(global_t::chrrom())
        compiler_warning(global_t::chrrom()->pstring(), fmt("Mapper % ignores chrrom. Data will not appear in ROM.", mapper().name()));

    return rom;
}
