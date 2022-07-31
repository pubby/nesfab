#ifndef STATIC_ADDR_HPP
#define STATIC_ADDR_HPP

#include <array>
#include <cstdint>

#include "ram.hpp"
#include "rom.hpp"
#include "span.hpp"
#include "span_allocator.hpp"

class locator_t;

enum static_ram_name_t : std::uint16_t
{
    SRAM_ptr_temp,
    SRAM_nmi_call_ptr,
    SRAM_nmi_call_bank,
    SRAM_nmi_saved_x,
    SRAM_nmi_saved_y,
    SRAM_nmi_saved_bank,
    SRAM_nmi_counter,
    SRAM_buttons_held,
    SRAM_buttons_pressed,
    SRAM_mapper_state,
    SRAM_oam,
    NUM_SRAM,
};

enum static_rom_name_t : std::uint16_t
{
    SROM_vectors,
    SROM_nmi,
    SROM_nmi_exit,
    SROM_irq,
    SROM_reset,
    SROM_trampoline,
    SROM_iota,
    NUM_SROM,
};

ram_bitset_t alloc_static_ram();
span_allocator_t alloc_static_rom();

std::array<span_t, NUM_SRAM> const& sram_spans();
std::array<span_t, NUM_SROM> const& srom_spans();

span_t static_span(static_ram_name_t name);
span_t static_span(static_rom_name_t name);

locator_t static_locator(static_ram_name_t name, std::uint16_t offset = 0);
locator_t static_locator(static_rom_name_t name, std::uint16_t offset = 0);

rom_data_ht static_data(static_rom_name_t name);

#endif
