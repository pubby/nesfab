#ifndef RUNTIME_HPP
#define RUNTIME_HPP

#include <array>
#include <cstdint>
#include <string>
#include <ostream>

#include "ram.hpp"
#include "rom_decl.hpp"
#include "span.hpp"
#include "span_allocator.hpp"

class locator_t;

#define RTRAM_X \
RT(ptr_temp) \
RT(nmi_index) \
RT(nmi_saved_x) \
RT(nmi_saved_y) \
RT(nmi_saved_bank) \
RT(nmi_counter) \
RT(nmi_ready) \
RT(mapper_state)

enum runtime_ram_name_t : std::uint16_t
{
#define RT(name) RTRAM_##name,
RTRAM_X
#undef RT
    NUM_RTRAM,
};

#define RTROM_X \
RT(vectors) \
RT(nmi) \
RT(nmi_exit) \
RT(wait_nmi) \
RT(nmi_lo_table) \
RT(nmi_hi_table) \
RT(nmi_bank_table) \
RT(irq) \
RT(reset) \
RT(jmp_y_trampoline) \
RT(jsr_y_trampoline) \
RT(iota) \
RT(mul8) 

enum runtime_rom_name_t : std::uint16_t
{
#define RT(name) RTROM_##name,
RTROM_X
#undef RT
    NUM_RTROM,
};

ram_bitset_t alloc_runtime_ram();
span_allocator_t alloc_runtime_rom();

std::array<std::array<span_t, NUM_ROMV>, NUM_RTRAM> const& rtram_spans();
std::array<std::array<span_t, NUM_ROMV>, NUM_RTROM> const& rtrom_spans();

span_t runtime_span(runtime_ram_name_t name, romv_t romv);
span_t runtime_span(runtime_rom_name_t name, romv_t romv);

rom_data_ht runtime_data(runtime_rom_name_t name);

std::string to_string(runtime_ram_name_t name);
std::string to_string(runtime_rom_name_t name);

std::ostream& operator<<(std::ostream& o, runtime_ram_name_t name);
std::ostream& operator<<(std::ostream& o, runtime_rom_name_t name);

#endif
