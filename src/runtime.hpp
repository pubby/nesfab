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
class asm_proc_t;

#define RTRAM_X \
RT(ptr_temp) \
RT(nmi_index) \
RT(nmi_saved_x) \
RT(nmi_saved_y) \
RT(nmi_saved_bank) \
RT(nmi_counter) \
RT(nmi_ready) \
RT(irq_index) \
RT(irq_saved_x) \
RT(irq_saved_y) \
RT(irq_saved_bank) \
RT(mapper_state) \
RT(mapper_detail) \
RT(system) \

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
RT(irq_exit) \
RT(irq_lo_table) \
RT(irq_hi_table) \
RT(irq_bank_table) \
RT(reset) \
RT(jmp_y_trampoline) \
RT(jsr_y_trampoline) \
RT(iota) \
RT(mul8) \
RT(mapper_reset) \
RT(shl4_table) \
RT(shl5_table) \
RT(shl6_table) \
RT(shl7_table)

constexpr int MIN_SHL_TABLE = 4;
constexpr int MAX_SHL_TABLE = 7;

enum runtime_rom_name_t : std::uint16_t
{
#define RT(name) RTROM_##name,
RTROM_X
#undef RT
    NUM_RTROM,
};

inline runtime_rom_name_t shl_table(int amount) 
{
    switch(amount)
    {
    default: throw std::runtime_error("Missing shift table: " + std::to_string(amount));
    case 4: return RTROM_shl4_table;
    case 5: return RTROM_shl5_table;
    case 6: return RTROM_shl6_table;
    case 7: return RTROM_shl7_table;
    }
}

ram_bitset_t alloc_runtime_ram();
span_allocator_t alloc_runtime_rom();

std::array<std::array<span_t, NUM_ROMV>, NUM_RTRAM> const& rtram_spans();
std::array<std::array<span_t, NUM_ROMV>, NUM_RTROM> const& rtrom_spans();

span_t runtime_span(runtime_ram_name_t name, romv_t romv);
span_t runtime_span(runtime_rom_name_t name, romv_t romv);

rom_data_ht runtime_data(runtime_rom_name_t name);

extern rom_proc_ht reset_proc;
void create_reset_proc();
void set_reset_proc();

std::string to_string(runtime_ram_name_t name);
std::string to_string(runtime_rom_name_t name);

std::ostream& operator<<(std::ostream& o, runtime_ram_name_t name);
std::ostream& operator<<(std::ostream& o, runtime_rom_name_t name);

unsigned bankswitch_a(asm_proc_t& proc, unsigned next_label, bool x = false);
unsigned bankswitch_x(asm_proc_t& proc, unsigned next_label);
unsigned bankswitch_y(asm_proc_t& proc, unsigned next_label);

#endif
