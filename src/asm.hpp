#ifndef ASM_HPP
#define ASM_HPP

#include <array>
#include <cassert>
#include <cstdint>
#include <string>

enum addr_mode_t : std::uint8_t
{
#define ADDR_MODE(name) MODE_##name,
#include "addr_mode.inc"
#undef ADDR_MODE
    NUM_ADDR_MODES,
};

enum op_name_t : std::uint8_t
{
#define OP_NAME(name) name,
#include "op_name.inc"
#undef OP_NAME
    NUM_OP_NAMES,
};

enum op_t : std::uint8_t
{
#define OP(name) name,
#include "op.inc"
#undef OP
    NUM_OPS,
};

std::string to_string(addr_mode_t addr_mode);
std::string to_string(op_name_t name);
std::string to_string(op_t op);

using regs_t = std::uint8_t;
constexpr regs_t REG_A   = 0;
constexpr regs_t REG_X   = 1;
constexpr regs_t REG_Y   = 2;
constexpr regs_t REG_C   = 3;
constexpr regs_t REG_Z   = 4;
constexpr unsigned NUM_CPU_REGS = 5;
constexpr regs_t REG_M   = 5; // RAM

// Works like a bitset.
constexpr regs_t REGF_A   = 1 << REG_A;
constexpr regs_t REGF_X   = 1 << REG_X;
constexpr regs_t REGF_Y   = 1 << REG_Y;
constexpr regs_t REGF_C   = 1 << REG_C;
constexpr regs_t REGF_Z   = 1 << REG_Z;
constexpr regs_t REGF_M   = 1 << REG_M;

constexpr regs_t REGF_AX   = REGF_A | REGF_X;
constexpr regs_t REGF_AC   = REGF_A | REGF_C;
constexpr regs_t REGF_CPU = REGF_A | REGF_X | REGF_Y | REGF_C | REGF_Z;

using asm_flags_t = std::uint32_t;

constexpr asm_flags_t ASMF_MAYBE_STORE  = 1 << 0;

struct op_def_t
{
    op_t op;
    op_name_t op_name;
    addr_mode_t addr_mode;
    std::uint8_t op_code;
    std::uint8_t size;
    std::uint8_t cycles;
    regs_t input_regs;
    regs_t output_regs;
    asm_flags_t flags;
};

#include "asm_tables.hpp"

constexpr op_name_t op_name(op_t op)
    { return op_defs_table[op].op_name; }

constexpr std::uint8_t op_code(op_t op)
    { return op_defs_table[op].op_code; }

constexpr addr_mode_t op_addr_mode(op_t op)
    { return op_defs_table[op].addr_mode; }

constexpr unsigned op_cycles(op_t op)
    { return op_defs_table[op].cycles; }

constexpr unsigned op_size(op_t op)
    { return op_defs_table[op].size; }

constexpr regs_t op_input_regs(op_t op)
    { return op_defs_table[op].input_regs; }

constexpr regs_t op_output_regs(op_t op)
    { return op_defs_table[op].output_regs; }

constexpr asm_flags_t op_flags(op_t op)
    { return op_defs_table[op].flags; }

using addr_mode_table_t = std::array<op_t, NUM_ADDR_MODES>;
using op_name_mode_table_t = std::array<addr_mode_table_t, NUM_OP_NAMES>;

constexpr op_name_mode_table_t op_name_mode_table = []() consteval
{
    op_name_mode_table_t ret = {};

    for(unsigned i = 0; i < NUM_OPS; ++i)
    {
        op_t op = (op_t)i;
        ret[op_name(op)][op_addr_mode(op)] = op;
    }

    return ret;
}();

consteval op_t get_op(op_name_t name, addr_mode_t mode)
{
    return op_name_mode_table[name][mode];
}

inline addr_mode_table_t const& get_addr_modes(op_name_t name)
{
    return op_name_mode_table[name];
}

constexpr op_name_t invert_branch(op_name_t name)
{
    switch(name)
    {
    case BPL: return BMI;
    case BMI: return BPL;
    case BEQ: return BNE;
    case BNE: return BEQ;
    case BCC: return BCS;
    case BCS: return BCC;
    case BVC: return BVS;
    case BVS: return BVC;
    default: return BAD_OP_NAME;
    }
}

constexpr bool indirect_addr_mode(addr_mode_t mode)
{
    switch(mode)
    {
    case MODE_INDIRECT:
    case MODE_INDIRECT_X:
    case MODE_INDIRECT_Y:
        return true;
    default: return false;
    }
}

#endif
