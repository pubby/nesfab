#ifndef ASM_HPP
#define ASM_HPP

// Code related to individual assembly instructions.

#include <array>
#include <cassert>
#include <cstdint>
#include <string>
#include <ostream>

using regs_t = std::uint8_t;
constexpr regs_t REG_A   = 0;
constexpr regs_t REG_X   = 1;
constexpr regs_t REG_Y   = 2;
constexpr regs_t REG_C   = 3;
constexpr unsigned NUM_CROSS_REGS = 4; // Registers that propagate across CFG nodes
constexpr regs_t REG_Z   = 4;
constexpr regs_t REG_N   = 5;
constexpr unsigned NUM_KNOWN_REGS = 6; // Registers that isel::cpu_t tracks constants of
constexpr unsigned NUM_ISEL_REGS = 6; // Registers that isel::cpu_tracks locators of
constexpr regs_t REG_V   = 6;
constexpr unsigned NUM_6502_REGS = 7;
constexpr regs_t REG_M   = 7; // RAM
constexpr unsigned NUM_REGS = 8;

// Works like a bitset.
constexpr regs_t REGF_A = 1 << REG_A;
constexpr regs_t REGF_X = 1 << REG_X;
constexpr regs_t REGF_Y = 1 << REG_Y;
constexpr regs_t REGF_C = 1 << REG_C;
constexpr regs_t REGF_Z = 1 << REG_Z;
constexpr regs_t REGF_N = 1 << REG_N;
constexpr regs_t REGF_V = 1 << REG_V;
constexpr regs_t REGF_M = 1 << REG_M;

constexpr regs_t REGF_AX   = REGF_A | REGF_X;
constexpr regs_t REGF_AY   = REGF_A | REGF_Y;
constexpr regs_t REGF_AC   = REGF_A | REGF_C;
constexpr regs_t REGF_NZ   = REGF_N | REGF_Z;
constexpr regs_t REGF_ACNZ = REGF_A | REGF_C | REGF_N | REGF_Z;
constexpr regs_t REGF_CNZ  = REGF_C | REGF_NZ;
constexpr regs_t REGF_CROSS = REGF_A | REGF_X | REGF_Y | REGF_C;
constexpr regs_t REGF_ISEL = REGF_A | REGF_X | REGF_Y | REGF_C | REGF_N | REGF_Z;
constexpr regs_t REGF_6502 = REGF_A | REGF_X | REGF_Y | REGF_C | REGF_N | REGF_Z | REGF_V;

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

enum op_t : std::uint16_t
{
#define OP(name) name,
#include "op.inc"
#undef OP
    NUM_NORMAL_OPS,
    BEGIN_REG_READ_OP = NUM_NORMAL_OPS,
    END_REG_READ_OP = BEGIN_REG_READ_OP + (1 << NUM_REGS),
    BEGIN_REG_WRITE_OP = END_REG_READ_OP,
    END_REG_WRITE_OP = BEGIN_REG_WRITE_OP + (1 << NUM_REGS),
    NUM_OPS = END_REG_WRITE_OP,
};

std::string to_string(addr_mode_t addr_mode);
std::string to_string(op_name_t name);
std::string to_string(op_t op);

std::ostream& operator<<(std::ostream&, addr_mode_t addr_mode);
std::ostream& operator<<(std::ostream&, op_name_t name);
std::ostream& operator<<(std::ostream&, op_t op);

using asm_flags_t = std::uint32_t;

constexpr asm_flags_t ASMF_FAKE        = 1 << 0;
constexpr asm_flags_t ASMF_MAYBE_STORE = 1 << 1;
constexpr asm_flags_t ASMF_BRANCH      = 1 << 2;
constexpr asm_flags_t ASMF_JUMP        = 1 << 3;
constexpr asm_flags_t ASMF_CALL        = 1 << 4;
constexpr asm_flags_t ASMF_RETURN      = 1 << 5;
constexpr asm_flags_t ASMF_SWITCH      = 1 << 6;
constexpr asm_flags_t ASMF_IMPURE      = 1 << 7;
constexpr asm_flags_t ASMF_FENCE       = 1 << 8;

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

constexpr bool op_normal(op_t op)
    { return op < NUM_NORMAL_OPS; }

constexpr op_name_t op_name(op_t op)
    { return op < NUM_NORMAL_OPS ? op_defs_table[op].op_name : BAD_OP_NAME; }

constexpr std::uint8_t op_code(op_t op)
    { return op < NUM_NORMAL_OPS ? op_defs_table[op].op_code : 0; }

constexpr addr_mode_t op_addr_mode(op_t op)
    { return op < NUM_NORMAL_OPS ? op_defs_table[op].addr_mode : addr_mode_t{}; }

constexpr unsigned op_cycles(op_t op)
    { return op < NUM_NORMAL_OPS ? op_defs_table[op].cycles : 0; }

constexpr unsigned op_size(op_t op)
    { return op < NUM_NORMAL_OPS ? op_defs_table[op].size : 0; }

constexpr regs_t op_input_regs(op_t op)
{ 
    if(op < NUM_NORMAL_OPS)
        return op_defs_table[op].input_regs;
    if(op >= BEGIN_REG_READ_OP && op < END_REG_READ_OP)
        return op - BEGIN_REG_READ_OP;
    return 0;
}

constexpr regs_t op_output_regs(op_t op)
{ 
    if(op < NUM_NORMAL_OPS)
        return op_defs_table[op].output_regs;
    if(op >= BEGIN_REG_WRITE_OP && op < END_REG_WRITE_OP)
        return op - BEGIN_REG_WRITE_OP;
    return 0;
}

constexpr op_t read_reg_op(regs_t regs)
    { return op_t(BEGIN_REG_READ_OP + regs); }
constexpr op_t write_reg_op(regs_t regs)
    { return op_t(BEGIN_REG_WRITE_OP + regs); }

constexpr regs_t op_regs(op_t op)
    { return op_input_regs(op) | op_output_regs(op); }

constexpr asm_flags_t op_flags(op_t op)
    { return op < NUM_NORMAL_OPS ? op_defs_table[op].flags : 0; }

using addr_mode_table_t = std::array<op_t, NUM_ADDR_MODES>;
using op_name_mode_table_t = std::array<addr_mode_table_t, NUM_OP_NAMES>;

constexpr op_name_mode_table_t op_name_mode_table = []() consteval
{
    op_name_mode_table_t ret = {};

    for(unsigned i = 0; i < NUM_OPS; ++i)
    {
        op_t op = (op_t)i;

        if(op_name(op) && op_addr_mode(op))
            ret[op_name(op)][op_addr_mode(op)] = op;
    }

    return ret;
}();

constexpr op_t get_op(op_name_t name, addr_mode_t mode)
{ 
    assert(name < op_name_mode_table.size()); 
    assert(mode < op_name_mode_table[name].size()); 
    return op_name_mode_table[name][mode]; 
}

inline addr_mode_table_t const& get_addr_modes(op_name_t name)
    { assert(name < op_name_mode_table.size()); return op_name_mode_table[name]; }

constexpr op_t change_addr_mode(op_t op, addr_mode_t mode)
    { return get_op(op_name(op), mode); }

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

constexpr op_t invert_branch(op_t op)
{
    return get_op(invert_branch(op_name(op)), op_addr_mode(op));

}

constexpr bool indirect_addr_mode(addr_mode_t mode)
{
    switch(mode)
    {
    case MODE_INDIRECT:
    case MODE_INDIRECT_X:
    case MODE_INDIRECT_Y:
        return true;
    default: 
        return false;
    }
}

constexpr bool xy_addr_mode(addr_mode_t mode)
{
    switch(mode)
    {
    case MODE_ZERO_PAGE_X:
    case MODE_ZERO_PAGE_Y:
    case MODE_ABSOLUTE_X:
    case MODE_ABSOLUTE_Y:
    case MODE_INDIRECT_X:
    case MODE_INDIRECT_Y:
        return true;
    default: 
        return false;
    }
}

constexpr addr_mode_t zp_equivalent(addr_mode_t mode)
{
    switch(mode)
    {
    default: return MODE_BAD;
    case MODE_ABSOLUTE: return MODE_ZERO_PAGE;
    case MODE_ABSOLUTE_X: return MODE_ZERO_PAGE_X;
    case MODE_ABSOLUTE_Y: return MODE_ZERO_PAGE_Y;
    }
}

constexpr op_t tail_call_op(op_t op)
{
    switch(op)
    {
    case JSR_ABSOLUTE: return JMP_ABSOLUTE;
    case BANKED_X_JSR: return BANKED_X_JMP;
    case BANKED_Y_JSR: return BANKED_Y_JMP;
    case BANKED_JSR: return BANKED_JMP;
    default: return BAD_OP;
    }
}

constexpr bool is_branch(op_t op) 
{ 
    return op_flags(op) & ASMF_BRANCH; 
}

constexpr bool is_relative_branch(op_t op)
{
    return is_branch(op) && op_addr_mode(op) == MODE_RELATIVE;
}

constexpr bool is_long_branch(op_t op)
{
    return is_branch(op) && op_addr_mode(op) == MODE_LONG;
}

constexpr op_t fast_op(op_t op)
{
    switch(op)
    {
    default: return BAD_OP;
#define FAST(x) case x: return x##_FAST;
    FAST(MAYBE_STORE_C)
    FAST(STORE_C_ABSOLUTE)
    FAST(MAYBE_STORE_N)
    FAST(STORE_N_ABSOLUTE)
    FAST(MAYBE_STORE_Z)
    FAST(STORE_Z_ABSOLUTE)
    }
}

#endif
