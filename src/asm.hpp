#ifndef ASM_HPP
#define ASM_HPP

// Code related to individual assembly instructions.

#include <array>
#include <cassert>
#include <cstdint>
#include <string>
#include <ostream>

#ifdef ISA_SNES
#  define ISA_65C02
#  define LEGAL
#endif

#ifdef ISA_PCE
#  define ISA_65C02
#  define LEGAL
#endif

// ISA flags:
constexpr unsigned ISAF_6502         = (1 << 0);
constexpr unsigned ISAF_6502_ILLEGAL = (1 << 1) | ISAF_6502;
constexpr unsigned ISAF_65C02        = (1 << 2) | ISAF_6502;
constexpr unsigned ISAF_SNES         = (1 << 3) | ISAF_65C02;
constexpr unsigned ISAF_PCE          = (1 << 4) | ISAF_65C02;

constexpr unsigned isa_flags() 
{
#ifndef LEGAL
    return ISAF_6502_ILLEGAL;
#endif

#ifdef ISA_SNES
    return ISAF_SNES;
#endif

#ifdef ISA_PCE
    return ISAF_PCE;
#endif

    return ISAF_6502;
}

// Processor flags:
constexpr std::uint8_t PFLAG_C  = 1 << 0;
constexpr std::uint8_t PFLAG_Z  = 1 << 1;
constexpr std::uint8_t PFLAG_I  = 1 << 2;
constexpr std::uint8_t PFLAG_D  = 1 << 3;
constexpr std::uint8_t PFLAG_BX = 1 << 4;
constexpr std::uint8_t PFLAG_M  = 1 << 5;
constexpr std::uint8_t PFLAG_V  = 1 << 6;
constexpr std::uint8_t PFLAG_N  = 1 << 7;

#ifdef ISA_SNES
using regs_t = std::uint16_t;
constexpr regs_t REG_A    = 0;
constexpr regs_t REG_A_HI = 1;
constexpr regs_t REG_X    = 2;
constexpr regs_t REG_X_HI = 3;
constexpr regs_t REG_Y    = 4;
constexpr regs_t REG_Y_HI = 5;
constexpr regs_t REG_C    = 6;
constexpr regs_t REG_M16  = 7; // 16-bit flag
constexpr regs_t REG_X16  = 8; // 16-bit flag
constexpr regs_t REG_B    = 9; // Data bank
constexpr unsigned NUM_CROSS_REGS = 10; // Registers that propagate across CFG nodes
constexpr regs_t REG_Z    = 10;
constexpr regs_t REG_N    = 11;
constexpr unsigned NUM_KNOWN_REGS = 12; // Registers that isel::cpu_t tracks constants of
constexpr unsigned NUM_ISEL_REGS  = 12; // Registers that isel::cpu_tracks locators of
constexpr regs_t REG_V    = 12;
constexpr unsigned NUM_6502_REGS = 13;
constexpr regs_t REG_M    = 13; // RAM
constexpr unsigned NUM_REGS = 13;

constexpr regs_t REGF_A_HI = 1 << REG_A_HI;
constexpr regs_t REGF_X_HI = 1 << REG_X_HI;
constexpr regs_t REGF_Y_HI = 1 << REG_Y_HI;
constexpr regs_t REGF_M16  = 1 << REG_M16;
constexpr regs_t REGF_X16  = 1 << REG_X16;
constexpr regs_t REGF_B    = 1 << REG_B;
#else
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
#endif

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
#define OP(name, flags) name,
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
constexpr asm_flags_t ASMF_IDEMPOTENT  = 1 << 9;

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

constexpr unsigned op_isa_flags(op_t op)
{
    switch(op)
    {
#define OP(name, flags) case name: return (flags);
#include "op.inc"
#undef OP
    default: return 0;
    }
}

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

constexpr bool op_illegal(op_t op)
    { return op_normal(op) && (op_isa_flags(op) & isa_flags()) == 0; }

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
    case BBR0: return BBS0;
    case BBR1: return BBS1;
    case BBR2: return BBS2;
    case BBR3: return BBS3;
    case BBR4: return BBS4;
    case BBR5: return BBS5;
    case BBR6: return BBS6;
    case BBR7: return BBS7;
    case BBS0: return BBR0;
    case BBS1: return BBR1;
    case BBS2: return BBR2;
    case BBS3: return BBR3;
    case BBS4: return BBR4;
    case BBS5: return BBR5;
    case BBS6: return BBR6;
    case BBS7: return BBR7;
    default: return BAD_OP_NAME;
    }
}

constexpr op_t invert_branch(op_t op)
{
    return get_op(invert_branch(op_name(op)), op_addr_mode(op));

}

constexpr bool direct_addr_mode(addr_mode_t mode)
{
    switch(mode)
    {
    case MODE_ZERO_PAGE:
    case MODE_ABSOLUTE:
#ifdef ISA_SNES
    case MODE_LONG:
#endif
        return true;
    default: 
        return false;
    }
}

constexpr bool indirect_addr_mode(addr_mode_t mode)
{
    switch(mode)
    {
    case MODE_INDIRECT:
    case MODE_INDIRECT_X:
    case MODE_INDIRECT_Y:
#ifdef ISA_65C02
    case MODE_INDIRECT_0:
#endif
#ifdef ISA_SNES
    case MODE_INDIRECT_0_LONG:
    case MODE_INDIRECT_LONG:
    case MODE_INDIRECT_Y_LONG:
    case MODE_INDIRECT_STACK_S_Y:
#endif
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
#ifdef ISA_SNES
    case MODE_LONG_X:
    case MODE_INDIRECT_Y_LONG:
#endif
#ifdef ISA_PCE
    case MODE_IMMEDIATE_ZERO_PAGE_X:
    case MODE_IMMEDIATE_ABSOLUTE_X:
#endif
        return true;
    default: 
        return false;
    }
}

constexpr bool zp_addr_mode(addr_mode_t mode, bool indirect = false)
{
    switch(mode)
    {
    case MODE_ZERO_PAGE:
    case MODE_ZERO_PAGE_X:
    case MODE_ZERO_PAGE_Y:
        return true;
    case MODE_INDIRECT_X:
    case MODE_INDIRECT_Y:
#ifdef ISA_PCE
    case MODE_IMMEDIATE_ZERO_PAGE_X:
    case MODE_ZERO_PAGE_RELATIVE:
#endif
        return indirect;
    default: 
        return false;
    }
}

// References 'simple_op' in cg_isel.cpp:
constexpr bool simple_addr_mode(addr_mode_t mode)
{
    switch(mode)
    {
    case MODE_IMPLIED:
    case MODE_RELATIVE:
    case MODE_IMMEDIATE:
    case MODE_BAD:
#ifdef ISA_SNES
    case MODE_IMMEDIATE_16:
    case MODE_RELATIVE_16:
    case MODE_IMMEDIATE_IMMEDIATE:
#endif
#ifdef ISA_PCE
    case MODE_BLOCK_MOVE:
#endif
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
#ifdef ISA_SNES
    case MODE_LONG: return MODE_ZERO_PAGE;
    case MODE_LONG_X: return MODE_ZERO_PAGE_X;
#endif
#ifdef ISA_PCE
    case MODE_IMMEDIATE_ABSOLUTE: return MODE_IMMEDIATE_ZERO_PAGE;
    case MODE_IMMEDIATE_ABSOLUTE_X: return MODE_IMMEDIATE_ZERO_PAGE_X;
#endif
    }
}

constexpr bool is_bbr(op_name_t op) { return op >= BBR0 && op <= BBR7; }
constexpr bool is_bbs(op_name_t op) { return op >= BBS0 && op <= BBS7; }

constexpr unsigned bbr_bit(op_name_t op) { assert(is_bbr(op)); return op - BBR0; }
constexpr unsigned bbs_bit(op_name_t op) { assert(is_bbs(op)); return op - BBS0; }

constexpr std::uint8_t bbr_mask(op_name_t op) { return 1 << bbr_bit(op); }
constexpr std::uint8_t bbs_mask(op_name_t op) { return 1 << bbs_bit(op); }

constexpr addr_mode_t buggy_equivalent(addr_mode_t mode)
{
    switch(mode)
    {
    default: return MODE_BAD;
    case MODE_IMMEDIATE: return MODE_BUGGY_IMMEDIATE;
    }
}

constexpr op_t tail_call_op(op_t op)
{
    switch(op)
    {
    case JSR_ABSOLUTE: return JMP_ABSOLUTE;
    case JSR_INDIRECT: return JMP_INDIRECT;
    case BANKED_X_JSR: return BANKED_X_JMP;
    case BANKED_Y_JSR: return BANKED_Y_JMP;
    case BANKED_JSR: return BANKED_JMP;
    default: return BAD_OP;
    }
}

constexpr op_t unbanked_call_op(op_t op)
{
    switch(op)
    {
    case BANKED_X_JSR:
    case BANKED_Y_JSR:
    case BANKED_JSR:
        return JSR_ABSOLUTE;
    case BANKED_X_JMP:
    case BANKED_Y_JMP:
    case BANKED_JMP: 
        return JMP_ABSOLUTE;
    default: 
        return BAD_OP;
    }
}

constexpr op_t banked_call_load_op(op_t op)
{
    switch(op)
    {
    case BANKED_X_JSR:
    case BANKED_X_JMP:
        return LDX_IMMEDIATE;
    case BANKED_Y_JSR:
    case BANKED_Y_JMP:
        return LDY_IMMEDIATE;
    default: 
        return BAD_OP;
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

constexpr bool is_maybe_relative_branch(op_t op)
{
    return is_branch(op) && op_addr_mode(op) == MODE_MAYBE_RELATIVE;
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
#undef FAST
    }
}

constexpr bool is_simple_load(op_name_t name)
{
    switch(name)
    {
    case LDA:
    case LDX:
    case LDY:
    case LAX:
        return true;
    default:
        return false;
    }
}

constexpr bool is_simple_store(op_name_t name)
{
    switch(name)
    {
    case STA:
    case STX:
    case STY:
    case STZ:
        return true;
    default:
        return false;
    }
}

#endif
