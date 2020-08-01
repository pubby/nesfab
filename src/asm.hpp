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

// Works like a bitset.
using regs_t = std::uint8_t;
constexpr regs_t REG_A = 1 << 0;
constexpr regs_t REG_X = 1 << 1;
constexpr regs_t REG_Y = 1 << 2;
constexpr regs_t REG_C = 1 << 3;

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
};

#include "asm_tables.hpp"

constexpr op_name_t op_name(op_t op)
{
    assert(op_defs_table[op].op == op);
    return op_defs_table[op].op_name;
}

constexpr std::uint8_t op_code(op_t op)
{
    assert(op_defs_table[op].op == op);
    return op_defs_table[op].op_code;
}

constexpr addr_mode_t op_addr_mode(op_t op)
{
    assert(op_defs_table[op].op == op);
    return op_defs_table[op].addr_mode;
}

constexpr unsigned op_cycles(op_t op)
{
    assert(op_defs_table[op].op == op);
    return op_defs_table[op].cycles;
}

constexpr unsigned op_size(op_t op)
{
    assert(op_defs_table[op].op == op);
    return op_defs_table[op].size;
}

constexpr regs_t op_input_regs(op_t op)
{
    assert(op_defs_table[op].op == op);
    return op_defs_table[op].input_regs;
}

constexpr regs_t op_output_regs(op_t op)
{
    assert(op_defs_table[op].op == op);
    return op_defs_table[op].output_regs;
}

using addr_mode_table_t = std::array<op_t, NUM_ADDR_MODES>;
using op_name_mode_table_t = std::array<addr_mode_table_t, NUM_OP_NAMES>;

constexpr op_name_mode_table_t op_name_mode_table = []() constexpr
{
    op_name_mode_table_t ret = {};

    for(unsigned i = 0; i < NUM_OPS; ++i)
    {
        op_t op = (op_t)i;
        ret[op_name(op)][op_addr_mode(op)] = op;
    }

    return ret;
}();

constexpr op_t get_op(op_name_t name, addr_mode_t mode)
{
    return op_name_mode_table[name][mode];
}

inline addr_mode_table_t const& get_addr_modes(op_name_t name)
{
    return op_name_mode_table[name];
}

#endif
