#ifndef SSA_OP_HPP
#define SSA_OP_HPP

#include <array>
#include <ostream>
#include <string_view>

#define SSA_VERSION_NUMBER 1
#define SSA_VERSION(V) \
    static_assert(SSA_VERSION_NUMBER == (V), "SSA version mismatch.")

// SSA flags:
constexpr unsigned SSAF_TRACE_INPUTS   = 1 << 0;
constexpr unsigned SSAF_COPY           = 1 << 1;
constexpr unsigned SSAF_CLOBBERS_CARRY = 1 << 2;
constexpr unsigned SSAF_WRITE_GLOBALS  = 1 << 3;
constexpr unsigned SSAF_IMPURE         = 1 << 4;

enum input_class_t
{
    INPUT_NONE,
    INPUT_VALUE,
    INPUT_CARRY,
    INPUT_LINK,
};

enum ssa_op_t : short
{
#define SSA_DEF(x, ...) SSA_##x,
#include "ssa_op.inc"
    NUM_SSA_OPS,
};


constexpr std::array<signed char, NUM_SSA_OPS> const ssa_argn_table =
{{
#define SSA_DEF(x, argn, class, flags) argn,
#include "ssa_op.inc"
}};

constexpr std::array<input_class_t, NUM_SSA_OPS> const ssa_input0_class_table =
{{
#define SSA_DEF(x, argn, class, flags) class,
#include "ssa_op.inc"
}};

constexpr std::array<unsigned, NUM_SSA_OPS> const ssa_flags_table =
{{
#define SSA_DEF(x, argn, class, flags) flags,
#include "ssa_op.inc"
}};

constexpr unsigned ssa_argn(ssa_op_t op)
    { return ssa_argn_table[op]; }

constexpr input_class_t ssa_input0_class(ssa_op_t op) 
    { return ssa_input0_class_table[op]; }

constexpr unsigned ssa_flags(ssa_op_t op)
    { return ssa_flags_table[op]; }

std::string_view to_string(ssa_op_t node_type);
std::ostream& operator<<(std::ostream& o, ssa_op_t node_type);

#endif
