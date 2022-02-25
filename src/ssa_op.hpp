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
constexpr unsigned SSAF_ARG0_ORDERS    = 1 << 5; // Behaves like INPUT_ORDER
constexpr unsigned SSAF_WRITE_ARRAY    = 1 << 6;
constexpr unsigned SSAF_READ_ARRAY     = 1 << 7;
constexpr unsigned SSAF_INDEXES_ARRAY  = 1 << 8;
constexpr unsigned SSAF_CG_NEVER_STORE = 1 << 9; // The op has no associated memory during code gen
constexpr unsigned SSAF_NO_GVN         = 1 << 10; // Won't be optimized in GVN pass
constexpr unsigned SSAF_COMMUTATIVE    = 1 << 11; // First two args can be swapped

enum input_class_t
{
    INPUT_NONE,
    INPUT_VALUE,
    INPUT_LINK,  // Used when multiple nodes behave like one node
    INPUT_ORDER, // Used when a node must occur after another node, but has no true data dependency.
    INPUT_TRACE,
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
