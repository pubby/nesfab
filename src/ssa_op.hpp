#ifndef SSA_OP_HPP
#define SSA_OP_HPP

#include <array>
#include <cassert>
#include <ostream>
#include <string_view>

// SSA flags:
constexpr unsigned SSAF_TRACE_INPUTS   = 1 << 0;
constexpr unsigned SSAF_COPY           = 1 << 1;
constexpr unsigned SSAF_CLOBBERS_CARRY = 1 << 2;
constexpr unsigned SSAF_WRITE_GLOBALS  = 1 << 3;
constexpr unsigned SSAF_IO_IMPURE      = 1 << 4;
constexpr unsigned SSAF_WRITE_ARRAY    = 1 << 5; // Modifies an array in arg0
constexpr unsigned SSAF_READ_ARRAY     = 1 << 6;
constexpr unsigned SSAF_INDEXES_ARRAY  = 1 << 7;
constexpr unsigned SSAF_INDEXES_PTR    = 1 << 8;
constexpr unsigned SSAF_CG_NEVER_STORE = 1 << 9; // The op has no associated memory during code gen
constexpr unsigned SSAF_NO_GVN         = 1 << 10; // Won't be optimized in GVN pass
constexpr unsigned SSAF_COMMUTATIVE    = 1 << 11; // First two args can be swapped
constexpr unsigned SSAF_BRANCHY_CG     = 1 << 12; // Potentially uses a conditional in code gen
constexpr unsigned SSAF_NULL_INPUT_VALID = 1 << 13; // Can use nulls as input

// Parameter indexes for SSA ops
namespace ssai
{
    namespace rw_ptr
    {
        constexpr unsigned ORDER_AFTER = 0;
        constexpr unsigned PTR         = 1;
        constexpr unsigned PTR_HI      = 2;
        constexpr unsigned BANK        = 3;
        constexpr unsigned INDEX       = 4;
        constexpr unsigned ASSIGNMENT  = 5;
    }
}

enum input_class_t
{
    INPUT_NONE,
    INPUT_VALUE,
    INPUT_LINK,  // Used when multiple nodes behave like one node
    INPUT_TRACE,
};

constexpr bool provides_ordering(input_class_t ic)
{
    switch(ic)
    {
    case INPUT_NONE:
        return false;
    default:
        return true;
    }
}

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

constexpr bool fn_like(ssa_op_t op) { return op == SSA_fn_call || op == SSA_goto_mode; }

constexpr bool is_make_ptr(ssa_op_t op) { return op == SSA_make_ptr_lo || op == SSA_make_ptr_hi; }

constexpr bool ssa_indexes(ssa_op_t op)
{
    constexpr unsigned SSAFS_INDEXES = SSAF_INDEXES_ARRAY | SSAF_INDEXES_PTR;
    return ssa_flags(op) & SSAFS_INDEXES;
}

inline unsigned write_globals_begin(ssa_op_t op)
{
    assert(ssa_flags(op) & SSAF_WRITE_GLOBALS);
    switch(op)
    {
    case SSA_return: return 0;
    case SSA_fn_call: return 1;
    case SSA_goto_mode: return 2;
    default: assert(false); return 0;
    }
}

inline unsigned ssa_copy_input(ssa_op_t op)
{
    assert(ssa_flags(op) & SSAF_COPY);
    switch(op)
    {
    case SSA_make_ptr_lo:
    case SSA_make_ptr_hi:
        return 1;
    case SSA_early_store:
    case SSA_aliased_store:
    case SSA_phi_copy:
        return 0;
    default:
        assert(false);
        return ~0;
    }
}

inline unsigned ssa_index_input(ssa_op_t op)
{
    assert(ssa_indexes(op));
    switch(op)
    {
    case SSA_read_array:
    case SSA_cg_read_array_direct:
    case SSA_write_array:
        return 2;
    case SSA_read_ptr:
    case SSA_write_ptr:
        return 3;
    default:
        assert(false);
        return ~0;
    }
}

#endif
