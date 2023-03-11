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
constexpr unsigned SSAF_INDEXES_ARRAY8 = 1 << 7;
constexpr unsigned SSAF_INDEXES_ARRAY16= 1 << 8;
constexpr unsigned SSAF_INDEXES_PTR    = 1 << 9;
constexpr unsigned SSAF_ARRAY_OFFSET   = 1 << 10;
constexpr unsigned SSAF_CG_NEVER_STORE = 1 << 11; // The op has no associated memory during code gen
constexpr unsigned SSAF_NO_GVN         = 1 << 12; // Won't be optimized in GVN pass
constexpr unsigned SSAF_COMMUTATIVE    = 1 << 13; // First two args can be swapped
constexpr unsigned SSAF_BRANCHY_CG     = 1 << 14; // Potentially uses a conditional in code gen
constexpr unsigned SSAF_NULL_INPUT_VALID = 1 << 15; // Can use nulls as input
constexpr unsigned SSAF_FENCE          = 1 << 16; 
constexpr unsigned SSAF_BANK_INPUT     = 1 << 17; 
constexpr unsigned SSAF_CG_UNLIVE      = 1 << 18; // Has no liveness
constexpr unsigned SSAF_PRIO_SCHEDULE  = 1 << 19;
constexpr unsigned SSAF_CONDITIONAL    = 1 << 20;
constexpr unsigned SSAF_CHEAP_SCHEDULE = 1 << 21;
constexpr unsigned SSAF_FREE           = 1 << 22;
constexpr unsigned SSAF_EXPENSIVE      = 1 << 23;

// Parameter indexes for SSA ops
namespace ssai
{
    namespace rw_ptr // also applies to read_hw and write_hw
    {
        //constexpr unsigned ORDER_AFTER = 0;
        constexpr unsigned PTR         = 0;
        constexpr unsigned PTR_HI      = 1;
        constexpr unsigned BANK        = 2;
        constexpr unsigned INDEX       = 3;
        constexpr unsigned ASSIGNMENT  = 4;
    }

    namespace array
    {
        constexpr unsigned ARRAY        = 0;
        constexpr unsigned OFFSET       = 1;
        constexpr unsigned INDEX_HI     = 1;
        constexpr unsigned INDEX        = 2;
        constexpr unsigned ASSIGNMENT   = 3;
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

inline unsigned write_globals_begin(ssa_op_t op)
{
    assert(ssa_flags(op) & SSAF_WRITE_GLOBALS);
    switch(op)
    {
    case SSA_wait_nmi:
    case SSA_fence:
    case SSA_return: 
        return 0;
    case SSA_cli: 
        return 1;
    case SSA_fn_call: return 2;
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

constexpr bool ssa_indexes8(ssa_op_t op)
{
    constexpr unsigned SSAFS_INDEXES = SSAF_INDEXES_ARRAY8 | SSAF_INDEXES_PTR;
    return ssa_flags(op) & SSAFS_INDEXES;
}

constexpr unsigned ssa_index8_input(ssa_op_t op)
{
    assert(ssa_indexes8(op));
    switch(op)
    {
    case SSA_read_array8:
    case SSA_cg_read_array8_direct:
    case SSA_write_array8:
        return 2;
    case SSA_read_ptr:
    case SSA_read_ptr_hw:
    case SSA_write_ptr:
    case SSA_write_ptr_hw:
        return 3;
    default:
        assert(false);
        return ~0;
    }
}

constexpr bool ssa_banks(ssa_op_t op)
{
    return ssa_flags(op) & SSAF_BANK_INPUT;
}

constexpr unsigned ssa_bank_input(ssa_op_t op)
{
    assert(ssa_flags(op) & SSAF_BANK_INPUT);
    switch(op)
    {
    case SSA_read_ptr:
    case SSA_read_ptr_hw:
    case SSA_write_ptr:
    case SSA_write_ptr_hw:
        return ssai::rw_ptr::BANK;
    default:
        assert(false);
        return ~0;
    }
}

// Returns an offset to the first CFG output representing a case.
constexpr unsigned ssa_switch_cases(ssa_op_t op)
{
    switch(op)
    {
    case SSA_switch_full:
        return 0;
    case SSA_switch_partial:
        return 1;
    default:
        assert(false);
        return ~0;
    }
}

constexpr bool is_switch(ssa_op_t op) 
    { return op == SSA_switch_full || op == SSA_switch_partial; }

template<bool Possible>
constexpr int carry_input_i_impl(ssa_op_t op)
{
    switch(op)
    {
    case SSA_rol:
    case SSA_ror:
        return 1;
    case SSA_add:
    case SSA_sub:
        return 2;
    case SSA_if:
        return Possible ? 0 : -1;
    default:
        return -1;
    }
}

constexpr int carry_input_i(ssa_op_t op) { return carry_input_i_impl<false>(op); }
constexpr int possible_carry_input_i(ssa_op_t op) { return carry_input_i_impl<true>(op); }

constexpr bool is_basic_comparison(ssa_op_t op)
{
    switch(op)
    {
    case SSA_eq:
    case SSA_not_eq:
    case SSA_lt:
    case SSA_lte:
        return true;
    default:
        return false;
    }
}

#endif
