#ifndef CG_ISEL_HPP
#define CG_ISEL_HPP

// Instruction selection

#include <array>

#include "asm.hpp"
#include "ir.hpp"

struct sel_t
{
    sel_t const* prev = nullptr;
    op_t op = BAD_OP;
    unsigned cost = 0;
    ssa_value_t arg = {};
};

sel_t const* select_instructions(ssa_ht const* schedule_begin, 
                                 ssa_ht const* schedule_end);

#endif
