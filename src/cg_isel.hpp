#ifndef CG_ISEL_HPP
#define CG_ISEL_HPP

// Instruction selection

#include <array>

#include "asm.hpp"
#include "cg.hpp"
#include "ir.hpp"

struct sel_t
{
    sel_t(sel_t const* prev, unsigned cost, cg_inst_t inst)
    : prev(prev)
    , cost(cost)
    , inst(inst)
    {}

    sel_t const* prev = nullptr;
    unsigned cost = 0;
    cg_inst_t inst = {};
};

std::vector<cg_inst_t> select_instructions(cfg_ht cfg_node);

#endif
