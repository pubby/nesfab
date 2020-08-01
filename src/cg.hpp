#ifndef CG_HPP
#define CG_HPP

// Code-gen related code.

#include <vector>

#include "ir.hpp"

using schedule_t = std::vector<ssa_ht>;

struct ssa_cg_d
{
    ssa_value_t set_head = {}; // Basically a union-find pointer.
    ssa_ht next = {}; // A linked-list to the next node
    int index = 0;

    // Used to sequentialize parallel copies:
    ssa_ht loc = {};
    ssa_ht pred = {};
};

struct cfg_cg_d
{
    schedule_t schedule;

    // Bitsets
    unsigned* live_in;
    unsigned* live_out; // Also used to hold the 'KILL' set temporarily.

    // Holds parallel copies introduced to handle phi nodes.
    std::vector<ssa_ht> entry_copies;
    std::vector<ssa_ht> exit_copies;
};

#endif
