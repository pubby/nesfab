#ifndef CG_PTR_HPP
#define CG_PTR_HPP

#include "ir_decl.hpp"
#include "decl.hpp"

class locator_t;

// Moves bank switches out of loops, when possible.
void cg_hoist_bank_switches(ir_t& ir);

// Call after scheduling.
// Sets FLAG_BANK_PRELOADED on SSA nodes that don't need to bankswitch,
// and returns the dominating bankswitch in the IR.
// A.K.A. the optimal bank the function should be in when called.
locator_t cg_calc_bank_switches(fn_ht fn, ir_t& ir);

#endif
