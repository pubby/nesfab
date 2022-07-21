#ifndef CG_CSET_HPP
#define CG_CSET_HPP

#include <vector>

#include "decl.hpp"
#include "ir_decl.hpp"
#include "cg.hpp"

// Nodes will be partitioned into congruence classes for coalescing
// purposes, dubbed "cset" for brevity.
// These are implemented as union-find on top of a singly-linked list.
// Below are some helper functions.

locator_t asm_arg(ssa_value_t v);

bool cset_is_head(ssa_ht h);
bool cset_is_last(ssa_ht h);
ssa_ht cset_next(ssa_ht h);

ssa_ht cset_head(ssa_ht h);

locator_t cset_locator(ssa_ht h, bool convert_ssa = false);

bool cset_locators_mergable(locator_t a, locator_t b);

void cset_remove(ssa_ht h);

ssa_ht cset_append(ssa_value_t last, ssa_ht h);

bool fn_interferes(fn_ht fn, ir_t const& ir, locator_t loc, ssa_ht fn_node);

ssa_ht csets_dont_interfere(fn_ht fn, ir_t const& ir, ssa_ht a, ssa_ht b, std::vector<ssa_ht> const& fn_nodes);
ssa_ht csets_appendable(fn_ht fn, ir_t const& ir, ssa_ht a, ssa_ht b, std::vector<ssa_ht> const& fn_nodes);

bool cset_live_at_any_def(ssa_ht a, ssa_ht const* b_begin, ssa_ht const* b_end);

#endif
