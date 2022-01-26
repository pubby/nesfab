#ifndef O_UNUSED_HPP
#define O_UNUSED_HPP

#include "ir_decl.hpp"

// Removes SSA nodes that don't have any observable effect.
bool o_remove_no_effect(ir_t& ir);

// Removes
bool o_remove_unused_linked(ir_t& ir);

bool o_remove_unused_ssa(ir_t& ir);

#endif
