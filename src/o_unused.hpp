#ifndef O_UNUSED_HPP
#define O_UNUSED_HPP

#include "ir_decl.hpp"
#include "debug_print.hpp"

// Removes SSA nodes that have no observable effect.
bool o_remove_no_effect(log_t* log, ir_t& ir);

// Removes
bool o_remove_unused_linked(log_t* log, ir_t& ir);

bool o_remove_unused_ssa(log_t* log, ir_t& ir);

#endif
