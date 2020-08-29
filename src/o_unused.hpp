#ifndef O_UNUSED_HPP
#define O_UNUSED_HPP

#include "ir_decl.hpp"

// Removes SSA nodes that aren't used anywhere in the IR,
// and don't have any observable effect.
bool o_remove_unused_ssa(ir_t& ir);

#endif
