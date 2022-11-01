#ifndef O_DEFORK_HPP
#define O_DEFORK_HPP

#include "debug_print.hpp"
#include "ir_decl.hpp"

// Removes forking control flow, when all outputs lead to the same node.
bool o_defork(log_t* log, ir_t& ir);

// Turns single 'SSA_if' CFG nodes into many, removing phis.
bool o_fork(log_t* log, ir_t& ir);

#endif
