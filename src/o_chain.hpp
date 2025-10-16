#ifndef O_CHAIN_HPP
#define O_CHAIN_HPP

#include "debug_print.hpp"
#include "ir_decl.hpp"

class ir_t;

// Chains together increments and shifts.
bool o_chain(log_t* log, ir_t& ir);

#endif
