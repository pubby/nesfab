#ifndef O_SHIFT_HPP
#define O_SHIFT_HPP

#include "debug_print.hpp"
#include "ir_decl.hpp"

// Replaces certain shifts with table lookups.
bool o_shl_tables(log_t* log, ir_t& ir);

#endif
