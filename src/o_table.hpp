#ifndef O_TABLE_HPP
#define O_TABLE_HPP

#include "debug_print.hpp"
#include "ir_decl.hpp"

// Replaces certain if statements with 2-element tables.
bool o_bool_tables(log_t* log, ir_t& ir);

// Similar to above, but for switch tables.
bool o_switch_tables(log_t* log, ir_t& ir);

#endif
