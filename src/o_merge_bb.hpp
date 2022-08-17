#ifndef O_MERGE_BB_HPP
#define O_MERGE_BB_HPP

#include "debug_print.hpp"
#include "ir_decl.hpp"

// Combines chained CFG blocks together.
bool o_merge_basic_blocks(log_t* log, ir_t& ir);

#endif
