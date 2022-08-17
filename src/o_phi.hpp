#ifndef O_PHI_HPP
#define O_PHI_HPP

#include <functional>

#include "ir_decl.hpp"
#include "debug_print.hpp"

ssa_value_t get_trivial_phi_value(log_t* log, ssa_node_t const& node);
bool o_remove_trivial_phis(log_t* log, ir_t& ir);
bool o_remove_redundant_phis(log_t* log, ir_t& ir);
bool o_phis(log_t* log, ir_t& ir);

#endif
