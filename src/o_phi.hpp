#ifndef O_PHI_HPP
#define O_PHI_HPP

#include <functional>

#include "ir_decl.hpp"

ssa_value_t get_trivial_phi_value(ssa_node_t const& node);
bool o_remove_trivial_phis(ir_t& ir);
bool o_remove_redundant_phis(ir_t& ir);
bool o_phis(ir_t& ir);

#endif
