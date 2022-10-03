#ifndef SWITCH_HPP
#define SWITCH_HPP

#include <vector>

#include "robin/map.hpp"

#include "ir_decl.hpp"
#include "locator.hpp"

class ir_t;
class ssa_node_t;

// Converts a SSA_switch_partial to a SSA_switch_full,
// also updating the CFG by removing the default case.
// Return 'true' on success.
bool switch_partial_to_full(ssa_node_t& switch_node);

// Converts every SSA_switch_partial node to SSA_switch_full.
// Return 'true' if any node updated.
bool switch_partial_to_full(ir_t& ir);

using switch_table_t = std::vector<locator_t>;

#endif
