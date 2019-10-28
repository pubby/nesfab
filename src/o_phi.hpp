#ifndef O_PHI_HPP
#define O_PHI_HPP

#include <functional>

class ir_t;
class ssa_node_t;
class ssa_value_t;

struct phi_ssa_t
{
    unsigned index;
    unsigned low_link;
    unsigned subgraph_i;
};

ssa_value_t get_trivial_phi_value(ssa_node_t const& node);
void o_remove_trivial_phis(ir_t& ir);
void o_remove_redundant_phis(ir_t& ir);
void o_optimize_phis(ir_t& ir);

#endif
