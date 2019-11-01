#ifndef O_ABSTRACT_INTERPRET_HPP
#define O_ABSTRACT_INTERPRET_HPP

#include <cstdint>

#include "flat/flat_map.hpp"

#include "constraints.hpp"

class ir_t;
class ssa_node_t;
class ssa_value_t;

struct ai_cfg_t
{
    // This bitset tracks if the abstract interpreter has executed the given 
    // output edge. It will point to 'flags' for nodes with <= 2 outputs.
    std::uint64_t* out_executable;
    
    // Used in branch threading to track the path.
    unsigned input_taken;

    // Used to rebuild the SSA after inserting trace nodes.
    using rebuild_map_t = fc::vector_map<ssa_node_t*, ssa_node_t*>;
    rebuild_map_t* rebuild_map;
};

struct ai_ssa_t
{
    // The imprecise set of all values this node can have during runtime.
    constraints_t* active_constraints;
    constraints_t constraints;

    // If this node is a key to 'ai_cfg_data_t::rebuild_map', this pointer
    // holds the mapped value.
    // i.e. it holds the original value.
    ssa_node_t* rebuild_mapping;

    // How many times this node has been visited by the abstract interpreter.
    // This is used to determine when to widen.
    unsigned visited_count;

    /* TODO
    void set_active_constraints(executable_index_t index)
        { active_constraints = &constraints_data[index]; }
        */
};

inline void o_abstract_interpret(ir_t& ir) {}

#endif
