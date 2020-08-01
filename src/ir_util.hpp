#ifndef IR_UTIL_HPP
#define IR_UTIL_HPP

#include "flat/flat_set.hpp"

#include "ir.hpp"

constexpr unsigned UNVISITED = -1;


struct cfg_util_t
{
    unsigned preorder_i = UNVISITED;
    unsigned postorder_i = UNVISITED;
    cfg_ht idom = {};
    cfg_ht iloop_header = {};

    // Incoming edges with
    std::uint64_t reentry_in = 0;
    std::uint64_t reentry_out = 0;
};

extern std::vector<cfg_util_t> cfg_util_pool;
extern std::vector<cfg_ht> postorder;
extern std::vector<cfg_ht> preorder;
extern fc::vector_set<cfg_ht> loop_headers;

// Fills 'postorder' and 'preorder' and sets 'postorder_i' and 'preorder_i'.
// Also sets 'succ_edge_types', hurray!
void build_order(ir_t const& ir);

// Does everything 'build_order' does, but also identifies loops.
void build_loops_and_order(ir_t& ir);

// Builds the dominance tree.
// Requires that the order was built.
void build_dominators_from_order(ir_t& ir);

#endif
