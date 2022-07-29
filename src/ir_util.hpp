#ifndef IR_UTIL_HPP
#define IR_UTIL_HPP

#include <vector>

#include "ir_decl.hpp"

constexpr unsigned UNVISITED = -1;

struct cfg_util_d
{
    unsigned preorder_i = UNVISITED;
    unsigned postorder_i = UNVISITED;
    cfg_ht idom = {};
    cfg_ht iloop_header = {};
    bool is_loop_header = false;

    // Incoming edges with
    std::uint64_t reentry_in = 0;
    std::uint64_t reentry_out = 0;
};

extern thread_local std::vector<cfg_util_d> cfg_util_pool;
extern thread_local std::vector<cfg_ht> postorder;
extern thread_local std::vector<cfg_ht> preorder;
extern thread_local std::vector<cfg_ht> loop_headers;

inline cfg_util_d& util(cfg_ht h)
{ 
    assert(h.id <= cfg_util_pool.size());
    return cfg_util_pool[h.id];
}

// Fills 'postorder' and 'preorder' and sets 'postorder_i' and 'preorder_i'.
// Also sets 'succ_edge_types', hurray!
void build_order(ir_t const& ir);

// Does everything 'build_order' does, but also identifies loops.
void build_loops_and_order(ir_t& ir);

// Returns how many loops a node is in.
unsigned loop_depth(cfg_ht cfg);

// Builds the dominance tree.
// Requires that the order was built.
void build_dominators_from_order(ir_t& ir);

// Returns a dominator common to both.
cfg_ht dom_intersect(cfg_ht a, cfg_ht b);

// Sorts a single cfg_node. Outputs in 'vec' (which should be large enough)
void toposort_cfg_node(cfg_ht cfg_node, ssa_ht* vec);

#endif
