#ifndef IR_ALGO_HPP
#define IR_ALGO_HPP

#include <memory>
#include <vector>

#include "flat/small_set.hpp"

#include "bitset.hpp"
#include "ir_decl.hpp"

constexpr unsigned UNVISITED = -1;

using reentry_set_t = fc::small_set<unsigned, 2>;

struct cfg_algo_d
{
    unsigned preorder_i = UNVISITED;
    unsigned postorder_i = UNVISITED;
    cfg_ht idom = {};
    cfg_ht iloop_header = {};
    bool is_loop_header = false;

    // Incoming edges with
    std::unique_ptr<reentry_set_t> reentry_in;
    std::unique_ptr<reentry_set_t> reentry_out;
};

extern thread_local std::vector<cfg_algo_d> cfg_algo_pool;
extern thread_local std::vector<cfg_ht> postorder;
extern thread_local std::vector<cfg_ht> preorder;
extern thread_local std::vector<cfg_ht> loop_headers;

inline cfg_algo_d& algo(cfg_ht h)
{ 
    assert(h.id <= cfg_algo_pool.size());
    return cfg_algo_pool[h.id];
}

// Fills 'postorder' and 'preorder' and sets 'postorder_i' and 'preorder_i'.
// Also sets 'succ_edge_types', hurray!
void build_order(ir_t const& ir);

// Does everything 'build_order' does, but also identifies loops.
void build_loops_and_order(ir_t& ir);

bool loop_is_parent_of(cfg_ht loop_header, cfg_ht node);

// If 'h' is a loop header, returns itself.
// Otherwise, returns its immediate loop header.
cfg_ht this_loop_header(cfg_ht h);

// Returns how many loops a node is in.
unsigned loop_depth(cfg_ht cfg);
unsigned edge_depth(cfg_ht cfg, cfg_ht output);
std::uint64_t depth_exp(std::uint64_t depth, std::uint64_t scale = 4, std::uint64_t max_shifts = 32);

// Builds the dominance tree.
// Requires that the order was built.
void build_dominators_from_order(ir_t& ir);

// If 'a' dominates 'b'
bool dominates(cfg_ht a, cfg_ht b); 

// Returns a dominator common to both.
cfg_ht dom_intersect(cfg_ht a, cfg_ht b);

// Sorts a single cfg_node. Outputs in 'vec' (which should be large enough)
void toposort_cfg_node(cfg_ht cfg_node, ssa_ht* vec);

void split_critical_edges(ir_t& ir);

#endif
