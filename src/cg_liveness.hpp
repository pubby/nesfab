#ifndef LIVENESS_HPP
#define LIVENESS_HPP

// A self-contained implementation of live variable analysis.

#include "robin/map.hpp"

#include "array_pool.hpp"
#include "bitset.hpp"
#include "ir_decl.hpp"

namespace liveness_impl
{
    inline thread_local bitset_pool_t bitset_pool;
    inline thread_local unsigned set_size;
}

inline unsigned live_set_size() { return liveness_impl::set_size; }

void calc_ssa_liveness(ssa_ht node); // only does a single node
unsigned calc_ssa_liveness(ir_t const& ir);
unsigned calc_ssa_liveness(ir_t const& ir, unsigned pool_size);

void clear_liveness_for(ir_t const& ir, ssa_ht node);

// If 'range' intersects 'def'.
bool live_at_def(ssa_ht range, ssa_ht def);

bool live_at_any_def(ssa_ht range, ssa_ht const* defs_begin,
                     ssa_ht const* defs_end);

// 'before' returns true if 'a' comes before 'b' in the same CFG node.
// (It will never be called if 'a' and 'b' belong to different cfg nodes)
bool live_range_overlap(ssa_ht a, ssa_ht b);

// A rough approximation of how much a live range overlaps with all others.
std::size_t live_range_busyness(ir_t& ir, ssa_ht h);

// Key: Store targets (locators or ssa nodes)
// Value: A unique integer in the range [0, size), used to index into bitsets
using cg_store_map_t = rh::batman_map<ssa_value_t, unsigned>;

void calc_asm_liveness(ir_t const& ir, cg_store_map_t const& store_set);

#endif
