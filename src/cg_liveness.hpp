#ifndef LIVENESS_HPP
#define LIVENESS_HPP

// A self-contained implementation of live variable analysis.

#include "array_pool.hpp"
#include "bitset.hpp"
#include "ir_decl.hpp"

namespace liveness_impl
{
    inline thread_local bitset_pool_t bitset_pool;
    inline thread_local unsigned set_size;
}

inline unsigned live_set_size() { return liveness_impl::set_size; }

void calc_liveness(ir_t const& ir);

// If 'range' intersects 'def'.
bool live_at_def(ssa_ht range, ssa_ht def);

bool live_at_any_def(ssa_ht range, ssa_ht const* defs_begin,
                     ssa_ht const* defs_end);

// 'before' returns true if 'a' comes before 'b' in the same CFG node.
// (It will never be called if 'a' and 'b' belong to different cfg nodes)
bool live_range_overlap(ssa_ht a, ssa_ht b);

// A rough approximation of how much a live range overlaps with all others.
std::size_t live_range_busyness(ir_t& ir, ssa_ht h);

#endif
