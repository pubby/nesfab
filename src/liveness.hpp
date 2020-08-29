#ifndef LIVENESS_HPP
#define LIVENESS_HPP

#include <vector>

#include "array_pool.hpp"
#include "bitset.hpp"

struct liveness_d
{
    bitset_uint_t* in;
    bitset_uint_t* out; // Also used to hold the 'KILL' set temporarily.
};

namespace liveness_impl
{
    inline thread_local std::vector<liveness_d> vec;
    inline thread_local array_pool<bitset_uint_t> bitset_pool;
    inline thread_local unsigned set_size;
}

inline liveness_d& live(ssa_ht h) 
{ 
    using namespace liveness_impl;
    assert(h.index < vec.size());
    return vec[h.index];
}

inline unsigned live_set_size() { return liveness_impl::set_size; }

void calc_liveness(class ir_t const& ir);

#endif
