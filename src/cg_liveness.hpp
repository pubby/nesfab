#ifndef LIVENESS_HPP
#define LIVENESS_HPP

// A self-contained implementation of live variable analysis.

#include "robin/map.hpp"

#include "array_pool.hpp"
#include "bitset.hpp"
#include "ir_decl.hpp"
#include "ir.hpp"

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

// Tracks all vars used in assembly code, assigning them an index.
class cg_var_map_t
{
public:
    explicit cg_var_map_t(ir_t const& ir);

    unsigned index(locator_t var) const
    {
        auto result = map.find(var.mem_head());
        if(result)
            return result->second;
        throw std::runtime_error("Unknown locator in cg_locator_map_t.");
    }

    locator_t operator[](unsigned i) const 
    { 
        assert(i < map.size());
        return map.begin()[i].first;
    }

    std::size_t size() const { return map.size(); }

    static bool is_valid_var(locator_t arg);

    template<typename Fn>
    void for_each(Fn fn)
    {
        for(auto const& pair : map)
            fn(pair.first, pair.second);
    }

private:
    rh::batman_map<locator_t, unsigned> map;
};

void calc_asm_liveness(ir_t const& ir, cg_var_map_t const& var_map);

#endif
