#include "cg_liveness.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "worklist.hpp"

static inline cfg_liveness_d& live(cfg_ht h) 
{ 
    return cg_data(h).live;
}

static void _live_visit(ssa_ht def, cfg_ht cfg_node)
{
    if(def->cfg_node() == cfg_node)
        return;

    if(bitset_test(live(cfg_node).in, def.index))
        return;

    bitset_set(live(cfg_node).in, def.index);

    unsigned const input_size = cfg_node->input_size();
    assert(input_size > 0);
    for(unsigned i = 0; i < input_size; ++i)
    {
        cfg_ht input = cfg_node->input(i);
        bitset_set(live(input).out, def.index);
        _live_visit(def, input);
    }
}

void calc_liveness(ssa_ht node)
{
    unsigned const output_size = ssa_it->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto const oe = ssa_it->output_edge(i);
        cfg_ht const ocfg = oe.handle->cfg_node();

        if(oe.handle->op() == SSA_phi)
        {
            bitset_set(live(ocfg).in, ssa_it.index);
            bitset_set(live(ocfg->input(oe.index)).out, ssa_it.index);
            _live_visit(ssa_it, ocfg->input(oe.index));
        }
        else
            _live_visit(ssa_it, ocfg);
    }
}

unsigned calc_liveness(ir_t const& ir)
{
    return calc_liveness(ir, cfg_pool::array_size());
}

unsigned calc_liveness(ir_t const& ir, unsigned pool_size)
{
    using namespace liveness_impl;
    cg_data_resize();
    bitset_pool.clear();
    set_size = bitset_size<>(pool_size);

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        auto& d = live(cfg_it);
        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        calc_liveness(ssa_it);

    return set_size;
}

bool live_at_def(ssa_ht range, ssa_ht def)
{
    if(range == def)
        return false;

    auto const& def_live = live(def->cfg_node()); 

    // If 'range' begins before 'def':
    if((range->cfg_node() == def->cfg_node() 
        && cg_data(range).schedule.rank < cg_data(def).schedule.rank)
       || bitset_test(def_live.in, range.index))
    {
        // Interfere if range is also live-out at def.
        if(bitset_test(def_live.out, range.index))
            return true;

        // Test to see if a use occurs after def:
        for(unsigned i = 0; i < range->output_size(); ++i)
        {
            ssa_ht output = range->output(i);
            if(output->cfg_node() == def->cfg_node() 
               && cg_data(def).schedule.rank < cg_data(output).schedule.rank)
            {
                return true;
            }
        }
    }

    return false;
}

bool live_at_any_def(ssa_ht range, ssa_ht const* defs_begin, 
                     ssa_ht const* defs_end)
{
    for(ssa_ht const* it = defs_begin; it < defs_end; ++it)
        if(live_at_def(range, *it))
            return true;
    return false;
}

bool live_range_overlap(ssa_ht a, ssa_ht b)
{
    return live_at_def(a, b) || live_at_def(b, a);
}

std::size_t live_range_busyness(ir_t& ir, ssa_ht h)
{
    using namespace liveness_impl;

    std::size_t total_size = 0;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& ld = live(cfg_it);

        if(bitset_test(ld.in, h.index))
            total_size += bitset_popcount(set_size, ld.in);

        if(bitset_test(ld.out, h.index))
            total_size += bitset_popcount(set_size, ld.out);
    }

    return total_size;
}
