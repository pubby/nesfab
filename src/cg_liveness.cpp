#include "cg_liveness.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "globals.hpp"
#include "ir.hpp"
#include "lvar.hpp"
#include "worklist.hpp"
#include "assert.hpp"

namespace liveness_impl
{
    TLS array_pool_t<bitset_uint_t> bitset_pool;
    TLS unsigned set_size;
}

//////////////////
// cfg liveness //
//////////////////

static inline cfg_liveness_d& live(cfg_ht h) 
{ 
    return cg_data(h).live;
}

static void _live_visit(ssa_ht def, cfg_ht cfg_node)
{
    if(def->cfg_node() == cfg_node)
        return;

    if(bitset_test(live(cfg_node).in, def.id))
        return;

    bitset_set(live(cfg_node).in, def.id);

    unsigned const input_size = cfg_node->input_size();
    passert(input_size > 0, cfg_node, input_size);
    for(unsigned i = 0; i < input_size; ++i)
    {
        cfg_ht input = cfg_node->input(i);
        bitset_set(live(input).out, def.id);
        _live_visit(def, input);
    }
}

void calc_ssa_liveness(ssa_ht node)
{
    unsigned const output_size = node->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto const oe = node->output_edge(i);
        cfg_ht const ocfg = oe.handle->cfg_node();

        // We only care about values
        if(oe.input_class() != INPUT_VALUE)
            continue;

        if(oe.handle->op() == SSA_phi)
        {
            assert(node->op() == SSA_phi_copy);
            assert(node->cfg_node() == ocfg->input(oe.index));

            //bitset_set(live(ocfg).in, node.index);
            bitset_set(live(node->cfg_node()).out, node.id);
            //_live_visit(node, node->cfg_node());
        }
        else
        {
            assert(node->op() != SSA_phi_copy);
            _live_visit(node, ocfg);
        }
    }
}

unsigned calc_ssa_liveness(ir_t const& ir)
{
    return calc_ssa_liveness(ir, ssa_pool::array_size());
}

unsigned calc_ssa_liveness(ir_t const& ir, unsigned pool_size)
{
    using namespace liveness_impl;
    cg_data_resize();
    bitset_pool.clear();
    set_size = ::bitset_size<>(pool_size);

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = live(cfg_it);
        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
        assert(d.in);
        assert(d.out);
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        calc_ssa_liveness(ssa_it);

    return set_size;
}

void clear_liveness_for(ir_t const& ir, ssa_ht node)
{
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = live(cfg_it);
        bitset_clear(d.in, node.id);
        bitset_clear(d.out, node.id);
    }
}

bool live_at_def(ssa_ht range, ssa_ht def)
{
    if(range == def || (ssa_flags(range->op()) & SSAF_CG_UNLIVE))
        return false;

    bool const same_cfg = range->cfg_node() == def->cfg_node();
    auto const& def_live = live(def->cfg_node()); 

    // If 'range' begins before 'def':
    if((same_cfg && cg_data(range).schedule.index < cg_data(def).schedule.index)
       || bitset_test(def_live.in, range.id))
    {
        // Interfere if range is also live-out at def.
        if(bitset_test(def_live.out, range.id))
            return true;

        // Test to see if a use occurs after def:
        for(unsigned i = 0; i < range->output_size(); ++i)
        {
            auto const oe = range->output_edge(i);

            // We only care about values
            if(oe.input_class() != INPUT_VALUE)
                continue;

            ssa_ht const output = oe.handle;
            if(output->cfg_node() == def->cfg_node() && cg_data(def).schedule.index < cg_data(output).schedule.index)
                return true;
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

        assert(ld.in);
        assert(ld.out);

        if(bitset_test(ld.in, h.id))
            total_size += bitset_popcount(set_size, ld.in);

        if(bitset_test(ld.out, h.id))
            total_size += bitset_popcount(set_size, ld.out);
    }

    return total_size;
}

