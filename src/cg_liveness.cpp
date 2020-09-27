#include "cg_liveness.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "worklist.hpp"

static inline cfg_liveness_d& live(cfg_ht h) 
{ 
    return cg_data(h).live;
}

void calc_liveness(ir_t const& ir)
{
    using namespace liveness_impl;
    cg_data_resize();
    bitset_pool.clear();

    set_size = bitset_size<>(cfg_pool::array_size());

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        auto& d = live(cfg_it);
        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
        bitset_set_all(set_size, d.out);

        // Set 'd.in's initial value to be the set of variables used in
        // this cfg node, minus any variables that were also defined here.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'd.out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set is sometimes called 'KILL')
        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            // Remove ssa defs introduced in this cfg node from KILL:
            bitset_clear(d.out, ssa_it.index);

            unsigned const input_size = ssa_it->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = ssa_it->input(i);
                if(!input_v.holds_ref())
                    continue;
                // Build GEN to be ssa nodes used in this cfg node.
                // (Only care about nodes defined in other basic blocks.)
                if(input_v->cfg_node() != cfg_it)
                    bitset_set(d.in, input_v.handle().index);
            }
        }
    }

    // temp_set will hold a node's actual out-set while the algorithm
    // is running.
    auto* temp_set = ALLOCA_T(bitset_uint_t, set_size);

    assert(ir.exit);
    cfg_worklist.clear();
    cfg_worklist.push(ir.exit);

    while(!cfg_worklist.empty())
    {
    reenter:
        cfg_ht cfg_node = cfg_worklist.pop();
        auto& d = live(cfg_node);

        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        bitset_clear_all(set_size, temp_set);
        unsigned const output_size = cfg_node->output_size();
        for(unsigned i = 0; i < output_size; ++i)
            bitset_or(set_size, temp_set, live(cfg_node->output(i)).in);

        bitset_and(set_size, temp_set, d.out); // (d.out holds KILL)
        bitset_or(set_size, temp_set, d.in);

        // If 'd.in' is changing, add all predecessors to the worklist.
        if(!cfg_node->test_flags(FLAG_PROCESSED)
           || !bitset_eq(set_size, temp_set, d.in))
        {
            cfg_node->set_flags(FLAG_PROCESSED);
            unsigned const input_size = cfg_node->input_size();
            for(unsigned i = 0; i < input_size; ++i)
                cfg_worklist.push(cfg_node->input(i));
        }

        // Assign 'd.in':
        bitset_copy(set_size, d.in, temp_set);
    }

    // Some nodes (infinite loops, etc) might not be reachable travelling
    // backwards from the exit. Handle those now:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        if(!cfg_it->test_flags(FLAG_PROCESSED))
           cfg_worklist.push(cfg_it);
           
    if(!cfg_worklist.empty())
        goto reenter;

    // Now properly set 'out' to be the union of all successor inputs:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        // Might as well clear flags.
        cfg_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);
        auto& d = live(cfg_it);

        bitset_clear_all(set_size, d.out);
        unsigned const output_size = cfg_it->output_size();
        for(unsigned i = 0; i < output_size; ++i)
            bitset_or(set_size, d.out, live(cfg_it->output(i)).in);
    }
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
