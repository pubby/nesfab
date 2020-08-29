#include "liveness.hpp"

#include "ir.hpp"

void calc_liveness(ir_t const& ir)
{
    using namespace liveness_impl;
    liveness_vec.resize(cfg_pool::array_size());
    bitset_pool.clear();

    set_size = bitset_size<>(ssa_pool::array_size());

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = live(cfg_it);
        cfg_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
        bitset_set_all(set_size, d.out);

        // Set 'd.in's initial value to be the set of variables used in
        // this node, minus any variables that were also defined here.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'd.out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set is sometimes called 'KILL')
        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            // Remove all ssa definitions introduced in this cfg node.
            bitset_reset(d.out, ssa_it.index);

            unsigned const input_size = ssa_it->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = ssa_it->input(i);
                if(!input_v.holds_ref())
                    continue;
                ssa_ht input = input_v.handle();
                // Only care about nodes defined in other basic blocks.
                if(input->cfg_node() != cfg_it)
                    bitset_set(d.in, input.index);
            }
        }
    }

    // temp_set will hold a node's actual out-set while the algorithm
    // is running.
    unsigned* temp_set = ALLOCA_T(bitset_uint_t, set_size);

    assert(ir.exit);
    cfg_worklist::clear();
    cfg_worklist::push(ir.exit);

    while(!cfg_worklist::empty())
    {
    reenter:
        cfg_ht cfg_node = cfg_worklist::pop();
        auto& d = live(cfg_node);

        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        bitset_reset_all(set_size, temp_set);
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
                cfg_worklist::push(cfg_node->input(i));
        }

        // Assign 'd.in':
        bitset_copy(set_size, d.in, temp_set);
    }

    // Some nodes (infinite loops, etc) might not be reachable travelling
    // backwards from the exit. Handle those now:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        if(!cfg_it->test_flags(FLAG_PROCESSED))
           cfg_worklist::push(cfg_it);
           
    if(!cfg_worklist::empty())
        goto reenter;

    // Now properly set 'out' to be the union of all successor inputs:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        // Might as well clear flags.
        cfg_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);
        auto& d = live(cfg_it);

        bitset_reset_all(set_size, d.out);
        unsigned const output_size = cfg_it->output_size();
        for(unsigned i = 0; i < output_size; ++i)
            bitset_or(set_size, d.out, live(cfg_it->output(i)).in);
    }
}
