#include "o_merge_bb.hpp"

#include "ir.hpp"

bool o_merge_basic_blocks(log_t* log, ir_t& ir)
{
    bool did_work = false;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it;)
    {
        // Find an edge that can be merged.

        if(cfg_it->output_size() != 1)
        {
            ++cfg_it;
            continue;
        }

        auto const oe = cfg_it->output_edge(0);
        cfg_ht const output = oe.handle;

        if(cfg_it->ssa_size() == 0 && cfg_it != output)
        {
            // If the CFG node has no SSA inside it,
            // it can be merged by rediricting CFG nodes.

            dprint(log, "MERGE_EMPTY_BB", cfg_it);

            while(cfg_it->input_size())
            {
                auto ie = cfg_it->input_edge(0);

                ie.handle->link_change_output(ie.index, output, [&oe](ssa_ht phi)
                {
                    return phi->input(oe.index);
                });

                assert(ie.handle->output(ie.index) == output);
            }

            cfg_it->link_clear_outputs();
            cfg_it = ir.prune_cfg(cfg_it);

            did_work = true;

            continue;
        }

        if(output->input_size() != 1 || cfg_it == output)
        {
            ++cfg_it;
            continue;
        }

        // Perform the merge.

        dprint(log, "MERGE_BB", cfg_it, "<-", output);

        cfg_it->steal_ssa_nodes(output);
        assert(output->ssa_size() == 0);
        assert(!output->ssa_begin());

        cfg_it->link_clear_outputs();
        for(unsigned i = 0; i < output->output_size(); ++i)
        {
            cfg_ht output_output = output->output(i);
            unsigned const phi_i = output->output_edge(i).index;
            if(output_output == output)
                output_output = cfg_it;
            cfg_it->link_append_output(output_output, [phi_i](ssa_ht phi) -> ssa_value_t
                { return phi->input(phi_i); });
        }
        output->link_clear_outputs();

        if(output == ir.exit)
            ir.exit = cfg_it;

        ir.prune_cfg(output);

        did_work = true;
    }

    return did_work;
}
