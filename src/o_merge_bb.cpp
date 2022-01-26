#include "o_merge_bb.hpp"

#include "ir.hpp"

bool o_merge_basic_blocks(ir_t& ir)
{
    bool did_work = false;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it;)
    {
        if(cfg_it->output_size() != 1)
        {
            ++cfg_it;
            continue;
        }

        cfg_ht output = cfg_it->output(0);

        if(output->input_size() != 1 || cfg_it == output || output == ir.exit)
        {
            ++cfg_it;
            continue;
        }

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

        ir.prune_cfg(output);

        did_work = true;
    }

    return did_work;
}
