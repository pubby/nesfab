#include "o_defork.hpp"

#include "ir.hpp"

bool o_defork(log_t* log, ir_t& ir)
{
    bool updated = false;

    for(cfg_node_t& cfg_node : ir)
    {
        unsigned const output_size = cfg_node.output_size();
        if(output_size < 2)
            continue;

        cfg_ht const first = cfg_node.output(0);
        for(unsigned i = 1; i < output_size; ++i)
            if(cfg_node.output(i) != first)
                goto done;

        {
            ssa_ht const branch = cfg_node.last_daisy();
            assert(branch);
            assert(ssa_flags(branch->op()) & SSAF_CONDITIONAL);

            branch->prune();
            cfg_node.link_shrink_outputs(1);
            updated = true;
        }
    done:;
    }

    return updated;
}

