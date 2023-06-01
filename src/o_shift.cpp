#include "o_defork.hpp"

#include "ir.hpp"
#include "runtime.hpp"

bool o_shl_tables(log_t* log, ir_t& ir)
{
    bool modified = false;

    for(cfg_node_t& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() != SSA_shl || ssa_it->type().size_of() != 1 || carry_used(*ssa_it))
            continue;

        ssa_value_t const amount = ssa_it->input(1);
        if(!amount.is_num() || amount.whole() < MIN_SHL_TABLE || amount.whole() > MAX_SHL_TABLE)
            continue;

        // When using the tables, we have to mask the input to be in bounds.
        ssa_value_t const value  = ssa_it->input(0);
        unsigned const mask = 0xFF >> amount.whole();
        ssa_ht masked = cfg.emplace_ssa(SSA_and, value.type(), value, ssa_value_t(mask, value.type().name()));

        ssa_it->link_change_input(0, masked);
        ssa_it->unsafe_set_op(SSA_shl_table);

        // Remove the carry if it exists.
        if(ssa_ht carry = carry_output(*ssa_it))
        {
            assert(carry->output_size() == 0);
            carry->prune();
        }

        modified = true;
    }

    return modified;
}

