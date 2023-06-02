#include "o_defork.hpp"

#include "ir.hpp"
#include "runtime.hpp"

bool o_shl_tables(log_t* log, ir_t& ir)
{
    bool modified = false;

    for(cfg_node_t& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() != SSA_shl || (ssa_it->type() != TYPE_U && ssa_it->type() != TYPE_S) || carry_used(*ssa_it))
            continue;

        ssa_value_t const value  = ssa_it->input(0);
        ssa_value_t const amount_v = ssa_it->input(1);
        if(!amount_v.is_num())
            continue;
        unsigned amount = amount_v.whole();

        if(amount == 7)
        {
            // This can be implemented as 'ALR #1' followed by 'ROR'.
            ssa_ht const masked = cfg.emplace_ssa(SSA_and, value.type(), value, ssa_value_t(1, value.type().name()));
            ssa_ht const ror = cfg.emplace_ssa(SSA_ror, value.type(), masked, ssa_value_t(0, TYPE_BOOL));
            ssa_ht const carry = cfg.emplace_ssa(SSA_carry, TYPE_BOOL, ror);

            ssa_it->link_change_input(0, ror);
            ssa_it->link_change_input(1, carry);
            ssa_it->unsafe_set_op(SSA_ror);

            if(!carry_output(*ssa_it))
                cfg.emplace_ssa(SSA_carry, TYPE_BOOL, ssa_it);

            modified = true;
        }
        else if(amount >= MIN_SHL_TABLE && amount <= MAX_SHL_TABLE)
        {
            // When using the tables, we have to mask the input to be in bounds.
            unsigned const mask = 0xFF >> amount;
            ssa_ht const masked = cfg.emplace_ssa(SSA_and, value.type(), value, ssa_value_t(mask, value.type().name()));

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
    }

    return modified;
}

