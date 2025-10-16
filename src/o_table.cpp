#include "o_table.hpp"

#ifndef NDEBUG
#include <iostream>
#endif

#include "flat/flat_map.hpp"
#include "flat/small_map.hpp"

#include "ir.hpp"
#include "runtime.hpp"

bool o_bool_tables(log_t* log, ir_t& ir)
{
    bool modified = false;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        // Find a CFG node where both outputs are the same.
        if(cfg_it->output_size() != 2 || cfg_it->output(0) != cfg_it->output(1))
            continue;

        cfg_ht cfg_output = cfg_it->output(0);
        ssa_ht const branch = cfg_it->last_daisy();

        // Make sure it's an IF statement:
        if(!branch || branch->op() != SSA_if || !branch->input(0).holds_ref())
            continue;

        ssa_ht const condition = branch->input(0).handle();
        assert(condition->type() == TYPE_BOOL);
        
        // Make sure the IF condition is calculating a [0,1] value.
        if(ssa_flags(condition->op()) & SSAF_BRANCH_CONDITION)
            continue;

        unsigned i0 = cfg_it->output_edge(0).index;
        unsigned i1 = cfg_it->output_edge(1).index;

        // Require every phi have const inputs and be non-aggregate:
        for(ssa_ht phi_it = cfg_output->phi_begin(); phi_it; ++phi_it)
        {
            if(is_aggregate(phi_it->type().name()))
                goto fail;
            if(!phi_it->input(i0).is_const() || !phi_it->input(i1).is_const())
                goto fail;
        }

        // If the cfg output has more than 2 inputs, we need to split it.
        if(cfg_output->input_size() > 2)
        {
            cfg_ht new_output = ir.emplace_cfg(cfg_output->prop_flags());
            cfg_it->link_append_output(new_output, [](ssa_ht) -> ssa_value_t { assert(false); });
            cfg_it->link_append_output(new_output, [](ssa_ht) -> ssa_value_t { assert(false); });

            fc::small_map<ssa_ht, ssa_ht, 4> map;

            for(ssa_ht phi_it = cfg_output->phi_begin(); phi_it; ++phi_it)
            {
                ssa_ht new_phi = new_output->emplace_ssa(SSA_phi, phi_it->type(), phi_it->input(i0), phi_it->input(i1));
                map.emplace(phi_it, new_phi);
            }

            cfg_it->link_remove_output(1);
            cfg_it->link_remove_output(0);

            new_output->link_append_output(cfg_output, [&](ssa_ht h) -> ssa_value_t { return map.at(h); });

            // OK! Here's the new output:
            cfg_output = new_output;

            i0 = cfg_it->output_edge(0).index;
            i1 = cfg_it->output_edge(1).index;

            assert(i0 == 0);
            assert(i1 == 1);
        }

        {
            ssa_ht cast = cfg_it->emplace_ssa(SSA_cast, TYPE_U, condition);

            // Convert all the phis:
            while(ssa_ht phi_it = cfg_output->phi_begin())
            {
                passert(phi_it->input_size() == 2, phi_it->input_size());

                type_t const elem_type = phi_it->type();

                ssa_value_t elem0 = phi_it->input(i0);
                ssa_value_t elem1 = phi_it->input(i1);

                if(is_arithmetic(elem_type.name())
                   && whole_bytes(elem_type.name()) > 0
                   && frac_bytes(elem_type.name()) == 0
                   && elem0.eq_whole(0) 
                   && elem1.eq_whole(1))
                {
                    // If the phi is 0,1, then we can totally remove it.
                    // (You would have to be really dumb to write code that needs this!)
                    ssa_ht casted_elem = cfg_output->emplace_ssa(SSA_cast, elem_type, condition);
                    phi_it->replace_with(casted_elem);
                    phi_it->prune();
                }
                else
                {
                    // The usual case is to replace it with a table:
                    ssa_ht table = cfg_output->emplace_ssa(SSA_init_array, type_t::tea(elem_type, 2), elem0, elem1);
                    ssa_ht access = cfg_output->emplace_ssa(SSA_read_array8, elem_type, table, ssa_value_t(0u, TYPE_U20), cast);
                    phi_it->replace_with(access);
                    phi_it->prune();
                }
            }
            assert(!cfg_output->phi_begin());
        }

        // Remove the branch:
        branch->prune();
        cfg_it->link_remove_output(0);

        modified = true;
    fail:;
    }

    return modified;
}

bool o_switch_tables(log_t* log, ir_t& ir)
{
    bool modified = false;

    std::vector<ssa_value_t> table;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        {
            ssa_ht const switch_ = cfg_it->last_daisy();

            // Make sure it's a FULL SWITCH statement:
            if(!switch_ || switch_->op() != SSA_switch_full || !switch_->input(0).holds_ref())
                continue;

            // Make sure all outputs are the same:
            for(unsigned i = 1; i < cfg_it->output_size(); i += 1)
                if(cfg_it->output(i) != cfg_it->output(0))
                    goto fail;

            cfg_ht cfg_output = cfg_it->output(0);

            ssa_ht const condition = switch_->input(0).handle();

            // Require every phi have const inputs and be artihmetic:
            for(ssa_ht phi_it = cfg_output->phi_begin(); phi_it; ++phi_it)
            {
                if(!is_arithmetic(phi_it->type().name()))
                    goto fail;
                for(unsigned i = 0; i < phi_it->input_size(); i += 1)
                    if(cfg_output->input(i) == cfg_it && !phi_it->input(i).is_const())
                        goto fail;
            }

            // If the cfg output has more inputs, we need to split it.
            if(cfg_output->input_size() > cfg_it->output_size())
            {
                cfg_ht new_output = ir.emplace_cfg(cfg_output->prop_flags());
                unsigned const output_size = cfg_it->output_size();
                for(unsigned i = 0; i < output_size; i += 1)
                    cfg_it->link_append_output(new_output, [](ssa_ht) -> ssa_value_t { assert(false); });

                fc::small_map<ssa_ht, ssa_ht, 4> map;

                for(ssa_ht phi_it = cfg_output->phi_begin(); phi_it; ++phi_it)
                {
                    ssa_ht new_phi = new_output->emplace_ssa(SSA_phi, phi_it->type());
                    for(unsigned i = 0; i < output_size; i += 1)
                        new_phi->link_append_input(phi_it->input(cfg_it->output_edge(i).index));
                    map.emplace(phi_it, new_phi);
                }

                for(unsigned i = output_size - 1; i < output_size; i -= 1)
                    cfg_it->link_remove_output(i);

                new_output->link_append_output(cfg_output, [&](ssa_ht h) -> ssa_value_t { return map.at(h); });

                // OK! Here's the new output:
                cfg_output = new_output;
            }

            int min = 0xFF;
            int max = 0;

            for(unsigned i = 1; i < switch_->input_size(); i += 1)
            {
                std::uint8_t const value = switch_->input(i).whole();
                min = std::min<int>(min, value);
                max = std::max<int>(max, value);
            }

            int const size = max - min + 1;
            assert(size <= 256);

            {
                ssa_ht cast = cfg_it->emplace_ssa(SSA_cast, TYPE_U, condition);

                // Convert all the phis:
                while(ssa_ht phi_it = cfg_output->phi_begin())
                {
                    type_t const elem_type = phi_it->type();
                    assert(is_arithmetic(elem_type.name()));

                    table.clear();
                    table.resize(size, ssa_value_t(0u, elem_type.name()));

                    assert(cfg_it->output_size() + 1 == switch_->input_size());
                    passert(cfg_it->output_size() == phi_it->input_size(), cfg_it->output_size(), phi_it->input_size());

                    for(unsigned i = 1; i < switch_->input_size(); i += 1)
                    {
                        std::uint8_t const index = switch_->input(i).whole();
                        table[index - min] = phi_it->input(cfg_it->output_edge(i-1).index);
                    }

                    ssa_ht table_h = cfg_output->emplace_ssa(SSA_init_array, type_t::tea(elem_type, size));
                    table_h->assign_input(table.begin(), table.end());
                    ssa_ht access = cfg_output->emplace_ssa(SSA_read_array8, elem_type, table_h, ssa_value_t(-unsigned(min), TYPE_U20), cast);
                    phi_it->replace_with(access);
                    phi_it->prune();
                }
                assert(!cfg_output->phi_begin());
            }

            // Remove the switch:
            switch_->prune();
            while(cfg_it->output_size() > 1)
                cfg_it->link_remove_output(0);

            modified = true;
        }
    fail:;
    }

    return modified;
}
