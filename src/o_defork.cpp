#include "o_defork.hpp"

#include <flat/small_map.hpp>

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
            auto const oe = cfg_node.output_edge(0);

            for(ssa_ht phi = oe.handle->phi_begin(); phi; ++phi)
            {
                ssa_value_t const first = phi->input(oe.index);

                for(unsigned i = 1; i < output_size; ++i)
                    if(first != phi->input(cfg_node.output_edge(i).index))
                        goto done;
            }
        }

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

bool o_fork(log_t* log, ir_t& ir)
{
    bool updated = false;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it;)
    {
        {
            cfg_node_t& cfg_node = *cfg_it;

            unsigned const input_size = cfg_node.input_size();
            if(input_size < 2)
                goto next_iter;

            // Look for a single phi:
            ssa_ht const phi = cfg_node.phi_begin();
            if(!phi || phi.next() || phi->output_size() != 1)
                goto next_iter;

            // The phi should be used in an 'if' branch:
            ssa_ht const branch = phi->output(0);
            if(branch->cfg_node() != cfg_node.handle() || branch->op() != SSA_if)
                goto next_iter;

            assert(branch->cfg_node() == phi->cfg_node());

            // The phi and CFG shouldn't loop:
            for(unsigned i = 0; i < input_size; ++i)
            {
                if(cfg_node.input(i) == cfg_node.handle())
                    goto next_iter;
                if(phi->input(i) == phi)
                    goto next_iter;
            }

            // All other SSA nodes in the CFG node should be used only in that CFG node:
            for(ssa_ht ssa = cfg_node.ssa_begin(); ssa; ++ssa)
            {
                bool const others_valid = for_each_output_with_links(ssa, [&](ssa_ht from, ssa_ht output)
                {
                    return output->cfg_node() == cfg_node.handle();
                });

                if(!others_valid)
                    goto next_iter;
            }

            // OK! We can rewrite!

            // Identify duplicated phi inputs and create 1 new CFG node per unique phi input.
            // (Duplicated inputs will map to the same CFG node).
            fc::small_map<ssa_value_t, cfg_ht, 16> unique_map;
            unique_map.container.reserve(phi->input_size());
            for(unsigned i = 0; i < cfg_node.input_size();)
            {
                auto result = unique_map.emplace(phi->input(i), cfg_ht{});
                if(result.second)
                {
                    result.first.underlying->second = ir.split_edge(cfg_node.input_edge(i).output());
                    ++i;
                }
                else
                {
                    assert(result.first->second);
                    auto ie = cfg_node.input_edge(i);
                    ie.handle->link_change_output(ie.index, result.first->second, [](ssa_ht phi) 
                    { 
                        assert(false); 
                        return ssa_value_t{}; 
                    });
                }
            }

            // Split our input edges:

            fc::small_map<ssa_ht, ssa_ht, 32> clone_map;
            for(unsigned i = 0; i < cfg_node.input_size();)
            {
                cfg_ht const split = cfg_node.input(i);
                assert(split->ssa_size() == 0);

                // Copy nodes:
                for(ssa_ht ssa = cfg_node.ssa_begin(); ssa; ++ssa)
                {
                    if(ssa == phi)
                        continue;

                    ssa_ht const cloned = split->emplace_ssa(ssa->op(), ssa->type());
                    if(ssa->in_daisy())
                        cloned->append_daisy();
                    clone_map[ssa] = cloned;
                }

                // Define nodes:
                for(auto const& pair : clone_map)
                {
                    unsigned const input_size = pair.first->input_size();
                    for(unsigned j = 0; j < input_size; ++j)
                    {
                        ssa_value_t input = pair.first->input(j);
                        if(input.holds_ref())
                        {
                            if(input.handle() == phi)
                                input = phi->input(i);
                            else if(ssa_ht const* cloned = clone_map.has(input.handle()))
                                input = *cloned;
                        }
                        pair.second->link_append_input(input);
                    }
                }

                // Re-write outputs:
                split->link_change_output(0, cfg_node.output(0), [&](ssa_ht phi)
                { 
                    return phi->input(cfg_node.output_edge(0).index);
                });
                split->link_append_output(cfg_node.output(1), [&](ssa_ht phi)
                { 
                    return phi->input(cfg_node.output_edge(1).index);
                });
            }

            // Prune:
            cfg_node.prune_ssa();
            cfg_node.link_clear_outputs();
            cfg_it = ir.prune_cfg(cfg_it);

            updated = true;
            continue;
        }

    next_iter:
        ++cfg_it;
    }

    return updated;
}
