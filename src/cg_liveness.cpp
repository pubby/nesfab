#include "cg_liveness.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "globals.hpp"
#include "ir.hpp"
#include "lvar.hpp"
#include "worklist.hpp"

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

    if(bitset_test(live(cfg_node).in, def.index))
        return;

    bitset_set(live(cfg_node).in, def.index);

    unsigned const input_size = cfg_node->input_size();
    assert(input_size > 0);
    for(unsigned i = 0; i < input_size; ++i)
    {
        cfg_ht input = cfg_node->input(i);
        bitset_set(live(input).out, def.index);
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

        if(oe.handle->op() == SSA_phi)
        {
            assert(node->op() == SSA_phi_copy);
            assert(node->cfg_node() == ocfg->input(oe.index));

            //bitset_set(live(ocfg).in, node.index);
            bitset_set(live(node->cfg_node()).out, node.index);
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
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        auto& d = live(cfg_it);
        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
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
        bitset_clear(d.in, node.index);
        bitset_clear(d.out, node.index);
    }
}

bool live_at_def(ssa_ht range, ssa_ht def)
{
    if(range == def)
        return false;

    auto const& def_live = live(def->cfg_node()); 

    // If 'range' begins before 'def':
    if((range->cfg_node() == def->cfg_node() && cg_data(range).schedule.index < cg_data(def).schedule.index)
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
               && cg_data(def).schedule.index < cg_data(output).schedule.index)
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

///////////////////////
// assembly liveness //
///////////////////////

// Once the assembly is (mostly) generated, 
// a second set of liveness checks is run to build an interference graph
// and then allocate memory.

void calc_asm_liveness(ir_t const& ir, lvars_manager_t const& lvars)
{
    using namespace liveness_impl;
    cg_data_resize();
    bitset_pool.clear();
    set_size = lvars.bitset_size();

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        auto& d = live(cfg_it);
        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
        bitset_set_all(set_size, d.out);

        // Set 'd.in's initial value to be the set of variables used in
        // this cfg node before an assignment.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'd.out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set is sometimes called 'KILL')
        for(asm_inst_t const& inst : cg_data(cfg_it).code)
        {
            int const store_i = lvars.index(inst.arg);

            if(store_i < 0)
                continue;

            if((op_input_regs(inst.op) & REGF_M) && bitset_test(d.out, store_i))
                bitset_set(d.in, store_i);

            if(op_output_regs(inst.op) & REGF_M)
                bitset_clear(d.out, store_i);
        }
    }

    // temp_set will hold a node's actual out-set while the algorithm
    // is running.
    auto* temp_set = ALLOCA_T(bitset_uint_t, set_size);

    cfg_worklist.clear();

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        if(cfg_it->output_size() == 0)
            cfg_worklist.push(cfg_it);

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

void build_lvar_interferences(ir_t const& ir, lvars_manager_t& lvars)
{
    using namespace liveness_impl;

    assert(set_size == lvars.bitset_size());

    // Step-through the code backwards, maintaining a current liveness set
    // and using that to build an interference graph.
    bitset_uint_t* const live = ALLOCA_T(bitset_uint_t, set_size);
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it);

        // Since we're going backwards, reset 'live' to the node's output state.
        bitset_copy(set_size, live, d.live.out);

        for(auto it = d.code.rbegin(); it != d.code.rend(); ++it)
        {
            asm_inst_t const& inst = *it;

            if(inst.op == JSR_ABSOLUTE)
            {
                fn_ht const fn_h = inst.arg.fn();
                fn_t const& fn = *fn_h;

                // Every live var will interfere with this fn:
                bitset_for_each(set_size, live, [&](unsigned i)
                {
                    lvars.add_fn_interference(i, fn_h);
                });

                // The fn's arguments are now live:
                for(unsigned argn = 0; argn < fn.type.num_params(); ++argn)
                {
                    unsigned const num_fields = ::num_fields(fn.type.type(argn));
                    for(unsigned field = 0; field < num_fields; ++field)
                    {
                        int const lvar_i = lvars.index(locator_t::arg(fn_h, argn, field));
                        if(lvar_i >= 0)
                            bitset_set(live, lvar_i);
                    }
                }

                // TODO handle fn returns?
            }
            else
            {
                int const lvar_i = lvars.index(inst.arg);
                if(lvar_i < 0)
                    continue;

                if(op_input_regs(inst.op) & REGF_M)
                    bitset_set(live, lvar_i);
                else if(op_output_regs(inst.op) & REGF_M)
                    bitset_clear(live, lvar_i);
                else 
                    continue; // No change to 'live' this iteration.
            }

            /* TODO
            if(indirect_addr_mode(op_addr_mode(inst.op)))
            {
                // The argument is a pointer, 
                // meaning it must be allocated to zero-page.
                zp_only.push_back(i);
            }
            */

            // Variables that are live together interfere with each other.
            // Update the interference graph here:
            lvars.add_lvar_interferences(live);
        }
    }
}

#if 0

    // Cleanup: nodes never interfere with themself:
    for(unsigned i = 0; i < store_set.size(); ++i)
        bitset_clear(&interferences[i * bitset_size], i);

    struct rank_t
    {
        unsigned score;
        unsigned i;
        constexpr auto operator<=>(rank_t const&) const = default;
    };

    std::vector<rank_t> ranked;
    ranked.resize(num_vars);

    for(unsigned i = 0; i < num_vars; ++i)
    {
        unsigned score = 0;

        // Prioritize big nodes:
        unsigned const var_size = TODO;
        score += var_size << 16;

        // Less usable ram = higher priority
        unsigned const taken_ram = ram_size - usable_ram[i].popcount();
        unsigned const neighbors = bitset_popcount(bitset_size, interferences[i * bitset_size]);

        score += taken_ram * neighbors;

        ranked[i] = { .score = score, .i = i };
    }

    std::sort(ranked.begin(), ranked.end(), std::greater<>{});

    for(rank_t rank : ranked)
    {
        unsigned const var_size = TODO;
        bool const zp_only = TODO;
        span_t const span = alloc_ram(usable_ram[rank.i], var_size, zp_only);

        ram_bitset_t const mask = ~ram_bitset_t::filled(span.size, span.addr);

        // Propagate changes
        bitset_for_each(bitset_size, &interferences[i * bitset_size], [&](unsigned i)
        {
            usable_ram[i] &= mask;
        });
    }



    // GOAL OF THIS FUNCTION
    // - reduce many ssa node locators into a few locators
    //   - can merge two locators if their liveness doesn't overlap

    // 3 SETS:
    // - arg set
    // - lvar set
    // - 

    // IDEA: - 

    // - if a loc is live for a fn call, it interferes with every local of that fn (recursively)
    // - therefore, we should track fns with a set per loc

    // QUESTION: how do arguments interfere?
    // - arguments appear in fn itself, and calling fns

    // 1) Find the first node of a new colored set.
    {
        unsigned max_size = 0;
        int max_neighbors = -1;
        unsigned best_i;

        var_map.for_each([&](locator_t var, unsigned i)
        {
            unsigned const size = var.mem_size();
            if(size < max_size)
                return;

            int const neighbors = bitset_popcount(bitset_size, &interferences[i * bitset_size]);
            if(size > max_size || neighbors > max_negihbors)
            {
                max_size = size;
                max_neighbors = neighbors;
                best_i = i;
            }
        });




        for(int i = 0; i < var_map.size(); ++i)
        {
            unsigned const size = 
            if(freebie > min_freebie)
                continue;

            int const neighbors = bitset_popcount(bitset_size, &interferences[i * bitset_size]);
            if(freebie < min_freebie || neighbors > max_neighbors)
            {
                min_freebie = freebie;
                max_neighbors = neighbors;
                best_i = i;
            }
        }



        unsigned min_freebie = ~0;
        int max_neighbors = -1;
        unsigned best_i = ~0u;
        for(int i = 0; i < var_map.size(); ++i)
        {
            unsigned const freebie = ~(usable_ram[i] & freebie_ram).popcount();
            if(freebie > min_freebie)
                continue;

            int const neighbors = bitset_popcount(bitset_size, &interferences[i * bitset_size]);
            if(freebie < min_freebie || neighbors > max_neighbors)
            {
                min_freebie = freebie;
                max_neighbors = neighbors;
                best_i = i;
            }
        }

        // OK. We've selected a node: 'best_i'.

        // Add our starting node to the set:
        bitset_clear_all(bitset_size, S);
        bitset_clear_all(bitset_size, S_neighbors);
        bitset_set(S, best_i);
        bitset_or(bitset_size, S_neighbors, &interferences[best_i * bitset_size]);
        S_usable_ram = usable_ram[best_i];

        // Remove from our uncolored graph:
        assert(bitset_test(G, best_i));
        bitset_clear(G, best_i);
    }


    // start with largest vars
    // use RLF as tie breaker

    // setup bitsets matching the locator's size
    // when a node is colored, modify its interfering sets's locator bitsets





    // OK let's allocate all vars.
    // 1. sort by usage (most used end up in zp)
    // 2. for each, in order of usage
    //   3. pick a valid ram location
    //   4. remove said allocated memory from interfering ram's options

    // So what interferes?
    // - global vars interfere with everything in the same group
    // - fn locals interfere with called fns


    // Fuck man, let's simplify
    // -- allocate globals first
    // -- allocate fns next, in order
    // - 








    ////////////////





    // RAM used by this function, or functions it calls.
    ram_bitset_t fn_allocated_ram = {}; // TODO: add arguments / returns

    // Each var we need to alloc tracks which RAM addresses it can go into.
    // - 'usable_ram' tracks un-allocated addresses
    // - 'sizable_ram' tracks addresses suitable for the start of the var
    // ('sizable_ram' == 'usable_ram' for 1-byte variables, and is a subset for multi-byte ones)
    std::vector<ram_bitset_t> usable_ram(store_set.size(), 0);
    //std::vector<ram_bitset_t> sizable_ram(store_set.size(), 0); // TODO: implement

    // A 2d array representing an interference graph.
    std::vector<bitset_uint_t> interferences(bitset_size * store_set.size(), 0);

    // Step-through the code backwards, maintaining a current liveness set
    // and using that to build an interference graph.
    bitset_uint_t* const live = ALLOCA_T(bitset_uint_t, bitset_size);
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it);
        bitset_copy(bitset_size, live, d.live.out);

        for(auto it = d.code.rbegin(); it != d.code.rend(); ++it)
        {
            asm_inst_t const& inst = *it;

            if(inst.op == JSR_ABSOLUTE)
            {
                // Variables live at function calls cannot use the same
                // memory that the function uses.

                ram_bitset_t const fn_ram = the_called_fn.locals_ram();
                freebie_ram |= fn_ram;
                bitset_for_each(bitset_size, live, [&](unsigned i)
                {
                    usable_ram[i] &= ~fn_ram;
                });

                continue;
            }

            auto const* lookup = store_set.find(inst.arg.cg_mem());
            if(!lookup)
                continue;
            unsigned const store_i = lookup->second;

            else if(op_input_regs(inst.op) & REGF_M)
                bitset_set(live, i);
            else if(op_output_regs(inst.op) & REGF_M)
                bitset_clear(live, i);
            else 
                continue; // No change to 'live' this iteration.

            if(indirect_addr_mode(op_addr_mode(inst.op)))
            {
                // The argument is a pointer, 
                // meaning it must be allocated to zero-page.
                usable_ram[i] &= zp_bitset;
                zp_only.push_back(i);
            }

            // Variables that are live together interfere with each other.
            // Update the interference graph here:
            bitset_for_each(bitset_size, live, [&](unsigned i)
            {
                bitset_or(bitset_size, &interferences[i * bitset_size], live);
            });
        }
    }

    // Nodes never interfere with themself:
    for(unsigned i = 0; i < store_set.size(); ++i)
        bitset_clear(&interferences[i * bitset_size], i);

    // TODO: Properly handle multi-byte variables / arrays.
    for(unsigned i = 0; i < store_set.size(); ++i)
    {
        sizable_ram[i] = usable_ram[i];
        //ram_for_size(usable_ram, store_set[i]
    }

    // Build frequency
    std::array<unsigned, ram_size> usable_freq;
    for(int i = 0; i < store_set.size(); ++i)
    {
        bitset_for_each(bitset_size, usable_ram[i].data(), [&](unsigned addr)
        {
            usable_freq[i] += 1;
        });
    }

    // Search for the next node to color:
    int max_var_size = -1;
    int min_freebie = INT_MAX;
    int min_ease = INT_MAX;
    unsigned best_i;
    for(i = TODO)
    {
        // Prioritize big nodes:
        unsigned const var_size = TODO;
        if(var_size < max_var_size)
            continue;

        // Prioritize nodes with the least amount of freebie ram:
        int const freebie = (usable_ram[i] & fn_allocated_ram).popcount();
        if(freebie > min_freebie)
            continue;

        // Otherwise make our selection based on our ram options and our neighbor count.
        int const ram = (usable_ram[i]).popcount();
        int const neighbors = bitset_popcount(bitset_size, interferences[i *bitset_size]);
        int const ease = ram - neighbors;

        if(var_size > max_var_size || freebie < min_freebie || ease < min_ease)
        {
            max_var_size = var_size;
            min_freebie = freebie;
            min_ease = ease;
            best_i = i;
        }
    }

    // Now color the chosen node, allocating it in RAM:
    unsigned min_penalty = ~0;
    unsigned best_addr;
    bitset_for_each(bitset_size, usable_ram[i].data(), [&](unsigned addr)
    {
        unsigned penalty = 0;

        // Penalize high-frequency:
        unsigned freq = 0;
        for(unsigned i = 0; i < var_size; ++i)
            freq = std::max(freq, usable_freq[addr + i]);
        penalty += freq * ram_size;

        // Penalize cross-page arrays:
        if((addr & 0xFF) + size > 0xFF)
           penalty += ram_size;

        // Penalize later addresses:
        penalty += addr;

        if(penalty < min_penalty)
        {
            min_penalty = penalty;
            best_addr = addr;
        }
    });

    // Update:
    bitset_clear(G, best_i);

    ram_bitset_t const allocated_ram = ram_bitset_t::filled(var_size, best_addr);
    ram_bitset_t const allocated_mask = ~allocated_ram;
    fn_allocated_ram |= allocated_ram;

    bitset_copy(bitset_size, temp_set, &interferences[best_i * bitset_size);
    bitset_and(bitset_size, temp_set, G);
    bitset_for_each(bitset_size, temp_set, [&](unsigned i)
    {
        usable_ram[i] &= allocated_mask;
        //TODO: fix sizable ram
        //sizable_ram[i] = usable_ram[i];
    });






    usable_ram[best_i];



    // 'G' holds uncolored graph nodes:
    bitset_uint_t* G = ALLOCA_T(bitset_uint_t, bitset_size); 
    bitset_set_n(bitset_size, G, store_set.size());
    assert(bitset_popcount(bitset_size, G) == store_set.size());
        
    // 'S' holds nodes colored to the same color, this iteration
    bitset_uint_t* S const = ALLOCA_T(bitset_uint_t, bitset_size);
    bitset_uint_t* S_neighbors const = ALLOCA_T(bitset_uint_t, bitset_size);
    ram_bitset_t S_usable_ram;

    // 1) Find the first node of a new colored set.
    // We'll prioritize selecting nodes with the least freebie options availible,
    // using neighbor count as a tie breaker (like the basic RLF graph coloring algorithm)
    {
        unsigned min_freebie = ~0;
        int max_neighbors = -1;
        unsigned best_i = ~0u;
        for(int i = 0; i < store_set.size(); ++i)
        {
            unsigned const freebie = ~(usable_ram[i] & freebie_ram).popcount();
            if(freebie > min_freebie)
                continue;

            int const neighbors = bitset_popcount(bitset_size, &interferences[i * bitset_size]);
            if(freebie < min_freebie || neighbors > max_neighbors)
            {
                min_freebie = freebie;
                max_neighbors = neighbors;
                best_i = i;
            }
        }

        // OK. We've selected a node: 'best_i'.

        // Add our starting node to the set:
        bitset_clear_all(bitset_size, S);
        bitset_clear_all(bitset_size, S_neighbors);
        bitset_set(S, best_i);
        bitset_or(bitset_size, S_neighbors, &interferences[best_i * bitset_size]);
        S_usable_ram = usable_ram[best_i];

        // Remove from our uncolored graph:
        assert(bitset_test(G, best_i));
        bitset_clear(G, best_i);
    }

    // Now add additional nodes:
    for(int i = 0; i < store_set.size(); ++i)
    {
        combined_usable_ram = S_usable_ram & usable_ram[i];
        // Can't share a color if it results in no usable ram:
        if(!combined_usable_ram)
        next_iter: continue;

        bitset_uint_t* const F = &interferences[i * bitset_size];

        // Can't share a color if we interfere:
        for(unsigned j = 0; j < bitset_size; ++j)
            if(S[j] & F[j])
                goto next_iter;

        unsigned freebie_count = 0;
        if(using_freebie)
        {
            freebie_count = (combined_usable_ram & freebie_ram).popcount();

            // Don't convert non-freebie sets into freebie sets.
            if(freebie_count == 0)
                continue;

            // Prioritize maximizing freebie ram used:
            if(freebie_count < max_freebie_count)
                continue;
        }

        // Count and maximize the number of nodes adjacent to both G and S.
        // (Like RLF graph coloring algorithm)
        num_neighbors_adjacent = 0;
        for(unsigned j = 0; j < bitset_size; ++j)
            num_neighbors_adjacent += builtin::popcount(G[j] & S_neighbors[j]);

        if(num_neighbors_adjacent < max_neighbors_adjacent)
            continue;

        // Othewise minimize the number of neighbors not in S.
        // (like RLF graph coloring algorithm)
        num_not_in_s = 0;
        for(unsigned j = 0; j < bitset_size; ++j)
            num_not_in_s += builtin::popcount(G[j] & ~S[j]);

        // Check for a new best here:
        if(freebie_count > max_freebie_count
           || num_neighbors_adjacent > max_neighbors_adjacent
           || num_not_in_s < min_not_in_s)
        {
            max_freebie_count = freebie_count;
            max_neighbors_adjacent = num_neighbors_adjacent;
            min_not_in_s = num_not_in_s;
            best_i = i;
        }
    }

    // Add it to S
    bitset_set(S, best_i);
    bitset_or(bitset_size, S_neighbors, interferences[best_i * bitset_size]);
    bitset_difference(bitset_size, S_neighbors, S);
    S_usable_ram &= usable_ram[best_i];

    // TODO: Remove it from G



    // eventually...
    
    // Pick a ram:

    for_each_bit(bitset_size, S_usable_ram, [&](unsigned i)
    {
        // Prefer ram addresses not in G nodes. 

        // Prefer non-ZP

        // Prefer larger

    });







        unsigned num_neighbors = 0;
        unsigned num_S_neighbors = 0;
        for(unsigned j = 0; j < bitset_size; ++j)
        {
            num_neighbors += builtin::popcount(G[j]);
            num_S_neighbors += builtin::popcount(G[j] & in_S[j]);
        }

        unsigned const num_G_neighbors = num_neighbors - num_S_neighbors;

        if(num_S_neighbors > most_S_neighbors
           || (num_S_neighbors == most_S_neighbors 
               && num_G_neighbors < least_G_neighbors))
        {
            most_S_neighbors = num_S_neighbors;
            least_G_neighbors = G_neighbors;
            best_i = i;
        }
    }

    // Abort if we found no nodes.
    if(most_S_neighbors <= 0)
        break;

    // Add our found node:
    bitset_set(in_S, best_i);



    // Sort by interference size.
    std::vector<std::pair<unsigned, unsigned>> elim_order(store_set.size());
    for(unsigned i = 0; i < store_set.size(); ++i)
    {
        elim_order[i].first = bitset_popcount(bitset_size, &pool[i * bitset_size]);
        elim_order[i].first += ram_restrict[i].popcount();
        elim_order[i].second = i;
    }
    for(unsigned i : zp_only)
        elim_order[i].first += 1 << 16; // Prioritize zp-only allocations.
    std::sort(elim_order.begin(), elim_order.end(), std::greater<>{});

    // Greedily allocate ram:
    for(auto& pair : elim_order)
    {
        unsigned const i = pair.second;

        ram_bitset_t ram = ram_restrict[i];

        ram_range_t range = allocate_ram(ram, 1);
        if(!range)
            throw std::runtime_error("TODO");

        bitset_for_each(TODO, [](unsigned bit)
        {
            // TODO
        });
    }

}
#endif
