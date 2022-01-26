#include "cg_liveness.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "worklist.hpp"

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
    set_size = bitset_size<>(pool_size);

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

void calc_asm_liveness(ir_t const& ir, cg_store_map_t const& store_set)
{
    using namespace liveness_impl;
    cg_data_resize();
    bitset_pool.clear();

    set_size = bitset_size<>(store_set.size());

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
        for(ainst_t const& inst : cg_data(cfg_it).code)
        {
            auto const* lookup = store_set.find(inst.arg);
            assert(lookup);
            unsigned const store_i = lookup->second;

            if((op_input_regs(inst.op) & REGF_M) && bitset_test(d.out, store_i))
                bitset_set(d.in, store_i);

            if(op_output_regs(inst.op) & REGF_M)
                bitset_clear(d.out, store_i);
        }
    }

    // temp_set will hold a node's actual out-set while the algorithm
    // is running.
    auto* temp_set = ALLOCA_T(bitset_uint_t, set_size);

    assert(ir.exit);
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

#if 0
TODO
{
    // Build the interference graph.
    
    std::vector<ram_bitset_t> ram_restrict(store_set.size(), 0);
    std::vector<pointers> zp_only;








    std::vector<bitset_uint_t> pool(set_size * store_set.size(), 0);
    std::vector<ram_bitset_t> ram_restrict(store_set.size(), 0);
    std::vector<pointers> zp_only;

    auto& d = cg_data(cfg_it);
    bitset_uint_t* live = TODO;
    bitset_copy(set_size, live, d.live_out);

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        for(auto it = d.code.rbegin(); it != d.code.rend(); ++it)
        {
            ainst_t const& inst = *it;

            auto const* lookup = store_set.find(inst.arg.cg_mem());
            if(!lookup)
                continue;
            unsigned const store_i = lookup->second;

            if(op_input_regs(inst.op) & REGF_M)
                bitset_set(live, i);
            else if(op_output_regs(inst.op) & REGF_M)
                bitset_clear(live, i);

            if(indirect_addr_mode(op_addr_mode(inst.op)))
            {
                // The argument is a pointer, meaning it must be allocated
                // to zero-page.
                ram_restrict[i] |= ~zp_bitset;
                zp_only.push_back(i);
            }


            if(inst.op == JSR_ABSOLUTE)
            {
                // TODO

                // Can't allocate
                ram_restrict[i] |= the_called_fn.locals_ram();

                assert(false);
            }

            bitset_for_each_bit(set_size, [&](unsigned bit)
            {
                bitset_or(set_size, &pool[bit * set_size], live);
            });
        }
    }

    // Sort by interference size.
    std::vector<std::pair<unsigned, unsigned>> elim_order(store_set.size());
    for(unsigned i = 0; i < store_set.size(); ++i)
    {
        elim_order[i].first = bitset_popcount(set_size, &pool[i * set_size]);
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
