#include "cg_conventional.hpp"

void make_conventional(ir_t& ir)
{
    static rh::robin_map<int, ssa_ht> pin_map;
    pin_map.clear();

    // TODO
    static std::vector<coalesce_class_t*> coalesce_worklist;
    coalesce_worklist.clear();

    // Deal with conditional nodes that have both edges going to the same node
    // by splitting the edge and inserting a new node.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        assert(cfg_node.output_size() <= 2);
        if(cfg_node.output_size() == 2 &&
           cfg_node.output(0) == cfg_node.output(1))
        {
            // Introduce a new node as the fix:
            ir.split_edge(cfg_node.output_edge(1));
        }
    }

    cfg_data_pool::scope_guard_t<cfg_out_data_t> cg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_out_data_t> sg(ssa_pool::array_size());


    // TODO: resize data?

    // Insert copies around phi nodes to make the ir conventional.
    // - One copy per input in each predecessor block
    // - One copy of the phi node itself
    // 
    // Also insert copies around functions, to ensure their vars are pinned to
    // the correct memory locations.
    // - One copy per return byte
    // - One copy per argument byte
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();

        for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
        {
            ssa_ht last = phi_it;

            // Insert copies in predecessor nodes.
            unsigned const input_size = phi_it->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = phi_it->input(i);
                type_t input_type = (input_v.is_const() ? type_t{TYPE_BYTE} 
                                                        : input_v->type());

                assert(!input_v.is_const() 
                       || input_v.whole() == (input_v.whole() & 0xFF));

                cfg_ht pred_h = cfg_node.input(i);
                cfg_node_t& pred_node = *pred_h;

                ssa_ht input_copy_h = pred_node.emplace_ssa(
                    SSA_exit_copy, input_type, input_v);
                phi_it->link_change_input(i, input_copy_h);

                _cset_append(last, input_copy_h);
                last = input_copy_h;

                pred_h.data<cfg_out_data_t>()
                    .exit_copies.push_back(input_copy_h);

                if(!input_v.holds_ref())
                    continue;

                ssa_ht input_h = input_v.handle();
                
                // Give the copy a scheduling index that occurs in the
                // last load of :
                int last_input_index = INT_MAX;

                if(input_h->cfg_node() == cfg_it)
                {
                    assert(!is_copy(input_h->op()));
                    last_input_index = input_h.data<ssa_out_d>().input;
                }

                unsigned output_size = input_h->output_size();
                for(unsigned j = 0; j < output_size; ++j)
                {
                    ssa_ht output_h = input_h->output(j);
                    auto& output_data = output_h.data<ssa_out_d>().index;

                    if(output_h->cfg_node() == cfg_it
                       && !is_copy(output_h->op())
                       && output_data.index < last_input_index)
                    {
                        last_input_index = output_data.index;
                    }
                }
                input_copy_h.data<ssa_out_d>().index = last_input_index;
            }

            // Create a copy of the phi node.
            ssa_ht phi_copy_h = cfg_node.emplace_ssa(
                SSA_entry_copy, phi_it->type());
            phi_it->replace_with(phi_copy_h);
            phi_copy_h->link_append_input(phi_it);

            cfg_data.entry_copies.push_back(phi_copy_h);

            // Give the copy a scheduling index that comes before everything:
            phi_copy_h.data<ssa_out_d>().index = -1;

            // Add both coalesce sets to the worklist.
            coalesce_worklist.push_back(phi_it);
            coalesce_worklist.push_back(phi_copy_h);
        }

        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            if(ssa_node.op() == SSA_get_return)
            {
                // Each fn call is pinned to return in specific memory regions.
                // Insert copies for each return point, freeing the pinning.
                // Then, these copies can maybe be coalesced.
                ssa_ht copy_h = cfg_node.emplace_ssa(SSA_copy, ssa_it->type());
                ssa_it->replace_with(copy_h);
                copy_h->link_append_input(ssa_it);

                ssa_data_pool::resize<ssa_out_data_t>(ssa_pool::array_size());
                auto& ssa_data = ssa_it.data<ssa_out_data_t>();
                ssa_node_t& ssa_node = *ssa_it;

                ssa_node_t& fn_node = *ssa_node.input(0);
                global_t& global = globals.global(fn_node);

                int const pin_out = 
                    global.fn->return_bytes[ssa_node.input(1).whole()];

                auto result = pin_map.insert({ pin_out, ssa_it });
                if(result.inserted)
                {
                    ssa_data.set_head.set(pin_out);
                    coalesce_worklist.push_back(ssa_it);
                }
                else
                {
                    _cset_append(*result.mapped, ssa_it);
                    *result.mapped = ssa_it;
                }
            }
            else if(ssa_it->op() == SSA_fn_call)
            {
                // Each fn call has pinned arguments too.
                // Insert copies for each.
                // Then, these copies can maybe be coalesced.
                global_t& global = globals.global(*ssa_it);

                unsigned const input_size = ssa_it->input_size();
                for(unsigned i = 0; i < input_size; ++i)
                {
                    ssa_ht copy_h = cfg_node.emplace_ssa(
                        SSA_copy, global.fn->arg_bytes_types[i],
                        ssa_it->input(i));
                    ssa_node.link_change_input(i, copy_h);

                    ssa_data_pool::resize<ssa_out_data_t>(
                        ssa_pool::array_size());
                    auto& copy_data = copy_h.data<copy_out_data_t>();

                    int const pin_out = 
                        global.fn->return_bytes[global.fn->arg_bytes[i]];

                    auto result = pin_map.insert({ pin_out, copy_h });
                    if(result.inserted)
                    {
                        copy_data.set_head.set(pin_out);
                        coalesce_worklist.push_back(copy_h);
                    }
                    else
                    {
                        _cset_append(*result.mapped, copy_h);
                        *result.mapped = copy_h;
                    }
                }
            }
        }
    }

    // Now calculate liveness sets.

    // TODO: init data?

    // TODO
    thread_local array_pool<unsigned> _bitset_pool;
    _bitset_pool.clear();

    std::size_t const live_set_size = bitset_size(ssa_data_pool::array_size());

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();
        cfg_node.clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        cfg_data.live_in  = _bitset_pool.alloc(live_set_size);
        cfg_data.live_out = _bitset_pool.alloc(live_set_size);
        bitset_set_all(live_set_size, cfg_data.live_out);

        assert(bitset_all_reset(live_set_size, cfg_data.live_in));
        assert(bitset_all_set(live_set_size, cfg_data.live_out));
        assert(cfg_data.processed == false);

        // Set 'live_in's initial value to be the set of variables used in
        // this node, minus any variables that were also defined here.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'live_out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set is sometimes called 'KILL')
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t const& ssa_node = *ssa_it;

            // Remove all ssa definitions introduced in this cfg node.
            bitset_reset(cfg_data.live_out, ssa_it.index);

            unsigned const input_size = ssa_node.input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = ssa_node.input(i);
                if(!input_v.holds_ref())
                    continue;
                ssa_node_t const& input = *input_v;
                // Only care about nodes defined in other basic blocks.
                if(input.cfg_node() == cfg_h)
                    continue;
                bitset_set(cfg_data.live_in, input_v.handle().index);
            }
        }
    }

    unsigned* temp_set = ALLOCA_T(unsigned, live_set_size);

    assert(ir.exit);
    cfg_worklist::clear();
    cfg_worklist::push(ir.exit);

    while(!cfg_worklist::empty())
    {
    reenter:
        cfg_ht cfg_h = cfg_worklist::pop();
        cfg_node_t& cfg_node = *cfg_h;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();

        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        bitset_reset_all(live_set_size, temp_set);
        unsigned const output_size = cfg_node.output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht succ_h = cfg_node.output(i);
            auto& succ_data = succ_h.data<cfg_out_data_t>();
            bitset_or(live_set_size, temp_set, succ_data.live_in);
        }

        bitset_and(live_set_size, temp_set, cfg_data.live_out /* (KILL) */);
        bitset_or(live_set_size, temp_set, cfg_data.live_in);

        // If 'live_in' is changing, add all predecessors to the worklist.
        if((cfg_node.flags & FLAG_PROCESSED) == 0
           || !bitset_eq(live_set_size, temp_set, cfg_data.live_in))
        {
            cfg_node.set_flags(FLAG_PROCESSED);
            unsigned const input_size = cfg_node.input_size();
            for(unsigned i = 0; i < input_size; ++i)
                cfg_worklist::push(cfg_node.input(i));
        }

        bitset_copy(live_set_size, cfg_data.live_in, temp_set);
    }

    // Some nodes (infinite loops, etc) might not be reachable travelling
    // backwards from the exit. Handle those now:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        if(!cfg_it->test_flags(FLAG_PROCESSED))
           cfg_worklist::push(cfg_it);
           
    if(!cfg_worklist::empty())
        goto reenter;

    // Now properly set 'live_out'
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();

        bitset_reset_all(live_set_size, cfg_data.live_out);
        unsigned const output_size = cfg_node.output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht succ_h = cfg_node.output(i);
            auto& succ_data = succ_h.data<cfg_out_data_t>();
            bitset_or(live_set_size, cfg_data.live_out, succ_data.live_in);
        }
    }

    // Now schedule the nodes.
    // This is before coalescing, because it's convenient to have a total
    // order of nodes when coalescing to determine interferences.

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& data = cfg_it.data<cfg_cgen_data_t>();
        data.schedule = _schedule_cfg_node(cfg_node);
    }



    // OK! The liveness sets and schedule are built.
    // Now to perform coalescing.

    // - Pick a candidate copy.
    // - Check if the two classes on each side of the copy interfere.
    // - If they don't, coalesce them.

#ifndef NDEBUG
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        ssa_it->clear_flags(FLAG_PROCESSED);
#endif

    // Pick a candidate coalesce class:
    while(coalesce_worklist.size())
    {
        ssa_ht to_start = coalesce_worklist.back();
        ssa_ht to_cset = _cset_get_first(to_start);
        coalesce_worklist.pop_back();

        for(ssa_ht ssa_it = to_start; ssa_it; ssa_it = _cset_next(ssa_it))
        {
            ssa_node_t& ssa_node = *ssa_it;

            // Each SSA node should be checked at most once.
        #ifndef NDEBUG
            assert(!ssa_node.test_flags(FLAG_PROCESSED));
            ssa_node.set_flags(FLAG_PROCESSED);
        #endif

            if(!is_copy(ssa_node.op()))
                continue;

            ssa_value_t input_v = ssa_node.input(0);
            if(!input_v.holds_ref())
                continue;

            ssa_ht from_cset = _cset_get_first(input_v.handle());

            // If both belong to the same coalesce class,
            // they've already been coalesced together.
            if(from_cset == to_cset)
                continue;

            ssa_ht to_h;
            for(to_h = to_cset; to_h; to_h = _cset_next(to_h))
            for(ssa_ht from_h = from_cset; from_h; from_h = _cset_next(from_h))
                if(_interfere_TODO(from_h, to_h))
                    goto cant_coalesce;

            // Can coalesce! Do it now:
            _cset_union(to_cset, to_h, from_cset);
        cant_coalesce:;
        }
    }

    // Schedule parallel copies:


    // - TODO: Remove duplicate copies!

    std::vector<ssa_ht> new_schedule;
    // TODO: reserve

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();
        _schedule_parallel_copies(cfg_data.entry_copies, new_schedule);

        // TODO Copy old schedule here:
        new_schedule.insert(new_schedule.end(),
                            cfg_data.schedule.begin(), cfg_data.schedule.end);

        _schedule_parallel_copies(cfg_data.exit_copies,  new_schedule);

        std::size_t const size = new_schedule.size();
        for(unsigned i = 0; i < size; ++i)
        {
            ssa_ht ssa_it = new_schedule[i];
            ssa_it.data<ssa_out_data_t>().schedule_i = i;
        }

        cfg_data.schedule = std::move(new_schedule);
    }


}
