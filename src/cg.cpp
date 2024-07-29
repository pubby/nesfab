#include "cg.hpp"

#include <map>
#ifndef NDEBUG
#include <iostream>
#endif

#include "flat/small_map.hpp"

#include "robin/map.hpp"
#include "robin/set.hpp"

#include "alloca.hpp"
#include "cg_isel.hpp"
#include "cg_liveness.hpp"
#include "cg_schedule.hpp"
#include "cg_cset.hpp"
#include "cg_ptr.hpp"
#include "compiler_error.hpp"
#include "globals.hpp"
#include "ir_algo.hpp"
#include "ir.hpp"
#include "locator.hpp"
#include "rom.hpp"
#include "asm_graph.hpp"

// TODO: make this way more efficient
/*
static bool _reaching(ssa_ht def, cfg_ht cfg, ssa_ht use, fc::vector_set<cfg_ht>& visited)
{
    if(visited.count(cfg))
        return true;
    visited.insert(cfg);

    for(ssa_ht ssa_it = cset_head(def); ssa_it; ssa_it = cset_next(ssa_it))
    {
        if(ssa_it->cfg_node() != cfg)
            continue;

        if(orig_def(ssa_it) == def)
            continue;

        if(ssa_it->cfg_node() == use->cfg_node())
            if(cg_data(ssa_it).schedule.index > cg_data(use).schedule.index)
                continue;

        if(ssa_it->cfg_node() == def->cfg_node())
            if(cg_data(ssa_it).schedule.index < cg_data(def).schedule.index)
                continue;

        return false;

    }

    if(cfg == def->cfg_node())
        return true;

    unsigned const input_size = cfg->input_size();
    for(unsigned i = 0; i < input_size; ++i)
        if(!_reaching(def, cfg->input(i), use, visited))
            return false;

    return true;
}

static bool _reaching(ssa_ht def, ssa_ht use)
{
    fc::vector_set<cfg_ht> visited;
    return _reaching(def, use->cfg_node(), use, visited);
}
*/

namespace
{
// The canonical IR representation has a single exit 'ir.exit',
// but for code generation purposes, it's better to have multiple 'rts' instructions.
// The code below handles this, splitting the single 'ir.exit' into multiple.
struct dupe_exit_t
{
    void run(cfg_ht orig_cfg, unsigned edge_i, cfg_ht duped_cfg)
    {
        this->orig_cfg = orig_cfg;
        this->edge_i = edge_i;
        this->duped_cfg = duped_cfg;
        map.clear();

        for(ssa_ht ssa_it = orig_cfg->first_daisy(); ssa_it; ssa_it = ssa_it->next_daisy())
            visit(ssa_it);

        for(ssa_ht ssa_it = orig_cfg->ssa_begin(); ssa_it; ++ssa_it)
            visit(ssa_it);
    }

private:
    cfg_ht orig_cfg;
    unsigned edge_i;
    cfg_ht duped_cfg;

    // A map from original nodes to duped nodes:
    fc::small_map<ssa_ht, ssa_value_t, 8> map;

    void visit(ssa_ht orig_ssa)
    {
        if(map.count(orig_ssa))
            return;

        if(orig_ssa->op() == SSA_phi)
        {
            map[orig_ssa] = orig_ssa->input(edge_i);
            return;
        }

        ssa_ht duped_ssa = duped_cfg->emplace_ssa(
            orig_ssa->op(), orig_ssa->type());
        duped_ssa->alloc_input(orig_ssa->input_size());

        if(orig_ssa->in_daisy())
            duped_ssa->append_daisy();

        map[orig_ssa] = duped_ssa;

        for(unsigned i = 0; i < orig_ssa->input_size(); ++i)
        {
            ssa_value_t input = orig_ssa->input(i);

            if(input.holds_ref() && input->cfg_node() == orig_cfg)
            {
                if(map.count(input.handle()) == 0)
                    visit(input.handle());
                assert(map.count(input.handle()));
                input = map[input.handle()];
            }

            duped_ssa->build_set_input(i, input);
        }
    }
};
}// end anon namespace

std::size_t code_gen(log_t* log, ir_t& ir, fn_t& fn)
{
    /////////////////////////////////////
    // CFG EDGE SPLITTING AND HOISTING //
    /////////////////////////////////////

    build_loops_and_order(ir);
    if(cg_hoist_bank_switches(fn, ir))
        build_loops_and_order(ir);
    split_critical_edges(ir, false);

    ////////////////
    // PREPARE IR //
    ////////////////

    for(cfg_node_t& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it;)
    {
        switch(ssa_it->op())
        {
        case SSA_cast:
        case SSA_as_bool:
            // Remove 'SSA_cast' and 'SSA_as_bool' nodes:
            ssa_it->replace_with(ssa_it->input(0));
            ssa_it = ssa_it->prune();
            break;

        case SSA_shl_table:
            // Convert shl_table to read_array8
            {
                assert(ssa_it->input(1).is_num());
                int const amount = ssa_it->input(1).whole();
                ssa_it->unsafe_set_op(SSA_read_array8);
                ssa_it->link_append_input(ssa_it->input(0));
                ssa_it->link_change_input(0, locator_t::runtime_rom(shl_table(amount)));
                ssa_it->link_change_input(1, ssa_value_t(0u, TYPE_U20));
            }
            break;


        default:
            ++ssa_it;
        }
    }

    /////////////////////////
    // BRANCH INSTRUCTIONS //
    /////////////////////////

    // Replace 'SSA_if's with 'SSA_branch's, if possible:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {

        if(cfg_it->output_size() == 1)
        {
            ssa_ht h = cfg_it->emplace_ssa(SSA_jump, TYPE_VOID);
            h->append_daisy();
            continue;
        }
        else if(cfg_it->output_size() == 0)
            continue;

        ssa_ht if_h = cfg_it->last_daisy();

        if(if_h->op() != SSA_if)
            continue;

        ssa_value_t condition = if_h->input(0);

        if(!condition.holds_ref() || condition->cfg_node() != cfg_it)
            continue;

        if(condition->output_size() != 1)
            continue;

        if(condition->in_daisy())
            continue;

        switch(condition->op())
        {
        case SSA_multi_eq:
            condition->unsafe_set_op(SSA_branch_eq); 
            break;
        case SSA_multi_not_eq:
            condition->unsafe_set_op(SSA_branch_not_eq); 
            break;
        case SSA_multi_lt:
            condition->unsafe_set_op(SSA_branch_lt); 
            break;
        case SSA_multi_lte:
            condition->unsafe_set_op(SSA_branch_lte); 
            break;
        case SSA_sign:
            condition->unsafe_set_op(SSA_branch_sign); 
            break;
        case SSA_not_sign:
            condition->unsafe_set_op(SSA_branch_not_sign); 
            break;
        default: 
            continue;
        }

        if_h->prune();
        condition->append_daisy();
    }

    ////////////////
    // ROM ARRAYS //
    ////////////////

    locate_rom_arrays(ir, fn.rom_proc());
    ir.assert_valid(true);

    ///////////////////
    // DUPLICATE RTS //
    ///////////////////

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it;)
    {
        if(cfg_it == ir.root
           || cfg_it->output_size() != 0
           || !cfg_it->last_daisy() 
           || cfg_it->last_daisy()->op() != SSA_return)
        {
            ++cfg_it;
            continue;
        }

        dupe_exit_t duper;
        while(cfg_it->input_size())
        {
            cfg_ht duped_cfg = ir.emplace_cfg(cfg_it->prop_flags());
            duper.run(cfg_it, 0, duped_cfg);

            auto ie = cfg_it->input_edge(0);
            ie.handle->link_change_output(ie.index, duped_cfg,
                [](ssa_ht phi) { assert(false); return ssa_value_t(0u, TYPE_VOID); });
        }
        cfg_it = ir.prune_cfg(cfg_it);
    }

    ///////////////////
    // ALLOCATE CG_D //
    ///////////////////

    cfg_data_pool::scope_guard_t<cfg_cg_d> cg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_cg_d> sg(ssa_pool::array_size());

    ////////////////////
    // COPY INSERTION //
    ////////////////////

    ir.assert_valid(true);

    // Copies will be inserted to convert out of SSA form.
    // Additionally, copies will be used to pin locators to memory.

    struct copy_t
    {
        ssa_ht node;
        unsigned cost;
    };
    
    struct global_loc_data_t
    {
        // Holds the the coalesced set of all nodes using this locator:
        ssa_ht cset = {};

        // Holds all the SSA_read_global and SSA_early_store
        // copies used to implement locators:
        std::vector<copy_t> copies;

        // Used to implement constant writes to global memory.
        std::map<locator_t, bc::small_vector<ssa_ht, 8>> const_stores;
    };

    // Build a cache of the IR to be used by various cset functions:
    auto const cache = cset_build_cache(ir);

    // Maps specific locators - global reads and writes - to their copies.
    rh::batman_map<locator_t, global_loc_data_t> global_loc_map;

    bc::small_vector<copy_t, 32> phi_copies;
    bc::small_vector<ssa_ht, 16> phi_csets;

    auto const arg_ret_interferes = [&](ssa_ht h, locator_t loc) -> bool
    {
        if(is_arg_ret(loc.lclass()))
        {
            for(auto& called_pair : global_loc_map)
            {
                locator_t const called_loc = called_pair.first;

                if(!called_pair.second.cset
                   || !is_arg_ret(called_loc.lclass()) 
                   || called_loc.maybe_fn() == fn.handle() 
                   || (called_loc.maybe_fn() 
                       && called_loc.maybe_fn() == loc.maybe_fn() 
                       && loc.lclass() == called_loc.lclass()))
                {
                    continue;
                }

                ssa_ht const called_head = cset_head(called_pair.second.cset);
                for(ssa_ht i = called_head; i; i = cset_next(i))
                    if(h != i && live_range_overlap(h, i))
                        return true;
            }
        }

        return false;
    };

    auto const cset_arg_ret_interferes = [&](ssa_ht a, ssa_ht b) -> bool
    {
        locator_t const loc_a = cset_locator(a);
        locator_t const loc_b = cset_locator(b);

        if(is_arg_ret(loc_b.lclass()))
            for(ssa_ht i = cset_head(a); i; i = cset_next(i))
                if(arg_ret_interferes(i, loc_b))
                    return true;

        if(is_arg_ret(loc_a.lclass()))
            for(ssa_ht i = cset_head(b); i; i = cset_next(i))
                if(arg_ret_interferes(i, loc_a))
                    return true;

        return false;
    };

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_op_t const op = ssa_it->op();

        // Setup 'ptr_alt's for ptr inputs.
        if(ssa_derefs_ptr(ssa_it->op()))
        {
            unsigned const PTR = ssa_ptr_input(ssa_it->op());
            unsigned const PTR_HI = ssa_ptr_hi_input(ssa_it->op());
            assert(ssa_it->input(PTR).holds_ref() == ssa_it->input(PTR_HI).holds_ref());

            if(ssa_it->input(PTR).holds_ref() && ssa_it->input(PTR_HI).holds_ref())
            {
                ssa_ht const lo = ssa_it->input(PTR).handle();
                ssa_ht const hi = ssa_it->input(PTR_HI).handle();
                auto& lo_d = cg_data(lo);
                auto& hi_d = cg_data(hi);

                assert(!lo_d.ptr_alt || (lo_d.ptr_alt == hi && lo_d.is_ptr_hi == false));
                assert(!hi_d.ptr_alt || (hi_d.ptr_alt == lo && hi_d.is_ptr_hi == true));

                lo_d.ptr_alt = hi;
                lo_d.is_ptr_hi = false;
                hi_d.ptr_alt = lo;
                hi_d.is_ptr_hi = true;
            }
        }

        // Use copies when going into or out of global variables.
        // This is needed to correctly implement the loads and stores
        // of these pinned memory locations.

        // Most of these copies will get coalesced shortly after.

        if(op == SSA_read_global)
        {
            // Consider 'SSA_read_global' to be a copy in its own right.
            locator_t const loc = ssa_it->input(1).locator().mem_head();
            global_loc_map[loc].copies.push_back({ ssa_it });

            if(is_arg_ret(loc.lclass()) && loc.maybe_fn() != fn.handle())
                cg_data(ssa_it).call = loc;

            ir.assert_valid(true);
        }
        else if(ssa_flags(op) & SSAF_WRITE_GLOBALS)
        {
            // Global writes will get a tentative SSA_early_store node.
            // If this early_store can coalesce, we'll keep it.
            // Otherwise, it will be pruned later on.

            // We'll also add a SSA_late_store node,
            // closer to the actual write.

            unsigned const input_size = ssa_it->input_size();
            for(unsigned i = write_globals_begin(op); i < input_size; i += 2)
            {
                locator_t const loc = ssa_it->input(i + 1).locator().mem_head();
                ssa_fwd_edge_t ie = ssa_it->input_edge(i);

                ssa_ht late = {};
                global_loc_data_t& ld = global_loc_map[loc];

                if(ie.is_const())
                {
                    // Constant early_stores are handled later on,
                    // as it takes analysis to determine where to insert the copy.

                    // However, we can insert late_stores now:
                    late = cfg_it->emplace_ssa(SSA_late_store, ie.type(), ie);
                    ssa_it->link_change_input(i, late);

                    // Then track the late store for later:
                    locator_t c;
                    if(ie.is_num())
                    {
                        assert(is_byte(ie.fixed()));
                        c = locator_t::const_byte(ie.whole());
                    }
                    else if(ie.is_locator())
                        c = ie.locator();

                    // Update ld:
                    ld.const_stores[c].push_back(late);
                    ld.copies.push_back({ late });
                }
                else if(ie.holds_ref())
                {
                    // Create a new SSA_early_store node here:

                    ssa_ht const early = split_output_edge(ie.handle(), true, ie.index(), SSA_early_store);
                    assert(early->output_size() == 1);
                    early->link_append_input(loc);

                    // Then create a new SSA_late_store:
                    ie = ssa_it->input_edge(i);
                    assert(ie.handle()->op() == SSA_early_store);
                    late = split_output_edge(ie.handle(), false, ie.index(), SSA_late_store);
                    assert(early->output_size() == 1);
                    assert(late->output_size() == 1);
                    late->link_append_input(loc);

                    assert(early->type() == late->type());

                    // Track them:
                    ld.copies.push_back({ early });
                    ld.copies.push_back({ late });
                }

                // Resize the pool:
                ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());

                // Setup late's cset:
                assert(late);
                if(ld.cset)
                    cset_append(cset_last(ld.cset), late);
                else
                {
                    ld.cset = late;
                    cg_data(late).cset_head = loc;
                }
            }

            ir.assert_valid(true);
        }
        else if(op == SSA_phi)
        {
            // Insert a copy of every phi argument at the end of every
            // predecessor of the phi's cfg node.
            // This is needed to convert out of SSA.

            // The cset of the phi copies will have a unique locator.
            // (These locators may be merged)
            locator_t const loc = locator_t::phi(ssa_it);
            ssa_value_t last = loc;

            unsigned const input_size = ssa_it->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                assert(input_size == ssa_it->input_size());
                assert(input_size == cfg_it->input_size());

                cfg_ht cfg_pred = cfg_it->input(i);
                ssa_fwd_edge_t ie = ssa_it->input_edge(i);

                ir.assert_valid(true);
                
                ssa_ht copy;
                if(ie.holds_ref())
                {
                    ir.assert_valid(true);
                    ssa_value_t input = ie.handle();
                    ssa_ht const store = split_output_edge(input.handle(), true, ie.index(), SSA_early_store);
                    assert(store->output_size() == 1);
                    copy = cfg_pred->emplace_ssa(SSA_phi_copy, ssa_it->type(), store);
                    phi_copies.push_back({ copy });
                }
                else
                {
                    copy = cfg_pred->emplace_ssa(SSA_phi_copy, ssa_it->type(), ie);
                }

                ssa_it->link_change_input(i, copy);
                ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());

                // Add 'copy' to the daisy chain:
                assert(cfg_pred->last_daisy());
                copy->insert_daisy(cfg_pred->last_daisy());

                // Add it to the cset.
                // (All related phi_copys will belong to the same cset.)
                last = cset_append(last, copy);
                assert(cset_locator(copy) == loc);

                ir.assert_valid(true);
            }

            if(last.holds_ref())
                phi_csets.push_back(last.handle());

            ir.assert_valid(true);
        }
    }

    //////////////
    // RESIZING //
    //////////////

    // Some nodes may be created after scheduling. 
    // Estimate an upper bound for the number of nodes needed here:
    unsigned reserve = 0;
    for(auto& pair : global_loc_map)
    for(auto& pair : pair.second.const_stores)
    {
        assert(pair.second.size() > 0);
        reserve += ((pair.second.size()) * (pair.second.size() + 1)) / 2;
    }

    // Then reserve extra space:
    unsigned reserved_size = ssa_pool::array_size() + reserve;
    ssa_data_pool::resize<ssa_cg_d>(reserved_size);

    ////////////////
    // SCHEDULING //
    ////////////////

    constexpr std::size_t warn_ssa_size = 50000;
    std::size_t const ssa_size = ir.ssa_size();
    if(ssa_size >= warn_ssa_size)
    {
        compiler_warning(fn.global.pstring(), fmt(
            "Function is generating a lot of code (% nodes). Consider breaking it up into separate functions, or reducing inlining.",
            ssa_size));
    }
    
    ir.assert_valid(true);
    schedule_ir(ir);
    o_schedule(ir);

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it);
        assert(d.phi_order.empty());

        for(ssa_ht phi = cfg_it->phi_begin(); phi; ++phi)
            d.phi_order.push_back(phi);

        std::sort(d.phi_order.begin(), d.phi_order.end(), [](ssa_ht a, ssa_ht b)
        {
            return cg_data(a).schedule.index < cg_data(b).schedule.index;
        });
    }


    if(std::ostream* os = fn.info_stream())
    {
        *os << "\nSCHEDULE_START " << fn.global.name << '\n';

        for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        {
            *os << "  SCHEDULE_CFG " << cfg_it.id << '\n';
            auto& d = cg_data(cfg_it);
            for(ssa_ht h : d.schedule)
                *os << "    " << h->op() << ' ' << h.id << '\n';
        }
    }

    ///////////////////////////
    // LIVENESS SET CREATION //
    ///////////////////////////

    // Tries to insert 'node' into the cset of 'ld'.
    auto const coalesce_loc = [&](locator_t loc, global_loc_data_t& ld, ssa_ht node)
    {
        assert(node);

        ssa_ht const head = cset_head(node);

        // Check to see if the copy can be coalesced.
        // i.e. its live range doesn't overlap any point where the
        // locator is already live.

        dprint(log, "---COALESCE_LOC", loc, node, cset_locator(node));

        if(arg_ret_interferes(node, loc))
        {
            dprint(log, "----FAILURE ARG RET");
            return false;
        }

        if(ld.cset)
        {
            dprint(log, "----CSET FOUND");
            ld.cset = cset_head(ld.cset);

            // If they already are coalesced:
            if(head == ld.cset)
                return true;

            assert(cset_is_head(ld.cset));
            ssa_ht last = csets_dont_interfere(fn.handle(), ir, ld.cset, head, cache);
            if(!last) // If they interfere
                goto fail;
            // It can be coalesced; add it to the cset.
            ld.cset = cset_head(cset_append(last, head));
            assert(cset_is_head(ld.cset));
            assert(loc == cset_locator(ld.cset));
        }
        else
        {
            for(ssa_ht s : cache.special)
                for(ssa_ht h = head; h; h = cset_next(h))
                    if(special_interferes(fn.handle(), ir, h, loc, s))
                        if(live_at_def(h, s))
                            goto fail;

            // It can be coalesced; create a new set out of it;
            ld.cset = head;
            assert(cset_is_head(ld.cset));
            assert(ld.cset);

            // Also tag it to a locator:
            if(!cg_data(head).cset_head)
                cg_data(head).cset_head = loc;
        }

        dprint(log, "----SUCCESS");
        return true;

    fail:
        dprint(log, "----FAILURE");
        return false;
    };

    calc_ssa_liveness(ir, ssa_data_pool::array_size());

    // Note: once the live sets have been built, the IR cannot be modified
    // until all liveness checks are done.
    // Otherwise, the intersection tests will be buggy.

    passert(reserved_size >= ssa_pool::array_size(),
            reserved_size, ssa_pool::array_size());

#if 0
        std::cout << fn.global.name << std::endl;
        for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        {
            std::cout << cfg_it << std::endl;
            auto& d = cg_data(cfg_it);
            for(ssa_ht h : d.schedule)
                std::cout << "    " << h->op() << ' ' << h.id << std::endl;
        }
#endif

    ////////////////
    // COALESCING //
    ////////////////

    // Build order first:
    build_loops_and_order(ir);

    // Coalesce locators.

    auto const prune_early_store = [&](ssa_ht store, ssa_ht* new_head = nullptr) -> ssa_ht
    {
        assert(store->op() == SSA_early_store);

        ssa_ht const parent = store->input(0).handle();

        cset_remove(store);
        clear_liveness_for(ir, store);
        store->replace_with(parent);
        ssa_ht ret = store->prune();

        unsigned const index = cg_data(store).schedule.index;
        auto& schedule = cg_data(store->cfg_node()).schedule;
        assert(schedule.begin() + index < schedule.end());
        assert(schedule[index] == store);
        for(unsigned i = index+1; i < schedule.size(); ++i)
            cg_data(schedule[i]).schedule.index -= 1;
        schedule.erase(schedule.begin() + index);

        clear_liveness_for(ir, parent);
        calc_ssa_liveness(parent);

        // Because parent's liveness changed, it may no longer be compatible with its cset.
        // Thus, we'll remove it if necessary:
        locator_t const loc = cset_locator(parent);
        for(ssa_ht i = cset_head(parent); i; i = cset_next(i))
            if(i != parent && live_at_def(parent, i))
                goto remove;

        if(false)
        {
        remove:
            ssa_ht const nh = cset_remove(parent);
            if(new_head)
                *new_head = nh;

            if(auto* p = global_loc_map.mapped(loc))
                if(p->cset == parent)
                    p->cset = nh;
        }

        return ret;
    };

    // First coalesce gvar read/writes, mostly with other read/writes of the same locator,
    // but also with some SSA_phi nodes.
    for(auto& pair : global_loc_map)
    {
        auto& ld = pair.second;

        // Prioritize less busy ranges over larger ones.
        for(copy_t& copy : ld.copies)
            copy.cost = live_range_busyness(ir, copy.node);
        std::sort(ld.copies.begin(), ld.copies.end(),
            [](copy_t const& a, copy_t const& b) { return a.cost < b.cost; });
    }

    // Do the coalescing for early stores first:
    for(auto& pair : global_loc_map)
    {
        locator_t const loc = pair.first;
        auto& ld = pair.second;

        for(copy_t const& copy : ld.copies)
        {
            if(copy.node->op() != SSA_early_store)
                continue;

            if(!coalesce_loc(loc, ld, copy.node))
                prune_early_store(copy.node, &ld.cset);
        }
    }

    // Then do the coalescing for the others:
    for(auto& pair : global_loc_map)
    {
        locator_t const loc = pair.first;
        auto& ld = pair.second;

        for(copy_t const& copy : ld.copies)
            if(copy.node->op() && copy.node->op() != SSA_early_store)
                coalesce_loc(loc, ld, copy.node);
    }

    ir.assert_valid(true);

    // Coalesce phis:

    // First try to coalesce 'SSA_phi's with their input 'SSA_phi_copy's.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht phi_it = cfg_it->phi_begin(); phi_it; ++phi_it)
    {
        assert(phi_it->input(0).holds_ref());
        assert(phi_it->input(0)->op() == SSA_phi_copy);

        // Search for a ssa_node input:
        for(unsigned i = 0; i < phi_it->input_size(); ++i)
        {
            if(!phi_it->input(i).holds_ref())
                continue;

            assert(phi_it->input(i).is_handle());
            assert(phi_it->input(i)->op() == SSA_phi_copy);

            ssa_ht cset = cset_head(phi_it->input(i).handle());
            ssa_ht phi_cset = cset_head(phi_it);

            if(ssa_ht last = csets_dont_interfere(fn.handle(), ir, cset, phi_cset, cache))
                if(!cset_arg_ret_interferes(cset, phi_cset))
                    cset_append(last, phi_cset);

            break;
        }
    }

    passert(reserved_size >= ssa_pool::array_size(),
            reserved_size, ssa_pool::array_size());

    ir.assert_valid(true);

    // Prioritize less busy ranges over larger ones.
    for(copy_t& copy : phi_copies)
        copy.cost = live_range_busyness(ir, copy.node->input(0).handle());
    std::sort(phi_copies.begin(), phi_copies.end(),
    [](copy_t const& a, copy_t const& b) { return a.cost < b.cost; });

    // Coalesce 'SSA_phi_copy's with their input early_stores.
    for(copy_t const& copy : phi_copies)
    {
        assert(copy.node);
        assert(copy.node->op() == SSA_phi_copy);

        ssa_ht candidate = copy.node->input(0).handle();

        assert(candidate);
        assert(candidate->op() == SSA_early_store);

        ssa_ht copy_cset      = cset_head(copy.node);
        ssa_ht candidate_cset = cset_head(candidate);

        assert(cset_locators_mergable(cset_locator(copy_cset), cset_locator(candidate_cset)));

        if(ssa_ht last = csets_dont_interfere(fn.handle(), ir, copy_cset, candidate_cset, cache))
        {
            if(!cset_arg_ret_interferes(copy_cset, candidate_cset))
            {
                cset_append(last, candidate_cset);
                continue;
            }
        }
        prune_early_store(candidate);
    }

    passert(reserved_size >= ssa_pool::array_size(),
            reserved_size, ssa_pool::array_size());

    ir.assert_valid(true);

    // Coalesce early stores with their parent
    {
        bc::small_vector<std::pair<int, ssa_ht>, 32> early_stores;

        for(cfg_node_t& cfg_node : ir)
        {
            early_stores.clear();

            for(ssa_ht store = cfg_node.ssa_begin(); store; ++ store)
            {
                if(store->op() != SSA_early_store || !store->input(0).holds_ref())
                    continue;

                assert(store->output_size() == 1);

                ssa_ht const input = store->input(0).handle();
                ssa_value_t const orig_input = orig_def(input);
                ssa_ht const output = store->output(0);

                int score = cg_data(store).schedule.index;

                score += algo(output->cfg_node()).preorder_i * 256;

                if(orig_input->op() == SSA_read_global && output->op() == SSA_late_store 
                   && orig_input->input(1) != output->input(1))
                {
                    score -= 256;
                }

                if(output->cfg_node() != input->cfg_node())
                    score += 512 * 256; // Arbitrary penalty.

                if(store->cfg_node() != input->cfg_node())
                    score += 512 * 256; // Arbitrary penalty.

                if(store->cfg_node() != output->cfg_node())
                    score += 1024 * 256; // Arbitrary penalty.

                early_stores.push_back(std::make_pair(score, store));
            }

            std::sort(early_stores.begin(), early_stores.end());

            for(auto const& pair : early_stores)
            {
                ssa_ht store = pair.second;

                assert(store->input(0).holds_ref());
                ssa_ht parent = store->input(0).handle();

                ssa_ht store_cset = cset_head(store);
                ssa_ht parent_cset = cset_head(parent);

                passert(cset_locator(store_cset), store_cset);

                ssa_ht last;

                last = csets_appendable(fn.handle(), ir, store_cset, parent_cset, cache);

                if(last && !cset_arg_ret_interferes(store_cset, parent_cset))
                {
                    dprint(log, "-COALESCE_EARLY_STORE", store, parent, store->output(0), 
                           cset_locator(store), cset_locator(parent));
                    cset_append(last, parent_cset);
                    store->unsafe_set_op(SSA_aliased_store);

                    assert(cset_locator(store_cset));
                    assert(cset_locator(store_cset) == cset_locator(parent_cset));
                    assert(cset_head(store) == cset_head(parent));
                }
                else
                {
                    assert(store->output_size() == 1);
                    ssa_ht use = store->output(0);

                    if(is_array(store->type().name())
                       || loop_depth(store->cfg_node()) > loop_depth(use->cfg_node()))
                    {
                        dprint(log, "-FAIL_COALESCE_EARLY_STORE", store, parent, store->output(0), 
                               cset_locator(store), cset_locator(parent), (bool)last);
                        // The early store is either an array copy, or inside a loop, 
                        // meaning it will likely slow the code down.
                        // Thus, let's remove it.
                        store = prune_early_store(store);
                    }
                }
            }
        }
    }

    // Coalesce array operations
    for(cfg_ht cfg_it : postorder)
    {
        auto& d = cg_data(cfg_it);
        for(ssa_ht h : d.schedule)
        {
            if(!(ssa_flags(h->op()) & SSAF_WRITE_ARRAY))
                continue;

            if(!h->input(0).holds_ref())
                continue;

            ssa_ht const parent = h->input(0).handle();

            ssa_ht const this_cset = cset_head(h);
            ssa_ht const parent_cset = cset_head(parent);

            if(ssa_ht last = csets_appendable(fn.handle(), ir, this_cset, parent_cset, cache))
            {
                if(!cset_arg_ret_interferes(this_cset, parent_cset))
                {
                    cset_append(last, parent_cset);
                    dprint(log, "-COALESCE_ARRAY", h, parent, cset_locator(parent_cset));
                    assert(cset_head(h) == cset_head(parent));
                }
            }

            dprint(log, "-COALESCE_ARRAY_FAIL", h);
        }
    }

    // Now insert early_stores for constants, trying to minimize the amount of stores needed.
    build_loops_and_order(ir); // We'll need loop information eventually.
    build_dominators_from_order(ir);
    for(auto& pair : global_loc_map)
    {
        locator_t const loc = pair.first;
        auto& ld = pair.second;

        for(auto& pair : ld.const_stores)
        {
            auto& vec = pair.second;

            // If only a single constant is stored, a const_store isn't needed.
            if(vec.size() <= 1)
                continue;

            dprint(log, "--TRY CONST STORE", loc);

            // Otherwise we'll try to find 2 stores and combine them into 1.
            for(unsigned i = 0; i < vec.size()-1; ++i)
            for(unsigned j = i+1; j < vec.size(); ++j)
            {
                assert(i != j);

                // 'a' and 'b' are the two nodes we're trying to combine:
                ssa_ht a = vec[i];
                ssa_ht b = vec[j];

                cfg_ht a_cfg = a->cfg_node();
                cfg_ht b_cfg = b->cfg_node();

                // Create the store in a dominating spot:
                cfg_ht store_cfg = dom_intersect(a_cfg, b_cfg);
                ssa_value_t const v = pair.first.lclass() == LOC_CONST_BYTE
                                      ? ssa_value_t(pair.first.data(), TYPE_U) 
                                      : ssa_value_t(pair.first);
                ssa_ht store = store_cfg->emplace_ssa(SSA_const_store, v.type(), v);
                assert(ssa_data_pool::array_size() >= ssa_pool::array_size());
                auto& store_d = cg_data(store);

                a->link_change_input(0, store);
                b->link_change_input(0, store);

                if(store_cfg == a_cfg)
                {
                    // If both are the same, pick the earliest one
                    if(store_cfg == b_cfg && cg_data(b).schedule.index < cg_data(a).schedule.index)
                        goto before_b;

                    // pick the rank before 'a'
                    store_d.schedule.index = cg_data(a).schedule.index-1;
                }
                else if(store_cfg == b_cfg)
                {
                    // pick the rank before 'b'
                before_b:
                    store_d.schedule.index = cg_data(b).schedule.index-1;
                }
                else
                {
                    assert(store_cfg->last_daisy());
                    auto& last_d = cg_data(store_cfg->last_daisy());

                    store_d.schedule.index = last_d.schedule.index-1;
                }

                // Now try to coalesce it into the locator's cset.
                calc_ssa_liveness(store);

                if(coalesce_loc(loc, ld, store))
                {
                    dprint(log, "---COALESCED", loc, a, b, pair.first.data());
                    if(a->op() == SSA_const_store)
                        a->unsafe_set_op(SSA_aliased_store);
                    if(b->op() == SSA_const_store)
                        b->unsafe_set_op(SSA_aliased_store);

                    // 'i' becomes the new store:
                    vec[i] = store;

                    // remove 'j':
                    std::swap(vec[j], vec.back());
                    vec.pop_back();

                    int const index = store_d.schedule.index+1;

                    // add the store to the schedule, for real
                    auto& schedule = cg_data(store_cfg).schedule;
                    schedule.insert(schedule.begin() + index, store);

                    for(int k = index; k < schedule.size(); ++k)
                        cg_data(schedule[k]).schedule.index += 1;

#ifndef NDEBUG
                    for(int k = 0; k < (int)schedule.size(); ++k)
                        assert(cg_data(schedule[k]).schedule.index == k);
#endif

                    --i;
                    break;
                }
                else
                {
                    // Abort! Undo everything and prune it.
                    clear_liveness_for(ir, store);
                    store->replace_with(v);
                    store->prune();
                }
            }
        }
    }


    passert(reserved_size >= ssa_pool::array_size(),
            reserved_size, ssa_pool::array_size());

    ir.assert_valid(true);

    // Now update the IR.
    // (Liveness checks can't be done after this.)

#if 0 // TODO
    fc::small_set<ssa_ht, 32> unique_csets;
    for(auto& pair : global_loc_map)
    {
        //std::puts("x");
        if(pair.second.cset)
        {
            assert(pair.second.cset->op() != SSA_null);
            //std::cout << pair.second.cset->op() << '\n';
            unique_csets.insert(cset_head(pair.second.cset));
        }
        //std::puts("xx");
    }
    for(ssa_ht h : phi_csets)
    {
        std::puts("y");
        unique_csets.insert(cset_head(h));
    }

    // TODO
    std::puts("coalesce phis 5");
    for(ssa_ht cset : unique_csets)
    {
        assert(cset_is_head(cset));
        locator_t const loc = cset_locator(cset);

        for(ssa_ht ssa_it = cset; ssa_it; ssa_it = cset_next(ssa_it))
        {
            assert(cset_locator(ssa_it) == loc);

            //ssa_it->set_flags(FLAG_COALESCED);

            if(ssa_it->op() == SSA_early_store)
            {
                //ssa_ht orig = ssa_it->input(0).handle();

                // Replace uses of 'orig' with uses of 'ssa_it' whenever
                // said uses occur inside the live range of 'ssa_it'.
                /*
                for(unsigned i = 0; i < orig->output_size();)
                {
                    auto oe = orig->output_edge(i);
                    if(oe.input_class() == INPUT_VALUE
                       && !(ssa_flags(oe.handle->op()) & SSAF_COPY)
                       && ((loc.lclass() == LOC_CALL_ARG && live_at_def(ssa_it, oe.handle))
                           || (loc.lclass() != LOC_CALL_ARG && _reaching(orig, oe.handle))))
                    {
                        assert(ssa_it != oe.handle);
                        oe.handle->link_change_input(oe.index, ssa_it);
                    }
                    else
                        ++i;
                }
                */
            }

        }
    }
    std::puts("coalesce phis 6");
#endif

    // Merge additional csets to aid memory reuse:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_ht input;

        switch(ssa_it->op())
        {
        case SSA_rol:
        case SSA_ror:
            if(!ssa_it->input(0).holds_ref())
                continue;
            input = ssa_it->input(0).handle();
            break;

        case SSA_add:
        case SSA_sub:
        case SSA_and:
        case SSA_or:
        case SSA_xor:
            if(ssa_it->input(1).is_num() && ssa_it->input(0).holds_ref())
            {
                input = ssa_it->input(0).handle();
                break;
            }
            else if(ssa_it->input(0).is_num() && ssa_it->input(1).holds_ref())
            {
                input = ssa_it->input(1).handle();
                break;
            }
            continue;

        default:
            continue;
        }

        ssa_ht head_a = cset_head(ssa_it);
        ssa_ht head_b = cset_head(input);

        ssa_ht last = csets_appendable(fn.handle(), ir, head_a, head_b, cache);
        if(!last) // If they interfere
            continue;

        if(cset_arg_ret_interferes(head_a, head_b))
            continue;

        dprint(log, "-COALESCE_ADDITIONAL", ssa_it);

        // It can be coalesced; add it to the cset.
        cset_append(last, head_b);

        assert(cset_locator(ssa_it) == cset_locator(input));
    }

    passert(reserved_size >= ssa_pool::array_size(),
            reserved_size, ssa_pool::array_size());

    // Coalesce indirect pointers

    auto const valid_ptr_loc = [&](locator_t loc, bool hi) -> bool
    {
        return (!loc 
                || lvars_manager_t::is_this_lvar(fn.handle(), loc)
                || (loc.lclass() == LOC_GMEMBER 
                    && is_ptr(loc.gmember()->type().name())
                    && loc.atom() == hi
                    && loc.offset() == 0));
    };

    auto const valid_ptr_locs = [&](locator_t a, locator_t b) -> bool
    {
        if((a.lclass() == LOC_GMEMBER) != (b.lclass() == LOC_GMEMBER))
            return false;
        if(a.lclass() == LOC_GMEMBER)
            return a.gmember() == b.gmember();
        return true;
    };

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(!is_make_ptr(ssa_it->op()))
            continue;

        bool const hi = ssa_it->op() == SSA_make_ptr_hi;

        if(!ssa_it->input(1).holds_ref())
            continue;

        ssa_ht const head_input = cset_head(ssa_it->input(1).handle());
        locator_t const head_input_loc = cset_locator(head_input);

        // Can't coalesce with every locator:
        if(!valid_ptr_loc(head_input_loc, hi))
            continue;

        if(!ssa_it->input(0).holds_ref())
        {
            // Check if the alt is an immediate constant.
            if(ssa_it->input(0).is_locator() && !ssa_it->input(0).locator().is_immediate())
                continue;
        }
        else
        {
            ssa_ht const head_opposite = cset_head(ssa_it->input(0).handle());
            locator_t const head_opposite_loc = cset_locator(head_opposite);

            // Can't coalesce with every locator:
            if(!valid_ptr_loc(head_opposite_loc, !hi))
                continue;

            if(!valid_ptr_locs(head_input_loc, head_opposite_loc))
                continue;

            // If either is defined with the opposite parity, we can't coalesce.
            if(cg_data(head_opposite).has_ptr(hi) || cg_data(head_input).has_ptr(!hi))
                continue;
        }

        assert(cg_data(ssa_it).ptr_alt);

        ssa_ht const head_ssa = cset_head(ssa_it);
        assert(cg_data(head_ssa).ptr_alt);
        assert(cg_data(head_ssa).is_ptr_hi == hi);
        assert(valid_ptr_loc(cset_locator(head_ssa), hi));

        // First, make sure we can coalesce the ssa node with its relevant input.
        ssa_ht const last = csets_appendable(fn.handle(), ir, head_input, head_ssa, cache);
        if(!last) // If they interfere
            continue;

        if(cset_arg_ret_interferes(head_input, head_ssa))
            continue;

        ssa_ht const head_ssa_alt = cset_head(cg_data(head_ssa).ptr_alt);
        assert(cg_data(head_ssa_alt).ptr_alt);
        assert(cg_data(head_ssa_alt).is_ptr_hi == !hi);
        assert(valid_ptr_loc(cset_locator(head_ssa_alt), !hi));

        // Second, make sure both inputs are not part of separate pointers
        // by checking that their alts can be coalesced.
        // If they can, coalesce them.
        if(ssa_ht const input_alt = cg_data(head_input).ptr_alt)
        {
            if(ssa_ht const alt_last = csets_appendable(fn.handle(), ir, cset_head(input_alt), head_ssa_alt, cache))
            {
                if(cset_arg_ret_interferes(cset_head(input_alt), head_ssa_alt))
                    continue;
                cset_append(alt_last, head_ssa_alt);
            }
            else
                continue;
        }

        // Coalesce the main input.
        assert(csets_appendable(fn.handle(), ir, head_input, head_ssa, cache));
        cset_append(last, head_ssa);

        assert(head_input == cset_head(head_input));
        //assert(head_opposite == cset_head(head_opposite));
        assert(cg_data(head_input).ptr_alt);
    }

    passert(reserved_size >= ssa_pool::array_size(),
            reserved_size, ssa_pool::array_size());

    // Discover and tag "direct" array reads.
    // Such reads can be implemented more efficiently in cg_isel,
    // using ABSOLUTE_X and ABSOLUTE_Y modes without storing an intermediate.
    for(cfg_ht cfg_it : postorder)
    {
        auto& d = cg_data(cfg_it);
        for(ssa_ht h : d.schedule)
        {
            using namespace ssai::array;

            assert(h->cfg_node() == cfg_it);

            if(h->op() != SSA_read_array8)
                continue;

            ssa_value_t const array = h->input(ARRAY);
            ssa_value_t const index = h->input(INDEX);

            unsigned const size = h->output_size();
            for(unsigned i = 0; i < size; ++i)
            {
                ssa_ht const output = h->output(i);
                if(output->cfg_node() != cfg_it)
                    goto next_read_array_iter;

                if(array.holds_ref())
                {
                    // Make sure the array is live.
                    if(!live_at_def(array.handle(), output))
                        goto next_read_array_iter;

                    // Can't have a different value for the array:
                    for(ssa_ht ai = cset_head(array.handle()); ai; ai = cset_next(ai))
                        if(orig_def(ai) != orig_def(array) && live_at_def(ai, output))
                            goto next_read_array_iter;
                }

                if(index.holds_ref())
                {
                    // Can't have a different value for the index:
                    for(ssa_ht ai = cset_head(index.handle()); ai; ai = cset_next(ai))
                        if(orig_def(ai) != orig_def(index) && live_at_def(ai, output))
                            goto next_read_array_iter;
                }

                // It's not ideal to use direct reads if there's high X/Y register pressure.
                // Thus, we'll try to estimate register pressure here,
                // and only use direct reads when there's little pressure.
                auto const& schedule = cg_data(cfg_it).schedule;
                unsigned const start = cg_data(h).schedule.index;
                unsigned const end = cg_data(output).schedule.index;
                fc::small_set<ssa_value_t, 4> indexers;
                for(unsigned j = start; j <= end; ++j)
                {
                    assert(j < schedule.size());
                    ssa_ht const node = schedule[j];

                    if(ssa_indexes8(node->op()))
                        indexers.insert(node->input(ssa_index8_input(node->op())));
                    else
                    {
                        for_each_node_input(node, [&](ssa_ht input)
                        {
                            if(input->op() == SSA_cg_read_array8_direct)
                                indexers.insert(input->input(ssa_index8_input(SSA_cg_read_array8_direct)));
                        });
                    }

                    if(indexers.size() > 2) // Two registers: X, Y
                        goto next_read_array_iter;
                }
            }

            // Success! Make it direct:
            h->unsafe_set_op(SSA_cg_read_array8_direct);
        next_read_array_iter:;
        }
    }

    passert(reserved_size >= ssa_pool::array_size(),
            reserved_size, ssa_pool::array_size());

    ir.assert_valid(true);


    // All gsets must be coalesced.
    // (otherwise something went wrong in an earlier optimization pass)
#ifndef NDEBUG
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() == SSA_read_global && ssa_it->input(1).locator().lclass() == LOC_GMEMBER_SET)
        {
            passert(ssa_it->input(1).locator().mem_head() == cset_locator(ssa_it), 
                    ssa_it->input(1).locator(), " | ", cset_locator(ssa_it),
                    " | ", ssa_it, " ", fn.global.name);
            // TODO
            //assert(ssa_it->test_flags(FLAG_COALESCED));
            //assert(ssa_it->input(0)->op() == SSA_early_store || ssa_it->input(0)->op() == SSA_aliased_store);
            //assert(ssa_it->input(0)->test_flags(FLAG_COALESCED));
        }
    }
#endif

    /////////////////////////////
    // FINAL SCHEDULE ANALYSIS //
    /////////////////////////////

    fn.assign_first_bank_switch(cg_calc_bank_switches(fn.handle(), ir));

    ///////////////////////////
    // INSTRUCTION SELECTION //
    ///////////////////////////

    std::size_t const proc_size = select_instructions(log, fn, ir);

    return proc_size;
}

