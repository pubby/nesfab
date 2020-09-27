#include "cg.hpp"

#include "robin/map.hpp"
#include "robin/set.hpp"

#include "cg_isel.hpp"
#include "cg_liveness.hpp"
#include "cg_schedule.hpp"
#include "locator.hpp"

#include <iostream> // TODO

// Nodes will be partitioned into congruence classes for coalescing
// purposes, dubbed "cset" for brevity.
// These are implemented as union-find on top of a singly-linked list.
// Below are some helper functions.

bool cset_is_head(ssa_ht h) 
    { assert(h); return !cg_data(h).cset_head.holds_ref(); }
bool cset_is_last(ssa_ht h) 
    { assert(h); return !cg_data(h).cset_next; }
ssa_ht cset_next(ssa_ht h) 
    { assert(h); return cg_data(h).cset_next; }

ssa_ht cset_head(ssa_ht h)
{
    assert(h);
    while(true)
    {
        auto& d = cg_data(h);
        if(d.cset_head.holds_ref())
            h = d.cset_head.handle();
        else
            return h;
    }
}

locator_t cset_locator(ssa_ht h)
{
    h = cset_head(h);
    auto& d = cg_data(h);
    if(d.cset_head.is_locator())
        return d.cset_head.locator();
    return locator_t::null();
}

bool cset_mergable(ssa_ht a, ssa_ht b)
{
    locator_t const loc_a = cset_locator(a);
    locator_t const loc_b = cset_locator(b);
    return (!loc_a || !loc_b 
            || loc_a.lclass() == LCLASS_PHI || loc_b.lclass() == LCLASS_PHI
            || loc_a == loc_b);
}

void cset_merge_locators(ssa_ht head_a, ssa_ht head_b)
{
    assert(cset_is_head(head_a));
    assert(cset_is_head(head_b));

    auto& ad = cg_data(head_a);
    auto& bd = cg_data(head_b);

    if(ad.cset_head.is_locator())
    {
        assert(!bd.cset_head);
        bd.cset_head = ad.cset_head;
    }
    else if(bd.cset_head.is_locator())
    {
        assert(!ad.cset_head);
        ad.cset_head = bd.cset_head;
    }
}

// Appends 'h' onto the set of 'last'.
// Returns the new last.
ssa_ht cset_append(ssa_value_t last, ssa_ht h)
{
    assert(h);
    assert(cset_is_head(h));

    if(!last.holds_ref())
    {
        cg_data(h).cset_head = last;
        return h;
    }

    ssa_ht last_h = last.handle();

    assert(last_h != h);
    assert(!last_h || cset_is_last(last_h));
    assert(cset_mergable(last_h, h));

    if(cset_is_head(last_h))
    {
        cset_merge_locators(last_h, h);
        cg_data(h).cset_head = last_h;
    }
    else
    {
        ssa_ht const head = cset_head(last_h);
        cset_merge_locators(head, h);
        cg_data(h).cset_head = head;
    }

    cg_data(last_h).cset_next = h;

    while(ssa_ht next = cg_data(h).cset_next)
        h = next;

    assert(cset_is_last(h));
    return h;
}

// If theres no interference, returns a handle to the last node of 'a's cset.
ssa_ht csets_dont_interfere(ssa_ht a, ssa_ht b)
{
    assert(a && b);
    assert(cset_is_head(a) && cset_is_head(b));

    ssa_ht last_a;
    for(ssa_ht ai = a; ai; ai = cset_next(ai))
    {
        for(ssa_ht bi = b; bi; bi = cset_next(bi))
            if(live_range_overlap(ai, bi))
                return {};
        last_a = ai;
    }

    assert(cset_is_last(last_a));
    return last_a;
}

bool cset_live_at_any_def(ssa_ht a, ssa_ht const* b_begin, ssa_ht const* b_end)
{
    assert(a);
    assert(cset_is_head(a));

    for(ssa_ht ai = a; ai; ai = cset_next(ai))
        if(live_at_any_def(ai, b_begin, b_end))
            return true;

    return false;
}

void code_gen(ir_t& ir)
{
    ////////////////////////
    // CFG EDGE SPLITTING //
    ////////////////////////

    // Deal with conditional nodes that have both edges going to the same node
    // by splitting the edge and inserting a new node.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        assert(cfg_node.output_size() <= 2); // TODO: handle switch
        if(cfg_node.output_size() == 2 &&
           cfg_node.output(0) == cfg_node.output(1))
        {
            // Introduce a new node as the fix:
            ir.split_edge(cfg_node.output_edge(1));
        }
    }

    ///////////////////
    // ALLOCATE CG_D //
    ///////////////////

    cfg_data_pool::scope_guard_t<cfg_cg_d> cg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_cg_d> sg(ssa_pool::array_size());

    ////////////////////
    // COPY INSERTION //
    ////////////////////

    // Copies will be inserted to convert out of SSA form.
    // Additionally, copies will be used to pin locators to memory.

    struct copy_t
    {
        ssa_ht node;
        unsigned cost;
    };
    
    struct loc_data_t
    {
        // Holds the the coalesced set of all nodes using this locator:
        ssa_ht cset = {};

        // Holds all the SSA_read_global and SSA_store_locator
        // copies used to implement locators:
        std::vector<copy_t> copies;

        // Holds every node tagged with SSAF_WRITE_GLOBALS,
        // that also writes to this locator:
        std::vector<ssa_ht> write_points;
    };

    rh::batman_map<locator_t, loc_data_t> loc_map;
    bc::small_vector<copy_t, 32> phi_copies;
    bc::small_vector<ssa_ht, 16> phi_csets;
    unsigned phi_loc_index = 0;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_op_t const op = ssa_it->op();

        // Use copies when going into out of global variables.
        // This is needed to correctly implement the loads and stores
        // of these pinned memory locations.

        // Most of these copies will get coalesced shortly after.

        if(op == SSA_read_global)
        {
            // Consider 'SSA_read_global' to be a copy in its own right.
            locator_t const loc = ssa_it->input(1).locator();
            loc_map[loc].copies.push_back({ ssa_it });
        }
        if(ssa_flags(op) & SSAF_WRITE_GLOBALS)
        {
            // For every write, insert a copy after every input.

            unsigned const input_size = ssa_it->input_size();
            for(unsigned i = write_globals_begin(op); i < input_size; i += 2)
            {
                locator_t const loc = ssa_it->input(i + 1).locator();
                ssa_fwd_edge_t ie = ssa_it->input_edge(i);

                // For the time being, only nodes are copied.
                // TODO: lift consts too, providing an optimization when
                // multiple WRITE_GLOBALs use the same const.
                if(!ie.holds_ref())
                    continue;

                ssa_ht copy = ie.handle()->split_output_edge(
                    true, ie.index(), SSA_locator_store);
                copy->link_append_input(loc);
                ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());

                loc_data_t& ld = loc_map[loc];
                ld.copies.push_back({ copy });
                ld.write_points.push_back(ssa_it);
            }
        }

        // Insert a copy of every phi argument at the end of every
        // predecessor of the phi's cfg node.
        // This is needed to convert out of SSA.

        if(op == SSA_phi)
        {
            // The cset of the phi copies will have a unique locator.
            // (These locators may be merged)
            locator_t const loc = locator_t::phi(phi_loc_index++);
            ssa_value_t last = loc;

            unsigned const input_size = ssa_it->input_size();
            assert(input_size == cfg_it->input_size());
            for(unsigned i = 0; i < input_size; ++i)
            {
                cfg_ht cfg_pred = cfg_it->input(i);
                ssa_value_t input = ssa_it->input(i);

                ssa_ht copy = cfg_pred->emplace_ssa(
                    SSA_phi_copy, ssa_it->type(), input);
                ssa_it->link_change_input(i, copy);
                ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());

                // Add 'copy' to the daisy chain:
                if(cfg_pred->output_size() == 1)
                    copy->append_daisy();
                else
                {
                    assert(cfg_pred->last_daisy());
                    copy->insert_daisy(cfg_pred->last_daisy());
                }

                // Add it to the cset.
                last = cset_append(last, copy);

                if(input.holds_ref())
                    phi_copies.push_back({ copy });
            }

            if(last.holds_ref())
                phi_csets.push_back(cset_head(last.handle()));
        }
    }

    ////////////////
    // SCHEDULING //
    ////////////////

    schedule_ir(ir);

    ///////////////////////////
    // LIVENESS SET CREATION //
    ///////////////////////////

    calc_liveness(ir);
    
    // Note: once the live sets have been built, the IR cannot be modified
    // until all liveness checks are done.
    // Otherwise, the intersection tests will be buggy.

    ////////////////
    // COALESCING //
    ////////////////

    // Coalesce locators.

    std::vector<ssa_ht> coalesced_loc_stores;

    for(auto& pair : loc_map)
    {
        locator_t const loc = pair.first;
        auto& ld = pair.second;

        // Prioritize less busy ranges over larger ones.
        for(copy_t& copy : ld.copies)
            copy.cost = live_range_busyness(ir, copy.node);
        std::sort(ld.copies.begin(), ld.copies.end(),
        [](copy_t const& a, copy_t const& b) { return a.cost < b.cost; });

        for(copy_t const& copy : ld.copies)
        {
            assert(cset_is_head(copy.node) && cset_is_last(copy.node));
            assert(!cg_data(copy.node).cset_head);

            // Check to see if the copy can be coalesced.
            // i.e. its live range doesn't overlap any point where the
            // locator is already live.

            if(live_at_any_def(copy.node, &*ld.write_points.begin(), 
                                          &*ld.write_points.end()))
                continue;

            if(ld.cset)
            {
                ssa_ht last = csets_dont_interfere(ld.cset, copy.node);
                if(!last) // If they interfere
                    continue;
                // It can be coalesced; add it to the cset.
                ld.cset = cset_head(cset_append(last, copy.node));
                assert(loc == cset_locator(ld.cset));
            }
            else
            {
                // It can be coalesced; create a new set out of it;
                ld.cset = copy.node;
                // Also tag it to a locator:
                cg_data(copy.node).cset_head = loc;
            }
        }
    }

    // Coalesce phis:

    // First try to coalesce 'SSA_phi's with their input 'SSA_phi_copy's.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht phi_it = cfg_it->phi_begin(); phi_it; ++phi_it)
    {
        ssa_ht cset = cset_head(phi_it->input(0).handle());
        if(ssa_ht last = csets_dont_interfere(cset, phi_it))
        {
            cset_append(last, phi_it);
            phi_it->set_flags(FLAG_COALESCED);
        }
    }

    // Prioritize less busy ranges over larger ones.
    for(copy_t& copy : phi_copies)
        copy.cost = live_range_busyness(ir, copy.node->input(0).handle());
    std::sort(phi_copies.begin(), phi_copies.end(),
    [](copy_t const& a, copy_t const& b) { return a.cost < b.cost; });

    // Now coalesce 'SSA_phi_copy's with their inputs.
    for(copy_t const& copy : phi_copies)
    {
        assert(copy.node->op() == SSA_phi_copy);
        ssa_ht candidate = copy.node->input(0).handle();

        ssa_ht copy_cset      = cset_head(copy.node);
        ssa_ht candidate_cset = cset_head(candidate);

        if(locator_t const candidate_loc = cset_locator(candidate_cset))
        {
            locator_t const copy_loc = cset_locator(copy_cset);
            if(copy_loc && candidate_loc != copy_loc)
                continue;

            auto const& ld = loc_map[candidate_loc];
            if(cset_live_at_any_def(copy_cset, &*ld.write_points.begin(), 
                                               &*ld.write_points.end()))
                continue;
        }

        if(ssa_ht last = csets_dont_interfere(candidate_cset, copy_cset))
            cset_append(last, copy_cset);
    }

    // Now update the IR.
    // (Liveness checks can't be done after this.)
    auto const finish_cset = [&](ssa_ht cset)
    {
        assert(cset_is_head(cset));
        locator_t const loc = cset_locator(cset);

        for(ssa_ht ssa_it = cset; ssa_it; ssa_it = cset_next(ssa_it))
        {
            assert(cset_locator(ssa_it) == loc);

            ssa_it->set_flags(FLAG_COALESCED);

            switch(ssa_it->op())
            {
            case SSA_read_global: break;
            case SSA_locator_store:
                {
                    ssa_ht orig = ssa_it->input(0).handle();

                    if(orig->op() != SSA_read_global 
                       || orig->input(1).locator() != loc)
                    {
                        cg_data(orig).store_in_locs.insert(loc);
                    }

                    // Replace uses of 'orig' with uses of 'ssa_it' whenever
                    // said uses occur inside the live range of 'ssa_it'.
                    for(unsigned i = 0; i < orig->output_size();)
                    {
                        auto oe = orig->output_edge(i);
                        if(oe.input_class() == INPUT_VALUE
                           && !(ssa_flags(oe.handle->op()) & SSAF_COPY)
                           && live_at_def(ssa_it, oe.handle))
                        {
                            assert(ssa_it != oe.handle);
                            oe.handle->link_change_input(oe.index, ssa_it);
                        }
                        else
                            ++i;
                    }
                }
                break;
            default:
                cg_data(ssa_it).store_in_locs.insert(loc);
                break;
            }

        }
    };

    for(auto& pair : loc_map)
        finish_cset(pair.second.cset);

    for(ssa_ht cset : phi_csets)
        if(cset_locator(cset).lclass() == LCLASS_PHI)
            finish_cset(cset);

    // All gsets must be coalesced.
    // (otherwise something went wrong in an earlier optimization pass)
#ifndef NDEBUG
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() == SSA_read_global 
           && ssa_it->input(1).locator().lclass() == LCLASS_GLOBAL_SET)
        {
            assert(ssa_it->test_flags(FLAG_COALESCED));
            assert(ssa_it->input(0)->op() == SSA_locator_store);
            assert(ssa_it->input(0)->test_flags(FLAG_COALESCED));
        }
    }
#endif

    ///////////////////////////
    // INSTRUCTION SELECTION //
    ///////////////////////////

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it);

        std::cout << "\n\n";

        for(ssa_ht h : d.schedule)
            std::cout << "sched " << h->op() << '\n';

        d.code = select_instructions(cfg_it);

        for(ainst_t inst : d.code)
        {
            if(!inst.arg.holds_ref())
                continue;
            if(op_input_regs(inst.op) & REGF_M)
                inst.arg->set_flags(FLAG_STORED);
        }
    }

    // Replace used MAYBE stores with real stores, 
    // and prune unused MAYBE stores:
    std::vector<ainst_t> temp_code;
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it);

        temp_code.clear();
        temp_code.reserve(d.code.size());

        //for(ainst_t inst :  d.code)
            //std::cout << inst << '\n';

        for(ainst_t inst : d.code)
        {
            if(op_flags(inst.op) & ASMF_MAYBE_STORE)
            {
                if(!inst.arg->test_flags(FLAG_STORED))
                    continue;
                switch(inst.op)
                {
                case MAYBE_STA: inst.op = STA_ABSOLUTE; break;
                case MAYBE_STX: inst.op = STX_ABSOLUTE; break;
                case MAYBE_STY: inst.op = STY_ABSOLUTE; break;
                case MAYBE_SAX: inst.op = SAX_ABSOLUTE; break;
                default: assert(false);
                }
            }
            temp_code.push_back(std::move(inst));
        }
        
        for(ainst_t inst :  temp_code)
            std::cout << inst << '\n';

        d.code.swap(temp_code);
    }
}

std::ostream& operator<<(std::ostream& o, ainst_t const& inst)
{
    o << "{ " << to_string(inst.op) << ", " << inst.arg << " }";
    return o;
}

