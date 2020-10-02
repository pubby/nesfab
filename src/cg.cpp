#include "cg.hpp"

#include <map>

#include "robin/map.hpp"
#include "robin/set.hpp"

#include "cg_isel.hpp"
#include "cg_liveness.hpp"
#include "cg_order.hpp"
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

    locator_t const loc_a = cset_locator(head_a);
    locator_t const loc_b = cset_locator(head_b);

    if(loc_a && loc_a.lclass() != LCLASS_PHI)
        bd.cset_head = ad.cset_head;
    else if(loc_b && loc_b.lclass() != LCLASS_PHI)
        ad.cset_head = bd.cset_head;
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
    assert(cset_is_head(a));
    assert(cset_is_head(b));

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
        assert(if_h->op() == SSA_if); // TODO: handle switch

        ssa_value_t condition = if_h->input(0);

        if(!condition.holds_ref() ||condition->cfg_node() != cfg_it 
           || condition->output_size() != 1 || condition->in_daisy())
        {
            continue;
        }

        switch(condition->op())
        {
        case SSA_eq:
            condition->unsafe_set_op(SSA_branch_eq); break;
        case SSA_not_eq:
            condition->unsafe_set_op(SSA_branch_not_eq); break;
        case SSA_lt:
            condition->unsafe_set_op(SSA_branch_lt); break;
        case SSA_lte:
            condition->unsafe_set_op(SSA_branch_lte); break;
        default: continue;
        }

        if_h->prune();
        condition->append_daisy();
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

        std::map<fixed_t, bc::small_vector<ssa_bck_edge_t, 1>> 
            const_writes; 
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
                if(ie.is_const())
                {
                    loc_data_t& ld = loc_map[loc];
                    ld.const_writes[ie.fixed()].push_back({ ssa_it, i });
                    continue;
                }
                else if(!ie.holds_ref())
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
                assert(cfg_pred->last_daisy());
                copy->insert_daisy(cfg_pred->last_daisy());

                // Add it to the cset.
                last = cset_append(last, copy);

                if(input.holds_ref())
                    phi_copies.push_back({ copy });
            }

            if(last.holds_ref())
                phi_csets.push_back(last.handle());
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

    auto coalesce_loc = [&](locator_t loc, locator_data_t& ld, ssa_ht node)
    {
        assert(cset_is_head(node) && cset_is_last(node));
        assert(!cg_data(node).cset_head);

        // Check to see if the copy can be coalesced.
        // i.e. its live range doesn't overlap any point where the
        // locator is already live.

        if(live_at_any_def(node, &*ld.write_points.begin(), 
                                 &*ld.write_points.end()))
            return false;

        if(ld.cset)
        {
            assert(cset_is_head(node));
            ssa_ht last = csets_dont_interfere(ld.cset, node);
            if(!last) // If they interfere
                return false;
            // It can be coalesced; add it to the cset.
            ld.cset = cset_head(cset_append(last, node));
            assert(loc == cset_locator(ld.cset));
        }
        else
        {
            // It can be coalesced; create a new set out of it;
            ld.cset = node;
            // Also tag it to a locator:
            cg_data(copy.node).cset_head = loc;
        }

        return true;
    };

    for(auto& pair : loc_map)
    {
        locator_t const loc = pair.first;
        auto& ld = pair.second;

        std::puts("PAIR");

        // Prioritize less busy ranges over larger ones.
        for(copy_t& copy : ld.copies)
            copy.cost = live_range_busyness(ir, copy.node);
        std::sort(ld.copies.begin(), ld.copies.end(),
        [](copy_t const& a, copy_t const& b) { return a.cost < b.cost; });

        for(copy_t const& copy : ld.copies)
        {
            assert(ld.cset);
            assert(cset_is_head(ld.cset));

            coalesce_loc(loc, ld, copy.node);

            // Try to coalesce locator_stores with phis
            if(copy.node->op() == SSA_locator_store)
            {
                ssa_ht input = copy.node->input(0).handle();
                if(input->op() != SSA_phi)
                    continue;

                ssa_ht last = csets_dont_interfere(ld.cset, cset_head(input));
                if(!last)
                    continue;

                // It can be coalesced; add it to the cset.
                ld.cset = cset_head(cset_append(last, input));
                assert(loc == cset_locator(ld.cset));
            }
        }
    }

    // Consts TODO

    for(auto& pair : loc_map)
    {
        locator_t const loc = pair.first;
        auto& ld = pair.second;

        for(auto& pair : ld.const_writes)
        {
            auto& vec = pair.second;

            if(vec.size() <= 1)
                continue;

            if(vec[0].handle == vec[1].handle)
            {
                // TODO
            }

            cfg_ht copy_cfg = dom_intersect(vec[0].handle, vec[1].handle);

            ssa_ht copy = copy_cfg->emplace_ssa(SSA_copy, TYPE_BYTE, TODO);

            // TODO: add copy to schedule

            calc_liveness(copy);

            coalesce_loc(loc, ld, copy);

            // - Compute liveness for 'copy'
            // - Add 'copy' to order?
            // - try to emplace 'copy' into the cset.

            // pick two
            // find dominator
            // check if path from dominator to both nodes intersects with cset
            // if not, replace with node
            // repeat until no progress is made
        }
    }

    // Coalesce phis:

    // First try to coalesce 'SSA_phi's with their input 'SSA_phi_copy's.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht phi_it = cfg_it->phi_begin(); phi_it; ++phi_it)
    {
        ssa_ht cset = cset_head(phi_it->input(0).handle());
        ssa_ht phi_cset = cset_head(phi_it);

        if(ssa_ht last = csets_dont_interfere(cset, phi_cset))
            cset_append(last, phi_cset);
    }

    // Prioritize less busy ranges over larger ones.
    for(copy_t& copy : phi_copies)
        copy.cost = live_range_busyness(ir, copy.node->input(0).handle());
    std::sort(phi_copies.begin(), phi_copies.end(),
    [](copy_t const& a, copy_t const& b) { return a.cost < b.cost; });

    // Now coalesce 'SSA_phi_copy's with their inputs.
    for(copy_t const& copy : phi_copies)
    {
        std::puts("CHEKING PHI COPY");
        assert(copy.node->op() == SSA_phi_copy);
        ssa_ht candidate = copy.node->input(0).handle();

        ssa_ht copy_cset      = cset_head(copy.node);
        ssa_ht candidate_cset = cset_head(candidate);

        if(locator_t const candidate_loc = cset_locator(candidate_cset))
        {
            //locator_t const copy_loc = cset_locator(copy_cset);
            //if(copy_loc && candidate_loc != copy_loc)
            if(!cset_mergable(copy_cset, candidate_cset))
            {
                std::puts("not merg");
                continue;
            }

            if(auto* ptr = loc_map.find(candidate_loc))
            {
                if(cset_live_at_any_def(copy_cset, 
                                        &*ptr->second.write_points.begin(), 
                                        &*ptr->second.write_points.end()))
                {
                    std::puts("not live");
                    continue;
                }
            }
        }

        if(ssa_ht last = csets_dont_interfere(candidate_cset, copy_cset))
            cset_append(last, copy_cset);
        else
            std::puts("interfere");
    }

    // Now update the IR.
    // (Liveness checks can't be done after this.)

    fc::small_set<ssa_ht, 32> unique_csets;
    for(auto& pair : loc_map)
        unique_csets.insert(cset_head(pair.second.cset));
    for(ssa_ht h : phi_csets)
        unique_csets.insert(cset_head(h));

    for(ssa_ht cset : unique_csets)
    {
        assert(cset_is_head(cset));
        locator_t const loc = cset_locator(cset);

        for(ssa_ht ssa_it = cset; ssa_it; ssa_it = cset_next(ssa_it))
        {
            assert(cset_locator(ssa_it) == loc);

            /* TODO
            if(ssa_it->op() == SSA_read_global
               || (ssa_it->input_size() && ssa_it->input(0).holds_ref()
                   && cset_locator(ssa_it->input(0).handle()) == loc))
            {
                ssa_it->set_flags(FLAG_COALESCED);
            }
            */
            ssa_it->set_flags(FLAG_COALESCED);

            if(ssa_it->op() == SSA_locator_store)
            {
                ssa_ht orig = ssa_it->input(0).handle();

                if(cset_locator(orig) != loc)
                    cg_data(orig).store_in_locs.insert(loc);

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

        }
    }

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

        for(ainst_t inst : d.code)
        {
            if(op_flags(inst.op) & ASMF_MAYBE_STORE)
            {
                if(!inst.arg.holds_ref())
                {
                    temp_code.push_back(std::move(inst));
                    continue;
                }

                if(!inst.arg->test_flags(FLAG_STORED))
                    continue;

                switch(inst.op)
                {
                case MAYBE_STA: inst.op = STA_ABSOLUTE; break;
                case MAYBE_STX: inst.op = STX_ABSOLUTE; break;
                case MAYBE_STY: inst.op = STY_ABSOLUTE; break;
                case MAYBE_SAX: inst.op = SAX_ABSOLUTE; break;
                case MAYBE_STORE_C: 
                    temp_code.push_back({ PHP_IMPLIED });
                    temp_code.push_back({ PHA_IMPLIED });
                    temp_code.push_back({ ARR_IMMEDIATE, 0 });
                    inst.op = STA_ABSOLUTE;
                    temp_code.push_back(std::move(inst));
                    temp_code.push_back({ PLA_IMPLIED });
                    temp_code.push_back({ PLP_IMPLIED });
                    continue;
                default: assert(false);
                }
            }
            temp_code.push_back(std::move(inst));
        }
        
        d.code.swap(temp_code);
    }

    ////////////////////////
    // ORDER BASIC BLOCKS //
    ////////////////////////

    std::vector<cfg_ht> order = order_ir(ir);

    for(cfg_ht h : order)
    {
        std::cout << "CFG = " << h.index << '\n';
        for(ainst_t inst : cg_data(h).code)
            std::cout << inst << '\n';
    }

}

std::ostream& operator<<(std::ostream& o, ainst_t const& inst)
{
    o << "{ " << to_string(inst.op) << ", " << inst.arg << " }";
    return o;
}

