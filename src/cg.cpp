#include "cg.hpp"

#include <map>

#include "flat/small_map.hpp"

#include "robin/map.hpp"
#include "robin/set.hpp"

#include "alloca.hpp"
#include "cg_isel.hpp"
#include "cg_liveness.hpp"
#include "cg_order.hpp"
#include "cg_schedule.hpp"
#include "globals.hpp"
#include "ir_util.hpp"
#include "ir.hpp"
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
    assert(h->op());
    while(true)
    {
        auto& d = cg_data(h);
        if(d.cset_head.holds_ref())
        {
            assert(d.cset_head->op());
            assert(h != d.cset_head.handle());
            h = d.cset_head.handle();
        }
        else
        {
            assert(h->op());
            return h;
        }
    }
}

locator_t cset_locator(ssa_ht h)
{
    h = cset_head(h);
    auto& d = cg_data(h);
    if(d.cset_head.is_locator())
        return d.cset_head.locator();
    return locator_t::none();
}

bool csets_mergable(ssa_ht a, ssa_ht b)
{
    locator_t const loc_a = cset_locator(a);
    locator_t const loc_b = cset_locator(b);
    //if(loc_a.lclass() == LOC_CALL_ARG || loc_b.lclass() == LOC_CALL_ARG)
        //return false;
    return (!loc_a || !loc_b 
            || loc_a.lclass() == LOC_PHI || loc_b.lclass() == LOC_PHI
            || loc_a == loc_b);
}

// Mostly an implementation detail used inside 'cset_append'.
void cset_merge_locators(ssa_ht head_a, ssa_ht head_b)
{
    assert(cset_is_head(head_a));
    assert(cset_is_head(head_b));

    auto& ad = cg_data(head_a);
    auto& bd = cg_data(head_b);

    locator_t const loc_a = cset_locator(head_a);
    locator_t const loc_b = cset_locator(head_b);

    assert(csets_mergable(head_a, head_b));

    if(loc_a == loc_b)
        ad.cset_head = bd.cset_head;
    else if(!loc_b || (loc_a && loc_b.lclass() == LOC_PHI))
        bd.cset_head = ad.cset_head;
    else if(!loc_a || (loc_b && loc_a.lclass() == LOC_PHI))
        ad.cset_head = bd.cset_head;
    else
        assert(false);
}

void cset_remove(ssa_ht h)
{
    assert(h);
    assert(h->op());

    ssa_ht const head = cset_head(h);

    assert(head);
    assert(head->op());

    if(h == head)
    {
        ssa_ht const next = cset_next(head);

        if(!next)
        {
            cg_data(head).cset_head = {};
            return;
        }

        assert(next != head);
        std::cout << next.index << " : " << cg_data(head).cset_head << '\n';
        for(ssa_ht it = next; it; it = cset_next(it))
            cg_data(it).cset_head = next;
        cg_data(next).cset_head = cg_data(head).cset_head;
        assert(cg_data(next).cset_head != next);
    }
    else
    {
        // Re-write the head pointers in case 'h' is a head.
        assert(cset_next(head));
        for(ssa_ht it = cset_next(head); it; it = cset_next(it))
            cg_data(it).cset_head = head;

        // Find the node prior to 'h'
        ssa_ht prev = {};
        for(ssa_ht it = head; it != h; it = cset_next(it))
            prev = it;
        assert(prev); // 'h' would be head otherwise.

        cg_data(prev).cset_next = cg_data(h).cset_next;
    }

    // Clear 'h' data:
    cg_data(h).cset_head = {};
    cg_data(h).cset_next = {};
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
    assert(csets_mergable(last_h, h));

    if(cset_is_head(last_h))
    {
        if(last_h == h)
            return last_h;
        cset_merge_locators(last_h, h);
        cg_data(h).cset_head = last_h;
    }
    else
    {
        ssa_ht const head = cset_head(last_h);
        if(head == h)
            return last_h;
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
ssa_ht csets_dont_interfere(fn_ht fn, ir_t const& ir, ssa_ht a, ssa_ht b, std::vector<ssa_ht> const& fn_nodes)
{
    assert(a && b);
    assert(cset_is_head(a));
    assert(cset_is_head(b));

    if(a == b)
    {
        while(!cset_is_last(a))
            a = cset_next(a);
        return a;
    }

    auto const fn_interferes = [&ir, fn](locator_t loc, ssa_ht fn_node)
    {
        fn_t const& called = *get_fn(*fn_node);

        switch(loc.lclass())
        {
        case LOC_GMEMBER:
            return called.ir_writes(loc.gmember());
        case LOC_GMEMBER_SET:
            {
                std::size_t const size = gmanager_t::bitset_size();
                assert(size == called.ir_reads().size());

                bitset_uint_t* bs = ALLOCA_T(bitset_uint_t, size);
                bitset_copy(size, bs, called.ir_writes().data());
                bitset_and(size, bs, ir.gmanager.get_set(loc));

                return !bitset_all_clear(size, bs);
            }
        case LOC_ARG:
            return loc.fn() != fn; // TODO: this could be made more accurate
        case LOC_RETURN:
            return true; // TODO: this could be made more accurate, as some fns don't clobber these
        default: 
            return false;
        }
    };

    for(ssa_ht fn_node : fn_nodes)
    {
        assert(fn_node->op() == SSA_fn_call);

        if(fn_interferes(cset_locator(a), fn_node))
            for(ssa_ht bi = b; bi; bi = cset_next(bi))
                if(live_at_def(bi, fn_node))
                    return {};

        if(fn_interferes(cset_locator(b), fn_node))
            for(ssa_ht ai = a; ai; ai = cset_next(ai))
                if(live_at_def(ai, fn_node))
                    return {};
    }

    ssa_ht last_a = {};
    for(ssa_ht ai = a; ai; ai = cset_next(ai))
    {
        for(ssa_ht bi = b; bi; bi = cset_next(bi))
        {
            assert(ai != bi);
            if(orig_def(ai) != orig_def(bi) && live_range_overlap(ai, bi))
                return {};
        }
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

// TODO: make this way more efficient
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

void code_gen(ir_t& ir, fn_t& fn)
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
    // DUPLICATE RTS //
    ///////////////////

    if(ir.exit)
    {
        dupe_exit_t duper;
        while(ir.exit->input_size())
        {
            cfg_ht duped_cfg = ir.emplace_cfg();
            duper.run(ir.exit, 0, duped_cfg);

            auto ie = ir.exit->input_edge(0);
            ie.handle->link_change_output(ie.index, duped_cfg,
                [](ssa_ht phi) { assert(false); return 0u; });
        }
        ir.prune_cfg(ir.exit);
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
    
    struct global_loc_data_t
    {
        // Holds the the coalesced set of all nodes using this locator:
        ssa_ht cset = {};

        // Holds all the SSA_read_global and SSA_store_locator
        // copies used to implement locators:
        std::vector<copy_t> copies;

        // Used to implement constant writes to global memory.
        std::map<fixed_t, bc::small_vector<ssa_bck_edge_t, 1>> const_stores; 
    };

    // A list of all SSA_fn_call nodes
    std::vector<ssa_ht> fn_nodes;

    // Maps specific locators - global reads and writes - to their copies.
    rh::batman_map<locator_t, global_loc_data_t> global_loc_map;

    bc::small_vector<copy_t, 32> phi_copies;
    bc::small_vector<ssa_ht, 16> phi_csets;
    unsigned phi_loc_index = 0;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_op_t const op = ssa_it->op();

        // Use copies when going into or out of global variables.
        // This is needed to correctly implement the loads and stores
        // of these pinned memory locations.

        // Most of these copies will get coalesced shortly after.

        if(op == SSA_read_global)
        {
            // Consider 'SSA_read_global' to be a copy in its own right.
            locator_t const loc = ssa_it->input(1).locator();
            global_loc_map[loc].copies.push_back({ ssa_it });
        }
        else if(ssa_flags(op) & SSAF_WRITE_GLOBALS)
        {
            // Global writes will get a tentative SSA_early_store node.
            // If this early_store can coalesce, we'll keep it.
            // Otherwise, it will be pruned later on.

            unsigned const input_size = ssa_it->input_size();
            for(unsigned i = write_globals_begin(op); i < input_size; i += 2)
            {
                locator_t const loc = ssa_it->input(i + 1).locator();
                ssa_fwd_edge_t ie = ssa_it->input_edge(i);

                if(ie.is_const())
                {
                    // Constants are handled later on,
                    // as it takes analysis to determine where to insert the copy.

                    global_loc_data_t& ld = global_loc_map[loc];
                    ld.const_stores[ie.fixed()].push_back({ ssa_it, i });
                }
                else if(ie.holds_ref())
                {
                    // Don't bother with arrays.
                    if(ssa_flags(ie.handle()->op()) & SSAF_WRITE_ARRAY)
                        continue;

                    // Create a new SSA_early_store node here.

                    ssa_ht store = ie.handle()->split_output_edge(true, ie.index(), SSA_early_store);
                    store->link_append_input(loc);
                    ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());

                    global_loc_data_t& ld = global_loc_map[loc];
                    ld.copies.push_back({ store });
                }
            }

            if(op == SSA_fn_call)
                fn_nodes.push_back(ssa_it);
        }
        else if(op == SSA_phi)
        {
            // Insert a copy of every phi argument at the end of every
            // predecessor of the phi's cfg node.
            // This is needed to convert out of SSA.

            // The cset of the phi copies will have a unique locator.
            // (These locators may be merged)
            locator_t const loc = locator_t::phi(fn.handle(), phi_loc_index++);
            ssa_value_t last = loc;

            unsigned const input_size = ssa_it->input_size();
            assert(input_size == cfg_it->input_size());
            for(unsigned i = 0; i < input_size; ++i)
            {
                cfg_ht cfg_pred = cfg_it->input(i);
                ssa_fwd_edge_t ie = ssa_it->input_edge(i);
                
                ssa_ht copy;
                if(ie.holds_ref())
                {
                    ssa_value_t input = ie.handle();
                    ssa_ht store = input->split_output_edge(true, ie.index(), SSA_early_store);
                    copy = cfg_pred->emplace_ssa(SSA_phi_copy, ssa_it->type(), store);
                    phi_copies.push_back({ copy });
                }
                else
                    copy = cfg_pred->emplace_ssa(SSA_phi_copy, ssa_it->type(), ie);

                ssa_it->link_change_input(i, copy);
                ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());

                // Add 'copy' to the daisy chain:
                assert(cfg_pred->last_daisy());
                copy->insert_daisy(cfg_pred->last_daisy());

                // Add it to the cset.
                // (All related phi_copys will belong to the same cset.)
                last = cset_append(last, copy);
                assert(cset_locator(copy) == loc);

            }

            if(last.holds_ref())
                phi_csets.push_back(last.handle());
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
            reserve += pair.second.size() - 1;

    // Then reserve extra space:
    ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size() + reserve);

    ////////////////
    // SCHEDULING //
    ////////////////

    ir.assert_valid();
    schedule_ir(ir);

    ///////////////////////////
    // LIVENESS SET CREATION //
    ///////////////////////////

    calc_ssa_liveness(ir, ssa_pool::array_size() + reserve);
    
    // Note: once the live sets have been built, the IR cannot be modified
    // until all liveness checks are done.
    // Otherwise, the intersection tests will be buggy.

    ////////////////
    // COALESCING //
    ////////////////

    // Coalesce locators.

    auto const prune_early_store = [&](ssa_ht store) -> ssa_ht
    {
        assert(store->op() == SSA_early_store);
        //assert(!cg_data(store).cset_head);

        ssa_ht const parent = store->input(0).handle();

        cset_remove(store);
        clear_liveness_for(ir, store);
        store->replace_with(parent);
        ssa_ht ret = store->prune();

        unsigned const index = cg_data(store).schedule.index;
        auto& schedule = cg_data(store->cfg_node()).schedule;
        for(unsigned i = index+1; i < schedule.size(); ++i)
            cg_data(schedule[i]).schedule.index -= 1;
        schedule.erase(schedule.begin() + index);

        clear_liveness_for(ir, parent);
        calc_ssa_liveness(parent);

        return ret;
    };

    // Tries to insert 'node' into the cset of 'ld'.
    auto const coalesce_loc = [&](locator_t loc, global_loc_data_t& ld, ssa_ht node)
    {
        assert(node);
        assert(cset_is_head(node) && cset_is_last(node));
        assert(!cg_data(node).cset_head);

        // Check to see if the copy can be coalesced.
        // i.e. its live range doesn't overlap any point where the
        // locator is already live.

        if(ld.cset)
        {
            assert(cset_is_head(node));
            ssa_ht last = csets_dont_interfere(fn.handle(), ir, ld.cset, node, fn_nodes);
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
            cg_data(node).cset_head = loc;
        }

        return true;
    };

    // First coalesce gvar read/writes, mostly with other read/writes of the same locator,
    // but also with some SSA_phi nodes.
    for(auto& pair : global_loc_map)
    {
        locator_t const loc = pair.first;
        auto& ld = pair.second;

        // Prioritize less busy ranges over larger ones.
        for(copy_t& copy : ld.copies)
            copy.cost = live_range_busyness(ir, copy.node);
        std::sort(ld.copies.begin(), ld.copies.end(),
            [](copy_t const& a, copy_t const& b) { return a.cost < b.cost; });

        for(copy_t const& copy : ld.copies)
            if(!coalesce_loc(loc, ld, copy.node))
                if(copy.node->op() == SSA_early_store)
                    prune_early_store(copy.node);
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
            if(vec.size() < 2)
                continue;

            // Otherwise we'll try to find 2 stores and combine them into 1.
            for(unsigned i = 0; i < vec.size()-1; ++i)
            for(unsigned j = i+1; j < vec.size(); ++j)
            {
                assert(i != j);

                // 'a' and 'b' are the two nodes we're trying to combine:
                ssa_ht a = vec[i].handle;
                ssa_ht b = vec[j].handle;

                cfg_ht a_cfg = a->cfg_node();
                cfg_ht b_cfg = b->cfg_node();

                // Create the store in a dominating spot:
                cfg_ht store_cfg = dom_intersect(a_cfg, b_cfg);
                ssa_ht store = store_cfg->emplace_ssa(SSA_early_store, TYPE_U, pair.first); // TODO: type?
                assert(ssa_data_pool::array_size() >= ssa_pool::array_size());
                auto& store_d = cg_data(store);

                a->link_change_input(vec[i].index, store);
                b->link_change_input(vec[j].index, store);

                if(a->op() == SSA_early_store)
                    a->unsafe_set_op(SSA_aliased_store);
                if(b->op() == SSA_early_store)
                    b->unsafe_set_op(SSA_aliased_store);

                if(store_cfg == a_cfg)
                {
                    // If both are the same, pick the earliest one
                    if(store_cfg == b_cfg && cg_data(b).schedule.index < cg_data(a).schedule.index)
                        goto before_b;

                    // pick the rank before 'a'
                    store_d.schedule.index = cg_data(a).schedule.index;
                }
                else if(store_cfg == b_cfg)
                {
                    // pick the rank before 'b'
                before_b:
                    store_d.schedule.index = cg_data(b).schedule.index;
                }
                else
                {
                    assert(store_cfg->last_daisy());
                    auto& last_d = cg_data(store_cfg->last_daisy());

                    store_d.schedule.index = last_d.schedule.index;
                }

                // Now try to coalesce it into the locator's cset.
                calc_ssa_liveness(store);
                if(coalesce_loc(loc, ld, store))
                {
                    // 'i' becomes the new store:
                    vec[i] = { store, 0 };

                    // remove 'j':
                    std::swap(vec[j], vec.back());
                    vec.pop_back();

                    unsigned const index = store_d.schedule.index;

                    // add the store to the schedule, for real
                    auto& schedule = cg_data(store_cfg).schedule;
                    schedule.insert(schedule.begin() + index, store);

                    for(unsigned k = index+1; k < schedule.size(); ++k)
                        cg_data(schedule[k]).schedule.index += 1;

                    --i;
                    break;
                }
                else
                {
                    // Abort! Undo everything and prune it.
                    clear_liveness_for(ir, store);
                    store->replace_with(pair.first);
                    store->prune();
                }
            }
        }
    }
    // Coalesce phis:

    std::puts("coalesce phis");

    // First try to coalesce 'SSA_phi's with their input 'SSA_phi_copy's.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht phi_it = cfg_it->phi_begin(); phi_it; ++phi_it)
    {
        assert(phi_it->input(0)->op() == SSA_phi_copy);

        ssa_ht cset = cset_head(phi_it->input(0).handle());
        ssa_ht phi_cset = cset_head(phi_it);

        if(ssa_ht last = csets_dont_interfere(fn.handle(), ir, cset, phi_cset, fn_nodes))
            cset_append(last, phi_cset);
    }

    std::puts("coalesce phis 2");

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

        assert(csets_mergable(copy_cset, candidate_cset));

        if(ssa_ht last = csets_dont_interfere(fn.handle(), ir, copy_cset, candidate_cset, fn_nodes))
            cset_append(last, candidate_cset);
        else
            prune_early_store(candidate);
    }

    std::puts("coalesce phis 3");

    // Coalesce early stores with their parent
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht store = cfg_node.ssa_begin(); store;)
    {
        if(store->op() == SSA_early_store)
        {
            std::printf("try alias %i\n", store.index);
            ssa_ht parent = store->input(0).handle();

            ssa_ht store_cset = cset_head(store);
            ssa_ht parent_cset = cset_head(parent);

            assert(cset_locator(store_cset));

            ssa_ht last;

            if(!csets_mergable(store_cset, parent_cset))
                goto fail;

            last = csets_dont_interfere(fn.handle(), ir, store_cset, parent_cset, fn_nodes);

            if(last)
            {
                //cset_merge_locators(store_cset, parent_cset);
                //assert(last);
                cset_append(last, parent_cset);
                store->unsafe_set_op(SSA_aliased_store);
                assert(cset_locator(store_cset));
                assert(cset_locator(store_cset) == cset_locator(parent_cset));
                assert(cset_head(store) == cset_head(parent));
            }
            else
            {
            fail:
                std::printf("can't alias %i\n", store.index);

                assert(store->output_size() == 1);
                ssa_ht use = store->output(0);

                unsigned const use_depth = loop_depth(use->cfg_node());
                unsigned const def_depth = loop_depth(store->cfg_node());
                if(def_depth > use_depth)
                {
                    std::printf("depth diff! %i\n", store.index);
                    // The early store is inside a loop, 
                    // meaning it will likely slow the code down.
                    // Thus, let's remove it.
                    store = prune_early_store(store);
                    continue;
                }
            }
        }

        ++store;
    }

    // Coalesce array operations
    for(cfg_ht cfg_it : postorder)
    {
        auto& d = cg_data(cfg_it);
        for(ssa_ht h : d.schedule)
        {
            if(!(ssa_flags(h->op()) & SSAF_WRITE_ARRAY)/* || h->op() == SSA_copy_array*/) // TODO
                continue;

            if(!h->input(0).holds_ref())
                continue;

            ssa_ht parent = h->input(0).handle();

            ssa_ht this_cset = cset_head(h);
            ssa_ht parent_cset = cset_head(parent);

            if(!csets_mergable(this_cset, parent_cset))
                continue;

            if(ssa_ht last = csets_dont_interfere(fn.handle(), ir, this_cset, parent_cset, fn_nodes))
            {
                cset_append(last, parent_cset);
                assert(cset_head(h) == cset_head(parent));
            }
        }
    }

    std::puts("coalesce phis 4");

    // Now update the IR.
    // (Liveness checks can't be done after this.)

    // Discover and tag "direct" array reads.
    // Such reads can be implemented more efficiently in cg_isel,
    // using ABSOLUTE_X and ABSOLUTE_Y modes without storing an intermediate.
    for(cfg_ht cfg_it : postorder)
    {
        auto& d = cg_data(cfg_it);
        for(ssa_ht h : d.schedule)
        {
            if(h->op() != SSA_read_array || !h->input(0).holds_ref())
                continue;

            std::printf("trying read %i\n", h.index);

            ssa_ht const array = h->input(0).handle();

            unsigned const size = h->output_size();
            for(unsigned i = 0; i < size; ++i)
            {
                std::printf("output %i\n", i);
                ssa_ht const output = h->output(i);
                if(output->cfg_node() != cfg_it)
                {
                    std::printf("failed cfg %i\n", i);
                    goto next_read_array_iter;
                }

                if(!live_at_def(array, output))
                {
                    std::printf("failed liveness %i %i\n", array.index, output.index);
                    goto next_read_array_iter;
                }

                /*
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

                    if(ssa_flags(node->op()) & SSAF_INDEXES_ARRAY)
                        indexers.insert(node->input(2));
                    else
                    {
                        for_each_node_input(node, [&](ssa_ht input)
                        {
                            if(input->op() == SSA_cg_read_array_direct)
                                indexers.insert(input->input(2));
                        });
                    }

                    if(indexers.size() > 2) // Two registers: X, Y
                        goto next_read_array_iter;
                }
                */
            }

            // Success! Make it direct:
            h->unsafe_set_op(SSA_cg_read_array_direct);
        next_read_array_iter:;
        }
    }

    fc::small_set<ssa_ht, 32> unique_csets;
    for(auto& pair : global_loc_map)
    {
        std::puts("x");
        if(pair.second.cset)
        {
            assert(pair.second.cset->op() != SSA_null);
            std::cout << pair.second.cset->op() << '\n';
            unique_csets.insert(cset_head(pair.second.cset));
        }
        std::puts("xx");
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
                ssa_ht orig = ssa_it->input(0).handle();

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

    // All gsets must be coalesced.
    // (otherwise something went wrong in an earlier optimization pass)
#ifndef NDEBUG
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() == SSA_read_global && ssa_it->input(1).locator().lclass() == LOC_GMEMBER_SET)
        {
            assert(ssa_it->input(1).locator() == cset_locator(ssa_it));
            // TODO
            //assert(ssa_it->test_flags(FLAG_COALESCED));
            //assert(ssa_it->input(0)->op() == SSA_early_store || ssa_it->input(0)->op() == SSA_aliased_store);
            //assert(ssa_it->input(0)->test_flags(FLAG_COALESCED));
        }
    }
#endif

    ///////////////////////////
    // INSTRUCTION SELECTION //
    ///////////////////////////

    {
        rh::robin_map<locator_t, unsigned> store_map;

        for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        {
            auto& d = cg_data(cfg_it);

            std::cout << "\n\n";

//#ifndef DEBUG_PRINT
            for(ssa_ht h : d.schedule)
                std::cout << "sched " << h->op() << ' ' << h.index << '\n';
//#endif

            d.code = select_instructions(fn, cfg_it);

            for(asm_inst_t inst : d.code)
                if(op_input_regs(inst.op) & REGF_M)
                    store_map.emplace(inst.arg.mem_head(), [&]{ return store_map.size(); });
        }

        // Replace used MAYBE stores with real stores, 
        // and prune unused MAYBE stores:
#if 1 // Changing to #if 0 can be useful for debugging.
        std::vector<asm_inst_t> temp_code;
        for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        {
            auto& d = cg_data(cfg_it);

            temp_code.clear();
            temp_code.reserve(d.code.size());

            for(asm_inst_t inst : d.code)
            {
                if(op_flags(inst.op) & ASMF_MAYBE_STORE)
                {
                    if(!store_map.count(inst.arg.mem_head()))
                        continue;

                    switch(inst.op)
                    {
                    case MAYBE_STA: inst.op = STA_ABSOLUTE; break;
                    case MAYBE_STX: inst.op = STX_ABSOLUTE; break;
                    case MAYBE_STY: inst.op = STY_ABSOLUTE; break;
                    case MAYBE_SAX: inst.op = SAX_ABSOLUTE; break;
                    case MAYBE_STORE_C: 
                        temp_code.push_back({ PHP_IMPLIED, inst.ssa_op });
                        temp_code.push_back({ PHA_IMPLIED, inst.ssa_op });
                        temp_code.push_back({ ARR_IMMEDIATE, inst.ssa_op, locator_t::const_byte(0) });
                        inst.op = STA_ABSOLUTE;
                        temp_code.push_back(std::move(inst));
                        temp_code.push_back({ PLA_IMPLIED, inst.ssa_op });
                        temp_code.push_back({ PLP_IMPLIED, inst.ssa_op });
                        continue;
                    default: assert(false);
                    }
                }
                temp_code.push_back(std::move(inst));
            }
            
            d.code.swap(temp_code);
        }
#endif
    }

    ////////////////////////
    // ORDER BASIC BLOCKS //
    ////////////////////////

    std::vector<cfg_ht> order = order_ir(ir);

    for(cfg_ht h : order)
    {
        std::cout << "CFG = " << h.index << '\n';
        for(asm_inst_t inst : cg_data(h).code)
            std::cout << inst << '\n';
    }

    ///////////////
    // PEEP-HOLE //
    ///////////////

    // Remove unnecessary branch/jump ops that pointlessly jump over nothing.

    for(int i = 0; i < (int)order.size() - 1;)
    {
        auto& code = cg_data(order[i]).code;

        if(code.empty())
        {
            ++i;
            continue;
        }

        auto const unnecessary_jump = [&order, i](asm_inst_t const& inst) -> bool
        {
            return ((op_flags(inst.op) & ASMF_JUMP) 
                    && inst.arg.lclass() == LOC_CFG_LABEL
                    && inst.arg.cfg_node() == order[i+1]);
        };

        // Pairs of inverted branches can be flipped.
        if(code.size() >= 2)
        {
            asm_inst_t& inst = code[code.size() - 2];
            if(code.back().op == invert_branch(inst.op) && unnecessary_jump(inst))
            {
                std::swap(inst, code.back());
                code.pop_back();
            }
        }

        if(unnecessary_jump(code.back()))
            code.pop_back();
        else
            ++i;
    }

    /////////////////////
    // BUILD LVAR INFO //
    /////////////////////

    {
        lvars_manager_t lvars(fn.handle(), ir);

        calc_asm_liveness(ir, lvars);
        build_lvar_interferences(ir, lvars);

        // Add the lvars to the fn
        fn.assign_lvars(std::move(lvars));
    }


    //////////////////////////
    // CONVERT TO ASM_PROC //
    /////////////////////////

    {
        asm_proc_t proc;

        std::size_t size_upper_bound = 0;
        for(cfg_ht h : order)
            size_upper_bound += cg_data(h).code.size();
        proc.code.reserve(size_upper_bound);

        for(cfg_ht h : order)
            for(asm_inst_t inst : cg_data(h).code)
                proc.push_inst(inst);

        /* TODO
        std::cout << "RELOC\n";
        for(asm_inst_t inst : proc.code)
            std::cout << inst << '\n';
            */

        // Add the proc to the fn
        fn.assign_proc(std::move(proc));
    }

    ///////////////////////
    // MEMORY ALLOCATION //
    ///////////////////////

    /////////////////////////
    // CONVERT TO asm_fn_t //
    /////////////////////////

    /* TODO
    asm_fn_t asm_fn;
    asm_bb_t asm_bb;

    rh::robin_map<locator_t, int> label_map;

    for(cfg_ht h : order)
    {
        auto& code = cg_data(h).code;

        for(unsigned i = 0; i < code.size; ++i)
        {
            asm_inst_t const inst = code[i];

            if(inst.op == ASM_LABEL)
            {
                assert(inst.op.arg.is_label());
                auto result = label_map.emplace(inst.op.arg.label(), asm_fn.bbs.size());
                assert(result.inserted);

                if(asm_bb.code.size())
                {
                    asm_fn.bbs.push_back(std::move(asm_bb));
                    asm_bb = {};
                }
            }
            else if(op_flags(inst.op) & ASMF_BRANCH)
            {
                asm_bb.branch = 
                asm_bb.code.assign(code.begin(), code.begin() + i + 1);


                ++i;
            }
            else
                bb.code.push_back(inst);
        }

        for(asm_inst_t inst : cg_data(h).code)
            std::cout << inst << '\n';
    }
    */

}

