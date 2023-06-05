#include "cg_cset.hpp"

#include "globals.hpp"
#include "ir.hpp"
#include "ir_util.hpp"
#include "alloca.hpp"
#include "cg_liveness.hpp"

// Used to convert ssa_values into locators usable in code gen.
locator_t asm_arg(ssa_value_t v)
{
    if(v.holds_ref())
    {
        if(ssa_flags(v->op()) & SSAF_CG_NEVER_STORE)
            return {};
        if(locator_t loc = cset_locator(v.handle(), true))
            return loc.with_byteified(false);
    }
    return locator_t::from_ssa_value(v).with_byteified(false);
}

cset_ir_cache_t cset_build_cache(ir_t const& ir)
{
    cset_ir_cache_t cache;

    for(cfg_node_t const& cfg : ir)
    for(ssa_ht ssa = cfg.ssa_begin(); ssa; ++ssa)
    {
        if(ssa->op() == SSA_fn_call || (ssa_flags(ssa->op()) & SSAF_FENCE))
            cache.special.push_back(ssa);
    }

    return cache;
}

bool cset_is_head(ssa_ht h) 
    { assert(h); return !cg_data(h).cset_head.holds_ref(); }
bool cset_is_last(ssa_ht h) 
    { assert(h); return !cg_data(h).cset_next; }
ssa_ht cset_next(ssa_ht h) 
    { assert(h); return cg_data(h).cset_next; }

ssa_ht cset_head(ssa_ht h)
{
    assert(h);
    passert(h->op(), h, h->op());
    while(true)
    {
        // TODO: update pointers to point to head.
        auto& d = cg_data(h);
        if(d.cset_head.holds_ref())
        {
            assert(d.cset_head->op());
            assert(d.cset_head.is_handle());
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

locator_t cset_locator(ssa_ht const h, bool convert_ssa)
{
    ssa_ht const head = cset_head(h);
    auto& d = cg_data(head);
    if(d.cset_head.is_locator())
    {
        locator_t const loc = d.cset_head.locator();
        assert(loc.mem_head() == loc);
        return loc;
    }
    if(convert_ssa)
        return locator_t::ssa(head);
    return LOC_NONE;
}

bool cset_locators_mergable(locator_t loc_a, locator_t loc_b)
{
    return (!loc_a || !loc_b 
            || loc_a.lclass() == LOC_PHI || loc_b.lclass() == LOC_PHI
            || loc_a == loc_b);
}

// Mostly an implementation detail used inside 'cset_append'.
static void cset_merge_locators(ssa_ht head_a, ssa_ht head_b)
{
    assert(cset_is_head(head_a));
    assert(cset_is_head(head_b));

    auto& ad = cg_data(head_a);
    auto& bd = cg_data(head_b);

    locator_t const loc_a = cset_locator(head_a);
    locator_t const loc_b = cset_locator(head_b);

    assert(cset_locators_mergable(loc_a, loc_b));

    if(loc_a == loc_b)
        ad.cset_head = bd.cset_head;
    else if(!loc_b || (loc_a && loc_b.lclass() == LOC_PHI))
        bd.cset_head = ad.cset_head;
    else if(!loc_a || (loc_b && loc_a.lclass() == LOC_PHI))
        ad.cset_head = bd.cset_head;
    else
        assert(false);
}

ssa_ht cset_remove(ssa_ht h)
{
    assert(h);
    assert(h->op());

    ssa_ht const head = cset_head(h);
    ssa_ht ret;

    assert(head);
    assert(head->op());

    if(h == head)
    {
        ssa_ht const next = ret = cset_next(head);

        if(!next)
        {
            cg_data(head).cset_head = {};
            return ret;
        }

        assert(next != head);
        for(ssa_ht it = next; it; it = cset_next(it))
            cg_data(it).cset_head = next;
        cg_data(next).cset_head = cg_data(head).cset_head;
        assert(cg_data(next).cset_head != next);
    }
    else
    {
        ret = head;

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
    cg_data(h).ptr_alt = {}; 

    return ret;
}

// Appends 'h' onto the set of 'last'.
// Returns the new last.
ssa_ht cset_append(ssa_value_t last, ssa_ht h)
{
    assert(h);
    assert(cset_is_head(h));

    if(last.holds_ref())
    {

        ssa_ht last_h = last.handle();

        assert(last_h != h);
        assert(!last_h || cset_is_last(last_h));
        assert(cset_locators_mergable(cset_locator(last_h), cset_locator(h)));

        ssa_ht const head = cset_head(last_h);
        if(head == h)
            return last_h;
        cset_merge_locators(head, h);
        cg_data(h).cset_head = head;

        if(!cg_data(head).ptr_alt)
        {
            cg_data(head).ptr_alt = cg_data(h).ptr_alt;
            cg_data(head).is_ptr_hi = cg_data(h).is_ptr_hi;

            // NOTE: This doesn't check if both 'ptr_alt's are set.
            // One will always get overwritten.
        }

        cg_data(last_h).cset_next = h;
    }
    else
        cg_data(h).cset_head = last;

    while(ssa_ht next = cg_data(h).cset_next)
        h = next;

    assert(cset_is_last(h));
    return h;
}

// Handles 'loc' interfering with fns and fences.
bool special_interferes(fn_ht fn, ir_t const& ir, ssa_ht h, locator_t loc, ssa_ht with)
{
    if(with->op() == SSA_fn_call)
    {
        // Checks if 'loc' is used inside 'with'.
        fn_ht const called = get_fn(*with);

        switch(loc.lclass())
        {
        case LOC_GMEMBER:
            return called->ir_writes().test(loc.gmember().id);
        case LOC_GMEMBER_SET:
            {
                std::size_t const size = gmember_ht::bitset_size();
                assert(size == called->ir_reads().size());

                bitset_uint_t* bs = ALLOCA_T(bitset_uint_t, size);
                bitset_copy(size, bs, called->ir_writes().data());
                bitset_and(size, bs, ir.gmanager.get_set(loc));

                return !bitset_all_clear(size, bs);
            }
        case LOC_ARG:
        case LOC_RETURN:
            // The current RAM allocator expects this behavior:
            return loc.fn() != fn;
        default: 
            return false;
        }
    }
    else if(ssa_flags(with->op()) & SSAF_FENCE)
    {
        // Checks if 'loc' is stored / read by the fence.
        switch(loc.lclass())
        {
        default: 
            return false;
        case LOC_GMEMBER:
            return true;
        case LOC_GMEMBER_SET:
            return !bitset_all_clear(gmember_ht::bitset_size(), ir.gmanager.get_set(loc));
        }
    }

    return false;
}

// If theres no interference, returns a handle to the last node of 'a's cset.
ssa_ht csets_dont_interfere(fn_ht fn, ir_t const& ir, ssa_ht a, ssa_ht b, cset_ir_cache_t const& cache)
{
    assert(a && b);
    assert(cset_is_head(a));
    assert(cset_is_head(b));
    assert(a->op());
    assert(b->op());

    if(a == b)
    {
        while(!cset_is_last(a))
            a = cset_next(a);
        return a;
    }

    locator_t const a_loc = cset_locator(a);
    locator_t const b_loc = cset_locator(b);

    for(ssa_ht node : cache.special)
    {
        if(special_interferes(fn, ir, a, a_loc, node))
            for(ssa_ht bi = b; bi; bi = cset_next(bi))
                if(live_at_def(bi, node))
                    return {};

        if(special_interferes(fn, ir, b, b_loc, node))
            for(ssa_ht ai = a; ai; ai = cset_next(ai))
                if(live_at_def(ai, node))
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

// Returns a handle to the last node of 'a's cset if cset_append can be called with these parameters.
ssa_ht csets_appendable(fn_ht fn, ir_t const& ir, ssa_ht a, ssa_ht b, cset_ir_cache_t const& cache)
{
    assert(a && b);
    assert(cset_is_head(a));
    assert(cset_is_head(b));

    if(!cset_locators_mergable(cset_locator(a), cset_locator(b)))
        return {};

    return csets_dont_interfere(fn, ir, a, b, cache);
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

