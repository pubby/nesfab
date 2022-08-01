#include "ram_alloc.hpp"

#include <iostream> // TODO

#include "decl.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "compiler_error.hpp"
#include "options.hpp"
#include "ram.hpp"
#include "rom.hpp"

namespace  // anonymous namespace
{

enum zp_request_t
{
    ZP_NEVER,
    ZP_MAYBE,
    ZP_ONLY,
};

// Allocates a span inside 'usable_ram'.
span_t alloc_ram(ram_bitset_t const& usable_ram, std::size_t size, zp_request_t zp)
{
    auto const try_alloc = [](ram_bitset_t const& usable_ram, std::size_t size, zp_request_t zp) -> span_t
    {
        assert(size > 0);

        if(zp != ZP_NEVER && size == 1) // Fast path when 'size' == 1
        {
            int const addr = usable_ram.lowest_bit_set();

            if(addr < 0 || (zp == ZP_ONLY && addr > 0xFF ))
                return {};

            return { .addr = addr, .size = 1 };
        }
        else
        {
            ram_bitset_t usable_copy = usable_ram;

            if(zp == ZP_ONLY)
                usable_copy &= zp_bitset;
            else if(zp == ZP_NEVER || size > 1)
                usable_copy &= ~zp_bitset; // Don't put arrays in ZP

            bitset_mark_consecutive(usable_copy.size(), usable_copy.data(), size);

            int const addr = usable_copy.lowest_bit_set();

            if(addr < 0)
                return {};

            return { .addr = addr, .size = size };
        }
    };

    // Align, when possible
    if(size > 1 && size <= 256)
    {
        page_bitset_t page = page_bitset_t::filled(257 - size);
        ram_bitset_t aligned = usable_ram;

        static_assert(ram_bitset_t::num_ints % page_bitset_t::num_ints == 0);

        for(unsigned i = 0; i < ram_bitset_t::num_ints; i += page_bitset_t::num_ints)
            bitset_and(page_bitset_t::num_ints, aligned.data() + i, page.data());

        if(span_t span = try_alloc(aligned, size, zp))
            return span;
    }

    return try_alloc(usable_ram, size, zp);

}

class ram_allocator_t
{
public:
    explicit ram_allocator_t(ram_bitset_t const& initial_usable_ram);

private:
    enum step_t
    {
        UNALLOCATED,
        BUILD_ORDER,
        ZP_ONLY_ALLOC,
        FULL_ALLOC,
    };

    void build_order(std::vector<fn_ht>& fns);
    void build_order(fn_ht fn);

    template<step_t Step>
    void alloc_locals(fn_ht h);

    struct group_vars_d
    {
        // Addresses that can be used to allocate globals.
        ram_bitset_t usable_ram;

        // Tracks how it interferes with other group_vars
        bitset_t interferences;

        // Used to estimate how many bytes of ZP are left
        unsigned zp_estimate;
    };

    struct fn_d
    {
        step_t step = UNALLOCATED;

        // A recursive count of the amount of lvars a fn uses.
        // (The actual RAM required is typically less)
        // This is used to order in which fn lvars are allocated.
        unsigned lvar_count = 0;

        // Addresses that can be used to allocate lvars.
        ram_bitset_t usable_ram = ram_bitset_t::filled();

        // Ram allocated by lvars in this fn.
        ram_bitset_t lvar_ram = {};

        // Like above, but includes called fns too.
        ram_bitset_t recursive_lvar_ram = {};

        // Normal "ir_group_vars()" only looks up the call graph,
        // but this set also looks down.
        bitset_t maximal_group_vars;
    };

    group_vars_d& data(group_vars_ht h) { return group_vars_data[h.id]; }
    fn_d& data(fn_ht h) { return fn_data[h.id]; }

    std::vector<group_vars_d> group_vars_data;
    std::vector<fn_d> fn_data;
    std::vector<fn_ht> fn_order;
};

ram_allocator_t::ram_allocator_t(ram_bitset_t const& initial_usable_ram)
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);

    // Amount of bytes free in zero page
    int const zp_free = 256 - (initial_usable_ram & zp_bitset).popcount();

    // Amount of bytes in zp dedicated to locals
    int const max_local_zp = 32;

    // Amount of bytes in zp dedicated to gvars
    int const max_gvar_zp = std::max(zp_free - max_local_zp, zp_free / 2);

    group_vars_data.resize(group_vars_ht::pool().size());
    fn_data.resize(fn_ht::pool().size());

    ///////////////////
    // ALLOC GLOBALS //
    ///////////////////

    {
        // Alloc spans
        for(gmember_t& gm : gmember_ht::values())
            gm.alloc_spans();

        // Init 'maximal_group_vars'
        unsigned const group_vars_bs_size = group_vars_ht::bitset_size();
        for(fn_ht fn : fn_ht::handles())
        {
            if(fn->fclass == FN_CT)
                continue;

            fn_data[fn.id].maximal_group_vars = fn->ir_group_vars();
            assert(fn_data[fn.id].maximal_group_vars.size() == group_vars_ht::bitset_size());
        }

        // Build 'maximal_group_vars'
        for(fn_t* mode : global_t::modes())
        {
            assert(mode);
            assert(mode->fclass == FN_MODE);
            assert(mode->ir_group_vars());

            mode->ir_calls().for_each([&](unsigned j)
            {
                fn_data[j].maximal_group_vars |= mode->ir_group_vars();
            });
        }

        // Init 'group_vars_data'
        unsigned const interferences_size = group_vars_ht::bitset_size();
        for(unsigned i = 0; i < group_vars_ht::pool().size(); ++i)
        {
            auto& d = group_vars_data[i];
            d.interferences.reset(interferences_size);
            d.interferences.set(i); // always interfere with itself
            d.usable_ram = initial_usable_ram;
            d.zp_estimate = max_gvar_zp;
        }

        // Build interference graph among group vars:
        for(fn_t* mode : global_t::modes())
        {
            assert(mode);
            assert(mode->fclass == FN_MODE);

            assert(mode->ir_group_vars());
            mode->ir_group_vars().for_each([&](unsigned i)
            {
                auto& d = group_vars_data[i];
                d.interferences |= mode->ir_group_vars();
            });
        }

        // Count how often gmembers appears in emitted code.
        // We'll eventually allocate using the use count as a heuristic

        rh::batman_map<locator_t, unsigned> gmember_count;
        for(gvar_t const& gvar : gvar_ht::values())
            gvar.for_each_locator([&](locator_t loc){ gmember_count.insert({ loc.mem_head(), 0 }); });

        for(fn_t const& fn : fn_ht::values())
        {
            rom_proc_t const& rom_proc = fn.rom_proc().safe();

            if(!rom_proc.emits())
                continue;

            for(asm_inst_t const& inst : rom_proc.asm_proc().code)
                if(inst.arg.lclass() == LOC_GMEMBER)
                    if(unsigned* count = gmember_count.mapped(inst.arg.mem_head()))
                        *count += 1;
        }

        // Find unused variables and issue a warning

        for(gvar_t const& gvar : gvar_ht::values())
        {
            // PAAs won't appear in code, so we can't detect unused PAAs here.
            // TODO: detect them some other way
            if(is_paa(gvar.type().name()))
                continue;

            unsigned count = 0;
            gvar.for_each_locator([&](locator_t loc) { count += gmember_count[loc]; });

            if(count == 0)
                compiler_warning(gvar.global.pstring(), "Global variable is unused.");
        }

        // Order based on count.

        struct rank_t
        {
            unsigned score;
            locator_t loc;
        };

        std::vector<rank_t> ordered_gmembers_zp;
        std::vector<rank_t> ordered_gmembers;

        // Track which gvars are unused and use them to generate warning messages.
        fc::vector_set<gvar_t const*> unused_gvars;

        for(auto const& pair : gmember_count)
        {
            // Priority 1: gvar size
            // Priority 2: frequency in code

            constexpr unsigned size_scale = 256; // arbitrary constant

            if(is_paa(pair.first.gmember()->type().name()))
            {
                // PAAs are handled separately, as they won't appear in the code.
                assert(pair.second == 0);
                ordered_gmembers.push_back({ pair.first.mem_size() * size_scale, pair.first });
            }
            else if(pair.first.mem_zp_only())
                ordered_gmembers_zp.push_back({ pair.first.mem_size(), pair.first });
            else
                ordered_gmembers.push_back({ (pair.first.mem_size() * size_scale) + pair.second, pair.first });
        }

        std::sort(ordered_gmembers_zp.begin(), ordered_gmembers_zp.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });
        std::sort(ordered_gmembers.begin(), ordered_gmembers.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });

        // Estimate which locators will go into ZP.

        rh::batman_set<locator_t> estimated_in_zp;

        auto const estimate_gmember_loc = [&](locator_t loc)
        {
            gmember_t& gmember = *loc.gmember();
            group_vars_d& d = data(gmember.gvar.group_vars);

            unsigned const size = loc.mem_size();

            if(!d.interferences.for_each_test([&](unsigned i) -> bool
                { return group_vars_data[i].zp_estimate >= size; }))
            {
                std::cout << "alloc failed estimate " << loc << std::endl;
                return;
            }

            std::cout << "alloc estimated " << loc << std::endl;
            estimated_in_zp.insert(loc);

            d.interferences.for_each([&](unsigned i)
            {
                group_vars_data[i].zp_estimate -= size;
            });
        };

        for(rank_t const& rank : ordered_gmembers_zp)
            estimate_gmember_loc(rank.loc);

        for(rank_t const& rank : ordered_gmembers)
            estimate_gmember_loc(rank.loc);

        // For global vars that have init expressions,
        // we want to allocate their group to be contigious,
        // as this means we can more efficiently init them.
        //
        // To do this, we'll bundle init'd locators into structs.

        struct group_inits_t
        {
            group_vars_ht group_vars = {};
            unsigned score = 0;
            std::vector<locator_t> init;
        };

        std::vector<group_inits_t> ordered_inits;
        ordered_inits.reserve(group_vars_ht::pool().size());

        for(group_vars_ht g : group_vars_ht::handles())
        {
            group_inits_t zero_inits  = { g };
            group_inits_t value_inits = { g };

            for(gvar_ht v : g->gvars())
            {
                if(v->init_expr)
                {
                    v->for_each_locator([&](locator_t loc)
                    { 
                        group_inits_t* inits = &value_inits;
                        if(loc.gmember()->zero_init(loc.atom()))
                            inits = &zero_inits;

                        assert(gmember_count.count(loc));

                        // Score is the largest gmember_count
                        inits->score = std::max(inits->score, gmember_count[loc]);
                        inits->init.push_back(loc);
                    });
                }
            }

            if(!zero_inits.init.empty())
                ordered_inits.push_back(std::move(zero_inits));
            if(!value_inits.init.empty())
                ordered_inits.push_back(std::move(value_inits));
        }

        std::sort(ordered_inits.begin(), ordered_inits.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });

        auto const alloc_gmember_loc = [&](locator_t loc)
        {
            gmember_t& gmember = *loc.gmember();

            if(gmember.span(loc.atom())) // Abort if we've already allocated.
                return;

            unsigned size = loc.mem_size();

            bool const is_ptr = ::is_ptr(gmember.type().name());

            if(is_ptr)
            {
                assert(!gmember.span(!loc.atom()));
                size = 2;
            }

            group_vars_d& d = data(gmember.gvar.group_vars);

            std::cout << "allocing loc " << loc << std::endl;

            zp_request_t zp;
            if(loc.mem_zp_only())
                zp = ZP_ONLY;
            else if(estimated_in_zp.count(loc))
                zp = ZP_MAYBE;
            else
                zp = ZP_NEVER;

            span_t const span = alloc_ram(d.usable_ram, size, zp);

            if(!span)
                throw std::runtime_error("Unable to allocate global variable (out of RAM).");

            std::printf("allocing %i %i %i\n", loc.mem_size(), loc.mem_zp_only(), zp);
            std::cout << "allocating at " << span << std::endl;

            if(is_ptr)
            {
                assert(span.size == 2);
                gmember.assign_span(0, { .addr = span.addr, .size = 1 });
                gmember.assign_span(1, { .addr = span.addr+1, .size = 1 });
            }
            else
                gmember.assign_span(loc.atom(), span);

            ram_bitset_t const mask = ~ram_bitset_t::filled(span.size, span.addr);

            d.interferences.for_each([&](unsigned i)
            {
                group_vars_data[i].usable_ram &= mask;
            });
        };

        // Allocate

        for(group_inits_t const& inits : ordered_inits)
            for(locator_t loc : inits.init)
                alloc_gmember_loc(loc);

        std::cout << "allocing UNINIT\n";

        for(rank_t const& rank : ordered_gmembers_zp)
            alloc_gmember_loc(rank.loc);

        for(rank_t const& rank : ordered_gmembers)
            alloc_gmember_loc(rank.loc);
    }

    //////////////////
    // ALLOC LOCALS //
    //////////////////

    {
        // Use group_vars usable_ram to build fn usable_ram.
        for(fn_ht fn : fn_ht::handles())
        {
            if(fn->fclass == FN_CT)
                continue;

            fn->ir_calls().for_each([&](unsigned j)
            {
                fn_data[fn.id].maximal_group_vars.for_each([&](unsigned k)
                {
                    fn_data[j].usable_ram &= group_vars_data[k].usable_ram;
                });
            });
        }

        // Build an order to allocate fn lvars:
        for(fn_ht fn : fn_ht::handles())
        {
            if(fn->fclass == FN_CT)
                continue;

            fn_data[fn.id].lvar_count += fn->lvars().num_this_lvars();

            fn->ir_calls().for_each([&](unsigned i)
            {
                fn_data[i].lvar_count += fn->lvars().num_this_lvars();
            });
        }

        fn_order.reserve(fn_ht::pool().size());

        std::vector<fn_ht> mode_rank;
        mode_rank.reserve(global_t::modes().size());
        for(fn_t* mode : global_t::modes())
            mode_rank.push_back(mode->handle());

        build_order(mode_rank);

        for(fn_ht fn : fn_order)
            std::cout << "building " << fn->global.name << std::endl;

        for(fn_ht fn : fn_order)
            alloc_locals<ZP_ONLY_ALLOC>(fn);

        for(fn_ht fn : fn_order)
            alloc_locals<FULL_ALLOC>(fn);
    }
}

void ram_allocator_t::build_order(std::vector<fn_ht>& fns)
{
    std::sort(fns.begin(), fns.end(), [&](fn_ht a, fn_ht b)
    {
        return data(a).lvar_count > data(b).lvar_count;
    });

    for(fn_ht fn : fns)
        build_order(fn);
}

void ram_allocator_t::build_order(fn_ht fn)
{
    fn_d& d = data(fn);

    if(d.step == BUILD_ORDER)
        return;

    // Make sure all called fns are ordered first:
    {
        std::vector<fn_ht> fn_rank;
        fn_rank.reserve(fn->ir_calls().popcount());
        fn->ir_calls().for_each([&fn_rank](unsigned i){ fn_rank.push_back(fn_ht{i}); });
        std::cout << "fn rank size = " << fn_rank.size() << std::endl;
        build_order(fn_rank);
    }

    fn_order.push_back(fn);
    d.step = BUILD_ORDER;
}

template<ram_allocator_t::step_t Step>
void ram_allocator_t::alloc_locals(fn_ht h)
{
    fn_t& fn = *h;
    fn_d& d = data(h);

    assert(d.step < Step);
    assert((data(h).usable_ram & data(h).lvar_ram).all_clear());

    std::cout << h->global.name << " lvars =  " << fn.lvars().num_this_lvars() << ' ' << fn.lvars().num_all_lvars() << std::endl;

    // Setup lvar usable ram:
    std::vector<ram_bitset_t> lvar_usable_ram;
    lvar_usable_ram.resize(fn.lvars().num_this_lvars(), d.usable_ram);

    for(unsigned i = 0; i < fn.lvars().num_this_lvars(); ++i)
    {
        for(fn_ht interfering_fn : fn.lvars().fn_interferences(i))
        {
            assert(data(interfering_fn).step == Step);
            lvar_usable_ram[i] &= data(interfering_fn).usable_ram;
            lvar_usable_ram[i] -= data(interfering_fn).recursive_lvar_ram;
        }
    }

    // Some tracked lvars don't belong to this fn and were already allocated.
    // Handle these interferences here:
    for(unsigned i = fn.lvars().num_this_lvars(); i < fn.lvars().num_all_lvars(); ++i)
    {
        span_t const span = fn.lvar_span(i);

        if(!span) // It might not have been allocated yet, or it might not exist in the generated assembly.
            continue;

        ram_bitset_t const mask = ~ram_bitset_t::filled(span.size, span.addr);

        bitset_for_each(fn.lvars().bitset_size(), fn.lvars().lvar_interferences(i), [&](unsigned j)
        {
            if(j < lvar_usable_ram.size())
                lvar_usable_ram[j] &= mask;
        });
    }

    // Prefer to use the same RAM addresses that called fns use.
    // We'll call this set 'freebie_ram'.
    ram_bitset_t freebie_ram = {};
    fn.ir_calls().for_each([&](unsigned i)
    {
        assert(fn_data[i].step == Step);
        freebie_ram |= fn_data[i].recursive_lvar_ram;
    });

    // Sort the lvars before allocating.

    struct rank_t
    {
        float score;
        unsigned lvar_i;
        constexpr auto operator<=>(rank_t const&) const = default;
    };

    std::vector<rank_t> ordered_lvars;
    ordered_lvars.reserve(fn.lvars().num_this_lvars());

    for(unsigned i = 0; i < fn.lvars().num_this_lvars(); ++i)
    {
        auto const& info = fn.lvars().this_lvar_info(i);

        if(Step == ZP_ONLY_ALLOC && !info.zp_only)
            continue;

        if(info.ptr_hi) // We'll allocate lo only, then assign to hi.
            continue;

        if(fn.lvar_span(i))
            continue;

        int const usable = lvar_usable_ram[i].popcount();
        int const interferences = bitset_popcount(fn.lvars().bitset_size(), fn.lvars().lvar_interferences(i));
        float const score = float(usable - int(info.size)) / interferences;

        ordered_lvars.push_back({ score, i });
    }

    std::sort(ordered_lvars.begin(), ordered_lvars.end());

    // Alright, now we'll allocate.

    // If we're not allocating everything, we'll have to propagate
    // our allocations upwards to fns we call.
    // (We always propagate downwards, to fns that call this)
    bitset_t propagate_calls;
    if(Step < FULL_ALLOC)
        propagate_calls.reset(fn_ht::bitset_size());

    for(rank_t const& rank : ordered_lvars)
    {
        unsigned const lvar_i = rank.lvar_i;
        auto const& info = fn.lvars().this_lvar_info(lvar_i);
        assert(!info.ptr_hi);

        std::cout << "allocating lvar " << fn.lvars().locator(lvar_i) << ' ' << info.size << std::endl;
        assert(lvar_i < lvar_usable_ram.size());

        // First try to allocate in 'freebie_ram'.
        span_t span = alloc_ram(lvar_usable_ram[lvar_i] & freebie_ram, info.size, info.zp_only ? ZP_ONLY : ZP_MAYBE);

        // If that fails, try to allocate anywhere.
        if(!span)
            span = alloc_ram(lvar_usable_ram[lvar_i], info.size, info.zp_only ? ZP_ONLY : ZP_MAYBE);

        // If that fails, we're fucked.
        if(!span)
            throw std::runtime_error("Unable to allocate local variable (out of RAM).");

        std::cout << "allocating at " << span << std::endl;

        // Record the allocation.

        if(info.ptr_alt >= 0)
        {
            assert(span.size == 2);
            fn.assign_lvar_span(lvar_i,       { .addr = span.addr,     .size = 1 }); 
            fn.assign_lvar_span(info.ptr_alt, { .addr = span.addr + 1, .size = 1 }); 
        }
        else
            fn.assign_lvar_span(lvar_i, span); 

        d.lvar_ram |= ram_bitset_t::filled(span.size, span.addr);
        d.usable_ram -= d.lvar_ram;

        if(Step < FULL_ALLOC)
            propagate_calls.clear_all();

        ram_bitset_t const mask = ~ram_bitset_t::filled(span.size, span.addr);
        bitset_for_each(fn.lvars().bitset_size(), fn.lvars().lvar_interferences(lvar_i), [&](unsigned i)
        {
            if(i < lvar_usable_ram.size())
                lvar_usable_ram[i] &= mask;
            else if(Step < FULL_ALLOC)
            {
                locator_t const loc = fn.lvars().locator(i);
                if(has_fn(loc.lclass()))
                {
                    fn_ht h = loc.fn();
                    propagate_calls.set(h.id);
                    propagate_calls |= h->ir_calls();
                }
            }
        });

        if(Step < FULL_ALLOC)
        {
            for(fn_ht fn : fn.lvars().fn_interferences(lvar_i))
            {
                propagate_calls.set(fn.id);
                propagate_calls |= fn->ir_calls();
            }

            propagate_calls.for_each([&](unsigned j)
            {
                fn_data[j].usable_ram &= mask;
            });
        }
    }

    // Update bitsets for fns that call this fn.
    d.recursive_lvar_ram = d.lvar_ram | freebie_ram;

    d.step = Step;
}

} // end anonymous namespace

void alloc_ram(ram_bitset_t const& initial)
{
    ram_allocator_t a(initial);
}

void print_ram(std::ostream& o)
{
    o << "Global variable RAM:\n\n";

    for(group_vars_ht g : group_vars_ht::handles())
    {
        o << fmt("  /%:\n", g->group.name);

        for(gvar_ht v : g->gvars())
        {
            o << fmt("    %: (%)\n", v->global.name, v->type());

            v->for_each_locator([&](locator_t loc)
            { 
                o << fmt("      % = %\n", loc, loc.gmember()->span(loc.atom()));
            });

            o << '\n';
        }

        o << '\n';
    }
}
