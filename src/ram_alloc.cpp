#include "ram_alloc.hpp"

#include <iostream> // TODO

#include "decl.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "compiler_error.hpp"
#include "ram.hpp"

namespace  // anonymous namespace
{

// Modifies 'ram' to only include addresses that can hold a contiguous span of 'size' bytes
// (This is useful for allocating multi-byte regions like arrays and pointers)
void ram_for_size(ram_bitset_t& ram, std::size_t size)
{
    if(size <= 1)
        return;

    unsigned shift_by = 1;
    for(size -= 1; shift_by <= size; shift_by <<= 1)
        ram &= ram >> shift_by;
    ram &= ram >> (size - (shift_by >> 1));
}

// Allocates a span inside 'usable_ram'.
span_t alloc_ram(ram_bitset_t const& usable_ram, std::size_t size, bool zp_only)
{
    assert(size > 0);

    if(size == 1) // Fast path when 'size' == 0
    {
        int const addr = usable_ram.lowest_bit_set();

        if(addr < 0 || (zp_only && addr > 0xFF ))
            return {};

        return { .addr = addr, .size = 1 };
    }
    else
    {
        ram_bitset_t usable_copy = usable_ram;

        if(zp_only)
            usable_copy &= zp_bitset;
        else if(size > 1)
            usable_copy &= ~zp_bitset; // Don't put arrays in ZP

        ram_for_size(usable_copy, size);

        int const addr = usable_copy.lowest_bit_set();

        if(addr < 0)
            return {};

        return { .addr = addr, .size = size };
    }
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
    };

    group_vars_d& data(group_vars_ht h) { return group_vars_data[h.value]; }
    fn_d& data(fn_ht h) { return fn_data[h.value]; }

    std::vector<group_vars_d> group_vars_data;
    std::vector<fn_d> fn_data;
    std::vector<fn_ht> fn_order;
};

ram_allocator_t::ram_allocator_t(ram_bitset_t const& initial_usable_ram)
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);

    group_vars_data.resize(impl_deque<group_vars_t>.size());
    fn_data.resize(impl_deque<fn_t>.size());

    ///////////////////
    // ALLOC GLOBALS //
    ///////////////////

    {
        for(gmember_t& gm : impl_vector<gmember_t>)
            gm.alloc_spans();

        // Init data
        for(unsigned i = 0; i < impl_deque<group_vars_t>.size(); ++i)
        {
            auto& d = group_vars_data[i];
            d.interferences.reset(impl_bitset_size<group_vars_t>());
            d.interferences.set(i); // always interfere with itself
            d.usable_ram = initial_usable_ram;
        }

        // Build interference graph among 'group vars':
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

        // Count how often gmembers appears in emitted code...

        rh::batman_map<locator_t, unsigned> gmember_count;
        for(gvar_t const& gvar : impl_deque<gvar_t>)
            gvar.for_each_locator([&](locator_t loc){ gmember_count.insert({ loc, 0 }); });

        for(fn_t const& fn : impl_deque<fn_t>)
        {
            if(!fn.emits_code())
                continue;

            for(asm_inst_t const& inst : fn.proc().code)
                if(inst.arg.lclass() == LOC_GMEMBER)
                    if(unsigned* count = gmember_count.mapped(inst.arg.mem_head()))
                        *count += 1;
        }

        // Then allocate, using the use count as a heuristic

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
            if(pair.second == 0)
            {
                unused_gvars.insert(&pair.first.gmember()->gvar);
                continue;
            }

            // Priority 1: gvar size
            // Priority 2: frequency in code

            if(pair.first.mem_zp_only())
                ordered_gmembers_zp.push_back({ pair.first.mem_size(), pair.first });
            else
                ordered_gmembers.push_back({ (pair.first.mem_size() * 256) + pair.second, pair.first });
        }

        for(gvar_t const* gvar : unused_gvars)
            compiler_warning(gvar->global.pstring(), "Not every byte of global variable is used.");

        std::sort(ordered_gmembers_zp.begin(), ordered_gmembers_zp.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });
        std::sort(ordered_gmembers.begin(), ordered_gmembers.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });

        auto const alloc_gmember_loc = [&](locator_t loc)
        {
            gmember_t& gmember = *loc.gmember();
            group_vars_d& d = data(gmember.gvar.group_vars);

            std::printf("allocing %i %i\n", loc.mem_size(), loc.mem_zp_only());

            span_t const span = alloc_ram(
                d.usable_ram, loc.mem_size(), loc.mem_zp_only());

            if(!span)
                throw std::runtime_error("Unable to allocate global variable (out of RAM).");

            gmember.assign_span(loc.atom(), span);

            ram_bitset_t const mask = ~ram_bitset_t::filled(span.size, span.addr);

            d.interferences.for_each([&](unsigned i)
            {
                group_vars_data[i].usable_ram &= mask;
            });
        };

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
        for(unsigned i = 0; i < impl_deque<fn_t>.size(); ++i)
        {
            fn_t const& fn = *fn_ht{i};

            if(fn.fclass == FN_CT)
                continue;

            fn.ir_group_vars().for_each([&](unsigned j)
            {
                fn_data[i].usable_ram &= group_vars_data[j].usable_ram;
            });
        }

        // Build an order to allocate fn lvars:
        for(unsigned i = 0; i < impl_deque<fn_t>.size(); ++i)
        {
            fn_t const& fn = *fn_ht{i};

            if(fn.fclass == FN_CT)
                continue;

            fn_data[i].lvar_count += fn.lvars().num_this_lvars();

            fn.ir_calls().for_each([&](unsigned i)
            {
                fn_data[i].lvar_count += fn.lvars().num_this_lvars();
            });
        }

        fn_order.reserve(impl_deque<fn_t>.size());

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

    std::cout << "lvars =  " << fn.lvars().num_all_lvars() << std::endl;

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
        if(Step == ZP_ONLY_ALLOC && !fn.lvars().mem_zp_only(i))
            continue;

        if(fn.lvar_span(i))
            continue;

        unsigned const lvar_size = fn.lvars().mem_size(i);
        unsigned const usable = lvar_usable_ram[i].popcount();
        unsigned const interferences = bitset_popcount(fn.lvars().bitset_size(), 
                                                       fn.lvars().lvar_interferences(i));
        float const score = float(usable - lvar_size) / interferences;

        ordered_lvars.push_back({ score, i });
    }

    std::sort(ordered_lvars.begin(), ordered_lvars.end());

    // Alright, now we'll allocate.

    // If we're not allocating everything, we'll have to propagate
    // our allocations upwards to fns we call.
    // (We always propagate downwards, to fns that call this)
    bitset_t propagate_calls;
    if(Step < FULL_ALLOC)
        propagate_calls.reset(impl_bitset_size<fn_t>());

    for(rank_t const& rank : ordered_lvars)
    {
        unsigned const lvar_i = rank.lvar_i;
        assert(lvar_i < lvar_usable_ram.size());

        // First try to allocate in 'freebie_ram'.
        span_t span = alloc_ram(
            lvar_usable_ram[lvar_i] & freebie_ram,
            fn.lvars().mem_size(lvar_i), fn.lvars().mem_zp_only(lvar_i));

        // If that fails, try to allocate anywhere.
        if(!span)
        {
            span = alloc_ram(
                lvar_usable_ram[lvar_i],
                fn.lvars().mem_size(lvar_i), fn.lvars().mem_zp_only(lvar_i));
        }

        // If that fails, we're fucked.
        if(!span)
            throw std::runtime_error("Unable to allocate local variable (out of RAM).");

        // Record the allocation.

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
                fn_ht h = fn.lvars().locator(i).fn();
                propagate_calls.set(h.value);
                propagate_calls |= h->ir_calls();
            }
        });

        if(Step < FULL_ALLOC)
        {
            for(fn_ht fn : fn.lvars().fn_interferences(lvar_i))
            {
                propagate_calls.set(fn.value);
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

void alloc_ram(ram_bitset_t const& initial_usable_ram)
{
    ram_allocator_t a(initial_usable_ram);
}
