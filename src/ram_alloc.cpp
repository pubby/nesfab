#include "ram_alloc.hpp"

#ifndef NDEBUG
#include <iostream>
#endif

#include "flat/small_set.hpp"

#include "decl.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "compiler_error.hpp"
#include "options.hpp"
#include "ram.hpp"
#include "rom.hpp"
#include "debug_print.hpp"

namespace  // anonymous namespace
{

enum zp_request_t
{
    ZP_NEVER,
    ZP_MAYBE,
    ZP_ONLY,
};

enum sram_request_t
{
    SRAM_NEVER,
    SRAM_MAYBE,
    SRAM_ONLY,
};

zp_request_t zp_request(bool valid, bool only, pstring_t at)
{
    if(valid)
    {
        if(only)
            return ZP_ONLY;
        return ZP_MAYBE;
    }
    if(only)
        compiler_error(at, "Unable to allocate in zero page.");
    return ZP_NEVER;
}

sram_request_t sram_request(mods_t const* mods)
{
    if(mod_test(mods, MOD_sram))
        return SRAM_ONLY;
    if(mod_test(mods, MOD_sram, false))
        return SRAM_NEVER;
    return SRAM_MAYBE;
}

// Allocates a span inside 'usable_ram'.
static span_t alloc_ram(ram_sets_t const& usable_ram, std::size_t size, 
                        zp_request_t zp, sram_request_t sram,
                        bool insist_alignment = false)
{
    passert(size < 0x10000, size);

    auto const reduce_zp = [&](ram_bitset_t& ram, bool size_check = true)
    {
        if(zp == ZP_ONLY)
            ram &= zp_bitset;
        else if(zp == ZP_NEVER || (size_check && size > 1))
            ram &= ~zp_bitset; // Don't put arrays in ZP
    };

    auto const try_alloc_ram = [&](ram_bitset_t const& usable_ram, bool size_check) -> span_t
    {
        if(sram == SRAM_ONLY)
            return {};

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
            reduce_zp(usable_copy, size_check);

            bitset_mark_consecutive(usable_copy.size(), usable_copy.data(), size);

            int const addr = usable_copy.lowest_bit_set();
            if(addr < 0)
                return {};
            return { .addr = addr, .size = size };
        }
    };

    auto const try_alloc_sram = [&](sram_bitset_t const& usable_ram) -> span_t
    {
        // SRAM has no zp:
        if(zp == ZP_ONLY)
            return {};

        if(sram == SRAM_NEVER)
            return {};

        if(size == 1)
        {
            int const addr = usable_ram.lowest_bit_set();
            if(addr < 0)
                return {};
            return { .addr = addr + sram_addr, .size = 1 };
        }
        else
        {
            sram_bitset_t usable_copy = usable_ram;
            bitset_mark_consecutive(usable_copy.size(), usable_copy.data(), size);

            int const addr = usable_copy.lowest_bit_set();
            if(addr < 0)
                return {};
            return { .addr = addr + sram_addr, .size = size };
        }
    };

    // Align, when possible
    if(size > 1 && (insist_alignment || size <= 256))
    {
        page_bitset_t page = page_bitset_t::filled(0, (size > 256) ? 1 : (257 - size));

        if(sram != SRAM_ONLY)
        {
            auto const try_alloc_aligned = [&](bool size_check = true) -> span_t
            {
                assert(page.test(0));
                ram_bitset_t aligned = usable_ram.ram;
                reduce_zp(aligned, size_check);

                bitset_mark_consecutive(aligned.size(), aligned.data(), size);

                static_assert(ram_bitset_t::num_ints % page_bitset_t::num_ints == 0);
                static_assert(sram_bitset_t::num_ints % page_bitset_t::num_ints == 0);

                for(unsigned i = 0; i < ram_bitset_t::num_ints; i += page_bitset_t::num_ints)
                    bitset_and(page_bitset_t::num_ints, aligned.data() + i, page.data());

                int const addr = aligned.lowest_bit_set();

                if(addr >= 0)
                    return { .addr = addr, .size = size };
                return {};
            };

            if(span_t span = try_alloc_aligned(true))
                return span;
            if(span_t span = try_alloc_aligned(false))
                return span;
        }

        if(insist_alignment)
        {
            if(sram != SRAM_NEVER && usable_ram.sram)
            {
                sram_bitset_t aligned = *usable_ram.sram;
                bitset_mark_consecutive(aligned.size(), aligned.data(), size);

                for(unsigned i = 0; i < ram_bitset_t::num_ints; i += page_bitset_t::num_ints)
                    bitset_and(page_bitset_t::num_ints, aligned.data() + i, page.data());

                int const addr = aligned.lowest_bit_set();
                if(addr >= 0)
                    return { .addr = addr + sram_addr, .size = size };
            }

            return {};
        }
    }

    if(span_t span = try_alloc_ram(usable_ram.ram, true))
        return span;
    if(usable_ram.sram)
        if(span_t span = try_alloc_sram(*usable_ram.sram))
            return span;
    if(span_t span = try_alloc_ram(usable_ram.ram, false))
        return span;
    return {};
}

class ram_allocator_t
{
public:
    ram_allocator_t(log_t* log, ram_bitset_t const& initial_usable_ram);

private:
    enum step_t
    {
        UNALLOCATED,
        BUILD_ORDER,
        ZP_ONLY_ALLOC,
        FULL_ALLOC,
    };

    void build_order(romv_t romv, std::vector<fn_ht>& fn_order, std::vector<fn_ht>& input_fns);
    void build_order(romv_t romv, std::vector<fn_ht>& fn_order, fn_ht fn);

    template<step_t Step>
    void alloc_locals(romv_t romv, fn_ht h);

    struct group_vars_d
    {
        // Addresses that can be used to allocate globals.
        ram_sets_t usable_ram;

        // Tracks how it interferes with other group_vars
        xbitset_t<group_vars_ht> interferences;

        // Used to estimate how many bytes of ZP are left
        unsigned zp_estimate = 0;
    };

    struct fn_d
    {
        std::array<step_t, NUM_ROMV> step = {};

        // A recursive count of the amount of lvars a fn uses.
        // (The actual RAM required is typically less)
        // This is used to order in which fn lvars are allocated.
        unsigned lvar_count = 0;

        // Addresses that can be used to allocate lvars.
        std::array<ram_sets_t, NUM_ROMV> usable_ram;

        // Ram allocated by lvars in this fn.
        std::array<ram_sets_t, NUM_ROMV> lvar_ram = {};

        // Like above, but includes called fns too.
        std::array<ram_sets_t, NUM_ROMV> recursive_lvar_ram = {};

        // Stores indexes into 'romv_allocated'.
        std::array<fc::small_set<unsigned, 2>, NUM_ROMV> romv_self;
        std::array<fc::small_set<unsigned, 2>, NUM_ROMV> romv_interferes;

        // Normal "ir_group_vars()" only looks up the call graph,
        // but this set also looks down.
        xbitset_t<group_vars_ht> maximal_group_vars;
    };

    group_vars_d& data(group_vars_ht h) { assert(h.id < group_vars_data.size()); return group_vars_data[h.id]; }
    fn_d& data(fn_ht h) { assert(h.id < fn_data.size()); return fn_data[h.id]; }

    std::vector<group_vars_d> group_vars_data;
    std::vector<fn_d> fn_data;

    // Tracks allocations for an entire mode / nmi / irq.
    // This is used to implement romv.
    std::array<std::vector<ram_sets_t>, NUM_ROMV> romv_allocated;

    // Handles static allocations:
    ram_sets_t static_usable_ram;

    log_t* log = nullptr;
};

ram_allocator_t::ram_allocator_t(log_t* log, ram_bitset_t const& initial_usable_ram)
: static_usable_ram(initial_usable_ram)
, log(log)
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);

    if(static_usable_ram.sram)
        static_usable_ram.sram->set_all();

    // Amount of bytes free in zero page
    int const zp_free = (static_usable_ram.ram & zp_bitset).popcount();

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

        // Init romv stuff:
        {
            static_assert(NUM_ROMV == 3);
            romv_allocated[ROMV_MODE].resize(global_t::modes().size());
            romv_allocated[ROMV_NMI].resize(global_t::nmis().size());
            romv_allocated[ROMV_IRQ].resize(global_t::irqs().size());

            rh::batman_map<fn_ht, unsigned> interrupt_map;

            // Setup romv_self:

            for(unsigned i = 0; i < global_t::modes().size(); ++i)
            {
                fn_t const* mode = global_t::modes()[i];
                fn_data[mode->handle().id].romv_self[ROMV_MODE].insert(i);
                mode->ir_calls().for_each([&](fn_ht call) { fn_data[call.id].romv_self[ROMV_MODE].insert(i); });
            }

            for(unsigned i = 0; i < global_t::nmis().size(); ++i)
            {
                fn_t const* nmi = global_t::nmis()[i];
                fn_data[nmi->handle().id].romv_self[ROMV_NMI].insert(i);
                nmi->ir_calls().for_each([&](fn_ht call) { fn_data[call.id].romv_self[ROMV_NMI].insert(i); });
                interrupt_map.insert({ nmi->handle(), i });
            }

            for(unsigned i = 0; i < global_t::irqs().size(); ++i)
            {
                fn_t const* irq = global_t::irqs()[i];
                fn_data[irq->handle().id].romv_self[ROMV_IRQ].insert(i);
                irq->ir_calls().for_each([&](fn_ht call) { fn_data[call.id].romv_self[ROMV_IRQ].insert(i); });
                interrupt_map.insert({ irq->handle(), i });
            }

            // Setup romv_interferes:

            for(unsigned i = 0; i < global_t::modes().size(); ++i)
            {
                fn_t const* mode = global_t::modes()[i];

                if(fn_ht nmi = mode->mode_nmi())
                {
                    unsigned const nmi_index = *interrupt_map.mapped(nmi);

                    mode->ir_calls().for_each([&](fn_ht call) { fn_data[call.id].romv_interferes[ROMV_NMI].insert(nmi_index); });
                    fn_data[mode->handle().id].romv_interferes[ROMV_NMI].insert(nmi_index);

                    nmi->ir_calls().for_each([&](fn_ht call){ fn_data[call.id].romv_interferes[ROMV_MODE].insert(i); });
                    fn_data[nmi.id].romv_interferes[ROMV_MODE].insert(i);

                    if(fn_ht irq = mode->mode_irq())
                    {
                        unsigned const irq_index = *interrupt_map.mapped(irq);
                        nmi->ir_calls().for_each([&](fn_ht call){ fn_data[call.id].romv_interferes[ROMV_IRQ].insert(irq_index); });
                        fn_data[nmi.id].romv_interferes[ROMV_IRQ].insert(irq_index);
                    }
                }

                if(fn_ht irq = mode->mode_irq())
                {
                    unsigned const irq_index = *interrupt_map.mapped(irq);

                    mode->ir_calls().for_each([&](fn_ht call) { fn_data[call.id].romv_interferes[ROMV_IRQ].insert(irq_index); });
                    fn_data[mode->handle().id].romv_interferes[ROMV_IRQ].insert(irq_index);

                    irq->ir_calls().for_each([&](fn_ht call){ fn_data[call.id].romv_interferes[ROMV_MODE].insert(i); });
                    fn_data[irq.id].romv_interferes[ROMV_MODE].insert(i);

                    if(fn_ht nmi = mode->mode_nmi())
                    {
                        unsigned const nmi_index = *interrupt_map.mapped(nmi);
                        irq->ir_calls().for_each([&](fn_ht call){ fn_data[call.id].romv_interferes[ROMV_NMI].insert(nmi_index); });
                        fn_data[irq.id].romv_interferes[ROMV_NMI].insert(nmi_index);
                    }
                }
            }
        }

        // Init 'maximal_group_vars':
        for(fn_ht fn : fn_ht::handles())
        {
            if(fn->fclass == FN_CT)
                continue;

            fn_data[fn.id].maximal_group_vars = fn->ir_group_vars();
            assert(fn_data[fn.id].maximal_group_vars);
        }

        // Build 'maximal_group_vars'
        auto const propagate = [&](fn_t const* fn, auto const& additional)
        {
            assert(fn->ir_calls() && fn->ir_group_vars());
            fn->ir_calls().for_each([&](fn_ht call)
            {
                // Propagate down call graph, not up
                fn_data[call.id].maximal_group_vars |= fn->ir_group_vars();
                fn_data[call.id].maximal_group_vars |= additional;
            });
            fn_data[fn->handle().id].maximal_group_vars |= additional;
        };

        xbitset_t<group_vars_ht> additional(0);

        for(fn_t const* mode : global_t::modes())
        {
            additional.clear_all();
            if(fn_ht nmi = mode->mode_nmi())
                additional |= nmi->ir_group_vars();
            if(fn_ht irq = mode->mode_irq())
                additional |= irq->ir_group_vars();
            propagate(mode, additional);
        }

        for(fn_t const* nmi : global_t::nmis())
        {
            additional.clear_all();
            nmi->nmi_used_in_modes().for_each([&](fn_ht mode)
            {
                additional |= mode->ir_group_vars();
                if(fn_ht irq = mode->mode_irq())
                    additional |= irq->ir_group_vars();
            });
            propagate(nmi, additional);
        }

        for(fn_t const* irq : global_t::irqs())
        {
            additional.clear_all();
            irq->irq_used_in_modes().for_each([&](fn_ht mode)
            {
                additional |= mode->ir_group_vars();
                if(fn_ht nmi = mode->mode_nmi())
                    additional |= nmi->ir_group_vars();
            });
            propagate(irq, additional);
        }

        // Init 'group_vars_data'
        for(unsigned i = 0; i < group_vars_ht::pool().size(); ++i)
        {
            auto& d = group_vars_data[i];
            d.interferences.alloc();
            d.interferences.set(i); // always interfere with itself
            d.usable_ram = static_usable_ram;
            d.zp_estimate = max_gvar_zp;
        }

        // Build interference graph among group vars:
        {
            xbitset_t<group_vars_ht> group_vars;
            for(fn_t const* mode : global_t::modes())
            {
                group_vars = mode->ir_group_vars();
                if(fn_ht nmi = mode->mode_nmi())
                    group_vars |= nmi->ir_group_vars();
                if(fn_ht irq = mode->mode_irq())
                    group_vars |= irq->ir_group_vars();

                group_vars.for_each([&](group_vars_ht gv)
                {
                    auto& d = group_vars_data[gv.id];
                    d.interferences |= group_vars;
                });
            }
        }

        // Count how often gmembers appears in emitted code.
        // We'll eventually allocate using the use count as a heuristic

        rh::batman_map<locator_t, unsigned> gmember_count;
        for(gvar_t const& gvar : gvar_ht::values())
            gvar.for_each_locator([&](locator_t loc){ gmember_count.insert({ loc.mem_head(), 0 }); });

        for(fn_t const& fn : fn_ht::values())
        {
            rom_proc_t const* rom_proc = &fn.rom_proc().safe();

            for(asm_inst_t const& inst : rom_proc->asm_proc().code)
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

            if(count == 0 && !mod_test(gvar.mods(), MOD_unused))
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
        std::vector<rank_t> ordered_gmembers_aligned;

        for(auto const& pair : gmember_count)
        {
            // Priority 1: gvar size
            // Priority 2: frequency in code

            constexpr unsigned size_scale = 256; // arbitrary constant

            auto const& gmember = *pair.first.gmember();
            bool const insist_align = mod_test(gmember.gvar.mods(), MOD_align);
            auto& non_zp_vec = insist_align ? ordered_gmembers_aligned : ordered_gmembers;

            if(is_paa(gmember.type().name()))
            {
                // PAAs are handled separately, as they won't appear in the code.
                non_zp_vec.push_back({ pair.first.mem_size() * size_scale, pair.first });
            }
            else
            {
                // Don't allocate unused shit. 
                if(pair.second == 0)
                    continue;

                if(pair.first.mem_zp_only())
                    ordered_gmembers_zp.push_back({ pair.first.mem_size(), pair.first });
                else
                    non_zp_vec.push_back({ (pair.first.mem_size() * size_scale) + pair.second, pair.first });
            }
        }

        std::sort(ordered_gmembers_zp.begin(), ordered_gmembers_zp.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });
        std::sort(ordered_gmembers.begin(), ordered_gmembers.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });
        std::sort(ordered_gmembers_aligned.begin(), ordered_gmembers_aligned.end(), 
                  [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });

        // Estimate which locators will go into ZP.

        rh::batman_set<locator_t> estimated_in_zp;

        auto const estimate_gmember_loc = [&](locator_t loc)
        {
            dprint(log, "-ALLOC_RAM_ESTIMATE", loc);

            gmember_t& gmember = *loc.gmember();
            unsigned const size = loc.mem_size();

            if(gmember.gvar.group_vars)
            {
                group_vars_d& d = data(gmember.gvar.group_vars);

                if(!d.interferences.for_each_test([&](group_vars_ht gv) -> bool
                    { return group_vars_data[gv.id].zp_estimate >= size; }))
                {
                    dprint(log, "--FAILED_ESTIMATE");
                    return;
                }

                dprint(log, "--SUCCEEDED_ESTIMATE");
                estimated_in_zp.insert(loc);

                assert(d.zp_estimate >= size);
                d.interferences.for_each([&](group_vars_ht gv)
                {
                    assert(group_vars_data[gv.id].zp_estimate >= size);
                    group_vars_data[gv.id].zp_estimate -= size;
                });
            }
            else
            {
                for(group_vars_d& d : group_vars_data)
                {
                    if(d.zp_estimate < size)
                    {
                        dprint(log, "--FAILED_ESTIMATE");
                        return;
                    }
                }

                dprint(log, "--SUCCEEDED_ESTIMATE");
                estimated_in_zp.insert(loc);

                for(group_vars_d& d : group_vars_data)
                {
                    assert(d.zp_estimate >= size);
                    d.zp_estimate -= size;
                }
            }
        };

        for(rank_t const& rank : ordered_gmembers_zp)
            estimate_gmember_loc(rank.loc);

        for(rank_t const& rank : ordered_gmembers)
            estimate_gmember_loc(rank.loc);

        for(rank_t const& rank : ordered_gmembers_aligned)
            estimate_gmember_loc(rank.loc);

        // For global vars that have init expressions,
        // we want to allocate their group to be contiguous,
        // as this means we can more efficiently init them.
        //
        // To do this, we'll bundle init'd locators into structs.

        struct group_inits_t
        {
            unsigned score = 0;
            std::vector<locator_t> init;
        };

        std::vector<group_inits_t> ordered_inits;
        ordered_inits.reserve(group_vars_ht::pool().size());

        auto const check_init = [&](gvar_ht v, group_inits_t& zero_inits, group_inits_t& value_inits)
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
        };

        for(group_t* g : group_vars_ht::values())
        {
            group_inits_t zero_inits  = {};
            group_inits_t value_inits = {};

            for(gvar_ht v : g->vars()->gvars())
                check_init(v, zero_inits, value_inits);

            if(!zero_inits.init.empty())
                ordered_inits.push_back(std::move(zero_inits));
            if(!value_inits.init.empty())
                ordered_inits.push_back(std::move(value_inits));
        }

        // Vars with no group:
        {
            group_inits_t zero_inits  = {};
            group_inits_t value_inits = {};

            for(gvar_ht v : gvar_t::groupless_gvars())
                check_init(v, zero_inits, value_inits);

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
                if(loc.atom() != 0) // The first atom handles both atoms!
                    return;
            }

            dprint(log, "-RAM_GMEMBER_ALLOCATION", loc, loc.mem_size(), loc.mem_zp_only());

            zp_request_t const zp = zp_request(loc.mem_zp_valid() && 
                (loc.mem_zp_only() || estimated_in_zp.count(loc)), 
                loc.mem_zp_only(),
                gmember.gvar.global.pstring());
            sram_request_t const sram = sram_request(loc.mods());
            bool const insist_align = mod_test(gmember.gvar.mods(), MOD_align);

            span_t span;

            if(gmember.gvar.group_vars)
            {
                // Try to allocate in a position that minimizes the amount of 
                // 'usable_ram' changed in interfering group vars bitsets.
                //
                // To approximate this, we'll calculate two sets: 'all' and 'any'.
                // 'all' represents ram locations that do not modify interfering bitsets.
                // 'any' represents ram locations that does not modify at least 1 interfering bitset.

                group_vars_d& d = data(gmember.gvar.group_vars);

                ram_sets_t all;
                ram_sets_t any;
                all.clear_all();
                any.set_all();
                d.interferences.for_each([&](group_vars_ht gv)
                {
                    all |= group_vars_data[gv.id].usable_ram;
                    any &= group_vars_data[gv.id].usable_ram;
                });
                all.flip_all();
                any.flip_all();

                // Allocate, prioritizing 'all', then 'any', then just 'd.usable_ram'.
                span = alloc_ram(d.usable_ram & all, size, zp, sram, insist_align);
                if(!span)
                    span = alloc_ram(d.usable_ram & any, size, zp, sram, insist_align);
                if(!span)
                    span = alloc_ram(d.usable_ram, size, zp, sram, insist_align);
            }
            else
            {
                ram_sets_t all = static_usable_ram;

                for(group_vars_d const& d : group_vars_data)
                    all &= d.usable_ram;

                span = alloc_ram(all, size, zp, sram, insist_align);
            }

            if(!span)
                throw std::runtime_error("Unable to allocate global variable (out of RAM).");

            dprint(log, "--RESULT", span);

            if(is_ptr)
            {
                assert(span.size == 2);
                gmember.assign_span(0, { .addr = span.addr, .size = 1 });
                gmember.assign_span(1, { .addr = span.addr+1, .size = 1 });
            }
            else
                gmember.assign_span(loc.atom(), span);

            auto const update = [&](auto const& mask, auto const& get)
            {
                if(gmember.gvar.group_vars)
                {
                    group_vars_d& d = data(gmember.gvar.group_vars);

                    d.interferences.for_each([&](group_vars_ht gv)
                    {
                        get(group_vars_data[gv.id].usable_ram) &= mask;
                    });
                }
                else
                {
                    get(static_usable_ram) &= mask;

                    for(group_vars_d& d : group_vars_data)
                        get(d.usable_ram) &= mask;
                }
            };

            if(span.addr < sram_addr)
                update(~ram_bitset_t::filled(span.addr, span.size), get_ram);
            else
            {
                assert(mapper().sram);
                update(~sram_bitset_t::filled(span.addr - sram_addr, span.size), get_sram);
            }
        };

        // Allocate

        for(rank_t const& rank : ordered_gmembers_aligned)
            alloc_gmember_loc(rank.loc);

        for(group_inits_t const& inits : ordered_inits)
            for(locator_t loc : inits.init)
                alloc_gmember_loc(loc);

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

            // Init static_usable_ram:
            for(auto& bs : fn_data[fn.id].usable_ram)
                bs = static_usable_ram;

            fn_data[fn.id].maximal_group_vars.for_each([&](group_vars_ht gv)
            {
                for(auto& bs : fn_data[fn.id].usable_ram)
                    bs &= group_vars_data[gv.id].usable_ram;
            });
        }

        // Build an order to allocate fn lvars:
        for(fn_ht fn : fn_ht::handles())
        {
            if(fn->fclass == FN_CT)
                continue;

            fn_data[fn.id].lvar_count += fn->lvars().num_this_lvars();

            fn->ir_calls().for_each([&](fn_ht call)
            {
                fn_data[call.id].lvar_count += fn->lvars().num_this_lvars();
            });
        }

        std::array<std::vector<ram_sets_t>, NUM_ROMV> total;
        total[ROMV_MODE].resize(global_t::modes().size());
        total[ROMV_NMI].resize(global_t::nmis().size());
        total[ROMV_IRQ].resize(global_t::irqs().size());

        std::array<std::vector<fn_ht>, NUM_ROMV> fn_orders;
        for(auto& order : fn_orders)
            order.reserve(fn_ht::pool().size());

        std::array<std::vector<fn_ht>, NUM_ROMV> ranks;
        for(auto& rank : ranks)
            rank.reserve(global_t::modes().size());
        for(fn_t* mode : global_t::modes())
            ranks[ROMV_MODE].push_back(mode->handle());
        for(fn_t* nmi : global_t::nmis())
            ranks[ROMV_NMI].push_back(nmi->handle());
        for(fn_t* irq : global_t::irqs())
            ranks[ROMV_IRQ].push_back(irq->handle());

        for(unsigned i = 0; i < ranks.size(); ++i)
            build_order(romv_t(i), fn_orders[i], ranks[i]);

        for(fn_set_t const& set : fn_set_ht::values())
        {
            romv_t const set_romv = set.romv();
            for(fn_ht fn : set)
                build_order(set_romv, fn_orders[set_romv], fn);
        }

        for(unsigned i = 0; i < ranks.size(); ++i)
            for(fn_ht fn : fn_orders[i])
                dprint(log, "-RAM_ALLOC_BUILD_ORDER", i, fn->global.name);

        for(int romv = NUM_ROMV - 1; romv >= 0; --romv)
            for(fn_ht fn : fn_orders[romv])
                alloc_locals<ZP_ONLY_ALLOC>(romv_t(romv), fn);

        for(int romv = NUM_ROMV - 1; romv >= 0; --romv)
            for(fn_ht fn : fn_orders[romv])
                alloc_locals<FULL_ALLOC>(romv_t(romv), fn);
    }
}

void ram_allocator_t::build_order(romv_t romv, std::vector<fn_ht>& fn_order, std::vector<fn_ht>& input_fns)
{
    std::sort(input_fns.begin(), input_fns.end(), [&](fn_ht a, fn_ht b)
    {
        return data(a).lvar_count > data(b).lvar_count;
    });

    for(fn_ht input_fn : input_fns)
        build_order(romv, fn_order, input_fn);
}

void ram_allocator_t::build_order(romv_t romv, std::vector<fn_ht>& fn_order, fn_ht fn)
{
    fn_d& d = data(fn);

    dprint(log, "-RAM_ALLOC_BUILD_ORDER_STEP", romv, fn->global.name);

    if(d.step[romv] == BUILD_ORDER)
        return;

    dprint(log, "-RAM_ALLOC_BUILD_ORDER_CONTINUE", romv, fn->global.name, fn->ir_calls().popcount());

    // Make sure all called fns are ordered first:
    {
        std::vector<fn_ht> fn_rank;
        fn_rank.reserve(fn->ir_calls().popcount());
        fn->ir_calls().for_each([&fn_rank](fn_ht call){ fn_rank.push_back(call); });
        build_order(romv, fn_order, fn_rank);
    }

    fn_order.push_back(fn);
    d.step[romv] = BUILD_ORDER;
}

template<ram_allocator_t::step_t Step>
void ram_allocator_t::alloc_locals(romv_t const romv, fn_ht h)
{
    fn_t& fn = *h;
    fn_d& d = data(h);

    dprint(log, "RAM_ALLOC_LOCALS", Step, romv, fn.global.name, (unsigned)fn.precheck_romv());

    assert(d.step[romv] < Step);
    passert(h->precheck_romv() & (1 << romv), (int)h->precheck_romv(), (int)romv, h->global.name);
    assert((data(h).usable_ram[romv] & data(h).lvar_ram[romv]).all_clear());

    // Refine 'usable_ram', adding in romv interferences:
    for(unsigned i = 0; i < NUM_ROMV; ++i)
    {
        if(i != romv)
        {
            for(unsigned j : d.romv_interferes[i])
            {
                dprint(log, "-INTERFERE", fn.global.name, i, j);
                d.usable_ram[romv] -= romv_allocated[i][j];
            }
        }
    }

    // Setup lvar usable ram:
    std::vector<ram_sets_t> lvar_usable_ram;
    lvar_usable_ram.resize(fn.lvars().num_this_lvars(), d.usable_ram[romv]);

    for(unsigned i = 0; i < fn.lvars().num_this_lvars(); ++i)
    {
        for(fn_ht interfering_fn : fn.lvars().fn_interferences(i))
        {
            assert(data(interfering_fn).step[romv] == Step);

            lvar_usable_ram[i] &= data(interfering_fn).usable_ram[romv];
            lvar_usable_ram[i] -= data(interfering_fn).recursive_lvar_ram[romv];
        }
    }

    // Some tracked lvars don't belong to this fn and were already allocated.
    // Handle these interferences here:
    for(unsigned i = fn.lvars().num_this_lvars(); i < fn.lvars().num_all_lvars(); ++i)
    {
        locator_t const loc = fn.lvars().locator(i);
        if(!has_fn(loc.lclass()))
            continue;
        span_t const span = loc.fn()->lvar_span(romv, loc);

        if(!span) // It might not have been allocated yet, or it might not exist in the generated assembly.
            continue;

        auto const update = [&](auto const& mask, auto const& get)
        {
            bitset_for_each(fn.lvars().bitset_size(), fn.lvars().lvar_interferences(i), [&](unsigned j)
            {
                if(j < lvar_usable_ram.size())
                    get(lvar_usable_ram[j]) &= mask;
            });
        };

        if(span.addr < sram_addr)
            update(~ram_bitset_t::filled(span.addr, span.size), get_ram);
        else
        {
            assert(mapper().sram);
            update(~sram_bitset_t::filled(span.addr - sram_addr, span.size), get_sram);
        }
    }

    // Prefer to use the same RAM addresses that called fns use.
    // We'll call this set 'freebie_ram'.
    ram_sets_t freebie_ram = {};
    fn.ir_calls().for_each([&](fn_ht call)
    {
        assert(fn_data[call.id].step[romv] == Step);
        freebie_ram |= fn_data[call.id].recursive_lvar_ram[romv];
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
        {
            dprint(log, "-SKIP LVAR 1", fn.global.name, i);
            continue;
        }

        if(info.ptr_hi) // We'll allocate lo only, then assign to hi.
        {
            dprint(log, "-SKIP LVAR 2", fn.global.name, i);
            continue;
        }

        if(fn.lvar_span(romv, i))
        {
            dprint(log, "-SKIP LVAR 3", fn.global.name, i);
            continue;
        }

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
    xbitset_t<fn_ht> propagate_calls(0);

    for(rank_t const& rank : ordered_lvars)
    {
        unsigned const lvar_i = rank.lvar_i;
        auto const& info = fn.lvars().this_lvar_info(lvar_i);
        locator_t const loc = fn.lvars().locator(lvar_i);
        assert(!info.ptr_hi);

        dprint(log, "-RAM_ALLOC_LOCALS", loc, info.size);

        assert(lvar_i < lvar_usable_ram.size());

        zp_request_t const zp = zp_request(info.zp_valid, info.zp_only, h->global.pstring());
        sram_request_t const sram = SRAM_MAYBE;

        // First try to allocate in 'freebie_ram'.
        span_t span = alloc_ram(lvar_usable_ram[lvar_i] & freebie_ram, info.size, zp, sram);

        // If that fails, try to allocate anywhere.
        if(!span)
            span = alloc_ram(lvar_usable_ram[lvar_i], info.size, zp, sram);

        // If that fails, we're fucked.
        if(!span)
            throw std::runtime_error(fmt("Unable to allocate local variable in fn % (out of RAM).", fn.global.name));

        dprint(log, "--RESULT", span);

        // Record the allocation.

        if(info.ptr_alt >= 0)
        {
            assert(span.size == 2);
            assert(static_cast<int>(lvar_i) != info.ptr_alt);
            dprint(log, "---PTR_ALT");
            fn.assign_lvar_span(romv, lvar_i,       { .addr = span.addr,     .size = 1 }); 
            fn.assign_lvar_span(romv, info.ptr_alt, { .addr = span.addr + 1, .size = 1 }); 
        }
        else
            fn.assign_lvar_span(romv, lvar_i, span); 

        propagate_calls.clear_all();

        auto const update = [&](auto const& filled, auto const& get)
        {
            assert((get(lvar_usable_ram[lvar_i]) & filled) == filled);
            get(d.lvar_ram[romv]) |= filled;
            get(d.usable_ram[romv]) -= get(d.lvar_ram[romv]);

            auto const mask = ~filled;
            bitset_for_each(fn.lvars().bitset_size(), fn.lvars().lvar_interferences(lvar_i), [&](unsigned i)
            {
                if(i < lvar_usable_ram.size())
                    get(lvar_usable_ram[i]) &= mask;
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
            }

            if(fn_set_t const* set = fn.fn_set())
            {
                romv_t const set_romv = set->romv();

                for(fn_ht co : *set)
                {
                    if(co == fn.handle())
                        continue;

                    propagate_calls.set(co.id);
                    propagate_calls |= co->ir_calls();

                    if(romv == set_romv && is_arg_ret(loc.lclass()))
                    {
                        locator_t co_loc = loc;
                        co_loc.set_handle(co.id);

                        int const co_i = co->lvars().index(co_loc);
                        if(co_i < 0)
                            continue;

                        if(info.ptr_alt >= 0)
                        {
                            assert(span.size == 2);
                            auto const& co_info = co->lvars().this_lvar_info(co_i);
                            co->assign_lvar_span(romv, co_i,            { .addr = span.addr,     .size = 1 }); 
                            co->assign_lvar_span(romv, co_info.ptr_alt, { .addr = span.addr + 1, .size = 1 }); 
                        }
                        else
                            co->assign_lvar_span(romv, co_i, span); 
                    }
                }
            }

            propagate_calls.for_each([&](fn_ht call)
            {
                get(fn_data[call.id].usable_ram[romv]) &= mask;
            });
        };

        if(span.addr < sram_addr)
            update(ram_bitset_t::filled(span.addr, span.size), get_ram);
        else
        {
            assert(mapper().sram);
            update(sram_bitset_t::filled(span.addr - sram_addr, span.size), get_sram);
        }
    }

    // Update bitsets for fns that call this fn.
    d.recursive_lvar_ram[romv] = d.lvar_ram[romv] | freebie_ram;

    // Propagate romv interferences:
    for(unsigned i : d.romv_self[romv])
    {
        dprint(log, "-PROPAGATE", fn.global.name, romv, i);
        romv_allocated[romv][i] |= d.recursive_lvar_ram[romv];
    }

    d.step[romv] = Step;
}

} // end anonymous namespace

void alloc_ram(log_t* log, ram_bitset_t const& initial)
{
    dprint(log, "ALLOCATING_RAM");
    ram_allocator_t a(log, initial);
}

void print_ram(std::ostream& o)
{
    o << "Global variable RAM:\n\n";

    o << fmt("  /:\n");

    for(gvar_ht v : gvar_t::groupless_gvars())
    {
        o << fmt("    %: (%)\n", v->global.name, v->type());

        v->for_each_locator([&](locator_t loc)
        { 
            o << fmt("      % = %\n", loc, loc.gmember()->span(loc.atom()));
        });

        o << '\n';
    }

    for(group_vars_ht g : group_vars_ht::handles())
    {
        o << fmt("  %:\n", (*g)->name);

        for(gvar_ht v : (*g)->vars()->gvars())
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

    o << "fn RAM:\n\n";
    for(fn_t const& fn : fn_ht::values())
    {
        o << fmt("  %:\n", fn.global.name);
        fn.lvars().for_each_lvar(true, [&](locator_t loc, int i)
        {
            o << fmt("    %:", loc);
            for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
                o << fmt(" (%)", fn.lvar_span(romv_t(romv), i));
            o << '\n';
        });
    }
}
