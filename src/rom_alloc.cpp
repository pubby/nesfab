#include "rom_alloc.hpp"

#include <cmath>
#include <vector>
#ifndef NDEBUG
#include <iostream>
#endif
#include <iostream> // TODO

#include "rom.hpp"
#include "handle.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "eternal_new.hpp"
#include "span_allocator.hpp"
#include "debug_print.hpp"
#include "lt.hpp"

class rom_allocator_t
{
public:
    rom_allocator_t(log_t* log, span_allocator_t& allocator);

private:
    struct bank_rank_t
    {
        float score;
        unsigned bank_index;

        constexpr auto operator<=>(bank_rank_t const& o) const = default;
    };

    /////////////
    // MEMBERS //
    /////////////

    std::vector<bitset_uint_t> required_many_bitsets;
    std::vector<bitset_uint_t> group_data_once_bitsets;
    std::vector<bitset_uint_t> group_data_many_bitsets;

    std::vector<rom_bank_t> banks;
    std::vector<bank_rank_t> bank_ranks;

    unsigned many_bs_size = 0;
    unsigned once_bs_size = 0;

    span_t switched_span = {};
    unsigned const num_switched_banks = mapper().num_switched_prg_banks();

    log_t* log = nullptr;

    ///////////////
    // FUNCTIONS //
    ///////////////

    // Used to create an allocation order for onces.
    float once_rank(rom_once_t const& once);

    // Used to find the best bank to allocate a once in
    float bank_rank(rom_bank_t const& bank, rom_once_t const& once);

    // Builds 'bank_ranks'.
    void rank_banks_for(rom_once_t const& once);

    // Allocates a 'once', while also allocating the 'many's it uses.
    void alloc(rom_once_ht once_h);

    // Reallocates the many to satisfy a new 'in_banks' set.
    bool realloc_many(rom_many_ht many_h, bank_bitset_t in_banks);

    // Removes the 'many' from a bank.
    void free_many(rom_many_ht many_h, unsigned bank_i);

    // Tries to allocate an existing many in a new bank.
    bool try_include_many(rom_many_ht many_h, unsigned bank_i);

    // Allocate a static span
    span_t alloc_static(unsigned size);

    // Allocate a DPCM span
    span_t alloc_dpcm(unsigned size);
};

rom_allocator_t::rom_allocator_t(log_t* log, span_allocator_t& allocator)
: switched_span(mapper().switched_rom_span())
, log(log)
{
    // If we have a this_bank_addr, leave it unallocated.
    if(mapper().this_bank_addr())
    {
        assert(mapper().this_bank_addr() == switched_span.addr + switched_span.size - 1);
        switched_span.size -= 1;
    }

    /////////////////////////////////////////////////
    // Track which procs directly use which arrays //
    /////////////////////////////////////////////////

    std::vector<rh::batman_set<rom_proc_ht>> rom_array_used_by(rom_array_ht::pool().size());
    std::vector<rh::batman_set<rom_array_ht>> rom_proc_directly_uses(rom_proc_ht::pool().size());

    std::function<void(rom_proc_ht, locator_t)> try_insert = [&](rom_proc_ht proc, locator_t loc)
    {
        if(rom_data_ht data = loc.rom_data())
        {
            if(data.rclass() == ROMD_ARRAY)
            {
                rom_array_ht rom_array = { data.handle() };
                rom_proc_directly_uses[proc.id].insert(rom_array);
                rom_array_used_by[rom_array.id].insert(proc);
            }
        }
        else if(loc.lclass() == LOC_LT_EXPR)
            loc.lt()->for_each_locator([&](locator_t loc) { try_insert(proc, loc); });
    };

    std::size_t const gd_bs_size = group_data_ht::bitset_size();
    bitset_uint_t* gd_bs = ALLOCA_T(bitset_uint_t, gd_bs_size);
    bitset_uint_t* intersect_bs = ALLOCA_T(bitset_uint_t, gd_bs_size);

    for(rom_proc_ht proc : rom_proc_ht::handles())
    {
        if(!proc->emits())
            continue;

        // Assume asm functions use any rom array in their groups:
        if(fn_ht fn = proc->asm_proc().fn)
        {
            if(fn->iasm)
            {
                bitset_clear_all(gd_bs_size, gd_bs);
                fn->ir_deref_groups().for_each([&](group_ht group)
                {
                    if(group->data_handle())
                        bitset_set(gd_bs, group->data_handle().id);
                });

                for(rom_array_ht rom_array_h : rom_array_ht::handles())
                {
                    bitset_copy(gd_bs_size, intersect_bs, rom_array_h->used_in_group_data().data());
                    bitset_and(gd_bs_size, intersect_bs, gd_bs);
                    if(!bitset_all_clear(gd_bs_size, intersect_bs))
                        try_insert(proc, locator_t::rom_array(rom_array_h));
                }

                goto insert_from_asm;
            }
            else
            {
                for(rom_array_ht rom_array : fn->direct_rom_arrays())
                {
                    rom_proc_directly_uses[proc.id].insert(rom_array);
                    rom_array_used_by[rom_array.id].insert(proc);
                    rom_array->max_rule(proc->rule());
                }
            }
        }
        else
        {
        insert_from_asm:
            for(asm_inst_t const& inst : proc->asm_proc().code)
            {
                try_insert(proc, inst.arg);
                try_insert(proc, inst.alt);
            }
        }
    }

    //////////////////////////
    // Convert 'rom_array's //
    //////////////////////////

    for(rom_array_ht rom_array_h : rom_array_ht::handles())
    {
        dprint(log, "-PREP_ALLOC_ROM_ARRAY", rom_array_h);
        rom_array_t& rom_array = *rom_array_h;
        assert(rom_array.desired_romv() == ROMVF_IN_MODE);

        if(!rom_array.emits())
        {
            dprint(log, "--SKIPPING (no emit)", rom_array_h);
            continue;
        }

        bool once = !rom_array.omni();

        if(rom_array.used_in_group_data().all_clear())
        {
            // We'll decide this being 'once' or 'many' by looking at the size
            // and comparing it to the size of functions that use it.

            std::size_t summed_size = 0;
            std::size_t proc_count = 0;

            for(rom_proc_ht rom_proc : rom_array_used_by[rom_array_h.id])
            {
                if(rom_proc->emits())
                {
                    summed_size += rom_proc->maxest_size();
                    ++proc_count;
                }
            }

            once &= (float(summed_size) / float(proc_count)) < float(rom_array.data().size());
        }

        if(rom_array.get_alloc(ROMV_MODE))
        {
            dprint(log, "--SKIPPING (already allocated)", rom_array_h);
            continue;
        }

        unsigned alignment = 1;
        if(rom_array.align())
            alignment = std::min<unsigned>(256, std::bit_ceil(rom_array.data().size()));

        if(rom_array.rule() == ROMR_DPCM)
        {
            span_allocation_t const spans = allocator.alloc_linear(rom_array.data().size(), 64, 0xC000);
            if(!spans)
                throw std::runtime_error("Unable to allocate DPCM address (out of ROM space).");
            rom_array.set_alloc(ROMV_MODE, rom_static_ht::pool_make(ROMV_MODE, spans.object, rom_array_h), rom_key_t());
            continue;
        }
        else if(rom_array.rule() == ROMR_SECTOR)
            alignment = mapper().sector_size;
        else if(rom_array.rule() == ROMR_STATIC)
        {
            span_allocation_t const spans = allocator.alloc(rom_array.data().size(), alignment);
            assert(spans.object.addr % alignment == 0);
            if(!spans)
                throw std::runtime_error(fmt("Unable to allocate ROM static address (out of ROM space). Size = %.", rom_array.data().size()));
            rom_array.set_alloc(ROMV_MODE, rom_static_ht::pool_make(ROMV_MODE, spans.object, rom_array_h), rom_key_t());
            continue;
        }

        if(once)
        {
            rom_once_ht const once = rom_once_ht::pool_make(ROMV_MODE, rom_array_h, alignment);
            rom_array.set_alloc(ROMV_MODE, once, rom_key_t());
            dprint(log, "--PREPPED ONCE", rom_array_h, once);
        }
        else
        {
            rom_many_ht const many = rom_many_ht::pool_make(ROMV_MODE, rom_array_h, alignment);
            rom_array.set_alloc(ROMV_MODE, many, rom_key_t());
            dprint(log, "--PREPPED MANY", rom_array_h, many, once);
        }
        assert(rom_array.get_alloc(ROMV_MODE));
        assert(rom_array.get_alloc(ROMV_MODE).rclass());
    }

    ///////////////////////////
    // Convert 'rom_proc_t's //
    ///////////////////////////

    // Organize every rom array by group:
    rh::batman_map<group_data_ht, std::vector<rom_array_ht>> group_rom_arrays;
    for(rom_array_ht ra : rom_array_ht::handles())
    {
        ra->used_in_group_data().for_each([&](unsigned gd)
        {
            group_rom_arrays[group_data_ht{gd}].push_back(ra);
        });
    }

    for(rom_proc_ht rom_proc_h : rom_proc_ht::handles())
    {
        dprint(log, "-PREP_ALLOC_ROM_PROC", rom_proc_h);
        rom_proc_t& rom_proc = *rom_proc_h;

        if(!rom_proc.emits())
        {
            dprint(log, "--SKIPPING (no emit)", rom_proc_h);
            continue;
        }

        if(rom_proc.rule() == ROMR_STATIC)
        {
            romv_for_each(rom_proc.desired_romv(), [&](romv_t romv)
            {
                span_allocation_t const spans = allocator.alloc(rom_proc.max_size(romv));
                if(!spans)
                    throw std::runtime_error("Unable to allocate ROM static address (out of ROM space).");
                rom_proc.set_alloc(romv, rom_static_ht::pool_make(romv, spans.object, rom_proc_h), rom_key_t());
            });
            continue;
        }

        // Check if the proc uses any 'ONCE'.
        // If it does, the proc has to be MANY, otherwise its ONCE.

        unsigned const alignment = rom_proc.align() ? 256 : 1;
        bool once = true;

        for(rom_array_ht use : rom_proc_directly_uses[rom_proc_h.id])
        {
            if(use->get_alloc(ROMV_MODE).rclass() == ROMA_ONCE)
            {
                once = false;
                goto is_many;
            }
        }

        once &= rom_proc.for_each_group_test([&](group_ht group_h) -> bool
            {
                group_t const& group = *group_h;

                for(rom_array_ht ra : group_rom_arrays[group.data_handle()])
                    if(ra->get_alloc(ROMV_MODE).rclass() == ROMA_ONCE)
                        return false;

                return true;
            });

    is_many:

        romv_for_each(rom_proc.desired_romv(), [&](romv_t romv)
        {
            if(rom_proc.get_alloc(romv))
            {
                dprint(log, "--SKIPPING (already allocated)", rom_proc_h);
                return;
            }

            if(once)
            {
                if(rom_proc_h->asm_proc().fn)
                    dprint(log, "--ONCE FN", rom_proc_h->asm_proc().fn->global.name, rom_proc_h->uses_groups());
                rom_proc.set_alloc(romv, rom_once_ht::pool_make(romv, rom_proc_h, alignment), rom_key_t());
            }
            else
                rom_proc.set_alloc(romv, rom_many_ht::pool_make(romv, rom_proc_h, alignment), rom_key_t());
        });
    }

    // OK! All the MANYs and ONCEs have been created.

    assert(compiler_phase() == PHASE_PREPARE_ALLOC_ROM);
    set_compiler_phase(PHASE_ALLOC_ROM);

    ////////////////////////
    // Initialize bitsets //
    ////////////////////////

    unsigned const num_onces = rom_once_ht::pool().size();
    unsigned const num_manys = rom_many_ht::pool().size();

    once_bs_size = std::max<unsigned>(1, bitset_size<>(num_onces));
    many_bs_size = std::max<unsigned>(1, bitset_size<>(num_manys));

    // Alloc 'required_manys'
    assert(once_bs_size);
    assert(many_bs_size);
    assert(required_many_bitsets.empty());
    passert(num_onces == rom_once_ht::handles().size(), num_onces, rom_once_ht::handles().size());

    required_many_bitsets.resize(num_onces * many_bs_size);
    for(rom_once_ht once : rom_once_ht::handles())
    {
        passert(once.id < num_onces, once.id, num_onces);
        once->required_manys = &required_many_bitsets[once.id * many_bs_size];
    }

    //////////////////////////////////////////////
    // Calculate group data and 'related_onces' //
    //////////////////////////////////////////////

    unsigned const num_group_data = group_data_ht::pool().size();
    group_data_once_bitsets.resize(num_group_data * once_bs_size);
    group_data_many_bitsets.resize(num_group_data * many_bs_size);

    auto const gd_once_bs = [this](unsigned i) { return &group_data_once_bitsets[i * once_bs_size]; };
    auto const gd_many_bs = [this](unsigned i) { return &group_data_many_bitsets[i * many_bs_size]; };

    for(rom_array_ht ra : rom_array_ht::handles())
    {
        auto const& rom_array = *ra;

        if(rom_array.used_in_group_data().all_clear())
            continue;

        if(rom_array.get_alloc(ROMV_MODE).rclass() == ROMA_ONCE)
        {
            unsigned const once_i = rom_array.get_alloc(ROMV_MODE).handle();
            assert(once_i < num_onces);
            rom_array.used_in_group_data().for_each([&](unsigned gd)
            {
                bitset_set(gd_once_bs(gd), once_i);
                // Set the pointer to 'related_onces' now:
                rom_once_ht{once_i}->related_onces = gd_once_bs(gd);
            });
        }
        else if(rom_array.get_alloc(ROMV_MODE).rclass() == ROMA_MANY)
        {
            unsigned const many_i = rom_array.get_alloc(ROMV_MODE).handle();
            passert(many_i < num_manys, many_i, num_manys);
            rom_array.used_in_group_data().for_each([&](unsigned gd)
            {
                bitset_set(gd_many_bs(gd), many_i);
            });
        }
    }

    ///////////////////////////
    // Link 'required_manys' //
    ///////////////////////////

    bitset_t use_many(many_bs_size);
    bitset_t use_once(once_bs_size);

    for(rom_proc_ht rom_proc_h : rom_proc_ht::handles())
    {
        rom_proc_t& rom_proc = *rom_proc_h;

        if(!rom_proc.emits())
        {
            dprint(log, "--SKIPPING (no emit)", rom_proc_h);
            continue;
        }

        // Build the 'use_many' and 'use_once' bitsets for this function.

        use_many.clear_all();
        use_once.clear_all();

        rom_proc.for_each_group_test([&](group_ht group_h) -> bool
        {
            if(group_data_ht h = group_h->data_handle())
            {
                bitset_or(once_bs_size, use_once.data(), gd_once_bs(h.id));
                bitset_or(many_bs_size, use_many.data(), gd_many_bs(h.id));
            }
            return true;
        });

        for(rom_array_ht use : rom_proc_directly_uses[rom_proc_h.id])
        {
            auto const alloc = use->get_alloc(ROMV_MODE);
            dprint(log, "ALLOC_ROM_USES", rom_proc_h, use);

            if(alloc.rclass() == ROMA_ONCE)
            {
                dprint(log, "-ALLOC_ROM_USES ONCE");
                use_once.set(alloc.handle());
            }
            else if(alloc.rclass() == ROMA_MANY)
            {
                dprint(log, "-ALLOC_ROM_USES MANY");
                use_many.set(alloc.handle());
            }
        }

        // OK! The bitsets are built.

        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(!rom_proc.get_alloc(romv_t(romv)))
                continue;

            if(rom_proc.get_alloc(romv_t(romv)).rclass() == ROMA_MANY)
            {
                assert(!use_once.all_clear());

                // Add our own allocation to 'use_many':
                use_many.set(rom_proc.get_alloc(romv_t(romv)).handle());

                // For each ONCE we use, make all MANYs we've built requirements
                use_once.for_each([&](unsigned once_i)
                {
                    assert(rom_once_ht{once_i}->required_manys);
                    bitset_or(many_bs_size, rom_once_ht{once_i}->required_manys, use_many.data());
                    assert(bitset_test(rom_once_ht{once_i}->required_manys, rom_proc.get_alloc(romv_t(romv)).handle()));
                });
            }
            else if(rom_proc.get_alloc(romv_t(romv)).rclass() == ROMA_ONCE)
            {
                passert(use_once.all_clear(), rom_proc.asm_proc().fn->global.name);

                // Set our own requirements to include the many set we built
                unsigned const once_i = rom_proc.get_alloc(romv_t(romv)).handle();
                bitset_or(many_bs_size, rom_once_ht{once_i}->required_manys, use_many.data());
            }
        }
    }

    ////////////////
    // Init banks //
    ////////////////

    // Copy 'allocator' to fill banks.
    banks.clear();
    for(unsigned i = 0; i < num_switched_banks; ++i)
        banks.emplace_back(mapper().fixed_16k ? span_allocator_t(switched_span) : allocator, 
                           many_bs_size, once_bs_size);

    bank_ranks.resize(num_switched_banks);

    struct once_rank_t
    {
        float score;
        rom_once_ht once;

        constexpr auto operator<=>(once_rank_t const&) const = default;
    };

    //////////////////////////////
    // Allocate ONCEs and MANYs //
    //////////////////////////////

    // Order 'oncesbathing suit'
    std::vector<once_rank_t> ordered_onces;
    ordered_onces.reserve(rom_once_ht::pool().size());
    for(unsigned i = 0; i < rom_once_ht::pool().size(); ++i)
        ordered_onces.push_back({ once_rank(*rom_once_ht{i}), {i} });
    std::sort(ordered_onces.begin(), ordered_onces.end(), std::greater<>{});

    // Allocate onces (this also allocates their required_manys)
    for(once_rank_t const& rank : ordered_onces)
        alloc(rank.once);
}

float rom_allocator_t::once_rank(rom_once_t const& once)
{
    int many_size = 0;
    bitset_for_each(many_bs_size, once.required_manys, [&](unsigned i)
    {
        rom_many_t const& many = *rom_many_ht{i};
        many_size += many.max_size();
    });

    int related = 0;
    if(once.related_onces)
        related = bitset_popcount(once_bs_size, once.related_onces);

    return many_size + once.max_size() * 4 + related * 2;
}

float rom_allocator_t::bank_rank(rom_bank_t const& bank, rom_once_t const& once)
{
    // Count how much we have to allocate for required_manys
    bitset_uint_t* const unallocated_manys = ALLOCA_T(bitset_uint_t, many_bs_size);
    bitset_copy(many_bs_size, unallocated_manys, once.required_manys);
    bitset_difference(many_bs_size, unallocated_manys, bank.allocated_manys.data());

    int unallocated_many_size = 0;
    bitset_for_each(many_bs_size, unallocated_manys, [&](unsigned i)
    {
        rom_many_t const& many = *rom_many_ht{ i };
        unallocated_many_size += many.max_size();
    });

    // Count related / unrelated onces
    int related = 0;
    int unrelated = 0;
    if(once.related_onces)
    {
        bitset_uint_t* const onces = ALLOCA_T(bitset_uint_t, once_bs_size);
        bitset_copy(once_bs_size, onces, once.related_onces);
        bitset_and(once_bs_size, onces, bank.allocated_onces.data());
        related = bitset_popcount(once_bs_size, onces);
        bitset_xor(once_bs_size, onces, bank.allocated_onces.data());
        unrelated = bitset_popcount(once_bs_size, onces);
    }

    float const m = 4 * (bank.allocator.bytes_free());// - once.max_size());
    float const r = bank.allocator.initial_bytes_free() * std::sqrt((float)bank.allocator.spans_free());
    // Old formula:
    //return -(unallocated_many_size) + related - (unrelated * 0.125f) + (bank.allocator.bytes_free() / r);
    return -(std::sqrt(unallocated_many_size) * 0.0625f) + (related) - (unrelated * 0.125f) + (m / r);
}

void rom_allocator_t::rank_banks_for(rom_once_t const& once)
{
    assert(bank_ranks.size() == banks.size());

    for(unsigned i = 0; i < banks.size(); ++i)
        bank_ranks[i] = { bank_rank(banks[i], once), i };

    std::sort(bank_ranks.begin(), bank_ranks.end(), std::greater<>{});
}

void rom_allocator_t::alloc(rom_once_ht once_h)
{
    rom_once_t& once = *once_h;
    bc::small_vector<rom_many_ht, 32> realloced_manys;

    rank_banks_for(once); // Builds 'bank_ranks'

    for(bank_rank_t const& r : bank_ranks)
    {
        unsigned const bank_i = r.bank_index;
        rom_bank_t& bank = banks[bank_i];

        // 1. try to allocate manys required by 'once'
        // 2. then try to allocate 'once'

        realloced_manys.clear();

        bitset_for_each_test(many_bs_size, once.required_manys, [&](unsigned i)
        {
            rom_many_ht many_h = rom_many_ht{ i };
            rom_many_t& many = *many_h;

            return true;
        });

        // TODO: Sort the manys first, instead of iterating bitset.
        bool const allocated_manys = 
        bitset_for_each_test(many_bs_size, once.required_manys, [&](unsigned i)
        {
            rom_many_ht many_h = rom_many_ht{ i };
            rom_many_t& many = *many_h;

            if(many.in_banks.test(bank_i))
                return true;

            if(try_include_many(many_h, bank_i))
            {
                realloced_manys.push_back(many_h);
                return true;
            }

            bank_bitset_t in_banks = many.in_banks;
            in_banks.set(bank_i);
            if(realloc_many(many_h, std::move(in_banks)))
            {
                realloced_manys.push_back(many_h);
                return true;
            }

            return false;
        });
        
        // If we succeeded in allocating manys, try to allocate 'once's span:
        // (conditional has side effect assignment)
        if(!allocated_manys || !(once.span = bank.allocator.alloc(once.max_size(), once.desired_alignment).object))
        {
            // If we fail, free allocated 'many' memory.
            for(rom_many_ht many_h : realloced_manys)
                free_many(many_h, bank_i);
            continue;
        }

        // If we succeed, update and we're done
        once.bank = bank_i;
        bank.allocated_onces.set(once_h.id);
        passert(once.span.addr % once.desired_alignment == 0, once.span.addr, once.desired_alignment);

#ifndef NDEBUG
        bool const did_manys  = 
        bitset_for_each_test(many_bs_size, once.required_manys, [&](unsigned i)
        {
            rom_many_ht many_h = rom_many_ht{ i };
            rom_many_t& many = *many_h;
            return many.span == span_t{0,1} || many.in_banks.test(bank_i);
        });
        assert(did_manys);
#endif
        return;
    }

    throw std::runtime_error(fmt("Unable to allocate address of size % (out of ROM space).", once.max_size()));
}

bool rom_allocator_t::try_include_many(rom_many_ht many_h, unsigned bank_i)
{
    rom_many_t& many = *many_h;

    assert(!many.in_banks.test(bank_i)); // Handle prior.

    if(many.span == span_t{ .addr = 0, .size = 1 })
        return true;

    if(!many.span)
        return false;

    span_allocation_t const allocated_spans = banks[bank_i].allocator.alloc_at(many.span);
    if(!allocated_spans)
        return false;

    auto result = banks[bank_i].many_spans.insert({ many_h, allocated_spans.allocation });
    assert(result.second);

    many.in_banks.set(bank_i);
    banks[bank_i].allocated_manys.set(many_h.id);

    return true;
}

void rom_allocator_t::free_many(rom_many_ht many_h, unsigned bank_i)
{
    rom_many_t& many = *many_h;
    rom_bank_t& bank = banks[bank_i];

    if(many.max_size() == 0)
    {
        many.in_banks.clear(bank_i);
        return;
    }

    assert(many.in_banks.test(bank_i));

    // We have to free the span returned by the allocator,
    // NOT the span we stored in many, which is smaller.
    span_t const* allocated_span = bank.many_spans.mapped(many_h);

    passert(allocated_span, many.span, bank_i);
    assert(*allocated_span);
    assert(allocated_span->contains(many.span));

    bank.allocator.free(*allocated_span);
    bank.allocated_manys.clear(many_h.id);
    bank.many_spans.remove(many_h);

    assert(!bank.many_spans.mapped(many_h));

    many.in_banks.clear(bank_i);
}

bool rom_allocator_t::realloc_many(rom_many_ht many_h, bank_bitset_t in_banks)
{
    rom_many_t& many = *many_h;

    assert((many.in_banks & in_banks) == many.in_banks);

    if(many.max_size() == 0)
    {
        many.span = { .addr = 0, .size = 1 };
        many.in_banks = std::move(in_banks);
        return true;
    }

    // Build bitset 'free', which tracks which spans are free in every banks required.
    span_allocator_t::bitset_t free = {};
    in_banks.for_each([&](unsigned bank_i){ free |= banks[bank_i].allocator.allocated_bitset(); });
    bitset_flip_all(free.size(), free.data());
    unsigned const bpb = span_allocator_t::bytes_per_bit(switched_span);
    bitset_mark_consecutive(free.size(), free.data(), (many.max_size() + bpb - 1) / bpb);

    int const highest_bit = bitset_highest_bit_set(free.size(), free.data());
    if(highest_bit < 0)
        return false;

    // OK! We can allocate at 'free_addr', but we can do better.
    // We'll find the highest possible address to allocate at.

    unsigned const free_addr = highest_bit * span_allocator_t::bytes_per_bit(switched_span) + switched_span.addr;
    unsigned max_start = 0;
    unsigned min_end = ~0u;

    in_banks.for_each([&](unsigned bank_i)
    {
        span_t const span = banks[bank_i].allocator.unallocated_span_at(free_addr);
        assert(span);
        assert(span.size >= many.max_size());
        max_start = std::max<unsigned>(max_start, span.addr);
        min_end = std::min<unsigned>(min_end, span.end());
    });
    assert(min_end != ~0u);

    // Here's where our many will be stored:
    span_t const range = { max_start, min_end - max_start };
    span_t alloc_at;

    if(!(alloc_at = aligned(range, many.max_size(), many.desired_alignment)))
        return false;

    if(many.span)
    {
        // If we're already allocated, free our memory first.
        // NOTE: Ideally, we would do this much earlier.
        assert(!many.in_banks.all_clear());
        many.in_banks.for_each([&](unsigned bank_i){ free_many(many_h, bank_i); });
    }

    // Now allocate in each bank:
    in_banks.for_each([&](unsigned bank_i)
    {
        span_allocation_t const allocated_spans = banks[bank_i].allocator.alloc_at(alloc_at);

        passert(allocated_spans, alloc_at);
        assert(allocated_spans.allocation.contains(alloc_at));

        auto result = banks[bank_i].many_spans.insert({ many_h, allocated_spans.allocation });
        banks[bank_i].allocated_manys.set(many_h.id);
        assert(result.second);
    });

    // And store it in the many:
    assert(!in_banks.all_clear());
    many.span = alloc_at;
    assert(many.span.addr % many.desired_alignment == 0);
    many.in_banks = std::move(in_banks);

    return true;
}
    
void alloc_rom(log_t* log, span_allocator_t allocator)
{
    rom_allocator_t alloc(log, allocator);
}

void print_rom(std::ostream& o)
{
    std::vector<std::pair<unsigned, span_t>> sorted;

    o << "ROM:\n\n";

    for(auto const& st : rom_static_ht::values())
    {
        o << "STATIC " << st.span << '\n';
        if(mapper().fixed_16k)
            sorted.emplace_back(st.span.size, st.span);
        else
            sorted.emplace_back(st.span.size * mapper().num_banks, st.span);
    }

    for(auto const& many : rom_many_ht::values())
    {
        o << "MANY " << many.span << ' ' << (int)many.data.rclass() << " banks: ";
        many.in_banks.for_each([&](unsigned bank_i) { o << ' ' << bank_i; });
        o << std::endl;
        sorted.emplace_back(many.span.size * many.in_banks.popcount() , many.span);
    }
    for(auto const& once : rom_once_ht::values())
    {
        o << "ONCE " << once.span << ' ' << (int)once.data.rclass() << " banks: " << once.bank << std::endl;
        sorted.emplace_back(once.span.size, once.span);
    }

    std::sort(sorted.begin(), sorted.end(), std::greater<>());
    for(auto const& pair : sorted)
        o << "SIZE: " << pair.first << " SPAN: " << pair.second << '\n';

    for(fn_t const& fn : fn_ht::values())
    {
        o << "\n\n" << fn.global.name << " / " << fn.rom_proc() << ": \n";
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(auto a = fn.rom_proc()->get_alloc(romv_t(romv)))
            {
                o << romv << ' ' << a.get()->span << " (" << fn.rom_proc()->asm_proc().size() << ')' <<  std::endl;
                o << "banks:";
                a.for_each_bank([&](unsigned bank)
                {
                    o << ' ' << bank;
                });
                o << std::endl;
                fn.rom_proc()->asm_proc().write_assembly(o, romv_t(romv));
            }
            else
                o << romv << " PRUNED\n";
        }
    }

    for(const_t const& c : const_ht::values())
    {
        if(!c.rom_array())
            continue;

        o << "\n\n" << c.global.name << " / " << c.rom_array() << ": " << "\n";
        if(group_ht group = c.group())
            o << "group: " << group->name << '\n';
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(!c.rom_array())
                continue;
            o << "omni: " << c.rom_array()->omni() << '\n';
            if(auto a = c.rom_array()->get_alloc(romv_t(romv)))
            {
                o << romv << ' ' << a.get()->span << ' ' << a.first_bank() << std::endl;
                o << "banks:";
                a.for_each_bank([&](unsigned bank)
                {
                    o << ' ' << bank;
                });
                o << std::endl;
                for(auto const& d : c.rom_array()->data())
                    o << d << std::endl;
            }
            else
                o << romv << " PRUNED\n";
        }

    }

    for(group_t* g : group_vars_ht::values())
    {
        if(!g->vars()->init_proc())
            continue;
        o << "\n\n" << g->name << " / " << g->vars()->init_proc() << ": \n";
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(auto a = g->vars()->init_proc()->get_alloc(romv_t(romv)))
            {
                o << romv << ' ' << a.get()->span << ' ' << a.first_bank() << std::endl;
                g->vars()->init_proc()->asm_proc().write_assembly(o, romv_t(romv));
            }
            else
                o << romv << " PRUNED\n";
        }
    }
}

