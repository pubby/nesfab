#include "rom_alloc.hpp"

#include <cmath>
#include <vector>
#include <iostream> // TODO

#include "rom.hpp"
#include "handle.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "eternal_new.hpp"
#include "span_allocator.hpp"
#include "debug_print.hpp"

class rom_allocator_t
{
public:
    rom_allocator_t(log_t* log, span_allocator_t& allocator, unsigned num_banks);

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

    span_t const initial_span = {};

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

    // Allocate a DPCM span
    span_t alloc_dpcm(unsigned size);
};

rom_allocator_t::rom_allocator_t(log_t* log, span_allocator_t& allocator, unsigned num_banks)
: initial_span(allocator.initial())
, log(log)
{
    /////////////////////////////////////////////////
    // Track which procs directly use which arrays //
    /////////////////////////////////////////////////

    std::vector<rh::batman_set<rom_proc_ht>> rom_array_used_by(rom_array_ht::pool().size());
    std::vector<rh::batman_set<rom_array_ht>> rom_proc_directly_uses(rom_proc_ht::pool().size());

    auto const try_insert = [&](rom_proc_ht proc, locator_t loc)
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
    };

    for(rom_proc_ht proc : rom_proc_ht::handles())
    {
        for(asm_inst_t const& inst : proc->asm_proc().code)
        {
            try_insert(proc, inst.arg);
            try_insert(proc, inst.alt);
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

        bool once = true;

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
                    summed_size += rom_proc->max_size();
                    ++proc_count;
                }
            }

            if(proc_count == 0) // TODO
            {
                dprint(log, "--SKIPPING (UNUSED BY PROCS)", rom_array_h);
                continue;
            }

            once = (float(summed_size) / float(proc_count)) < float(rom_array.data().size());
        }
        else
        {
            // Since this is associated with group(s), we'll use
            // the group defs to determine once/many.

            // NOTE: Not locking mutex, as we're in single thread.
            rom_array.used_in_group_data().for_each([&](unsigned bit)
            {
                once &= group_data_ht{bit}->once;
            });
        }

        if(rom_array.get_alloc(ROMV_MODE))
        {
            dprint(log, "--SKIPPING (already allocated)", rom_array_h);
            continue;
        }

        if(rom_array.dpcm())
        {
            span_t const span = allocator.alloc_linear(rom_array.data().size(), 64, 0xC000);
            if(!span)
                throw std::runtime_error("Unable to allocate DPCM address (out of ROM space).");
            rom_array.set_alloc(ROMV_MODE, rom_static_ht::pool_make(ROMV_MODE, span, rom_array_h), rom_key_t());
            continue;
        }

        unsigned alignment = 1;
        if(rom_array.align())
            alignment = std::min<unsigned>(256, std::bit_ceil(rom_array.data().size()));

        dprint(log, "--PREPPED", rom_array_h);
        if(once)
            rom_array.set_alloc(ROMV_MODE, rom_once_ht::pool_make(ROMV_MODE, rom_array_h, alignment), rom_key_t());
        else
            rom_array.set_alloc(ROMV_MODE, rom_many_ht::pool_make(ROMV_MODE, rom_array_h, alignment), rom_key_t());
        assert(rom_array.get_alloc(ROMV_MODE));
        assert(rom_array.get_alloc(ROMV_MODE).rclass());
    }

    ///////////////////////////
    // Convert 'rom_proc_t's //
    ///////////////////////////

    for(rom_proc_ht rom_proc_h : rom_proc_ht::handles())
    {
        dprint(log, "-PREP_ALLOC_ROM_PROC", rom_proc_h);
        rom_proc_t& rom_proc = *rom_proc_h;

        if(!rom_proc.emits())
        {
            dprint(log, "--SKIPPING (no emit)", rom_proc_h);
            continue;
        }

        // Check if the proc uses any 'ONCE'.
        // If it does, the proc has to be MANY, otherwise its ONCE.

        unsigned const alignment = rom_proc.align() ? 256 : 1;
        bitset_t const* groups;
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

                if(group.gclass() != GROUP_DATA)
                    return true;

                group_data_t const& gd = group.impl<group_data_t>();
                for(const_ht c : gd.consts())
                    if(c->rom_array()->get_alloc(ROMV_MODE).rclass() == ROMA_ONCE)
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
                rom_proc.set_alloc(romv, rom_once_ht::pool_make(romv, rom_proc_h, alignment), rom_key_t());
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

    // TODO: replace
    many_bs_size = bitset_size<>(num_manys);
    once_bs_size = bitset_size<>(num_onces);

    // Alloc 'required_manys'
    required_many_bitsets.resize(num_onces * many_bs_size);
    for(rom_once_ht once : rom_once_ht::handles())
        once->required_manys = &required_many_bitsets[once.id * many_bs_size];

    //////////////////////////////////////////////
    // Calculate group data and 'related_onces' //
    //////////////////////////////////////////////

    unsigned const num_group_data = group_data_ht::pool().size();
    group_data_once_bitsets.resize(num_group_data * once_bs_size);
    group_data_many_bitsets.resize(num_group_data * many_bs_size);

    auto const gd_once_bs = [this](unsigned i) { return &group_data_once_bitsets[i * once_bs_size]; };
    auto const gd_many_bs = [this](unsigned i) { return &group_data_many_bitsets[i * many_bs_size]; };

    for(group_data_ht gd : group_data_ht::handles())
    for(const_ht c : gd->consts())
    {
        if(!c->rom_array())
            continue;

        auto const& rom_array = *c->rom_array();

        if(rom_array.get_alloc(ROMV_MODE).rclass() == ROMA_ONCE)
        {
            unsigned const once_i = rom_array.get_alloc(ROMV_MODE).handle();
            assert(once_i < num_onces);
            bitset_set(gd_once_bs(gd.id), once_i);
            // Set the pointer to 'related_onces' now:
            rom_once_ht{once_i}->related_onces = gd_once_bs(gd.id);
        }
        else if(rom_array.get_alloc(ROMV_MODE).rclass() == ROMA_MANY)
        {
            unsigned const many_i = rom_array.get_alloc(ROMV_MODE).handle();
            passert(many_i < num_manys, many_i, num_manys);
            bitset_set(gd_many_bs(gd.id), many_i);
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

        // Build the 'use_many' and 'use_once' bitsets for this function.

        use_many.clear_all();
        use_once.clear_all();

        rom_proc.for_each_group_test([&](group_ht group_h) -> bool
        {
            if(group_h->gclass() != GROUP_DATA)
                return true;

            bitset_or(once_bs_size, use_once.data(), gd_once_bs(group_h->impl_id()));
            bitset_or(many_bs_size, use_many.data(), gd_many_bs(group_h->impl_id()));
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
                    bitset_or(many_bs_size, rom_once_ht{once_i}->required_manys, use_many.data());
                });
            }
            else if(rom_proc.get_alloc(romv_t(romv)).rclass() == ROMA_ONCE)
            {
                assert(use_once.all_clear());

                // Set our own requirements to include the many set we built
                unsigned const once_i = rom_proc.get_alloc(romv_t(romv)).handle();
                bitset_or(many_bs_size, rom_once_ht{once_i}->required_manys, use_many.data());
            }
        }
    }

    //////////////////////
    // Allocate statics //
    //////////////////////

    /* TODO: remove
    // Allocate static_addrs
    for(rom_static_t& sa : rom_vector<rom_static_t>)
    {
        if(sa.span)
        {
            if(!allocator.alloc_at(sa.span))
                throw std::runtime_error(fmt("Unable to allocate static address % - % (out of ROM space).", 
                                             sa.span.addr, sa.span.end() - 1));
        }
        else if(!(sa.span = allocator.alloc(sa.desired_size, sa.desired_alignment)))
            throw std::runtime_error("Unable to allocate static address (out of ROM space).");
    }
    */

    ////////////////
    // Init banks //
    ////////////////

    // Copy 'allocator' to fill banks.
    banks.clear();
    for(unsigned i = 0; i < num_banks; ++i)
        banks.emplace_back(allocator, many_bs_size, once_bs_size);
    bank_ranks.resize(num_banks);

    struct once_rank_t
    {
        float score;
        rom_once_ht once;

        constexpr auto operator<=>(once_rank_t const&) const = default;
    };

    //////////////////////////////
    // Allocate ONCEs and MANYs //
    //////////////////////////////

    // Order 'onces'
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
        many_size += many.data.max_size();
    });

    int related = 0;
    if(once.related_onces)
        related = bitset_popcount(once_bs_size, once.related_onces);

    return many_size + once.data.max_size() + related;
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
        unallocated_many_size += many.data.max_size();
    });

    // Count related / unrelated onces
    int related = 0;
    int unrelated = 0;
    if(once.related_onces)
    {
        //std::puts("RELATED ONCES");
        bitset_uint_t* const onces = ALLOCA_T(bitset_uint_t, once_bs_size);
        bitset_copy(once_bs_size, onces, once.related_onces);
        bitset_and(once_bs_size, onces, bank.allocated_onces.data());
        related = bitset_popcount(once_bs_size, onces);
        bitset_xor(once_bs_size, onces, bank.allocated_onces.data());
        unrelated = bitset_popcount(once_bs_size, onces);
    }

    //std::printf("related %i %i\n", related, unrelated);

    float const r = bank.allocator.initial_bytes_free() * std::sqrt((float)bank.allocator.spans_free());
    //std::printf("bank rank %i %i %f\n", unallocated_many_size, related, (bank.allocator.bytes_free() / r));
    return -unallocated_many_size + related - (unrelated * 0.125f) + (bank.allocator.bytes_free() / r);
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

            many.in_banks.set(bank_i);
            if(realloc_many(many_h, many.in_banks))
            {
                realloced_manys.push_back(many_h);
                return true;
            }
            many.in_banks.clear(bank_i);

            return false;
        });
        
        // If we succeeded in allocating manys, try to allocate 'once's span:
        // (conditional has side effect assignment)
        if(!allocated_manys || !(once.span = bank.allocator.alloc(once.data.max_size(), once.desired_alignment)))
        {
            // If we fail, free allocated 'many' memory.
            for(rom_many_ht many_h : realloced_manys)
                free_many(many_h, bank_i);
            continue;
        }

        // If we succeed, update and we're done
        once.bank = bank_i;
        bank.allocated_onces.set(once_h.id);
        return;
    }

    throw std::runtime_error("Unable to allocate address (out of ROM space).");
}

bool rom_allocator_t::try_include_many(rom_many_ht many_h, unsigned bank_i)
{
    rom_many_t& many = *many_h;

    assert(!many.in_banks.test(bank_i)); // Handle prior.

    if(!many.span)
        return false;

    span_t const allocated_span = banks[bank_i].allocator.alloc_at(many.span);
    if(!allocated_span)
        return false;

    auto result = banks[bank_i].many_spans.insert({ many_h, allocated_span });
    assert(result.second);

    many.in_banks.set(bank_i);
    banks[bank_i].allocated_manys.set(many_h.id);

    return true;
}

void rom_allocator_t::free_many(rom_many_ht many_h, unsigned bank_i)
{
    rom_many_t& many = *many_h;
    rom_bank_t& bank = banks[bank_i];

    assert(many.in_banks.test(bank_i));

    // We have to free the span returned by the allocator,
    // NOT the span we stored in many, which is smaller.
    span_t const* allocated_span = bank.many_spans.mapped(many_h);

    assert(allocated_span);
    assert(*allocated_span);
    assert(allocated_span->contains(many.span));

    bank.allocator.free(*allocated_span);
    bank.allocated_manys.clear(many_h.id);

    many.in_banks.clear(bank_i);
}

bool rom_allocator_t::realloc_many(rom_many_ht many_h, bank_bitset_t in_banks)
{
    rom_many_t& many = *many_h;
    
    if(many.data.max_size() == 0)
    {
        many.span = { .addr = 0, .size = 1 };
        return true;
    }

    if(many.span)
    {
        // If we're already allocated, free our memory first
        assert(!many.in_banks.all_clear());
        many.in_banks.for_each([&](unsigned bank_i){ free_many(many_h, bank_i); });
        many.span = {};
    }

    // Build bitset 'free', which tracks which spans are free in every banks required.
    span_allocator_t::bitset_t free = {};
    in_banks.for_each([&](unsigned bank_i){ free |= banks[bank_i].allocator.allocated_bitset(); });
    bitset_flip_all(free.size(), free.data());
    unsigned const bpb = span_allocator_t::bytes_per_bit(initial_span);
    bitset_mark_consecutive(free.size(), free.data(), (many.data.max_size() + bpb - 1) / bpb);

    int const highest_bit = bitset_highest_bit_set(free.size(), free.data());
    if(highest_bit < 0)
        return false;

    // OK! We can allocate at 'free_addr', but we can do better.
    // We'll find the highest possible address to allocate at.

    unsigned const free_addr = highest_bit * span_allocator_t::bytes_per_bit(initial_span) + initial_span.addr;
    unsigned max_start = 0;
    unsigned min_end = ~0u;

    in_banks.for_each([&](unsigned bank_i)
    {
        span_t const span = banks[bank_i].allocator.unallocated_span_at(free_addr);
        assert(span);
        assert(span.size >= many.data.max_size());
        max_start = std::max<unsigned>(max_start, span.addr);
        min_end = std::min<unsigned>(min_end, span.end());
    });
    assert(min_end != ~0u);

    // Here's where our many will be stored:
    // TODO
    //unsigned const store_addr = std::max<unsigned>(max_start, min_end - many.data.max_size());
    span_t const range = { max_start, min_end - max_start };
    span_t alloc_at;

    if(!(alloc_at = aligned(range, many.data.max_size(), many.desired_alignment)))
    {
        std::cout << "PANIC " << range << many.data.max_size() << ' ' << many.desired_alignment << std::endl;
        return false;
    }

    // Now allocate in each bank:
    in_banks.for_each([&](unsigned bank_i)
    {
        span_t const allocated_span = banks[bank_i].allocator.alloc_at(alloc_at);

        passert(allocated_span, alloc_at);
        assert(allocated_span.contains(alloc_at));

        auto result = banks[bank_i].many_spans.insert({ many_h, allocated_span });
        banks[bank_i].allocated_manys.set(many_h.id);
        assert(result.second);
    });

    // And store it in the many:
    many.span = alloc_at;
    many.in_banks = std::move(in_banks);

    return true;
}
    
void alloc_rom(log_t* log, span_allocator_t allocator, unsigned num_banks)
{
    rom_allocator_t alloc(log, allocator, num_banks);
}

void print_rom(std::ostream& o)
{
    o << "ROM:\n\n";

    for(auto const& st : rom_static_ht::values())
        o << "STATIC " << st.span << '\n';
    for(auto const& many : rom_many_ht::values())
    {
        o << "MANY " << many.span << ' ' << many.data.rclass();
        many.in_banks.for_each([&](unsigned bank_i) { o << ' ' << bank_i; });
        o << std::endl;
    }
    for(auto const& once : rom_once_ht::values())
        o << "ONCE " << once.span << ' ' << once.bank << ' ' << once.data.rclass() << std::endl;

    for(fn_t const& fn : fn_ht::values())
    {
        o << "\n\n" << fn.global.name << ": \n";
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(auto a = fn.rom_proc()->get_alloc(romv_t(romv)))
            {
                o << romv << ' ' << a.get()->span << std::endl;
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

        o << "\n\n" << c.global.name << " / " << c.rom_array() << ": \n";
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(!c.rom_array())
                continue;
            if(auto a = c.rom_array()->get_alloc(romv_t(romv)))
            {
                o << romv << ' ' << a.get()->span << ' ' << a.first_bank() << std::endl;
                for(auto const& d : c.rom_array()->data())
                    o << d << std::endl;
            }
            else
                o << romv << " PRUNED\n";
        }

    }

    for(group_vars_t const& gv : group_vars_ht::values())
    {
        if(!gv.init_proc())
            continue;
        o << "\n\n" << gv.group.name << ": \n";
        for(unsigned romv = 0; romv < NUM_ROMV; ++romv)
        {
            if(auto a = gv.init_proc()->get_alloc(romv_t(romv)))
            {
                o << romv << ' ' << a.get()->span << ' ' << a.first_bank() << std::endl;
                gv.init_proc()->asm_proc().write_assembly(o, romv_t(romv));
            }
            else
                o << romv << " PRUNED\n";
        }
    }
}
