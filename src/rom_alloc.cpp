#include "rom_alloc.hpp"

#include <cmath>
#include <vector>

#include "rom.hpp"
#include "handle.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "rom_array.hpp"
#include "eternal_new.hpp"

namespace // anonymous
{ 

class rom_allocator_t
{
public:
    rom_allocator_t(span_allocator_t& allocator, unsigned num_banks);

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
    
    template<typename Data>
    rom_once_ht make_once(Data&& data, unsigned alignment)
    {
        rom_once_ht const ret = { rom_vector<rom_once_t>.size() };

        rom_once_t once = {};
        once.desired_alignment = alignment;
        once.desired_size = rom_data_size(data);
        once.data = std::forward<Data>(data);

        rom_vector<rom_once_t>.push_back(std::move(once));
        return ret;
    }

    template<typename Data>
    rom_many_ht make_many(Data&& data, unsigned alignment)
    {
        rom_many_ht const ret = { rom_vector<rom_many_t>.size() };

        rom_many_t many = {};
        many.desired_alignment = alignment;
        many.desired_size = rom_data_size(data);
        many.data = std::forward<Data>(data);

        rom_vector<rom_many_t>.push_back(std::move(many));
        return ret;
    }
};

rom_allocator_t::rom_allocator_t(span_allocator_t& allocator, unsigned num_banks)
: initial_span(allocator.initial())
{
    // TODO: pre-allocate memory

    // This maps from fns to rom_array allocations.
    std::vector<std::vector<rom_alloc_ht>> fn_directly_uses;
    fn_directly_uses.resize(impl_deque<fn_t>.size());

    //////////////////////////
    // Convert 'rom_array's //
    //////////////////////////

    for(auto it = rom_array_map.begin(); it != rom_array_map.end(); ++it)
    {
        auto& pair = *it;
        rom_array_t const& array = pair.first;
        rom_array_meta_t& meta = pair.second;

        bool once = true;

        if(meta.used_by_group_data.all_clear())
        {
            // We'll decide this being 'once' or 'many' by looking at the size
            // and comparing it to the size of functions that use it.

            std::size_t summed_fn_sizes = 0;
            std::size_t fn_count = 0;

            // NOTE: Not locking mutex, as we're in single thread.
            meta.used_by_fns.for_each([&](unsigned bit)
            {
                fn_ht fn = { bit };
                if(fn->emits_code())
                {
                    summed_fn_sizes += fn->proc().size();
                    ++fn_count;
                }
            });

            if(fn_count == 0)
                continue;

            once = summed_fn_sizes / fn_count < array.data.size();
        }
        else
        {
            // Since this is associated with group(s), we'll use
            // the group defs to determine once/many.

            // NOTE: Not locking mutex, as we're in single thread.
            meta.used_by_group_data.for_each([&](unsigned bit)
            {
                group_data_ht group_data = { bit };
                once &= group_data->once;
            });
        }

        unsigned const alignment = 0; // TODO

        if(once)
        {
            rom_once_ht const once = make_once(&array.data, alignment);
            meta.alloc.assign(once);

            meta.used_by_fns.for_each([&](unsigned fn)
            { 
                assert(fn < fn_directly_uses.size());
                fn_directly_uses[fn].push_back({ ROM_ONCE, once.value }); 
            });
        }
        else
        {
            rom_many_ht const many = make_many(&array.data, alignment);
            meta.alloc.assign(many);

            meta.used_by_fns.for_each([&](unsigned fn)
            { 
                assert(fn < fn_directly_uses.size());
                fn_directly_uses[fn].push_back({ ROM_MANY, many.value }); 
            });
        }
    }

    /////////////////////
    // Convert 'fn_t's //
    /////////////////////

    unsigned const num_fns = impl_deque<fn_t>.size();
    for(unsigned fn_i = 0; fn_i < num_fns; ++fn_i)
    {
        fn_t& fn = impl_deque<fn_t>[fn_i];

        if(!fn.emits_code())
            continue;

        // Check if the fn uses any 'ONCE'.
        // If it does, the fn has to be MANY, otherwise its ONCE.

        unsigned const alignment = 0; // TODO
        bool once;

        for(rom_alloc_ht const& use : fn_directly_uses[fn_i])
            if(use.rclass() == ROM_ONCE)
                goto is_many;

        once = fn.ir_ptr_groups().for_each_test([&](unsigned bit) -> bool
        {
            group_ht group_h = { bit };
            group_t const& group = *group_h;

            if(group.gclass() != GROUP_DATA)
                return true;

            group_data_t const& gd = group.impl<group_data_t>();
            for(const_ht c : gd.consts())
            {
                //std::puts("GD CONST");
                auto const& meta = get_meta(c->rom_array());
                if(meta.alloc.rclass() == ROM_ONCE)
                    return false;
            }

            return true;
        });

        if(once)
        {
            rom_once_ht const once = make_once(&fn.proc(), alignment);
            fn.assign_rom_alloc({ ROM_ONCE, once.value });
        }
        else
        {
        is_many:
            rom_many_ht const many = make_many(&fn.proc(), alignment);
            fn.assign_rom_alloc({ ROM_MANY, many.value });
        }
    }

    // OK! All the MANYs and ONCEs have been created.

    ////////////////////////
    // Initialize bitsets //
    ////////////////////////

    auto& once_vec = rom_vector<rom_once_t>;
    auto& many_vec = rom_vector<rom_many_t>;

    unsigned const num_onces = once_vec.size();
    unsigned const num_manys = many_vec.size();

    many_bs_size = bitset_size<>(num_manys);
    once_bs_size = bitset_size<>(num_onces);

    // Alloc 'required_manys'
    required_many_bitsets.resize(num_onces * many_bs_size);
    for(unsigned i = 0; i < num_onces; ++i)
        rom_vector<rom_once_t>[i].required_manys = &required_many_bitsets[i * many_bs_size];

    //////////////////////////////////////////////
    // Calculate group data and 'related_onces' //
    //////////////////////////////////////////////

    unsigned const num_group_data = impl_deque<group_data_t>.size();
    group_data_once_bitsets.resize(num_group_data * once_bs_size);
    group_data_many_bitsets.resize(num_group_data * many_bs_size);

    auto const gd_once_bs = [this](unsigned i) { return &group_data_once_bitsets[i * once_bs_size]; };
    auto const gd_many_bs = [this](unsigned i) { return &group_data_many_bitsets[i * many_bs_size]; };

    for(unsigned gd_i = 0; gd_i < num_group_data; ++gd_i)
    {
        group_data_t const& gd = impl_deque<group_data_t>[gd_i];

        for(const_ht c : gd.consts())
        {
            auto const& meta = get_meta(c->rom_array());

            if(meta.alloc.rclass() == ROM_ONCE)
            {
                unsigned const once_i = meta.alloc.handle();
                bitset_set(gd_once_bs(gd_i), once_i);
                // Set the pointer to 'related_onces' now:
                once_vec[once_i].related_onces = gd_once_bs(gd_i);
            }
            else if(meta.alloc.rclass() == ROM_MANY)
                bitset_set(gd_many_bs(gd_i), meta.alloc.handle());
        }
    }

    ///////////////////////////
    // Link 'required_manys' //
    ///////////////////////////

    bitset_t use_many(many_bs_size);
    bitset_t use_once(once_bs_size);

    for(unsigned fn_i = 0; fn_i < num_fns; ++fn_i)
    {
        fn_t const& fn = impl_deque<fn_t>[fn_i];

        if(!fn.emits_code())
            continue;

        // Build the 'use_many' and 'use_once' bitsets for this function.

        use_many.clear_all();
        use_once.clear_all();

        fn.ir_ptr_groups().for_each([&](unsigned group_i)
        {
            group_t const& group = *group_ht{ group_i };

            if(group.gclass() != GROUP_DATA)
                return;

            bitset_or(once_bs_size, use_once.data(), gd_once_bs(group.index()));
            bitset_or(many_bs_size, use_many.data(), gd_many_bs(group.index()));
        });

        for(rom_alloc_ht const& use : fn_directly_uses[fn.handle().value])
        {
            if(use.rclass() == ROM_ONCE)
                use_once.set(use.handle());
            else if(use.rclass() == ROM_MANY)
                use_many.set(use.handle());
        }

        // OK! The bitsets are built.

        if(fn.rom_alloc().rclass() == ROM_MANY)
        {
            assert(!use_once.all_clear());

            // Add our own allocation to 'use_many':
            use_many.set(fn.rom_alloc().handle());

            // For each ONCE we use, make all MANYs we've built requirements
            use_once.for_each([&](unsigned once_i)
            {
                bitset_or(many_bs_size, once_vec[once_i].required_manys, use_many.data());
            });
        }
        else if(fn.rom_alloc().rclass() == ROM_ONCE)
        {
            assert(use_once.all_clear());

            // Set our own requirements to include the many set we built
            unsigned const once_i = fn.rom_alloc().handle();
            bitset_or(many_bs_size, once_vec[once_i].required_manys, use_many.data());
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
    ordered_onces.reserve(rom_vector<rom_once_t>.size());
    for(unsigned i = 0; i < rom_vector<rom_once_t>.size(); ++i)
        ordered_onces.push_back({ once_rank(rom_vector<rom_once_t>[i]), {i} });
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
        many_size += many.desired_size;
    });

    int related = 0;
    if(once.related_onces)
        related = bitset_popcount(once_bs_size, once.related_onces);

    return many_size + once.desired_size + related;
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
        unallocated_many_size += many.desired_size;
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
        
        //std::cout << " alloc rom " << once.desired_size << std::endl;

        // If we succeeded in allocating manys, try to allocate 'once's span:
        // (conditional has side effect assignment)
        if(!allocated_manys || !(once.span = bank.allocator.alloc(once.desired_size, once.desired_alignment)))
        {
            // If we fail, free allocated 'many' memory.
            for(rom_many_ht many_h : realloced_manys)
                free_many(many_h, bank_i);
            continue;
        }

        // If we succeed, update and we're done
        once.bank = bank_i;
        bank.allocated_onces.set(once_h.value);
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
    banks[bank_i].allocated_manys.set(many_h.value);

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
    bank.allocated_manys.clear(many_h.value);

    many.in_banks.clear(bank_i);
}

bool rom_allocator_t::realloc_many(rom_many_ht many_h, bank_bitset_t in_banks)
{
    rom_many_t& many = *many_h;

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
    bitset_mark_consecutive(free.size(), free.data(), many.desired_size / span_allocator_t::bytes_per_bit(initial_span));

    int const highest_bit = bitset_highest_bit_set(free.size(), free.data());
    if(highest_bit < 0)
        return false;

    // OK! We can allocate at 'free_addr', but we can do better.
    // We'll find the highest possible address to allocate at.

    unsigned const free_addr = highest_bit * span_allocator_t::bytes_per_bit(initial_span) + initial_span.addr;
    unsigned min_end = ~0u;

    in_banks.for_each([&](unsigned bank_i)
    {
        //std::cout << "free addr " << free_addr << std::endl;
        span_t const span = banks[bank_i].allocator.unallocated_span_at(free_addr);
        //std::cout << "free span " << span << std::endl;
        assert(span.size >= many.desired_size);
        min_end = std::min<unsigned>(min_end, span.end());
    });
    assert(min_end != ~0u);

    // Here's where our many will be stored:
    span_t const alloc_at = { min_end - many.desired_size, many.desired_size };

    // Now allocate in each bank:
    in_banks.for_each([&](unsigned bank_i)
    {
        span_t const allocated_span = banks[bank_i].allocator.alloc_at(alloc_at);
        assert(allocated_span);
        assert(allocated_span.contains(alloc_at));

        auto result = banks[bank_i].many_spans.insert({ many_h, allocated_span });
        banks[bank_i].allocated_manys.set(many_h.value);
        assert(result.second);
    });

    // And store it in the many:
    many.span = alloc_at;
    many.in_banks = std::move(in_banks);

    return true;
}
    
} // end anonymous namespace

void alloc_rom(span_allocator_t allocator, unsigned num_banks)
{
    rom_allocator_t alloc(allocator, num_banks);
    /*
    for(auto const& many : rom_vector<rom_many_t>)
    {
        //std::cout << "MANY " << many.span;
        many.in_banks.for_each([](unsigned bank_i) { std::cout << ' ' << bank_i; });
        std::cout << std::endl;
    }
    for(auto const& once : rom_vector<rom_once_t>)
        std::cout << "ONCE " << once.span << ' ' << once.bank << std::endl;
        */
}
