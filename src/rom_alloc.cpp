#include "rom_alloc.hpp"

#include "span_allocator.hpp"

/*
enum rom_alloc_class_t : char
{
    ROM_ONCE,
    ROM_MANY,
    ROM_STATIC,
};

struct rom_alloc_ht
{
    rom_alloc_class_t rclass;
    unsigned value;
};

struct rom_alloc_t
{
    std::uint16_t desired_alignment;
    std::uint16_t required_size;
    span_t span;
};

struct rom_static_t : public rom_alloc_t
{
};

struct rom_many_t : public rom_alloc_t
{
};
*/

struct rom_alloc_t
{
    std::uint16_t required_size = 0;
    std::uint16_t desired_alignment = 0;
    span_t span = {};
    static_bitset_t<256> in_banks = {};
    bitset_t required_manys; // Manys in same bank
    bitset_t related_onces; // Onces ideally in same bank
};


{
    bool emit_rom(const_t const& cnst) const;
    bool emit_rom(fn_t const& fn) const;

    std::vector<rom_once_ht> const_onces;
    std::vector<rom_many_ht> const_manys;

    std::deque<rom_once_t> onces;
    std::deque<rom_many_t> manys;
}


{
    // 1. Collect all locators used, by all functions

    // 2.
}


// Call from single-thread only
{
    // Pre-allocate memory:
    {
        unsigned const upper_bound = impl_deque<fn_t>.size() + rom_array_map.size();
        alloc_vector.reserve(upper_bound);
        //once_vector.reserve(upper_bound);
    }

    // Tracks all functions that must be 'many'
    bitset_t many_fns(impl_bitset_size<fn_t>());

    struct set_t
    {
        std::deque<once_ht> onces;
        std::deque<many_ht> manys;
    };

    std::vector<set_t> fn_map(impl_deque<fn_t>.size());
    std::vector<set_t> group_data_map(impl_deque<group_data_t>.size());

    for(auto it = rom_array_map.begin(); it != rom_array_map.end(); ++it)
    {
        auto const& pair = *it;
        rom_array_t const& array = pair.first;
        rom_array_meta_t const& meta = pair.second;

        bool once = false;
        bool many = false;

        if(meta.used_by_group_data.all_clear())
        {
            std::size_t summed_fn_sizes = 0;
            std::size_t pair.second.used_by_fns.pop_count();
            // NOTE: Not locking mutex, as we're in single thread.
            meta.used_by_fns.for_each([&](unsigned bit)
            {
                fn_ht fn = { bit };
                if(emit_rom(fn))
                {
                    summed_fn_sizes += fn->proc().size_in_bytes();
                    ++fn_count;
                }
            });

            if(fn_count == 0)
                continue;

            once = summed_fn_sizes / fn_count < array.data.size();
            many = !once;
        }
        else
        {
            // NOTE: Not locking mutex, as we're in single thread.
            meta.used_by_group_data.for_each([&](unsigned bit)
            {
                group_data_ht group_data = { bit };
                once |= group_data->once;
                many |= group_data->many;
            });
        }

        if(once)
        {
            many_fns |= meta.used_by_fns;

            unsigned const alignment = 0; // TODO
            rom_alloc_ht const once = alloc_once(array.data.size(), alignment);

            meta.used_by_group_data.for_each([&](unsigned bit)
            {
                group_data_map[bit].push_back(once);
            });

            meta.used_by_fns.for_each([&](unsigned bit)
            {
                assert(bit < fn_map.size());
                fn_map[bit].onces.push_back(once);
            });
        }

        if(many)
        {
            unsigned const alignment = 0; // TODO
            many_ht const many = alloc_many(array-data.size(), alignment);

            meta.used_by_group_data.for_each([&](unsigned bit)
            {
                group_data_map[bit].push_back(many);
            });

            meta.used_by_fns.for_each([&](unsigned bit)
            {
                assert(bit < fn_map.size());
                fn_map[bit].manys.push_back(many);
            });
        }
    }

    std::vector<many_ht> fn_manys;
    std::vector<once_ht> fn_onces;
    fn_manys.reserve(impl_deque<fn_t>.size());
    fn_onces.reserve(impl_deque<fn_t>.size());

    for(fn_t const& fn : impl_deque<fn_t>)
    {
        if(!emit_rom(fn))
            continue;

        if(many_fns.test(fn.handle().value))
            fn_manys.push_back(alloc_many());
        else
            fn_onces.push_back(alloc_once());
    }

    // OK! All our 'many's and 'once's are created.

    unsigned const many_bs_size = bitset_size<>(many_vector.size());
    unsigned const once_bs_size = bitset_size<>(once_vector.size());

    for(many_t& many : many_vector)
    {
        many.required_manys.reset(many_bs_size);
        many.interfering_static_manys.reset(many_bs_size);
    }

    for(once_t& once : once_vector)
    {
        once.required_manys.reset(many_bs_size);
        once.related_onces(once_bs_size);
    }

    bitset_uint_t* const many_temp = ALLOCA_T(bitset_uint_t, many_bs_size);
    bitset_uint_t* const once_temp = ALLOCA_T(bitset_uint_t, once_bs_size);

    for(unsigned i = 0; i < fn_map.size(); ++i)
    {
        set_t const& set = fn_map[i];
        fn_ht fn = { i };

        if(!emit_rom(fn))
            continue;

        bitset_clear_all(many_bs_size, many_temp);
        for(many_ht many : needs.manys)
            bitset_set(many_temp, many.value);

        bitset_clear_all(once_bs_size, once_temp);
        for(once_ht once : needs.onces)
            bitset_set(once_temp, once.value);

        //for(once_ht once : needs.onces)
            //bitset_or(once_bs_size, once->related_onces.data(), once_temp);






    }


    rh::robin_map<fn_ht, fn_needs_t> fn_needs_map;
    rh::robin_map<group_data_ht, std::deque<once_ht>> group_data_onces;

        

        // When is a fn once vs many?
        // - MANY WHEN: it uses some 'once' data
        // - ONCE WHEN: everything else

        if(fn.ir_group_data().all_clear())
        {
            // Create a 'once_t'

            rom_once_t once;
        }
        else
        {
            // Create a 'many_t'
            
            rom_many_t many = {};
            many.require_static_addr = true;
            
            // TODO: add 'interfering_static_manys'

            post.push_back([h, &fn]()
            {
                fn.ir_group_data().for_each([&](unsigned i)
                {
                    for(const_ht ch : group_data_ht{i}->consts())
                    {
                        rom_t& rom = get_rom(ch)rockstar;
                        rom.live_with(fn_rom);
                    }
                });
            });

        }
    }

    // Create the bitsets for onces and manys:

    for(rom_once_t& once : onces)
    {
        once.required_manys.reset();
        once.related_onces.reset();
    }

    for(rom_many_t& many : manys)
    {
        many.required_manys.reset();
        many.interfering_static_manys.reset();
    }

    // Populate bitsets

    for(auto const& pair : fn_static_manys)
    {
        fn_t& fn = *pair.first
        rom_many_t& many = get(pair.second);

        assert(many.require_static_addr);

        fn.ir_group_data().for_each([&](unsigned i)
        {

        });
    }
}

class rom_allocator_t
{
public:
    rom_allocator_t(unsigned num_banks)
    {
        constexpr span_t rom_span = { .addr = 0x8000, .size = 0x8000 };
        span_allocator_t allocator(rom_span);

        // Allocate static_addrs
        for(rom_static_t& sa : rom_deque<rom_static_t>)
        {
            if(sa.span)
            {
                if(!allocator.alloc_at(sa.span))
                    throw std::runtime_error(fmt("Unable to allocate static address % - % (out of ROM space).", 
                                                 sa.span.addr, sa.span.end() - 1));
            }
            else if(!(sa.span = allocator.alloc(sa.size, sa.alignment)))
                throw std::runtime_error("Unable to allocate static address (out of ROM space).");
        }

        // Copy 'allocator' to fill banks.
        banks.clear();
        for(unsigned i = 0; i < num_banks; ++i)
            banks.emplace_back(allocator);
        ranks.resize(num_banks);

        struct once_rank_t
        {
            float score;
            rom_once_t* once;

            constexpr auto operator<=>(once_rank_t const&) const = default;
        };

        // Order 'onces'
        std::vector<once_rank_t> ordered_onces;
        ordered_onces.reserve(rom_deque<rom_once_t>.size());
        for(rom_once_t& once : rom_deque<rom_once_t>)
            ordered_onces.push_back({ once_rank(once), &once });
        std::sort(ordered_onces.begin(), ordered_onces.end(), std::greater<>{});

        // Allocate onces (this also allocates their required_manys)
        for(once_rank_t const& rank : ordered_onces)
            alloc(*rank.once);
    }

private:
    struct bank_rank_t
    {
        float score;
        unsigned bank_index;

        constexpr auto operator<=>(bank_rank_t const& o) const = default;
    };

    std::vector<bank_t> banks;
    std::vector<bank_rank_t> bank_ranks;

    // Used to create an allocation order for onces.
    static float once_rank(rom_once_t const& once)
    {
        int many_size = 0;
        bitset_for_each(many_bitset_size(), once.required_manys.get(), [&](unsigned i)
        {
            rom_many_t const& many = *rom_many_ht{i};
            if(many.static_addr)
                many_size += many.size * 8; // Arbitrary constant
            else
                many_size += many.size;
        });

        int related = bitset_popcount(once_bitset_size(), once.related_onces.get());

        return many_size + once.size + related;
    }

    // Used to find the best bank to allocate a once in
    static float bank_rank(bank_t const& bank, rom_once_t const& once)
    {
        // Count how much we have to allocate for required_manys
        bitset_uint_t* const unallocated_manys = ALLOCA_T(bitset_uint_t, many_bitset_size());
        bitset_copy(many_bitset_size(), unallocated_manys, once.required_manys.get());
        bitset_difference(many_bitset_size(), unallocated_manys, bank.allocated_manys.get());

        int unallocated_many_size = 0;
        bitset_for_each(many_bitset_size(), unallocated_manys, [&](unsigned i)
        {
            rom_many_t const& many = *rom_many_ht{ i };
            unallocated_many_size += many.size;
        });

        // Count related / unrelated onces
        bitset_uint_t* const onces = ALLOCA_T(bitset_uint_t, once_bitset_size());
        bitset_copy(once_bitset_size(), onces, once.related_onces.get());
        bitset_and(once_bitset_size(), onces, bank.allocated_onces.get());
        int const related = bitset_popcount(once_bitset_size(), onces);
        bitset_xor(once_bitset_size(), onces, bank.allocated_onces.get());
        int const unrelated = bitset_popcount(once_bitset_size(), onces);

        float const r = bank.allocator.initial_bytes_free() * std::sqrt((float)bank.allocator.spans_free());
        return unallocated_many_size + related - (unrelated * 0.25f) + (bank.allocator.bytes_free() / r);
    }

    void rank_banks_for(rom_once_t const& once)
    {
        assert(ranks.size() == banks.size());

        for(unsigned i = 0; i < banks.size(); ++i)
            bank_ranks[i] = { bank_rank(banks[i], once), i };

        std::sort(bank_ranks.begin(), bank_ranks.end(), std::greater<>{});
    }

    void alloc(rom_once_t& once)
    {
        struct many_span_t
        {
            rom_many_ht many;
            span_t span;
        };

        bc::small_vector<many_span_t, 32> many_spans;

        rank_banks_for(once); // Builds 'bank_ranks'

        for(bank_rank_t const& r : bank_ranks)
        {
            bank_t& bank = banks[r.bank_index];

            // 1. try to allocate manys required by 'once'
            // 2. then try to allocate 'once'

            many_spans.clear();

            bool const allocated_manys = 
            bitset_for_each_test(once_bitset_size(), once.required_manys.get(), [&](unsigned i)
            {
                rom_many_ht h = rom_many_ht{ i };
                rom_many_t& many = *h;

                if(!bank.many_spans.find(h))
                {
                    span_t span = bank.alloc_many_span(many);
                    if(!span)
                        return false;
                    many_spans.push_back({ h, span });
                }

                return true;
            });

            // If we succeeded in allocating manys, try to allocate 'once's span:
            if(!allocated_manys || !(once.span = bank.allocator.alloc(once.size, once.alignment)))
            {
                // If we fail, free allocated 'many' memory.
                for(many_span_t const& ms : many_spans)
                    bank.allocator.free(ms.span);
                continue;
            }

            // If we succeed, record the results and return:
            for(many_span_t const& ms : many_spans)
            {
                ms.many->span = ms.span;
                bank.many_spans.insert({ ms.many, ms.span });
            }

            return;
        }

        throw std::runtime_error("Unable to allocate address (out of ROM space).");
    }
};
