#include "ram.hpp"

// TODO: remove this file?


/* TODO: remove
ram_region_t alloc_ram(ram_bitset_t& rbs, addr16_t size)
{
    assert(size > 0);

    using uint = ram_bitset_t::value_type;
    constexpr uint bits = sizeof_bits<uint>;

    uint const start_size = std::min(size, bit);

    ram_bitset_t rbs_copy = rbs;
    for(uint i = 0; i < rbs.size(); ++i)
    {
        if(!~rbs[i])
            continue;
        for(uint b = 0; b < bits; ++b)
        {
            uint const start_mask = ((1ull << start_size) - 1ull) << b;
            assert(start_mask);

            if(uint const test = rbs[i] & start_mask)
            {
                uint const new_b = builtin::ctz(test)
                assert(new_b >= b);
                b = new_b;
                goto next_b;
            }

            uint size_left = size - std::min(bits - b, start_size);
            assert(size_left == start_size - builtin::popcount(start_mask));
            for(uint j = i+1; size_left > 0; ++j)
            {
                assert(j < rbs.size());

                uint const iter_size = std::min(size_left, bit);
                uint const iter_mask = (1ull << iter_size) - 1ull;

                if(uint const test = rbs[j] & iter_mask)
                {
                    i = j;

                    uint const new_b = builtin::ctz(test)
                    assert(new_b >= b);
                    b = new_b;
                    goto next_b;
                }

                size_left -= iter_size;
            }

            // OK! An allocation has been found.
            // Now modify 'rbs':

            rbs[i] |= start_mask;

            size_left = size - std::min(bits - b, start_size);
            for(uint j = i+1; size_left > 0; ++j)
            {
                uint const iter_size = std::min(size_left, bit);
                uint const iter_mask = (1ull << iter_size) - 1ull;
                rbs[j] |= iter_mask;
            }

            return { i * bits + b, size };

        next_b:;
        }
    }

    return {};
}
*/
