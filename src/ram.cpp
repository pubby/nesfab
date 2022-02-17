#include "ram.hpp"

void ram_for_size(ram_bitset_t& ram, std::size_t size)
{
    if(size <= 1)
        return;

    unsigned shift_by = 1;
    for(size -= 1; shift_by <= size; shift_by <<= 1)
        ram &= ram >> shift_by;
    ram &= ram >> (size - (shift_by >> 1));
}

span_t alloc_ram(ram_bitset_t const& usable_ram, std::size_t size, bool zp_only)
{
    if(size == 1) // fast path
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
