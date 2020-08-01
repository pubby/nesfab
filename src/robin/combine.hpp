#ifndef ROBIN_HOOD_COMBINE_HPP
#define ROBIN_HOOD_COMBINE_HPP

// A function which combines two hashes.

#include <cstddef>

namespace rh
{
    [[gnu::always_inline]]
    constexpr std::size_t hash_combine(std::size_t a, std::size_t b)
    {
        return a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2));
    }
}

#endif
