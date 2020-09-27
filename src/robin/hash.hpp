#ifndef ROBIN_HOOD_HASH_HPP
#define ROBIN_HOOD_HASH_HPP

// Functions for defining / improving hashes.

#include <cstddef>

namespace rh
{
    [[gnu::always_inline]]
    constexpr std::size_t hash_combine(std::size_t a, std::size_t b)
    {
        return a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2));
    }

    // Just randomizes an integer hash a bit:
    [[gnu::always_inline]]
    constexpr std::size_t hash_finalize(std::size_t h)
    {
        h = (h ^ (h >> 30ull)) * 0xbf58476d1ce4e5b9ull;
        h = (h ^ (h >> 27ull)) * 0x94d049bb133111ebull;
        h = h ^ (h >> 31ull);
        return h;
    }
}

#endif
