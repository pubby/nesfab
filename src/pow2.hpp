#ifndef POW2_HPP
#define POW2_HPP

#include <cstdint>
#include <cassert>

constexpr std::uint64_t is_pow2(std::uint64_t v)
{
    return (v & (v - 1)) == 0ull;
}

constexpr std::uint64_t next_pow2(std::uint64_t v)
{
    std::uint64_t r = v;
    --r;
    r |= r >> 1ull;
    r |= r >> 2ull;
    r |= r >> 4ull;
    r |= r >> 8ull;
    r |= r >> 16ull;
    r |= r >> 32ull;
    ++r;

    assert(r >= v); 
    assert(is_pow2(r));

    return r;
}

#endif
