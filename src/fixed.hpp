#ifndef FIXED_HPP
#define FIXED_HPP

#include <cstdint>

using fixed_uint_t = std::uint64_t;
using fixed_sint_t = std::int64_t;
static_assert(sizeof(fixed_uint_t) >= sizeof(std::uintptr_t));

constexpr fixed_uint_t MAX_FIXED_MASK = (1ull << 56) - 1ull;

struct fixed_t
{
    fixed_uint_t value;

    static constexpr fixed_uint_t shift = 24; // Matches types

    constexpr explicit operator bool() const { return value; }
    constexpr auto operator<=>(fixed_t const& o) const = default;
    constexpr bool operator!() const { return !value; }

    static constexpr fixed_t whole(fixed_uint_t i) { return { i << shift }; }
    constexpr fixed_uint_t whole() const { return value >> shift; }
};

constexpr fixed_t operator""_f(unsigned long long int i) { return { i }; }

constexpr double to_double(fixed_t f)
{ 
    fixed_sint_t value = static_cast<fixed_sint_t>(f.value);
    return (double)value / (double)(1ull << fixed_t::shift); 
}

constexpr bool is_byte(fixed_t fixed)
{
    return (fixed.value & (0xFFull << fixed_t::shift)) == fixed.value;
}

#endif

