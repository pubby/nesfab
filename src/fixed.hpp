#ifndef FIXED_HPP
#define FIXED_HPP

#include <cstdint>

using fixed_int_t = std::uint64_t;
using sfixed_int_t = std::int64_t;
static_assert(sizeof(fixed_int_t) >= sizeof(std::uintptr_t));

struct fixed_t
{
    fixed_int_t value;

    static constexpr fixed_int_t shift = 24; // Matches types

    constexpr explicit operator bool() const { return value; }
    constexpr auto operator<=>(fixed_t const& o) const = default;
    constexpr bool operator!() const { return !value; }

    static constexpr fixed_t whole(fixed_int_t i) { return { i << shift }; }
    constexpr fixed_int_t whole() const { return value >> shift; }
};

constexpr fixed_t operator""_f(unsigned long long int i) { return { i }; }

constexpr double to_double(fixed_t f)
{ 
    return (double)f.value / (double)(1 << fixed_t::shift); 
}

#endif

