#ifndef FIXED_HPP
#define FIXED_HPP

#include <array>
#include <cstdint>

#include "types.hpp"

using fixed_int_t = std::uint64_t;
static_assert(sizeof(fixed_int_t) >= sizeof(std::uintptr_t));

struct fixed_t
{
    using int_type = fixed_int_t;
    int_type value;

    static constexpr fixed_int_t mask = 
        (1ull << (type_t::max_total_bytes * 8ull)) - 1ull;

    static constexpr int_type shift = 24;

    constexpr explicit operator bool() const { return value; }
    constexpr bool operator==(fixed_t o) const { return value == o.value; }
    constexpr bool operator!=(fixed_t o) const { return value != o.value; }
    constexpr bool operator<=(fixed_t o) const { return value <= o.value; }
    constexpr bool operator>=(fixed_t o) const { return value >= o.value; }
    constexpr bool operator<(fixed_t o) const { return value < o.value; }
    constexpr bool operator>(fixed_t o) const { return value > o.value; }

    constexpr bool operator!() const { return !value; }

    static constexpr fixed_t whole(int_type i) { return { i << shift }; }
    constexpr int_type whole() const { return value >> shift; }
};

constexpr fixed_t operator""_f(unsigned long long int i) { return { i }; }

template<typename T>
using fixed_lut_t = std::array<T, TYPE_LAST_NUM - TYPE_FIRST_NUM + 1>;
extern fixed_lut_t<fixed_t::int_type> numeric_bitmask_table;

[[gnu::pure]]
inline fixed_t::int_type numeric_bitmask(type_name_t type_name)
{
    assert(is_numeric(type_name));
    return numeric_bitmask_table[type_name - TYPE_FIRST_NUM];
}

[[gnu::pure]]
inline fixed_t::int_type numeric_bitmask(type_t type)
{
    assert(is_numeric(type));
    return numeric_bitmask(type.name());
}

inline fixed_t fixed_add(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value + rhs.value) & numeric_bitmask(type_name) }; }
    
inline fixed_t fixed_sub(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value - rhs.value) & numeric_bitmask(type_name) }; }

inline fixed_t fixed_and(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value & rhs.value) & numeric_bitmask(type_name) }; }

inline fixed_t fixed_or(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value | rhs.value) & numeric_bitmask(type_name) }; }

inline fixed_t fixed_xor(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value ^ rhs.value) & numeric_bitmask(type_name) }; }

constexpr double to_double(fixed_t f)
    { return (double)f.value / (double)(1 << fixed_t::shift); }

#endif

