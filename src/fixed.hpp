#ifndef FIXED_HPP
#define FIXED_HPP

#include <array>
#include <cstdint>

#include "types.hpp"

struct fixed_t
{
    using int_type = std::uint64_t;
    int_type value;

    constexpr explicit operator bool() const { return value; }
    constexpr bool operator==(fixed_t o) const { return value == o.value; }
    constexpr bool operator!=(fixed_t o) const { return value != o.value; }
    constexpr bool operator<=(fixed_t o) const { return value <= o.value; }
    constexpr bool operator>=(fixed_t o) const { return value >= o.value; }
    constexpr bool operator<(fixed_t o) const { return value < o.value; }
    constexpr bool operator>(fixed_t o) const { return value > o.value; }

    constexpr bool operator!() const { return !value; }
};

constexpr fixed_t operator""_f(unsigned long long int i) { return { i }; }

template<typename T>
using fixed_lut_t = std::array<T, TYPE_LAST_ARITH - TYPE_FIRST_ARITH + 1>;
extern fixed_lut_t<fixed_t::int_type> arithmetic_bitmask_table;

inline fixed_t::int_type arithmetic_bitmask(type_name_t type_name)
{
    assert(is_arithmetic(type_name));
    return arithmetic_bitmask_table[type_name - TYPE_FIRST_ARITH];
}

inline unsigned bit_i_begin(type_name_t type_name)
{
    assert(is_arithmetic(type_name));
    return 8 * (2 - frac_bytes(type_name));
}

inline unsigned bit_i_end(type_name_t type_name)
{
    assert(is_arithmetic(type_name));
    return 8 * (2 + whole_bytes(type_name));
}

inline fixed_t fixed_add(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value + rhs.value) & arithmetic_bitmask(type_name) }; }
    
inline fixed_t fixed_sub(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value - rhs.value) & arithmetic_bitmask(type_name) }; }

inline fixed_t fixed_and(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value & rhs.value) & arithmetic_bitmask(type_name) }; }

inline fixed_t fixed_or(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value | rhs.value) & arithmetic_bitmask(type_name) }; }

inline fixed_t fixed_xor(type_name_t type_name, fixed_t lhs, fixed_t rhs)
    { return { (lhs.value ^ rhs.value) & arithmetic_bitmask(type_name) }; }

#endif

