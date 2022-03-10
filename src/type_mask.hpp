#ifndef TYPE_MASK_HPP
#define TYPE_MASK_HPP

#include <array>

#include "fixed.hpp"
#include "type_name.hpp"

constexpr fixed_int_t fixed_mask = (1ull << (max_total_bytes * 8ull)) - 1ull;
constexpr fixed_int_t frac_mask = (1ull << fixed_t::shift) - 1ull;
constexpr fixed_int_t whole_mask = ~frac_mask;

template<typename T>
using fixed_lut_t = std::array<T, TYPE_LAST_SCALAR - TYPE_FIRST_SCALAR + 1>;
extern fixed_lut_t<fixed_int_t> const numeric_bitmask_table;
extern fixed_lut_t<fixed_int_t> const numeric_sub_bitmask_table;
extern fixed_lut_t<fixed_int_t> const numeric_super_bitmask_table;

[[gnu::pure]]
inline fixed_int_t numeric_bitmask(type_name_t type_name)
{
    assert(is_scalar(type_name));
    return numeric_bitmask_table[type_name - TYPE_FIRST_SCALAR];
}

[[gnu::pure]]
inline fixed_int_t numeric_submask(type_name_t type_name)
{
    assert(is_scalar(type_name));
    return numeric_sub_bitmask_table[type_name - TYPE_FIRST_SCALAR];
}

[[gnu::pure]]
inline fixed_int_t numeric_supermask(type_name_t type_name)
{
    assert(is_scalar(type_name));
    return numeric_super_bitmask_table[type_name - TYPE_FIRST_SCALAR];
}

[[gnu::pure]]
inline fixed_t mask_numeric(fixed_t f, type_name_t type_name)
{
    assert(is_scalar(type_name));
    return fixed_t{ f.value & numeric_bitmask(type_name) };
}

[[gnu::pure]]
inline bool is_masked(fixed_t f, type_name_t type_name)
{
    assert(is_scalar(type_name));
    return (f.value & numeric_bitmask(type_name)) == f.value;
}

[[gnu::pure]]
inline constexpr fixed_t boolify(fixed_t f)
{
    if(f)
        return { 1ull << fixed_t::shift };
    return { 0 };
}

sfixed_int_t to_signed(fixed_int_t f, fixed_int_t bitmask);
sfixed_int_t to_signed(fixed_int_t f, type_name_t type_name);

#endif
