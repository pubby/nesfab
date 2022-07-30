#ifndef TYPE_MASK_HPP
#define TYPE_MASK_HPP

#include <array>

#include "builtin.hpp"
#include "fixed.hpp"
#include "type_name.hpp"
#include "sizeof_bits.hpp"

constexpr fixed_uint_t fixed_mask = (1ull << (max_total_bytes * 8ull)) - 1ull;
constexpr fixed_uint_t frac_mask = (1ull << fixed_t::shift) - 1ull;
constexpr fixed_uint_t whole_mask = ~frac_mask;

template<typename T>
using fixed_lut_t = std::array<T, TYPE_LAST_SCALAR - TYPE_FIRST_SCALAR + 1>;

constexpr fixed_lut_t<fixed_uint_t> numeric_bitmask_table = []()
{
    fixed_lut_t<fixed_uint_t> table;
    for(int i = TYPE_FIRST_SCALAR; i <= TYPE_LAST_SCALAR; ++i)
    {
        type_name_t type_name = (type_name_t)i;

        fixed_uint_t v = 0;

        if(type_name == TYPE_BOOL)
            v = 1ull << fixed_t::shift;
        else
        {
            for(unsigned j = 0; j < frac_bytes(type_name); ++j)
                v |= 0xFFull << (8 * (2 - j));
            for(unsigned j = 0; j < whole_bytes(type_name); ++j)
                v |= 0xFFull << (8 * (3 + j));
        }

        table[i - TYPE_FIRST_SCALAR] = v;
    }
    return table;
}();

constexpr fixed_uint_t numeric_bitmask(type_name_t type_name)
{
    assert(is_scalar(type_name));
    return numeric_bitmask_table[type_name - TYPE_FIRST_SCALAR];
}

constexpr fixed_t mask_numeric(fixed_t f, type_name_t type_name)
{
    assert(is_scalar(type_name));
    return fixed_t{ f.value & numeric_bitmask(type_name) };
}

constexpr bool is_masked(fixed_t f, type_name_t type_name)
{
    assert(is_scalar(type_name));
    return (f.value & numeric_bitmask(type_name)) == f.value;
}

constexpr fixed_t boolify(fixed_t f)
{
    if(f)
        return { 1ull << fixed_t::shift };
    return { 0 };
}

constexpr fixed_uint_t high_bit_only(fixed_uint_t i) { return i & ~(i >> 1); }

constexpr fixed_uint_t low_bit_only(fixed_uint_t i) { return i & ~(i << 1); }

constexpr bool is_mask(fixed_uint_t i) { return !((low_bit_only(i) + i) & i); }

constexpr fixed_uint_t below_mask(fixed_uint_t i) { assert(is_mask(i)); return low_bit_only(i) - 1ull; }

constexpr fixed_uint_t above_mask(fixed_uint_t i) { assert(is_mask(i)); return ~((high_bit_only(i) << 1) - 1ull); }

constexpr fixed_uint_t submask(fixed_uint_t i) { assert(is_mask(i)); return i | below_mask(i); }

constexpr fixed_uint_t supermask(fixed_uint_t i) { assert(is_mask(i)); return i | above_mask(i); }

constexpr fixed_sint_t sign_extend(fixed_uint_t i, fixed_uint_t mask) 
{ 
    if(i & high_bit_only(mask))
        return static_cast<fixed_sint_t>(i | above_mask(mask));
    return static_cast<fixed_sint_t>(i);
}

constexpr fixed_sint_t to_signed(fixed_uint_t i, type_name_t tn) 
{
    if(is_signed(tn))
        return sign_extend(i, numeric_bitmask(tn));
    return static_cast<fixed_sint_t>(i);
}

constexpr bool in_mask(fixed_uint_t i, fixed_uint_t mask, bool signed_) 
{
    if(signed_)
        return i == fixed_uint_t(sign_extend(i & mask, mask));
    return i == (i & mask);
}

inline unsigned signed_clz(fixed_sint_t i)
{
    fixed_uint_t u = i;
    if(i < 0)
        u = ~u;
    return u ? builtin::clz(u) : sizeof_bits<fixed_sint_t>;
}

[[gnu::pure]]
inline fixed_sint_t type_unit(type_name_t type_name)
{
    assert(is_scalar(type_name));
    return low_bit_only(numeric_bitmask(type_name));
}

[[gnu::pure]]
inline fixed_sint_t type_min(type_name_t type_name)
{
    assert(is_scalar(type_name));
    if(is_signed(type_name))
        return -fixed_sint_t(high_bit_only(numeric_bitmask(type_name)));
    else
        return 0;
}

inline fixed_sint_t type_max(type_name_t type_name)
{
    assert(is_scalar(type_name));
    if(is_signed(type_name))
    {
        fixed_uint_t const mask = numeric_bitmask(type_name);
        return (mask >> 1) & mask;
    }
    else
        return numeric_bitmask(type_name);
}

#endif
