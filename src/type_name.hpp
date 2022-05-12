#ifndef TYPE_NAME_HPP
#define TYPE_NAME_HPP

#include <cassert>
#include <cstdint>

static constexpr unsigned max_frac_bytes = 3;
static constexpr unsigned max_whole_bytes = 4;
static constexpr unsigned max_total_bytes = max_frac_bytes + max_whole_bytes;

#define FRAC_X \
    FIXED(0,1) FIXED(0,2) FIXED(0,3)\

#define FIXED_X \
    FIXED(1,0) FIXED(1,1) FIXED(1,2) FIXED(1,3)\
    FIXED(2,0) FIXED(2,1) FIXED(2,2) FIXED(2,3)\
    FIXED(3,0) FIXED(3,1) FIXED(3,2) FIXED(3,3)\

enum type_name_t : std::uint8_t // Keep unsigned.
{
    // Have void be the zeroth/default value.
    TYPE_VOID = 0,

    TYPE_STRUCT_THUNK, // Will convert to struct type eventually.
    TYPE_FIRST_THUNK = TYPE_STRUCT_THUNK,
    TYPE_ARRAY_THUNK, // The size will be determined later.
    TYPE_LAST_THUNK = TYPE_ARRAY_THUNK,

    TYPE_STRUCT,

    TYPE_ARRAY,

    TYPE_BUFFER,

    TYPE_PTR,
    TYPE_FIRST_SCALAR = TYPE_PTR,
    TYPE_FIRST_PTR = TYPE_PTR,
    TYPE_BANKED_PTR,
    TYPE_FN, // should be named FN_PTR, but whatever
    TYPE_LAST_PTR = TYPE_FN,

    // Bools are considered arithmetic.
    TYPE_BOOL,
    TYPE_FIRST_ARITH = TYPE_BOOL,

    // Numerical literals readily convert to other numeric types,
    // assuming they fit in said representation.
#define FIXED(whole, frac) TYPE_F##frac,
    FRAC_X
#undef FIXED
#define FIXED(whole, frac) TYPE_U##whole##frac,
    FIXED_X
#undef FIXED
#define FIXED(whole, frac) TYPE_S##whole##frac,
    FIXED_X
#undef FIXED
    TYPE_INT,
    TYPE_FIRST_CT = TYPE_INT,
    TYPE_REAL,
    TYPE_LAST_CT = TYPE_REAL,
    NUM_TYPE_NAMES,
    TYPE_U = TYPE_U10,
    TYPE_S = TYPE_S10,

    TYPE_FIRST_F = TYPE_F1,
    TYPE_LAST_F  = TYPE_F3,

    TYPE_FIRST_U = TYPE_U10,
    TYPE_LAST_U  = TYPE_U33,

    TYPE_FIRST_S = TYPE_S10,
    TYPE_LAST_S  = TYPE_REAL,

    TYPE_LAST_ARITH  = TYPE_REAL,
    TYPE_LAST_SCALAR    = TYPE_REAL,

    TYPE_LARGEST_U = TYPE_U33,
    TYPE_LARGEST_S = TYPE_S33,
};

static_assert(NUM_TYPE_NAMES < 64);

constexpr bool is_arithmetic(type_name_t type_name)
    { return type_name >= TYPE_FIRST_ARITH && type_name <= TYPE_LAST_ARITH; }
constexpr bool is_quantity(type_name_t type_name)
    { return is_arithmetic(type_name) && type_name != TYPE_BOOL; }
constexpr bool is_scalar(type_name_t type_name)
    { return type_name >= TYPE_FIRST_SCALAR && type_name <= TYPE_LAST_SCALAR; }
constexpr bool is_frac(type_name_t type_name)
    { return type_name >= TYPE_FIRST_F && type_name <= TYPE_LAST_F; }
constexpr bool is_unsigned(type_name_t type_name)
    { return type_name >= TYPE_FIRST_U && type_name <= TYPE_LAST_U; }
constexpr bool is_signed(type_name_t type_name)
    { return type_name >= TYPE_FIRST_S && type_name <= TYPE_LAST_S; }
constexpr bool is_ptr(type_name_t type_name)
    { return type_name >= TYPE_FIRST_PTR && type_name <= TYPE_LAST_PTR; }
constexpr bool is_thunk(type_name_t type_name)
    { return type_name >= TYPE_FIRST_THUNK && type_name <= TYPE_LAST_THUNK; }
constexpr bool is_ct(type_name_t type_name)
    { return type_name >= TYPE_FIRST_CT && type_name <= TYPE_LAST_CT; }
constexpr bool is_array(type_name_t type_name)
    { return type_name == TYPE_ARRAY || type_name == TYPE_ARRAY_THUNK; }
constexpr bool is_struct(type_name_t type_name)
    { return type_name == TYPE_STRUCT || type_name == TYPE_STRUCT_THUNK; }
constexpr bool is_aggregate(type_name_t type_name)
    { return is_array(type_name) || is_struct(type_name); }

constexpr bool has_type_tail(type_name_t name)
    { return name == TYPE_ARRAY || name == TYPE_FN; }
constexpr bool has_group_tail(type_name_t name)
    { return is_ptr(name) || name == TYPE_BUFFER; }
constexpr bool has_tail(type_name_t name)
{ 
    return (has_type_tail(name) 
            || has_group_tail(name)
            || is_thunk(name)
            || name == TYPE_STRUCT); 
}

constexpr bool is_simple(type_name_t type_name)
{
    return !has_tail(type_name);
}

constexpr unsigned whole_bytes(type_name_t type_name)
{
    switch(type_name)
    {
    default: return 0;
    case TYPE_BOOL:  return 1;
    case TYPE_PTR:   return 2;
    case TYPE_BANKED_PTR:  return 3;
    case TYPE_INT:
    case TYPE_REAL: return 4;
#define FIXED(whole, frac) case TYPE_F##frac: return 0;
    FRAC_X
#undef FIXED
#define FIXED(whole, frac) case TYPE_U##whole##frac: case TYPE_S##whole##frac: return whole;
    FIXED_X
#undef FIXED
    }
}

constexpr unsigned frac_bytes(type_name_t type_name)
{
    switch(type_name)
    {
    default: return 0;
    case TYPE_REAL: return 3;
#define FIXED(whole, frac) case TYPE_F##frac: return frac;
    FRAC_X
#undef FIXED
#define FIXED(whole, frac) case TYPE_U##whole##frac: case TYPE_S##whole##frac: return frac;
    FIXED_X
#undef FIXED
    }
}

constexpr unsigned total_bytes(type_name_t type_name)
{
    return whole_bytes(type_name) + frac_bytes(type_name);
}

constexpr type_name_t type_f(unsigned f)
{
    assert(f > 0);
    assert(f <= 3);
    return type_name_t(TYPE_F1 - 1 + f);
}

constexpr type_name_t type_u(unsigned w, unsigned f)
{
    assert(w > 0);
    assert(w <= 3);
    assert(f <= 3);
    return type_name_t(TYPE_U10 - 4 + w*4 + f);
}

constexpr type_name_t type_s(unsigned w, unsigned f)
{
    assert(w > 0);
    assert(w <= 3);
    assert(f <= 3);
    return type_name_t(TYPE_S10 - 4 + w*4 + f);
}

constexpr unsigned begin_byte(type_name_t type_name)
{
    return max_frac_bytes - frac_bytes(type_name);
}

constexpr unsigned end_byte(type_name_t type_name)
{
    return max_frac_bytes + whole_bytes(type_name);
}

// True if 'super' can represent every value that 'sub' can.
constexpr bool is_arithmetic_subset(type_name_t sub, type_name_t super)
{
    if(sub == TYPE_BOOL)
        return true;

    if(!is_arithmetic(sub) || !is_arithmetic(super))
        return false;

    if(frac_bytes(sub) > frac_bytes(super))
        return false;

    if(whole_bytes(sub) > whole_bytes(super))
        return false;

    if(whole_bytes(sub) == whole_bytes(super) && is_signed(sub) != is_signed(super))
        return false;

    return true;
}

#endif
