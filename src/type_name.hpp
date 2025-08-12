#ifndef TYPE_NAME_HPP
#define TYPE_NAME_HPP

#include <cassert>
#include <cstdint>
#include <string>
#include <ostream>

static constexpr unsigned max_frac_bytes = 3;
static constexpr unsigned max_whole_bytes = 4;
static constexpr unsigned max_total_bytes = max_frac_bytes + max_whole_bytes;

static constexpr unsigned max_rt_frac_bytes = 3;
static constexpr unsigned max_rt_whole_bytes = 3;
static constexpr unsigned max_rt_total_bytes = max_rt_frac_bytes + max_rt_whole_bytes;

#define FRAC_X \
    FIXED(0,1) FIXED(0,2) FIXED(0,3)

#define FIXED_X \
    FIXED(1,0) FIXED(1,1) FIXED(1,2) FIXED(1,3)\
    FIXED(2,0) FIXED(2,1) FIXED(2,2) FIXED(2,3)\
    FIXED(3,0) FIXED(3,1) FIXED(3,2) FIXED(3,3)

enum type_name_t : std::uint8_t // Keep unsigned.
{
    // Have void be the zeroth/default value.
    TYPE_VOID = 0,
    TYPE_CHARMAP,
    TYPE_FN_SET,

    TYPE_ASM_LABEL,
    TYPE_ASM_VALUE,

    TYPE_STRUCT_THUNK, // Will convert to struct type eventually.
    TYPE_FIRST_THUNK = TYPE_STRUCT_THUNK,
    TYPE_TEA_THUNK, // The size will be determined later.
    TYPE_PAA_THUNK, // The size will be determined later.
    TYPE_LAST_THUNK = TYPE_PAA_THUNK,

    TYPE_STRUCT,
    TYPE_FN,

    TYPE_TEA,  // typed-element array
    TYPE_PAA,  // pointer-addressable array
    TYPE_VEC,  // resizable array

    TYPE_FN_PTR,
    TYPE_BANKED_APTR, // 'APTR' is used to represent assembly addresses, ignoring groups.
    TYPE_FIRST_PTR = TYPE_BANKED_APTR,
    TYPE_BANKED_CPTR, // banked pointer to immutable data
    TYPE_BANKED_MPTR, // banked pointer to mutable data
    TYPE_BANKED_PPTR, // banked pointer to data
    TYPE_APTR,
    TYPE_FIRST_SCALAR = TYPE_APTR,
    TYPE_CPTR,
    TYPE_MPTR,
    TYPE_PPTR,
    TYPE_LAST_PTR = TYPE_PPTR,

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

    // Dummy types below

    TYPE_GROUP_SET, // Just holds a group tail, nothing else
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
constexpr bool is_ptr(type_name_t type_name, bool include_fn_ptr = false)
{ 
    if(include_fn_ptr && type_name == TYPE_FN_PTR)
        return true;
    return type_name >= TYPE_FIRST_PTR && type_name <= TYPE_LAST_PTR;
}
constexpr bool is_aptr(type_name_t type_name)
    { return type_name == TYPE_APTR || type_name == TYPE_BANKED_APTR; }
constexpr bool is_cptr(type_name_t type_name)
    { return type_name == TYPE_CPTR || type_name == TYPE_BANKED_CPTR; }
constexpr bool is_mptr(type_name_t type_name)
    { return type_name == TYPE_MPTR || type_name == TYPE_BANKED_MPTR; }
constexpr bool is_pptr(type_name_t type_name)
    { return type_name == TYPE_PPTR || type_name == TYPE_BANKED_PPTR; }
constexpr bool is_banked_ptr(type_name_t type_name)
    { return type_name == TYPE_BANKED_CPTR || type_name == TYPE_BANKED_MPTR || type_name == TYPE_BANKED_PPTR || type_name == TYPE_BANKED_APTR; }
constexpr bool is_group_ptr(type_name_t type_name)
    { return is_mptr(type_name) || is_cptr(type_name) || is_pptr(type_name); }
constexpr bool is_unsigned(type_name_t type_name)
    { return is_ptr(type_name) || (type_name >= TYPE_FIRST_U && type_name <= TYPE_LAST_U); }
constexpr bool is_signed(type_name_t type_name)
    { return type_name >= TYPE_FIRST_S && type_name <= TYPE_LAST_S; }
constexpr bool is_summable(type_name_t type_name)
    { return is_quantity(type_name) || is_ptr(type_name); }
constexpr bool is_thunk(type_name_t type_name)
    { return type_name >= TYPE_FIRST_THUNK && type_name <= TYPE_LAST_THUNK; }
constexpr bool is_vec(type_name_t type_name)
    { return type_name == TYPE_VEC; }
constexpr bool is_ct(type_name_t type_name, bool include_vec = true)
    { return (type_name >= TYPE_FIRST_CT && type_name <= TYPE_LAST_CT) || (include_vec && is_vec(type_name)); }
constexpr bool is_tea(type_name_t type_name)
    { return type_name == TYPE_TEA || type_name == TYPE_TEA_THUNK; }
constexpr bool is_paa(type_name_t type_name)
    { return type_name == TYPE_PAA || type_name == TYPE_PAA_THUNK; }
constexpr bool is_array(type_name_t type_name)
    { return is_tea(type_name) || is_paa(type_name); }
constexpr bool is_struct(type_name_t type_name)
    { return type_name == TYPE_STRUCT || type_name == TYPE_STRUCT_THUNK; }
constexpr bool is_aggregate(type_name_t type_name)
    { return is_tea(type_name) || is_struct(type_name) || is_vec(type_name); }
constexpr bool is_byteified(type_name_t type_name)
    { return type_name == TYPE_U || type_name == TYPE_S || type_name == TYPE_BOOL; }

constexpr bool has_type_tail(type_name_t name)
    { return name == TYPE_TEA || name == TYPE_FN || name == TYPE_VEC; }
constexpr bool has_group_tail(type_name_t name)
    { return is_mptr(name) || is_cptr(name) || is_pptr(name) || name == TYPE_PAA || name == TYPE_GROUP_SET; }
constexpr bool has_tail(type_name_t name)
{ 
    return (has_type_tail(name) 
            || has_group_tail(name)
            || is_thunk(name)
            || name == TYPE_STRUCT
            || name == TYPE_FN_PTR); 
}

constexpr bool is_simple(type_name_t type_name)
{
    return !has_tail(type_name);
}

constexpr unsigned whole_bytes(type_name_t type_name)
{
    switch(type_name)
    {
    default: 
        return 0;
    case TYPE_BOOL: 
        return 1;
    case TYPE_APTR:
    case TYPE_CPTR:
    case TYPE_MPTR:
    case TYPE_PPTR:
        return 2;
    case TYPE_FN_PTR:
    case TYPE_BANKED_APTR:
    case TYPE_BANKED_CPTR:
    case TYPE_BANKED_MPTR:
    case TYPE_BANKED_PPTR:
        return 2; // Bank isn't counted.
    case TYPE_INT:
    case TYPE_REAL:
        return 4;
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
    default:
        return 0;
    case TYPE_REAL: 
        return 3;
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
    assert(f <= max_frac_bytes);
    return type_name_t(TYPE_F1 - 1 + f);
}

constexpr type_name_t type_u(unsigned w, unsigned f)
{
    assert(w + f > 0);
    assert(w <= max_whole_bytes);
    assert(f <= max_frac_bytes);
    return type_name_t(TYPE_U10 - 4 + w*4 + f);
}

constexpr type_name_t type_s(unsigned w, unsigned f)
{
    assert(w + f > 0);
    assert(w <= max_whole_bytes);
    assert(f <= max_frac_bytes);
    return type_name_t(TYPE_S10 - 4 + w*4 + f);
}

constexpr type_name_t type_ptr(bool muta, bool banked)
{
    if(muta)
        return banked ? TYPE_BANKED_MPTR : TYPE_MPTR;
    return banked ? TYPE_BANKED_CPTR : TYPE_CPTR;
}

constexpr type_name_t with_banked(type_name_t ptr, bool b = true)
{
    assert(is_ptr(ptr));
    if(is_aptr(ptr))
       return b ? TYPE_BANKED_APTR : TYPE_APTR;
    if(is_mptr(ptr))
       return b ? TYPE_BANKED_MPTR : TYPE_MPTR;
    if(is_cptr(ptr))
       return b ? TYPE_BANKED_CPTR : TYPE_CPTR;
    if(is_pptr(ptr))
       return b ? TYPE_BANKED_PPTR : TYPE_PPTR;
    assert(false);
    return {};
}

constexpr type_name_t type_s_or_u(unsigned w, unsigned f, bool s)
    { return s ? type_s(w, f) : type_u(w, f); }

constexpr type_name_t to_u(type_name_t t)
    { return type_u(whole_bytes(t), frac_bytes(t)); }

constexpr type_name_t to_s(type_name_t t)
    { return type_s(whole_bytes(t), frac_bytes(t)); }

constexpr unsigned begin_byte(type_name_t type_name)
    { return max_frac_bytes - frac_bytes(type_name); }

constexpr unsigned end_byte(type_name_t type_name)
    { return max_frac_bytes + whole_bytes(type_name); }

template<bool Bijective>
constexpr bool is_arithmetic_subset_impl(type_name_t sub, type_name_t super)
{
    if(sub == TYPE_BOOL)
        return true;

    if(!is_arithmetic(sub) || !is_arithmetic(super))
        return false;

    if(frac_bytes(sub) > frac_bytes(super))
        return false;

    if(whole_bytes(sub) > whole_bytes(super))
        return false;

    if(!Bijective)
    {
        if(whole_bytes(sub) == whole_bytes(super) && is_signed(sub) != is_signed(super))
            return false;
    }

    return true;
}

// True if 'super' can represent every value that 'sub' can.
constexpr bool is_arithmetic_subset(type_name_t sub, type_name_t super)
{
    return is_arithmetic_subset_impl<false>(sub, super);
}

// True if converting from 'sub' to 'super' can be reversed.
constexpr bool is_arithmetic_bijection(type_name_t sub, type_name_t super)
{
    return is_arithmetic_subset_impl<true>(sub, super);
}

constexpr bool same_scalar_layout(type_name_t a, type_name_t b)
{
    return (is_scalar(a) && is_scalar(b) 
            && whole_bytes(a) == whole_bytes(b)
            && frac_bytes(a) == frac_bytes(b));
}

std::string to_string(type_name_t type_name);

std::ostream& operator<<(std::ostream& o, type_name_t type_name);

#endif
