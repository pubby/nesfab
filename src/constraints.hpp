#ifndef CONSTRAINTS_HPP
#define CONSTRAINTS_HPP

// Structs that track approximate sets of values, to be used in const
// propagation to determine constants.
// (AKA this is an abstract domain.)

// The representation is the intersection of an interval domain
// and a "known bits" domain. The "known bits" domain represents sets of
// numbers that share some common bit pattern.

#include <limits>
#include <ostream>

#include <boost/container/small_vector.hpp>

#include "carry.hpp"
#include "fixed.hpp"
#include "type_mask.hpp"
#include "ssa_op.hpp"

namespace bc = ::boost::container;

struct constraints_mask_t ;
struct bounds_t;
struct known_bits_t;
struct constraints_t;

std::string to_string(bounds_t const& b);
std::string to_string(constraints_t const& c);
std::string to_string(known_bits_t const& b);

std::ostream& operator<<(std::ostream& o, bounds_t const& b);
std::ostream& operator<<(std::ostream& o, known_bits_t const& b);
std::ostream& operator<<(std::ostream& o, constraints_t const& c);

bounds_t apply_mask(bounds_t b, constraints_mask_t cm);
known_bits_t apply_mask(known_bits_t b, constraints_mask_t cm);
constraints_t apply_mask(constraints_t c, constraints_mask_t cm);

bounds_t from_bits(known_bits_t bits, constraints_mask_t cm);
known_bits_t from_bounds(bounds_t bounds, constraints_mask_t cm);

bounds_t intersect(bounds_t a, bounds_t b);
known_bits_t intersect(known_bits_t a, known_bits_t b);
constraints_t intersect(constraints_t a, constraints_t b);

bounds_t union_(bounds_t a, bounds_t b);
known_bits_t union_(known_bits_t a, known_bits_t b);
constraints_t union_(constraints_t a, constraints_t b);

// Fairly expensive; use sparingly.
bool is_subset(bounds_t small, bounds_t big);
bool is_subset(known_bits_t small, known_bits_t big);
bool is_subset(constraints_t small, constraints_t big, constraints_mask_t cm);

bounds_t tighten_bounds(constraints_t c, constraints_mask_t cm);
constraints_t normalize(constraints_t c, constraints_mask_t cm);

struct constraints_mask_t 
{
    fixed_uint_t mask = ~0ull;
    bool signed_ = false;

    constexpr auto operator<=>(constraints_mask_t const&) const = default;
};

constexpr constraints_mask_t BOOL_MASK = { 1ull << fixed_t::shift, false };
constexpr constraints_mask_t CARRY_MASK = BOOL_MASK;
constexpr constraints_mask_t REAL_MASK = { numeric_bitmask(TYPE_REAL), true };

inline constraints_mask_t type_constraints_mask(type_name_t type) 
    { return { numeric_bitmask(type), is_signed(type) }; }

struct bounds_t
{
    using int_type = fixed_sint_t;

    int_type min;
    int_type max;

    // Construction
    constexpr static bounds_t top() 
        { return { 1, 0 }; }
    constexpr static bounds_t bottom(constraints_mask_t cm) 
        { return cm.signed_ ? bounds_t{ -int_type(high_bit_only(cm.mask)), ((cm.mask >> 1) & cm.mask) } : bounds_t{ 0, cm.mask }; }
    constexpr static bounds_t const_(fixed_sint_t fixed) 
        { return { fixed, fixed }; }
    constexpr static bounds_t whole(fixed_sint_t fixed) 
        { return const_(fixed << fixed_t::shift); }
    static constexpr bounds_t bool_(bool b) 
        { return { fixed_t::whole(b).value, fixed_t::whole(b).value }; }
    static constexpr bounds_t any_bool()
        { return { 0, fixed_t::whole(1).value }; }

    // Predicate
    constexpr bool is_top() const 
        { return min > max; }
    constexpr bool is_const() const 
        { return min == max; }
    constexpr bool bit_eq(bounds_t o) const
        { return min == o.min && max == o.max; }
    constexpr bool in_mask(constraints_mask_t cm) const
        { bounds_t const b = bottom(cm); return min >= b.min && max <= b.max; }
    constexpr bool operator()(fixed_uint_t fixed, constraints_mask_t) const
        { return check(static_cast<fixed_sint_t>(fixed)); }
    constexpr bool check(fixed_sint_t fixed) const
        { return fixed >= min && fixed <= max; }

    // Data
    constexpr fixed_uint_t get_const() const 
        { assert(is_const()); return min; }
    constexpr fixed_uint_t umin() const { return min; }
    constexpr fixed_uint_t umax() const { return max; }
};

struct known_bits_t
{
    fixed_uint_t known0;
    fixed_uint_t known1;

    // Construction
    constexpr static known_bits_t top() 
        { return { ~0ull, ~0ll }; }
    constexpr static known_bits_t bottom(constraints_mask_t cm) 
        { return { ~cm.mask, 0 }; }
    constexpr static known_bits_t const_(fixed_sint_t fixed, constraints_mask_t cm) 
        { return { ~(fixed & cm.mask), fixed & cm.mask }; }
    constexpr static known_bits_t whole(fixed_sint_t fixed, constraints_mask_t cm) 
        { return const_(fixed << fixed_t::shift, cm); }
    static constexpr known_bits_t bool_(bool b)
        { return whole(b, { 1ull << fixed_t::shift, false }); }
    static constexpr known_bits_t any_bool()
        { return { ~fixed_t::whole(1).value, 0 }; }

    // Predicate
    constexpr bool is_top() const 
        { return (known0 & known1) != 0; }
    constexpr bool is_const() const 
        { return ~(known0 ^ known1) == 0; }
    constexpr bool bit_eq(known_bits_t o) const
        { return known0 == o.known0 && known1 == o.known1; }
    constexpr bool in_mask(constraints_mask_t cm) const
        { return ((known1 & cm.mask) == known1) && ((known0 & ~cm.mask) == ~cm.mask); }
    constexpr bool operator()(fixed_uint_t fixed, constraints_mask_t cm) const
    {
        fixed &= cm.mask;
        return (fixed & known1) == known1 && (~fixed & known0) == known0;
    }

    // Data
    constexpr fixed_uint_t get_const() const 
        { assert(is_const()); return known1; }
    constexpr fixed_uint_t known() const
        { return known0 | known1; }

    // Lowest possible number in this set
    fixed_sint_t min(constraints_mask_t cm) const
    { 
        assert(!is_top());
        assert(in_mask(cm));

        if(cm.signed_)
        {
            fixed_uint_t const sign_bit = high_bit_only(cm.mask);
            return sign_extend(known1 | (sign_bit & ~known0), cm.mask);
        }
        else
            return static_cast<fixed_sint_t>(known1);
    }

    // Highest possible number in this set
    fixed_sint_t max(constraints_mask_t cm) const
    { 
        assert(!is_top());
        assert(in_mask(cm));

        if(cm.signed_)
        {
            fixed_uint_t const sign_bit = high_bit_only(cm.mask);
            return sign_extend(~(known0 | sign_bit) | (known1 & sign_bit), cm.mask);
        }
        else
            return static_cast<fixed_sint_t>(~known0);
    }
};

struct constraints_t
{
    bounds_t bounds;
    known_bits_t bits;

    // Construction
    constexpr static constraints_t top() 
        { return { bounds_t::top(), known_bits_t::top() }; }
    constexpr static constraints_t bottom(constraints_mask_t cm) 
        { return { bounds_t::bottom(cm), known_bits_t::bottom(cm) }; }
    constexpr static constraints_t const_(fixed_sint_t fixed, constraints_mask_t cm) 
        { return { bounds_t::const_(fixed), known_bits_t::const_(fixed, cm) }; }
    constexpr static constraints_t whole(fixed_sint_t fixed, constraints_mask_t cm) 
        { return { bounds_t::whole(fixed), known_bits_t::whole(fixed, cm) }; }

    static constexpr constraints_t bool_(bool b) { return whole(b, CARRY_MASK); }
    static constexpr constraints_t any_bool()
        { return { bounds_t::any_bool(), known_bits_t::any_bool() }; }

    static constraints_t carry(carry_t cr)
    {
        switch(cr)
        {
        case CARRY_BOTTOM: return any_bool();
        case CARRY_CLEAR:  return bool_(false);
        case CARRY_SET:    return bool_(true);
        default:
        case CARRY_TOP:    return top();
        }
    }

    static constraints_t shifted_carry(carry_t cr, fixed_uint_t mask = 1)
    {
        assert(mask);
        fixed_uint_t const i = ~(mask << 1ull) & mask;
        switch(cr)
        {
        case CARRY_BOTTOM: return {{ 0, i }, {    ~i, 0 }};
        case CARRY_CLEAR:  return {{ 0, 0 }, { ~0ull, 0 }};
        case CARRY_SET:    return {{ i, i }, {    ~i, i }};
        default:
        case CARRY_TOP:    return top();
        }
    }

    // Predicate
    constexpr bool is_top(constraints_mask_t cm) const 
        { return (bounds.is_top() || bits.is_top() || bits.min(cm) > bounds.max || bits.max(cm) < bounds.min); }
    constexpr bool is_const() const 
        { return bounds.is_const() || bits.is_const(); }
    constexpr bool bit_eq(constraints_t o) const
        { return (bounds.bit_eq(o.bounds) && bits.bit_eq(o.bits)); }
    bool normal_eq(constraints_t o, constraints_mask_t cm) const
        { return ::normalize(*this, cm).bit_eq(::normalize(o, cm)); }
    constexpr bool in_mask(constraints_mask_t cm) const
        { return bounds.in_mask(cm) && bits.in_mask(cm); }
    bool is_normalized(constraints_mask_t cm) const // Relatively expensive; don't use often
        { return bit_eq(::normalize(*this, cm)); }
    constexpr bool operator()(fixed_uint_t fixed, constraints_mask_t cm) const
        { return bounds(fixed, cm) && bits(fixed, cm); }

    // Data
    fixed_uint_t get_const() const
    { 
        assert(is_const());
        return bounds.is_const() ? bounds.get_const() : bits.get_const();
    }

    carry_t to_carry() const
    {
        if(is_top({}))
            return CARRY_TOP;
        if(is_const())
            return get_const() ? CARRY_SET : CARRY_CLEAR;
        return CARRY_BOTTOM;
    }

    // Self-Modification
    void normalize(constraints_mask_t cm) { *this = ::normalize(*this, cm); }
};

using constraints_vec_t = bc::small_vector<constraints_t, 2>;

struct constraints_def_t
{
    constraints_mask_t cm;
    constraints_vec_t vec;

    constraints_t const& operator[](unsigned i) const { assert(i < vec.size()); return vec[i]; }
    constraints_t& operator[](unsigned i) { assert(i < vec.size()); return vec[i]; }
};

bool any_top(constraints_def_t const& def);
bool all_normalized(constraints_def_t const& def);
bool all_subset(constraints_vec_t const& a, constraints_vec_t const& b, constraints_mask_t cm);
bool bit_eq(constraints_vec_t const& a, constraints_vec_t const& b);

using abstract_fn_t = std::type_identity_t<
    void(constraints_def_t const*, unsigned, constraints_def_t&)>;
extern std::array<abstract_fn_t*, NUM_SSA_OPS> const abstract_fn_table;
[[gnu::pure]] inline abstract_fn_t* abstract_fn(ssa_op_t op) 
    { return abstract_fn_table[op]; }

using narrow_fn_t = std::type_identity_t<
    void(constraints_def_t*, unsigned, constraints_def_t const&)>;
extern std::array<narrow_fn_t*, NUM_SSA_OPS> const narrow_fn_table;
[[gnu::pure]] inline narrow_fn_t* narrow_fn(ssa_op_t op) 
    { return narrow_fn_table[op]; }

#endif
