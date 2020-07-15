#ifndef CONSTRAINTS_HPP
#define CONSTRAINTS_HPP

// Structs that track approximate sets of values, to be used in const
// propagation to determine constants.
// (AKA this is an abstract domain.)

// The representation is the intersection of an interval domain
// and a "known bits" domain. The "known bits" domain represents sets of
// numbers that share some common bit pattern.

#include <ostream>

#include "carry.hpp"
#include "fixed.hpp"
#include "ssa_op.hpp"

struct bounds_t;
struct known_bits_t;
struct constraints_t;

std::string to_string(bounds_t const& b);
std::string to_string(constraints_t const& c);
std::string to_string(known_bits_t const& b);

std::ostream& operator<<(std::ostream& o, bounds_t const& b);
std::ostream& operator<<(std::ostream& o, known_bits_t const& b);
std::ostream& operator<<(std::ostream& o, constraints_t const& c);

bounds_t apply_mask(fixed_int_t mask, bounds_t b);
known_bits_t apply_mask(fixed_int_t mask, known_bits_t b);
constraints_t apply_mask(fixed_int_t mask, constraints_t c);

bounds_t from_bits(known_bits_t bits);
known_bits_t from_bounds(bounds_t bounds, fixed_int_t first_bit = 0);

bounds_t intersect(bounds_t a, bounds_t b);
known_bits_t intersect(known_bits_t a, known_bits_t b);
constraints_t intersect(constraints_t a, constraints_t b);

bounds_t union_(bounds_t a, bounds_t b);
known_bits_t union_(known_bits_t a, known_bits_t b);
constraints_t union_(constraints_t a, constraints_t b);

// Fairly expensive; use sparingly.
bool is_subset(bounds_t small, bounds_t big);
bool is_subset(known_bits_t small, known_bits_t big);
bool is_subset(constraints_t small, constraints_t big);

bounds_t tighten_bounds(constraints_t c);
constraints_t normalize(constraints_t c);

using abstract_fn_t = std::type_identity_t<
    constraints_t(fixed_int_t, constraints_t const*, unsigned)>;
extern std::array<abstract_fn_t*, NUM_SSA_OPS> const abstract_fn_table;
[[gnu::pure]] inline abstract_fn_t* abstract_fn(ssa_op_t op) 
    { return abstract_fn_table[op]; }

using narrow_fn_t = std::type_identity_t<
    void(fixed_int_t, constraints_t, constraints_t*, unsigned)>;
extern std::array<narrow_fn_t*, NUM_SSA_OPS> const narrow_fn_table;
[[gnu::pure]] inline narrow_fn_t* narrow_fn(ssa_op_t op) 
    { return narrow_fn_table[op]; }

struct bounds_t
{
    fixed_int_t min;
    fixed_int_t max;

    // Construction
    constexpr static bounds_t top() 
        { return { ~0ull, 0 }; }
    constexpr static bounds_t bottom(fixed_int_t mask) 
        { return { 0, mask }; }
    constexpr static bounds_t const_(fixed_int_t fixed) 
        { return { fixed, fixed}; }
    constexpr static bounds_t whole(fixed_int_t fixed) 
        { return { fixed << fixed_t::shift, fixed << fixed_t::shift} ; }
    static constexpr bounds_t any_bool()
        { return { 0, fixed_t::whole(1).value }; }

    // Predicate
    constexpr bool is_top() const 
        { return min > max; }
    constexpr bool is_const() const 
        { return min == max; }
    constexpr bool bit_eq(bounds_t o) const
        { return min == o.min && max == o.max; }
    constexpr bool operator()(fixed_int_t fixed) const
        { return fixed >= min && fixed <= max; }

    // Data
    constexpr fixed_int_t get_const() const 
        { assert(is_const()); return min; }
};

struct known_bits_t
{
    fixed_int_t known0;
    fixed_int_t known1;

    // Construction
    constexpr static known_bits_t top() 
        { return { ~0ull, ~0ll }; }
    constexpr static known_bits_t bottom(fixed_int_t mask) 
        { return { ~mask, 0 }; }
    constexpr static known_bits_t const_(fixed_int_t fixed) 
        { return { ~fixed, fixed}; }
    constexpr static known_bits_t whole(fixed_int_t fixed) 
        { return { ~(fixed << fixed_t::shift), fixed << fixed_t::shift }; }
    static constexpr known_bits_t any_bool()
        { return { ~fixed_t::whole(1).value, 0 }; }

    // Predicate
    constexpr bool is_top() const 
        { return (known0 & known1) != 0; }
    constexpr bool is_const() const 
        { return ~(known0 ^ known1) == 0; }
    constexpr bool bit_eq(known_bits_t o) const
        { return known0 == o.known0 && known1 == o.known1; }
    constexpr bool operator()(fixed_int_t fixed) const
        { return (fixed & known1) == known1 && (~fixed & known0) == known0; }

    // Data
    constexpr fixed_int_t get_const() const 
        { assert(is_const()); return known1; }
    constexpr fixed_int_t known() const
        { return known0 | known1; }
};

struct constraints_t
{
    bounds_t bounds;
    known_bits_t bits;
    carry_t carry;

    // Construction
    constexpr static constraints_t top() 
    { 
        return { bounds_t::top(), known_bits_t::top(), CARRY_TOP }; 
    }

    constexpr static constraints_t bottom(fixed_int_t mask) 
    { 
        return 
        { 
            bounds_t::bottom(mask), 
            known_bits_t::bottom(mask), 
            CARRY_BOTTOM 
        };
    }

    constexpr static constraints_t const_(fixed_int_t fixed, carry_t cr) 
    { 
        return { bounds_t::const_(fixed), known_bits_t::const_(fixed), cr };
    }

    constexpr static constraints_t whole(fixed_int_t fixed, carry_t cr) 
    { 
        return { bounds_t::whole(fixed), known_bits_t::whole(fixed), cr };
    }

    static constexpr constraints_t any_bool(carry_t cr)
    { 
        return { bounds_t::any_bool(), known_bits_t::any_bool(), cr };
    }

    // Predicate
    constexpr bool is_top() const 
    { 
        return (carry == CARRY_TOP 
                || bounds.is_top() || bits.is_top()
                || bits.known1 > bounds.max 
                || ~bits.known0 < bounds.min);
    }

    constexpr bool is_val_const() const 
        { return bounds.is_const() || bits.is_const(); }

    fixed_int_t get_val_const() const
    { 
        assert(is_val_const());
        return bounds.is_const() ? bounds.get_const() : bits.get_const();
    }

    constexpr bool is_carry_const() const
        { return carry_const(carry); }
    constexpr bool get_carry_const() const
    { 
        assert(is_carry_const());
        return carry == CARRY_SET;
    }

    constexpr bool bit_eq(constraints_t o) const
    { 
        return (carry == o.carry
                && bounds.bit_eq(o.bounds) 
                && bits.bit_eq(o.bits));
    }
    bool normal_eq(constraints_t o) const
        { return ::normalize(*this).bit_eq(::normalize(o)); }
    constexpr bool operator()(fixed_int_t fixed) const
        { return bounds(fixed) && bits(fixed); }
    bool is_normalized() const // Relatively expensive; don't use often.
        { return bit_eq(::normalize(*this)); }

    // Self-Modification
    void normalize() { *this = ::normalize(*this); }
};

#endif
