#include "constraints.hpp"

#include <algorithm>
#include <string>
#include <ostream> // TODO
#include <iostream> // TODO

#include "builtin.hpp"
#include "format.hpp"

extern std::uint8_t const add_constraints_table[1024];

// For debugging mostly
std::string to_string(bounds_t const& b)
{
    return fmt("[%, %]", to_double(fixed_t{ b.min }), 
                         to_double(fixed_t{ b.max }));
}

// For debugging mostly
std::string to_string(known_bits_t const& b)
{
    fixed_int_t const known = b.known();
    std::string str;
    for(fixed_int_t i = 0; i < sizeof_bits<fixed_int_t>; ++i)
    {
        if(i > 0 && i % 8 == 0)
            str.push_back(' ');
        fixed_int_t bit = 1ull << i;
        if(known & bit)
        {
            if(b.known0 & b.known1 & bit)
                str.push_back('T');
             else if(b.known0 & bit)
                str.push_back('0');
             else
                str.push_back('1');
        }
        else
            str.push_back('?');
    }
    std::reverse(str.begin(), str.end());
    return str;
}

// For debugging mostly
std::string to_string(constraints_t const& c) 
{
    return fmt("{ %, %, % }%", 
               to_string(c.bits), 
               to_string(c.bounds), 
               c.is_const() ? " (CONST)" : "");
}

// For debugging mostly
std::ostream& operator<<(std::ostream& o, bounds_t const& b)
{
    o << to_string(b);
    return o;
}

// For debugging mostly
std::ostream& operator<<(std::ostream& o, known_bits_t const& b)
{
    o << to_string(b);
    return o;
}

// For debugging mostly
std::ostream& operator<<(std::ostream& o, constraints_t const& c)
{
    o << to_string(c);
    return o;
}

bounds_t apply_mask(fixed_int_t mask, bounds_t b)
{
    if(b.is_top())
        return bounds_t::top();
    if(b.max > mask)
    {
        fixed_int_t span = b.max - b.min;
        b.min &= mask;
        b.max = b.min + span;
        if(b.max > mask)
            b = { 0, mask };
    }
    assert(!b.is_top());
    return b;
}

known_bits_t apply_mask(fixed_int_t mask, known_bits_t b)
{
    if(b.is_top())
        return known_bits_t::top();
    b.known0 |= ~mask;
    b.known1 &= mask;
    assert(!b.is_top());
    return b;
}

constraints_t apply_mask(fixed_int_t mask, constraints_t c)
{
    return { apply_mask(mask, c.bounds), apply_mask(mask, c.bits) };
}

bounds_t from_bits(known_bits_t bits)
{
    if(bits.is_top())
        return bounds_t::top();
    bounds_t ret = { bits.known1, ~bits.known0 };
    assert(!ret.is_top());
    return ret;
}

known_bits_t from_bounds(bounds_t bounds, fixed_int_t first_bit)
{
    if(bounds.is_top())
        return known_bits_t::top();
    // Find upper bits that are the same.
    fixed_int_t x = bounds.min ^ bounds.max;
    fixed_int_t mask = ~0ull;
    if(x)
        mask = ~((1ull << (fixed_int_t)builtin::rclz(x)) - 1ull);
    assert((x & mask) == 0ull);
    known_bits_t ret;
    ret.known0 = ~bounds.min & mask;
    ret.known1 = bounds.min & mask;
    assert(!ret.is_top());
    return ret;
}

bounds_t intersect(bounds_t a, bounds_t b)
{
    return { std::max(a.min, b.min), std::min(a.max, b.max) };
}

known_bits_t intersect(known_bits_t a, known_bits_t b)
{
    return { a.known0 | b.known0, a.known1 | b.known1 };
}

constraints_t intersect(constraints_t a, constraints_t b)
{
    return 
    { 
        intersect(a.bounds, b.bounds),
        intersect(a.bits, b.bits),
    };
}

bounds_t union_(bounds_t a, bounds_t b)
{
    if(a.is_top())
        return b;
    if(b.is_top())
        return a;
    return { std::min(a.min, b.min), std::max(a.max, b.max) };
}

known_bits_t union_(known_bits_t a, known_bits_t b)
{
    if(a.is_top())
        return b;
    if(b.is_top())
        return a;
    return { a.known0 & b.known0, a.known1 & b.known1 };
}

constraints_t union_(constraints_t a, constraints_t b)
{
    return 
    { 
        union_(a.bounds, b.bounds),
        union_(a.bits, b.bits),
    };
}

bool is_subset(bounds_t small, bounds_t big)
{
    if(big.is_top())
        return small.is_top();
    if(small.is_top())
        return true;
    return small.min >= big.min && small.max <= big.max;
}

bool is_subset(known_bits_t small, known_bits_t big)
{
    if(big.is_top())
        return small.is_top();
    if(small.is_top())
        return true;
    return (intersect(big, small).bit_eq(small)
            && union_(small, big).bit_eq(big));
}

bool is_subset(constraints_t small, constraints_t big)
{
    return (intersect(big, small).normal_eq(small)
            && union_(small, big).normal_eq(big));
}

// Narrows bounds based on the constraint's bits.
// e.g. if bounds are [1, 5] and bit 0 is known to be 0, narrows to [2, 4].
bounds_t tighten_bounds(constraints_t c)
{
    fixed_int_t const known = c.bits.known();
    c.bounds.max = ~c.bounds.max;

    fixed_int_t const bit_min = builtin::ctz(known);
    fixed_int_t const bit_max = builtin::rclz(known);

    for(fixed_int_t i = bit_min; i < bit_max; ++i)
    {
        fixed_int_t const bit = (1ull << i) & known;
        if((c.bounds.min ^ c.bits.known1) & bit)
        {
            c.bounds.min += bit;
            c.bounds.min &= ~(bit - 1) | known;
        }
        if((c.bounds.max ^ c.bits.known0) & bit)
        {
            c.bounds.max += bit;
            c.bounds.max &= ~(bit - 1) | known;
        }
    }

    c.bounds.max = ~c.bounds.max;
    return c.bounds;
}

static constraints_t normalize_impl(constraints_t c)
{
    c.bounds = intersect(c.bounds, from_bits(c.bits));
    c.bits = intersect(c.bits, from_bounds(c.bounds));
    c.bounds = tighten_bounds(c);
    c.bits = intersect(c.bits, from_bounds(c.bounds));
    if(c.is_top())
        return constraints_t::top();
    return c;
}

constraints_t normalize(constraints_t c)
{
    constraints_t ret = normalize_impl(c);
    assert(ret.bit_eq(normalize_impl(ret)));
    return ret;
}

bool bit_eq(constraints_vec_t const& a, constraints_vec_t const& b)
{
    if(a.size() != b.size())
        return false;
    for(unsigned i = 0; i < a.size(); ++i)
        if(!a[i].bit_eq(b[i]))
            return false;
    return true;
}

bool all_subset(constraints_vec_t const& a, constraints_vec_t const& b)
{
    if(a.size() != b.size())
        return false;
    for(unsigned i = 0; i < a.size(); ++i)
        if(!is_subset(a[i], b[i]))
            return false;
    return true;
}

bool all_normalized(constraints_vec_t const& cv)
{
    for(constraints_t const& c : cv)
        if(!c.is_normalized())
            return false;
    return true;
}

bool any_top(constraints_vec_t const& cv)
{
    for(constraints_t const& c : cv)
        if(c.is_top())
            return true;
    return false;
}

static bool any_top(constraints_def_t const* cv, unsigned argn)
{
    for(unsigned i = 0; i < argn; ++i)
        if(any_top(cv[i].vec))
            return true;
    return false;
}

static bool handle_top(constraints_def_t const* cv, unsigned argn, 
                       constraints_def_t& r)
{
    if(any_top(cv, argn))
    {
        r.vec.assign(r.vec.size(), constraints_t::top());
        return true;
    }
    return false;
}

template<ssa_op_t>
static constexpr abstract_fn_t* abstract_fn_v = nullptr;

template<ssa_op_t>
static constexpr narrow_fn_t* narrow_fn_v = nullptr;

#define ABSTRACT_FN [](constraints_def_t const* cv,\
                       unsigned argn, constraints_def_t& result)
#define ABSTRACT(op) template<> constexpr abstract_fn_t* abstract_fn_v<op>

#define NARROW_FN [](constraints_def_t* cv, unsigned argn, \
                     constraints_def_t const& result)
#define NARROW(op) template<> constexpr narrow_fn_t* narrow_fn_v<op>

static constexpr auto abstract_bottom = ABSTRACT_FN
{
    assert(result.vec.size() >= 1);
    for(constraints_t& constraint : result.vec)
        constraint = constraints_t::bottom(result.mask);
};

ABSTRACT(SSA_read_global) = abstract_bottom;
ABSTRACT(SSA_fn_call) = abstract_bottom;
ABSTRACT(SSA_uninitialized) = abstract_bottom;

ABSTRACT(SSA_carry) = ABSTRACT_FN
{
    result[0] = cv[0][1];
};

ABSTRACT(SSA_write_array) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    auto& input_array = cv[0];
    constraints_t const index = cv[1][0];
    constraints_t const value = cv[2][0];

    assert(input_array.vec.size() == result.vec.size());

    result.vec = input_array.vec;

    if(index.is_const())
    {
        unsigned const i = index.bounds.min >> fixed_t::shift;
        assert(i < 256); // Arrays can't be larger than 256.
        if(i < result.vec.size())
            result[i] = value;
    }
    else
    {
        unsigned const min_bound = index.bounds.min >> fixed_t::shift;
        unsigned const max_bound = index.bounds.max >> fixed_t::shift;
        unsigned const iter_to = std::min<unsigned>(max_bound + 1, result.vec.size());

        for(unsigned i = min_bound; i < iter_to; ++i)
            if(index(fixed_t::whole(i).value))
                result[i] = union_(result[i], value);
    }
};

ABSTRACT(SSA_read_array) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    auto& input_array = cv[0];
    constraints_t const index = cv[1][0];

    unsigned const min_bound = index.bounds.min >> fixed_t::shift;
    unsigned const max_bound = index.bounds.max >> fixed_t::shift;
    unsigned const iter_to = std::min<unsigned>(max_bound + 1, input_array.vec.size());

    result[0] = constraints_t::top();
    for(unsigned i = min_bound; i < iter_to; ++i)
        if(index(fixed_t::whole(i).value))
            result[0] = union_(input_array[i], result[0]);
};

ABSTRACT(SSA_init_array) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    auto& input_array = cv[0];
    assert(input_array.vec.size() == result.vec.size());
    result = input_array;
};

ABSTRACT(SSA_init_array_fill) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    constraints_t const value = cv[0][0];
    result.vec.assign(result.vec.size(), value);
};

ABSTRACT(SSA_phi) = ABSTRACT_FN
{
    assert(argn >= 1);

    for(unsigned i = 0; i < result.vec.size(); ++i)
    {
        result[i] = constraints_t::top();
        for(unsigned j = 0; j < argn; ++j)
        {
            assert(cv[j].vec.size());
            assert(cv[j].vec.size() >= result.vec.size());
            result[i] = union_(result[i], cv[j][i]);
        }
    }
};

ABSTRACT(SSA_cast) = ABSTRACT_FN
{
    assert(argn == 1);
    result[0] = apply_mask(result.mask, cv[0][0]); // handles top itself
};

ABSTRACT(SSA_add) = ABSTRACT_FN
{
    assert(argn == 3);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    fixed_int_t const mask = result.mask;
    constraints_t const L = cv[0][0];
    constraints_t const R = cv[1][0];
    constraints_t const C = cv[2][0];
    constraints_t const shifted_C = 
        constraints_t::shifted_carry(C.to_carry(), mask);

    assert(L.is_normalized());
    assert(R.is_normalized());
    assert(C.is_normalized());

    // Outputs:
    constraints_t& value = result[0];
    constraints_t& carry = result[1];

    // If we know bits in L and R, we can determine which bits are
    // known in the output.
    //
    // SLOW BUT ACCURATE TECHNIQUE (used)
    // - Treat constraints as ternary. Trits can be 0, 1, or ? (unknown value).
    // - Add using the standard arithmetic algorithm, starting at the
    //   rightmost bit and working left, carrying as you go.
    // - When adding to a ?, the output trit is always ?, and the carry 
    //   depends on the other value. Carrying a ? is allowed!
    // - Performance is improved slightly by using lookup tables.
    //
    // FAST BUT INACCURATE TECHNIQUE (not used)
    // - If the rightmost bits of both L and R are known, we know
    //   the rightmost bits of the output.
    // - If we know both bits along with the carry, we know the outpit bit.
    //   (The carry is known when L bit == R bit.)
    //
    // (FAST is about 10x faster than SLOW)

    value = {};

    fixed_int_t const neg_mask = ~(L.bits.known0 & R.bits.known0 
                                   & shifted_C.bits.known0) & mask;
    std::uint64_t const start_i = neg_mask ? builtin::ctz(neg_mask) : 0;
    std::uint64_t const end_i = ((1 + (mask ? builtin::rclz(mask) 
                                             : sizeof_bits<fixed_int_t>))
                                 & ~1ull);
    value.bits.known0 = (1ull << start_i) - 1ull;

    known_bits_t lhs_bits = L.bits;
    known_bits_t rhs_bits = R.bits;

    lhs_bits.known0 >>= start_i;
    lhs_bits.known1 >>= start_i;
    rhs_bits.known0 >>= start_i;
    rhs_bits.known1 >>= start_i;

    fixed_int_t i = start_i;
    // 'j' holds the carry.
    fixed_int_t j = C.to_carry();
    assert(j <= 4);
    assert(C.to_carry() != CARRY_TOP);
    for(; i < end_i; i += 2ull)
    {
        j |= (lhs_bits.known0 & 0b11) << 2ull;
        j |= (lhs_bits.known1 & 0b11) << 4ull;
        j |= (rhs_bits.known0 & 0b11) << 6ull;
        j |= (rhs_bits.known1 & 0b11) << 8ull;

        j = add_constraints_table[j];
        value.bits.known0 |= ((j >> 2ull) & 0b11ull) << i;
        value.bits.known1 |= ((j >> 4ull) & 0b11ull) << i;
        j &= 0b11;

        lhs_bits.known0 >>= 2ull;
        lhs_bits.known1 >>= 2ull;
        rhs_bits.known0 >>= 2ull;
        rhs_bits.known1 >>= 2ull;
    }
    assert((carry_t)j != CARRY_TOP);
    carry = constraints_t::carry((carry_t)j);
    if(i < sizeof_bits<fixed_int_t>)
        value.bits.known0 |= ~((1ull << i) - 1ull);

    value.bits = apply_mask(mask, value.bits);
    assert(!value.bits.is_top());

    // OK! Now for the bounds.
    // Calculate 'min' and 'max.
    if(builtin::add_overflow(L.bounds.max, R.bounds.max, value.bounds.max)
    || builtin::add_overflow(value.bounds.max, shifted_C.bounds.max, 
                             value.bounds.max))
    {
        // Value overflowed. Derive min/max from known bits.
        value.bounds = apply_mask(mask, from_bits(value.bits));
        assert(!value.is_top());
        normalize(value);
        return;
    }

    value.bounds.min = L.bounds.min + R.bounds.min + shifted_C.bounds.min;
    value.bounds = apply_mask(mask, value.bounds);

    // 'min' and 'max' put constraints on the bits - neat!
    // Calculate those constraints here.
    assert(apply_mask(mask, value.bits).bit_eq(value.bits));
    known_bits_t bounds_bits = apply_mask(mask, from_bounds(value.bounds));
    assert(!bounds_bits.is_top());
    assert(apply_mask(mask, bounds_bits).bit_eq(bounds_bits));

    assert(apply_mask(mask, value.bits).bit_eq(value.bits));
    value.bits.known0 |= bounds_bits.known0;
    value.bits.known1 |= bounds_bits.known1;

    assert(!value.bounds.is_top());
    assert(!value.bits.is_top());
    assert(apply_mask(mask, value.bits).bit_eq(value.bits));
    normalize(value);
};

known_bits_t abstract_and(known_bits_t lhs, known_bits_t rhs, fixed_int_t mask)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    return { .known0 = (lhs.known0 | rhs.known0) | ~mask,
             .known1 = (lhs.known1 & rhs.known1) & mask };
}

ABSTRACT(SSA_and) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    result[0].bits = abstract_and(cv[0][0].bits, cv[1][0].bits, result.mask);
    result[0].bounds = from_bits(result[0].bits);
    assert(result[0].bounds.max <= result.mask && result[0].is_normalized());
};

known_bits_t abstract_or(known_bits_t lhs, known_bits_t rhs, fixed_int_t mask)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    return { .known0 = (lhs.known0 & rhs.known0) | ~mask,
             .known1 = (lhs.known1 | rhs.known1) & mask };
}

ABSTRACT(SSA_or) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(cv[0].vec.size() >= 1);
    assert(cv[1].vec.size() >= 1);
    result[0].bits = abstract_or(cv[0][0].bits, cv[1][0].bits, result.mask);
    result[0].bounds = from_bits(result[0].bits);
    assert(result[0].bounds.max <= result.mask && result[0].is_normalized());
};

known_bits_t abstract_xor(known_bits_t lhs, known_bits_t rhs, fixed_int_t mask)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    fixed_int_t const known = lhs.known() & rhs.known() & mask;
    fixed_int_t const x = lhs.known1 ^ rhs.known1;
    return { (~x & known) | ~mask, x & known };
}

ABSTRACT(SSA_xor) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    result[0].bits = abstract_xor(cv[0][0].bits, cv[1][0].bits, result.mask);
    result[0].bounds = from_bits(result[0].bits);
    assert(result[0].bounds.max <= result.mask && result[0].is_normalized());
};

known_bits_t abstract_eq(constraints_t lhs, constraints_t rhs)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    if(lhs.bits.known0 & rhs.bits.known1)
        return known_bits_t::bool_(false);
    if(lhs.bits.known1 & rhs.bits.known0)
        return known_bits_t::bool_(false);
    if(lhs.bounds.min > rhs.bounds.max 
       || lhs.bounds.max < rhs.bounds.min)
        return known_bits_t::bool_(false);
    if(lhs.is_const() && rhs.is_const() && lhs.get_const() == rhs.get_const())
        return known_bits_t::bool_(true);
    return known_bits_t::any_bool();
}

ABSTRACT(SSA_eq) = ABSTRACT_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;

    known_bits_t bits = known_bits_t::bool_(true);
    for(unsigned i = 0; i < argn; i += 2)
        bits = abstract_and(bits, abstract_eq(cv[i][0], cv[i+1][0]), ~0ull);

    result[0].bits = bits;
    result[0].bounds = from_bits(bits);
};

known_bits_t abstract_not_eq(constraints_t lhs, constraints_t rhs)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    if(lhs.bits.known0 & rhs.bits.known1)
        return known_bits_t::bool_(true);
    if(lhs.bits.known1 & rhs.bits.known0)
        return known_bits_t::bool_(true);
    if(lhs.bounds.min > rhs.bounds.max 
       || lhs.bounds.max < rhs.bounds.min)
        return known_bits_t::bool_(true);
    if(lhs.is_const() && rhs.is_const() 
       && lhs.get_const() == rhs.get_const())
        return known_bits_t::bool_(false);
    return known_bits_t::any_bool();
}

ABSTRACT(SSA_not_eq) = ABSTRACT_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;

    known_bits_t bits = known_bits_t::bool_(false);
    for(unsigned i = 0; i < argn; i += 2)
        bits = abstract_or(bits, abstract_not_eq(cv[i][0], cv[i+1][0]), ~0ull);

    result[0].bits = bits;
    result[0].bounds = from_bits(bits);
};

constraints_t abstract_lt(constraints_t lhs, constraints_t rhs)
{
    if(lhs.is_top() || rhs.is_top())
        return constraints_t::top();
    if(lhs.bounds.max < rhs.bounds.min)
        return constraints_t::bool_(true);
    if(rhs.bounds.max <= lhs.bounds.min)
        return constraints_t::bool_(false);
    return constraints_t::any_bool();
}

ABSTRACT(SSA_lt) = ABSTRACT_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);
    assert(argn > 0);

    if(handle_top(cv, argn, result))
        return;

    for(int i = (int)argn - 2; i > 0; i -= 2)
    {
        auto eq = abstract_eq(cv[i][0], cv[i+1][0]);
        if(eq.is_const())
        {
            if(!eq.get_const())
            {
                result[0] = abstract_lt(cv[i][0], cv[i+1][0]);
                return;
            }
        }
        else
        {
            result[0] = constraints_t::any_bool();
            return;
        }
    }

    // The only way to get here is if all higher comparisons were equal
    result[0] = abstract_lt(cv[0][0], cv[1][0]);
};

constraints_t abstract_lte(constraints_t lhs, constraints_t rhs)
{
    if(lhs.is_top() || rhs.is_top())
        return constraints_t::top();
    if(lhs.bounds.max <= rhs.bounds.min)
        return constraints_t::bool_(true);
    if(rhs.bounds.max < lhs.bounds.min)
        return constraints_t::bool_(false);
    return constraints_t::any_bool();
}

ABSTRACT(SSA_lte) = ABSTRACT_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);
    assert(argn > 0);

    if(handle_top(cv, argn, result))
        return;

    for(int i = (int)argn - 2; i > 0; i -= 2)
    {
        auto eq = abstract_eq(cv[i][0], cv[i+1][0]);
        if(eq.is_const())
        {
            if(!eq.get_const())
            {
                result[0] = abstract_lte(cv[i][0], cv[i+1][0]);
                return;
            }
        }
        else
        {
            result[0] = constraints_t::any_bool();
            return;
        }
    }

    // The only way to get here is if all higher comparisons were equal
    result[0] = abstract_lt(cv[0][0], cv[1][0]);
};

static constexpr auto narrow_bottom = NARROW_FN
{
    constraints_t& input = cv[0][0];
    input.bits.known0 |= result[0].bits.known0 & cv[0].mask;
    input.bits.known1 |= result[0].bits.known1 & cv[0].mask;
};

NARROW(SSA_uninitialized) = narrow_bottom;
NARROW(SSA_fn_call) = narrow_bottom;
NARROW(SSA_cast) = narrow_bottom;

NARROW(SSA_carry) = NARROW_FN
{
    cv[0][1] = intersect(cv[0][1], result[0]);
};

NARROW(SSA_phi) = NARROW_FN
{
    for(unsigned i = 0; i < result.vec.size(); ++i)
    for(unsigned j = 0; j < argn; ++j)
    {
        assert(cv[j].vec.size() >= result.vec.size());
        assert(cv[j].mask == result.mask);
        cv[j][i] = intersect(cv[j][i], result[i]);
    }
};

NARROW(SSA_add) = NARROW_FN
{
    assert(argn == 3 && result.vec.size() >= 2);

    if(any_top(result.vec))
        return;

    fixed_int_t const mask = result.mask;
    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];
    constraints_t& C = cv[2][0];

    assert(L.is_normalized() && R.is_normalized());

    // We use an approximation approach.
    // We can solve bit equations of the form KNOWN ^ KNOWN ^ UNKNOWN = KNOWN
    // (Three arguments because of carries).

    // Determine some of the carried bits:
    fixed_int_t carry0 = 
        ((L.bits.known0 & R.bits.known0 & mask) << 1ull) & mask;
    fixed_int_t carry1 = 
        ((L.bits.known1 & R.bits.known1 & mask) << 1ull) & mask;

    fixed_int_t const carry_i = ~mask ? (~(mask << 1) & mask) : 1;
    
    // First do the carry. If we know the lowest bit of L, R, and result
    // we can infer the required carry.
    if(result[0].bits.known() & L.bits.known() & R.bits.known() & carry_i)
    {
        if((result[0].bits.known1 ^ L.bits.known1 ^ R.bits.known1) & carry_i)
            C = constraints_t::carry(CARRY_SET);
        else
            C = constraints_t::carry(CARRY_CLEAR);
    }

    // If the SSA op has a carry input, use it in the lowest bit:
    switch(C.to_carry())
    {
    case CARRY_BOTTOM: break;
    case CARRY_CLEAR:  carry0 |= carry_i; break;
    case CARRY_SET:    carry1 |= carry_i; break;
    case CARRY_TOP:    return;
    }

    fixed_int_t const solvable = result[0].bits.known() & (carry0 | carry1);
    fixed_int_t const lsolvable = R.bits.known() & solvable;
    fixed_int_t const rsolvable = L.bits.known() & solvable;

    L.bits.known1 |= ((carry1 ^ R.bits.known1 ^ result[0].bits.known1) 
                      & lsolvable);
    R.bits.known1 |= ((carry1 ^ L.bits.known1 ^ result[0].bits.known1) 
                      & rsolvable);
    L.bits.known0 |= ~L.bits.known1 & lsolvable;
    R.bits.known0 |= ~R.bits.known1 & rsolvable;

    // Move the bounds in after calculating bits.
    L.bounds = intersect(L.bounds, from_bits(L.bits));
    R.bounds = intersect(R.bounds, from_bits(R.bits));

    L.normalize();
    R.normalize();

    constraints_t const shifted_C = constraints_t::shifted_carry(C.to_carry(), mask);

    fixed_int_t max_sum;
    if(builtin::add_overflow(L.bounds.max, R.bounds.max, max_sum)
    || builtin::add_overflow(max_sum, shifted_C.bounds.max, max_sum))
    {
        return;
    }

    constraints_t value = result[0];

    if(max_sum > mask)
    {
        // The original add can overflow! This complicates things.
        fixed_int_t const min_sum = L.bounds.min + R.bounds.min + shifted_C.bounds.min;
        fixed_int_t const span = max_sum - min_sum;
        fixed_int_t const masked_min_sum = min_sum & mask;

        if(masked_min_sum + span > mask)
            return;

        // Un-mask the result.
        fixed_int_t const masked_diff = min_sum - masked_min_sum;
        value.bounds.min += masked_diff;
        value.bounds.max += masked_diff;
    }

    // If the result's max is less than expected, try lowering
    // L and R's max bound.
    L.bounds.max = std::min(L.bounds.max, value.bounds.max - R.bounds.min - shifted_C.bounds.min);
    R.bounds.max = std::min(R.bounds.max, value.bounds.max - L.bounds.min - shifted_C.bounds.min);

    // If the result's min is greater than expected, try raising
    // L and R's min bound.
    if(value.bounds.min > R.bounds.max + shifted_C.bounds.max)
        L.bounds.min = std::max(L.bounds.min, value.bounds.min - R.bounds.max - shifted_C.bounds.max);
    if(value.bounds.min > L.bounds.max + shifted_C.bounds.max)
        R.bounds.min = std::max(R.bounds.min, value.bounds.min - L.bounds.max - shifted_C.bounds.max);
};

NARROW(SSA_and) = NARROW_FN
{
    assert(argn == 2 && result.vec.size() >= 1);

    if(result[0].is_top())
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];

    // If output bit is true, both inputs must be true.
    L.bits.known1 |= result[0].bits.known1;
    R.bits.known1 |= result[0].bits.known1;

    // If we know the output is 0, and we know one input bit is 1,
    // the other input bit must be 0.
    L.bits.known0 |= result[0].bits.known0 & R.bits.known1;
    R.bits.known0 |= result[0].bits.known0 & L.bits.known1;
};

NARROW(SSA_or) = NARROW_FN
{
    assert(argn == 2 && result.vec.size() >= 1);

    if(result[0].is_top())
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];

    // If output bit is false, both inputs must be false.
    L.bits.known0 |= result[0].bits.known0;
    R.bits.known0 |= result[0].bits.known0;

    // If we know the output is 1, and we know one input bit is 0,
    // the other input bit must be 1.
    L.bits.known1 |= result[0].bits.known1 & R.bits.known0;
    R.bits.known1 |= result[0].bits.known1 & L.bits.known0;
};

NARROW(SSA_xor) = NARROW_FN
{
    assert(argn == 2 && result.vec.size() >= 1);

    if(result[0].is_top())
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];


    // If we know the output is 0, and we know one input bit is 0,
    // the other input bit must be 0.
    L.bits.known0 |= result[0].bits.known0 & R.bits.known0;
    R.bits.known0 |= result[0].bits.known0 & L.bits.known0;

    // If we know the output is 0, and we know one input bit is 1,
    // the other input bit must be 1.
    L.bits.known1 |= result[0].bits.known0 & R.bits.known1;
    R.bits.known1 |= result[0].bits.known0 & L.bits.known1;

    // If we know the output is 1, and we know one input bit is 1,
    // the other input bit must be 0.
    L.bits.known0 |= result[0].bits.known1 & R.bits.known1;
    R.bits.known0 |= result[0].bits.known1 & L.bits.known1;

    // If we know the output is 1, and we know one input bit is 0,
    // the other input bit must be 1.
    L.bits.known1 |= result[0].bits.known1 & R.bits.known0;
    R.bits.known1 |= result[0].bits.known1 & L.bits.known0;
};

template<bool Eq>
static void narrow_eq(constraints_def_t* cv, 
                      unsigned argn, constraints_def_t const& result)
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);
    assert(argn > 0);

    if(!result[0].is_const())
        return;

    if(!!result[0].get_const() == Eq)
        for(unsigned i = 0; i < argn; i += 2)
            cv[i][0] = cv[i+1][0] = intersect(cv[i][0], cv[i+1][0]);
    else if(argn == 2)
    {
        assert(result[0].get_const() == fixed_t::whole(!Eq).value);
        for(unsigned i = 0; i < 2; ++i)
        {
            constraints_t& a = cv[i][0];
            constraints_t& b = cv[1 - i][0];

            if(!a.is_const())
                continue;

            fixed_int_t const const_ = a.get_const();

            if(b.bounds.min == const_)
                ++b.bounds.min;
            if(b.bounds.max == const_)
                --b.bounds.max;
        }
    }
}

NARROW(SSA_eq) = narrow_eq<true>;
NARROW(SSA_not_eq) = narrow_eq<false>;

NARROW(SSA_lt) = NARROW_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);
    assert(argn > 0);

    if(argn != 2 || !result[0].is_const())
        return;

    // TODO: implement argn > 2 narrows for this

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];

    if(result[0].get_const())
    {
        L.bounds.max = std::min(L.bounds.max, R.bounds.max - 1);
        R.bounds.min = std::max(R.bounds.min, L.bounds.min + 1);
    }
    else
    {
        L.bounds.min = std::max(L.bounds.min, R.bounds.min);
        R.bounds.max = std::min(R.bounds.max, L.bounds.max);
    }
};

NARROW(SSA_lte) = NARROW_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);

    if(argn != 2 || !result[0].is_const())
        return;

    // TODO: implement argn > 2 narrows for this

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];

    if(result[0].get_const())
    {
        L.bounds.max = std::min(L.bounds.max, R.bounds.max);
        R.bounds.min = std::max(R.bounds.min, L.bounds.min);
    }
    else
    {
        L.bounds.min = std::max(L.bounds.min, R.bounds.min + 1);
        R.bounds.max = std::min(R.bounds.max, L.bounds.max - 1);
    }
};

#undef ABSTRACT_FN
#undef NARROW_FN
#undef ABSTRACT
#undef NARROW

extern std::array<abstract_fn_t*, NUM_SSA_OPS> const abstract_fn_table =
{{
#define SSA_DEF(name, ...) abstract_fn_v<SSA_##name>,
#include "ssa_op.inc"
}};

extern std::array<narrow_fn_t*, NUM_SSA_OPS> const narrow_fn_table =
{{
#define SSA_DEF(name, ...) narrow_fn_v<SSA_##name>,
#include "ssa_op.inc"
}};
