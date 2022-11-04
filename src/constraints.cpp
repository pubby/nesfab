#include "constraints.hpp"

#include <algorithm>
#include <string>
#include <ostream> // TODO
#include <iostream> // TODO
#include <sstream>

#include "builtin.hpp"
#include "format.hpp"
#include "type_mask.hpp"

extern std::uint8_t const add_constraints_table[1024];

std::string to_string(constraints_mask_t const& cm)
{
    return fmt("{ %, % } (%)", cm.mask, cm.signed_, cm.mask >> fixed_t::shift);
}

// For debugging mostly
std::string to_string(bounds_t const& b)
{
    std::stringstream ss;
    ss << std::hex << '(' << b.min << ", " << b.max << ')';

    return fmt("[%, %] %)", 
               to_double(fixed_t{ b.min }), to_double(fixed_t{ b.max }), ss.str());
}

// For debugging mostly
std::string to_string(known_bits_t const& b)
{
    fixed_uint_t const known = b.known();
    std::string str;
    for(fixed_uint_t i = 0; i < sizeof_bits<fixed_uint_t>; ++i)
    {
        if(i > 0 && i % 8 == 0)
            str.push_back(' ');
        fixed_uint_t bit = 1ull << i;
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
std::ostream& operator<<(std::ostream& o, constraints_mask_t const& cm)
{
    o << to_string(cm);
    return o;
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

bounds_t apply_mask(bounds_t b, constraints_mask_t cm)
{
    assert(is_mask(cm.mask));

    if(b.is_top())
        return bounds_t::top();

    bounds_t const bottom = bounds_t::bottom(cm);

    if(b.min < bottom.min)
        return bottom;

    if(!cm.signed_)
    {
        fixed_uint_t const span = b.max - b.min;
        b.min &= cm.mask;
        b.max = b.min + span;
    }

    if(b.max > bottom.max)
        return bottom;

    assert(!b.is_top());
    assert(b.in_mask(cm));
    assert(bottom(b.min, cm));
    assert(bottom(b.max, cm));

    return b;
}

known_bits_t apply_mask(known_bits_t b, constraints_mask_t cm)
{
    assert(is_mask(cm.mask));

    if(b.is_top())
        return known_bits_t::top();

    b.known0 |= ~cm.mask;
    b.known1 &= cm.mask;

    assert(!b.is_top());
    assert(b.in_mask(cm));

    return b;
}

constraints_t apply_mask(constraints_t c, constraints_mask_t cm)
{
    assert(is_mask(cm.mask));
    return { apply_mask(c.bounds, cm), apply_mask(c.bits, cm) };
}

bounds_t from_bits(known_bits_t bits, constraints_mask_t cm)
{
    assert(is_mask(cm.mask));

    if(bits.is_top())
        return bounds_t::top();

    bounds_t ret;

    if(cm.signed_)
    {
        fixed_uint_t const sign_bit = high_bit_only(cm.mask);

        if(bits.known1 & sign_bit)
        {
            // Must be a negative
            fixed_sint_t const complement = static_cast<fixed_sint_t>(sign_bit << 1);
            assert(complement >= 0);

            assert(complement);
            bounds_t const pos = { bits.known1 & cm.mask, ~bits.known0 & cm.mask };
            assert(pos.min <= pos.max);
            
            ret = { pos.min - complement, pos.max - complement };

            //std::cout << "from bits" << std::endl;
            //std::cout << complement << std::endl;
            //std::cout << pos << std::endl;
            //std::cout << ret << std::endl;

            passert(ret.min < 0, ret.min);
            passert(ret.max < 0, ret.max);
            assert(ret.min <= ret.max);
        }
        else if(bits.known0 & sign_bit)
        {
            // Must be a positive
            ret = { bits.known1, ~bits.known0 };

            assert(ret.min >= 0);
            assert(ret.max >= 0);
            assert(ret.min <= ret.max);
        }
        else
        {
            // Positive or negative
            ret = bounds_t::bottom(cm);
            ret.min |= bits.known1;
            ret.max &= ~bits.known0;
        }
    }
    else
    {
        ret = { bits.known1 & cm.mask, ~bits.known0 & cm.mask };
    }

    assert(!ret.is_top());
    return ret;
}

known_bits_t from_bounds(bounds_t bounds, constraints_mask_t cm)
{
    assert(is_mask(cm.mask));
    assert(~cm.mask);

    if(bounds.is_top())
        return known_bits_t::top();

    // Find upper bits that are the same.

    fixed_uint_t const x = (bounds.umin() ^ bounds.umax()) & cm.mask;
    fixed_uint_t low_mask = ~0ull;

    if(x)
        low_mask = ~((1ull << (fixed_uint_t)builtin::rclz(x)) - 1ull);
    //std::cout << x << std::endl;
    //std::cout << low_mask << std::endl;
    //std::cout << (x & low_mask) << std::endl;
    assert((x & low_mask) == 0ull);

    known_bits_t ret;
    ret.known0 = (~bounds.min & low_mask) | ~cm.mask;
    ret.known1 = (bounds.min & low_mask) & cm.mask;

    //std::cout << "ret: " << bounds << std::endl;
    //std::cout << "ret: " << x << std::endl;
    //std::cout << ret << std::endl;
    //ret.known0 |= ~cm.mask;
    //ret.known1 &= cm.mask;
    //std::cout << ret << std::endl;

    if(cm.signed_ && bounds.min < 0 && bounds.max >= 0)
        assert(ret.bit_eq(known_bits_t::bottom(cm)));

    assert(!ret.is_top());
    assert(ret(bounds.umin(), cm));
    assert(ret(bounds.umax(), cm));
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

bool is_subset(constraints_t small, constraints_t big, constraints_mask_t cm)
{
    return (intersect(big, small).normal_eq(small, cm)
            && union_(small, big).normal_eq(big, cm));
}

// Narrows bounds based on the constraint's bits.
// e.g. if bounds are [1, 5] and bit 0 is known to be 0, narrows to [2, 4].
bounds_t tighten_bounds(constraints_t c, constraints_mask_t cm)
{
    //std::cout << "tighten:" << std::endl;
    //std::cout << c << std::endl;
    fixed_uint_t const known = c.bits.known() & cm.mask;

    if(!known)
        return c.bounds;

    fixed_uint_t min = c.bounds.min;
    fixed_uint_t max = ~c.bounds.max;

    fixed_uint_t const bit_min = builtin::ctz(known);
    fixed_uint_t const bit_max = builtin::rclz(known);

    //std::cout << "known= " << known << std::endl;
    //std::cout << "bit min = " << bit_min << std::endl;
    //std::cout << "bit max = " << bit_max << std::endl;

    /*
    for(int i = bit_max - 1; i >= bit_min; --i)
    {
        fixed_uint_t const bit = (1ull << i) & known;

        if((~c.bounds.min & c.bits.known1) & bit)
        {
            c.bounds.min &= ~(bit - 1) & cm.mask;
            c.bounds.min |= c.bits.known1;
            assert(!((~c.bounds.min & c.bits.known1) & bit));
            break;
        }
    }

    for(int i = bit_max - 1; i >= bit_min; --i)
    {
        //std::cout << i << std::endl;
        fixed_uint_t const bit = (1ull << i) & known;

        if((c.bounds.max & c.bits.known0) & bit)
        {
            //std::cout << 'f' << std::endl;
            c.bounds.max |= (bit - 1) & cm.mask;
            c.bounds.max &= ~c.bits.known0 | ~cm.mask;
            //std::cout << c.bounds.max << std::endl;
            assert(!((c.bounds.max & c.bits.known0) & bit));
            break;
        }
    }
    */

    // For each known bit
    for(fixed_uint_t i = bit_min; i < bit_max; ++i)
    {
        //std::cout << "i = " << i << std::endl;
        fixed_uint_t const bit = (1ull << i) & known;
        if((min ^ c.bits.known1) & bit)
        {
            min += bit;
            min &= ~(bit - 1) | known;
        }
        if((max ^ c.bits.known0) & bit)
        {
            //std::cout << "pre = " << ((max >> 24ull) & 0xF) << std::endl;
            max += bit;
            max &= ~(bit - 1) | known;
            //std::cout << "max = " << ((max >> 24ull) & 0xF) << std::endl;
        }
    }

    //std::cout << "return" << std::endl;
    //std::cout << c << std::endl;
    //std::cout << static_cast<fixed_sint_t>((~max >> 24) & 0xF) << std::endl;
    c.bounds.min = static_cast<fixed_sint_t>(min);
    c.bounds.max = static_cast<fixed_sint_t>(~max & cm.mask);
    if(cm.signed_)
    {
        c.bounds.min = sign_extend(c.bounds.min, cm.mask);
        c.bounds.max = sign_extend(c.bounds.max, cm.mask);
    }
    //std::cout << static_cast<fixed_sint_t>((c.bounds.max >> 24) & 0xF) << std::endl;
    //std::cout << c << std::endl;
    //std::cout << std::endl;
    return c.bounds;
}

static constraints_t normalize_impl(constraints_t c, constraints_mask_t cm)
{
    //std::cout << "bb = " << c.bounds << std::endl;
    //std::cout << 'a' << c << std::endl;
    c.bounds = intersect(c.bounds, from_bits(c.bits, cm));
    //std::cout << "bb = " << c.bounds << std::endl;
    //std::cout << 'b' << c << std::endl;
    c.bounds = intersect(c.bounds, from_bits(c.bits, cm));
    //std::cout << 'c' << c << std::endl;
    c.bits = intersect(c.bits, from_bounds(c.bounds, cm));
    //std::cout << 'd' << c << std::endl;
    //std::cout << "bits = " << c.bits << std::endl;
    c.bounds = tighten_bounds(c, cm);
    //std::cout << 'e' << c << std::endl;
    //std::cout << "from_bounds" << from_bounds(c.bounds, cm) << std::endl;
    c.bits = intersect(c.bits, from_bounds(c.bounds, cm));
    //std::cout << 'f' << c << std::endl;
    if(c.is_top(cm))
        return constraints_t::top();
    return c;
}

constraints_t normalize(constraints_t c, constraints_mask_t cm)
{
    //std::cout << std::endl;
    //std::cout << cm.signed_ << std::endl;
    //std::cout << "n1:" << c << std::endl;
    constraints_t ret = normalize_impl(c, cm);

#if 0
    std::cout << "n2:" << ret << std::endl;
    constraints_t again = normalize_impl(ret, cm);
    std::cout << "n3:" << again << std::endl;
    again = normalize_impl(ret, cm);
    std::cout << "n4:" << again << std::endl;
    again = normalize_impl(ret, cm);
    std::cout << "n5:" << again << std::endl;
#endif

    assert(ret.bit_eq(normalize_impl(ret, cm)));
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

bool all_subset(constraints_vec_t const& a, constraints_vec_t const& b, constraints_mask_t cm)
{
    if(a.size() != b.size())
        return false;
    for(unsigned i = 0; i < a.size(); ++i)
        if(!is_subset(a[i], b[i], cm))
            return false;
    return true;
}

bool all_normalized(constraints_def_t const& def)
{
    for(constraints_t const& c : def.vec)
        if(!c.is_normalized(def.cm))
            return false;
    return true;
}

bool any_top(constraints_def_t const& def)
{
    for(constraints_t const& c : def.vec)
        if(c.is_top(def.cm))
            return true;
    return false;
}

static bool any_top(constraints_def_t const* cv, unsigned argn)
{
    for(unsigned i = 0; i < argn; ++i)
        if(any_top(cv[i]))
            return true;
    return false;
}

static bool handle_top(constraints_def_t const* cv, unsigned argn, constraints_def_t& r)
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
        constraint = constraints_t::bottom(result.cm);
};

ABSTRACT(SSA_read_global) = abstract_bottom;
ABSTRACT(SSA_fn_call) = abstract_bottom;
ABSTRACT(SSA_uninitialized) = abstract_bottom;
ABSTRACT(SSA_read_ptr) = abstract_bottom;
ABSTRACT(SSA_read_ptr_hw) = abstract_bottom;
ABSTRACT(SSA_make_ptr_lo) = abstract_bottom;
ABSTRACT(SSA_make_ptr_hi) = abstract_bottom;
ABSTRACT(SSA_get_byte) = abstract_bottom;
ABSTRACT(SSA_array_get_byte) = abstract_bottom;
ABSTRACT(SSA_replace_byte) = abstract_bottom;
ABSTRACT(SSA_array_replace_byte) = abstract_bottom;
ABSTRACT(SSA_read_array16_b) = abstract_bottom;
ABSTRACT(SSA_write_array16_b) = abstract_bottom;
ABSTRACT(SSA_mul8_lo) = abstract_bottom;
ABSTRACT(SSA_mul8_hi) = abstract_bottom;

ABSTRACT(SSA_carry) = ABSTRACT_FN
{
    assert(argn == 1);
    passert(cv[0].vec.size() == 2, cv[0].vec.size());
    result[0] = cv[0][1];
};

ABSTRACT(SSA_cast) = ABSTRACT_FN
{
    assert(argn == 1);
    assert(result.vec.size() <= 2);
    assert(cv[0].vec.size() >= 1);
    result[0] = apply_mask(cv[0][0], result.cm); // handles top itself

    // Sign-extend
    if(result.cm.signed_ && cv[0].cm.signed_ && result.cm.mask > cv[0].cm.mask)
    {
        fixed_uint_t const sign_bit = high_bit_only(cv[0].cm.mask);
        fixed_uint_t const extended = result.cm.mask & ~submask(cv[0].cm.mask);

        result[0].bits.known0 &= ~extended;
        result[0].bits.known1 &= ~extended;

        if(result[0].bits.known0 & sign_bit)
            result[0].bits.known0 |= extended;
        if(result[0].bits.known1 & sign_bit)
            result[0].bits.known1 |= extended;
    }
};

constraints_t abstract_sign_extend(constraints_t c, constraints_mask_t cm)
{
    fixed_uint_t const sign_bit = high_bit_only(cm.mask);

    if(c.bits.known1 & sign_bit)
        return constraints_t::const_(cm.signed_ ? supermask(cm.mask) : cm.mask, cm);
    else if(c.bits.known0 & sign_bit)
        return constraints_t::const_(0, cm);
    return constraints_t::bottom(cm);
}

ABSTRACT(SSA_sign_extend) = ABSTRACT_FN
{
    assert(argn == 1);

    if(handle_top(cv, argn, result))
        return;

    result[0] = abstract_sign_extend(cv[0][0], cv[0].cm);
};

ABSTRACT(SSA_sign) = ABSTRACT_FN
{
    assert(argn == 1);

    if(handle_top(cv, argn, result))
        return;

    constraints_mask_t const cm = cv[0].cm;
    fixed_uint_t const sign_bit = high_bit_only(cm.mask);

    if(cv[0][0].bits.known1 & sign_bit)
        result[0] = constraints_t::bool_(true);
    else if(cv[0][0].bits.known0 & sign_bit)
        result[0] = constraints_t::bool_(false);
    else
        result[0] = constraints_t::any_bool();
};

ABSTRACT(SSA_phi) = ABSTRACT_FN
{
    assert(argn >= 1);
    assert(result.vec.size() > 0);

    //std::cout << "START PHI\n";
    //std::cout << constraints_t::bottom(type_constraints_mask(TYPE_F2)) << std::endl;

    assert(result.vec.size());
    assert(argn);

    for(unsigned i = 0; i < result.vec.size(); ++i)
    {
        result[i] = constraints_t::top();
        for(unsigned j = 0; j < argn; ++j)
        {
            assert(cv[j].vec.size());
            assert(cv[j].vec.size() >= result.vec.size());
            result[i] = union_(result[i], cv[j][i]);
            //std::cout << "PHI UNION " << cv[j][i] << std::endl;
            //std::cout << "PHI       " << result[i] << std::endl;
        }
    }
};

static known_bits_t abstract_and(known_bits_t lhs, known_bits_t rhs, fixed_uint_t mask)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    return { .known0 = (lhs.known0 | rhs.known0) | ~mask,
             .known1 = (lhs.known1 & rhs.known1) & mask };
}

ABSTRACT(SSA_and) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(result.cm == cv[0].cm);
    assert(result.cm == cv[1].cm);
    assert(cv[0].vec.size() >= 1);
    assert(cv[1].vec.size() >= 1);

    result[0].bits = abstract_and(cv[0][0].bits, cv[1][0].bits, result.cm.mask);
    result[0].bounds = from_bits(result[0].bits, result.cm);

    assert(result[0].is_normalized(result.cm));
};

static known_bits_t abstract_or(known_bits_t lhs, known_bits_t rhs, fixed_uint_t mask)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    return { .known0 = (lhs.known0 & rhs.known0) | ~mask,
             .known1 = (lhs.known1 | rhs.known1) & mask };
}

ABSTRACT(SSA_or) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(result.cm == cv[0].cm);
    assert(result.cm == cv[1].cm);
    assert(cv[0].vec.size() >= 1);
    assert(cv[1].vec.size() >= 1);

    result[0].bits = abstract_or(cv[0][0].bits, cv[1][0].bits, result.cm.mask);
    result[0].bounds = from_bits(result[0].bits, result.cm);

    assert(result[0].is_normalized(result.cm));
};

known_bits_t abstract_xor(known_bits_t lhs, known_bits_t rhs, fixed_uint_t mask)
{
    if(lhs.is_top() || rhs.is_top())
        return known_bits_t::top();
    fixed_uint_t const known = lhs.known() & rhs.known() & mask;
    fixed_uint_t const x = lhs.known1 ^ rhs.known1;
    return { (~x & known) | ~mask, x & known };
}

ABSTRACT(SSA_xor) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(result.cm == cv[0].cm);
    assert(result.cm == cv[1].cm);
    assert(cv[0].vec.size() >= 1);
    assert(cv[1].vec.size() >= 1);

    result[0].bits = abstract_xor(cv[0][0].bits, cv[1][0].bits, result.cm.mask);
    result[0].bounds = from_bits(result[0].bits, result.cm);

    //std::cout << result[0] << std::endl;
    assert(result[0].is_normalized(result.cm));
};

template<bool Add>
void abstract_add_sub(constraints_def_t const* cv, unsigned argn, constraints_def_t& result)
{
    assert(argn == 3);
    assert(result.cm == cv[0].cm);
    assert(result.cm == cv[1].cm);
    assert(CARRY_MASK == cv[2].cm);
    assert(cv[0].vec.size() >= 1);
    assert(cv[1].vec.size() >= 1);
    assert(cv[2].vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    constraints_mask_t const cm = result.cm;
    constraints_t const L = cv[0][0];
    constraints_t R = cv[1][0];
    constraints_t const C = cv[2][0];
    constraints_t const shifted_C = constraints_t::shifted_carry(C.to_carry(), cm.mask);

    assert(L.is_normalized(cm));
    assert(R.is_normalized(cm));
    assert(C.is_normalized(CARRY_MASK));

    if(!Add)
    {
        // Invert the bits:
        std::swap(R.bits.known0, R.bits.known1);
    }

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

    fixed_uint_t const neg_mask = ~(L.bits.known0 & R.bits.known0 & shifted_C.bits.known0) & cm.mask;
    std::uint64_t const start_i = neg_mask ? (builtin::ctz(neg_mask) & ~1ull): 0;
    std::uint64_t const end_i = ((1 + (cm.mask ? builtin::rclz(cm.mask) : sizeof_bits<fixed_uint_t>)) & ~1ull);
    value.bits.known0 = (1ull << start_i) - 1ull;

    known_bits_t lhs_bits = L.bits;
    known_bits_t rhs_bits = R.bits;

    lhs_bits.known0 >>= start_i;
    lhs_bits.known1 >>= start_i;
    rhs_bits.known0 >>= start_i;
    rhs_bits.known1 >>= start_i;

    fixed_uint_t i = start_i;
    // 'j' holds the carry.
    fixed_uint_t j = C.to_carry();
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
    if(i < sizeof_bits<fixed_uint_t>)
        value.bits.known0 |= ~((1ull << i) - 1ull);

    value.bits = apply_mask(value.bits, cm);
    assert(!value.bits.is_top());

    // OK! Now for the bounds.

    if(Add)
    {
        // Calculate 'min' and 'max.
        value.bounds.min = L.bounds.min + R.bounds.min + shifted_C.bounds.min;
        value.bounds.max = L.bounds.max + R.bounds.max + shifted_C.bounds.max;
        value.bounds = apply_mask(value.bounds, cm);

        // The bounds can prove the output carry is set:
        auto const masked_min = (L.bounds.min & cm.mask) + (R.bounds.min & cm.mask) + shifted_C.bounds.min;
        if((masked_min & cm.mask) != masked_min)
        {
            passert((carry_t)j == CARRY_SET || (carry_t)j == CARRY_BOTTOM, j, masked_min >> fixed_t::shift, bounds_t::bottom(cm),
                    '\n', L, '\n', R);
            carry = constraints_t::carry(CARRY_SET);
        }
    }
    else
    {
        // Calculate 'min' and 'max.
        fixed_sint_t const one = static_cast<fixed_sint_t>(low_bit_only(cm.mask));
        value.bounds.min = L.bounds.min - R.bounds.max - (one - shifted_C.bounds.min);
        value.bounds.max = L.bounds.max - R.bounds.min - (one - shifted_C.bounds.max);
        value.bounds = apply_mask(value.bounds, cm);

        // The bounds can prove the output carry is set:
        auto const masked_max = (L.bounds.max & cm.mask) - (R.bounds.min & cm.mask) - (one - shifted_C.bounds.max);
        if((masked_max & cm.mask) != masked_max)
        {
            passert((carry_t)j == CARRY_CLEAR || (carry_t)j == CARRY_BOTTOM, j, masked_max >> fixed_t::shift, bounds_t::bottom(cm),
                    '\n', L, '\n', R);
            carry = constraints_t::carry(CARRY_CLEAR);
        }
    }

    // 'min' and 'max' put constraints on the bits - neat!
    // Calculate those constraints here.
    assert(apply_mask(value.bits, cm).bit_eq(value.bits));
    known_bits_t bounds_bits = apply_mask(from_bounds(value.bounds, cm), cm);
    assert(!bounds_bits.is_top());
    assert(apply_mask(bounds_bits, cm).bit_eq(bounds_bits));

    assert(apply_mask(value.bits, cm).bit_eq(value.bits));
    value.bits.known0 |= bounds_bits.known0;
    value.bits.known1 |= bounds_bits.known1;

    assert(!value.bounds.is_top());
    assert(!value.bits.is_top());
    assert(apply_mask(value.bits, cm).bit_eq(value.bits));
    value.normalize(cm);
};

// Keep this up-to-date with SSA_sub
ABSTRACT(SSA_add) = abstract_add_sub<true>;
ABSTRACT(SSA_sub) = abstract_add_sub<false>;

ABSTRACT(SSA_mul) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;

    auto const& L = cv[0][0];
    auto const& R = cv[1][0];
    
    fixed_sint_t const a = fixed_mul(L.bounds.min, R.bounds.min);
    fixed_sint_t const b = fixed_mul(L.bounds.max, R.bounds.min);
    fixed_sint_t const c = fixed_mul(L.bounds.min, R.bounds.max);
    fixed_sint_t const d = fixed_mul(L.bounds.max, R.bounds.max);

    fixed_sint_t const min = result[0].bounds.min = std::min({ a, b, c, d });
    fixed_sint_t const max = result[0].bounds.max = std::max({ a, b, c, d });

    auto const bottom = constraints_t::bottom(result.cm);
    if(min < bottom.bounds.min || max > bottom.bounds.max)
    {
        result[0] = bottom;
        return;
    }

    result[0].bits = from_bounds(result[0].bounds, result.cm);
    assert(result[0].is_normalized(result.cm));
};

constraints_t abstract_eq(constraints_t lhs, constraints_mask_t lhs_cm, 
                          constraints_t rhs, constraints_mask_t rhs_cm,
                          bool sign_diff)
{
    // For now, require both types be the same size.
    assert(lhs_cm.mask == rhs_cm.mask);

    if(lhs.is_top(lhs_cm) || rhs.is_top(rhs_cm))
        return constraints_t::top();

    if(sign_diff || lhs_cm.signed_ != rhs_cm.signed_)
    {
        fixed_uint_t const sign_bit = high_bit_only(lhs_cm.mask);
        assert(sign_bit == high_bit_only(rhs_cm.mask));

        // If either sign bit is set, the values don't compare equal.
        if((lhs.bits.known1 | rhs.bits.known1) & sign_bit)
            return constraints_t::bool_(false);
    }

    if(lhs.bits.known0 & rhs.bits.known1)
        return constraints_t::bool_(false);
    if(lhs.bits.known1 & rhs.bits.known0)
        return constraints_t::bool_(false);
    if(lhs.bounds.min > rhs.bounds.max || lhs.bounds.max < rhs.bounds.min)
        return constraints_t::bool_(false);
    if(lhs.is_const() && rhs.is_const() && lhs.get_const() == rhs.get_const())
        return constraints_t::bool_(true);
    return constraints_t::any_bool();
}

ABSTRACT(SSA_eq) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(cv[0].cm.mask == cv[1].cm.mask);

    if(handle_top(cv, argn, result))
        return;
    
    assert(cv[0].cm.mask == cv[1].cm.mask);

    result[0] = abstract_eq(cv[0][0], cv[0].cm, cv[1][0], cv[1].cm);
};

ABSTRACT(SSA_multi_eq) = ABSTRACT_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;

    known_bits_t bits = known_bits_t::bool_(true);
    for(unsigned i = 0; i < argn; i += 2)
        bits = abstract_and(bits, abstract_eq(cv[i][0], cv[i].cm, cv[i+1][0], cv[i+1].cm, i == argn - 2).bits, BOOL_MASK.mask);

    result[0].bits = bits;
    result[0].bounds = from_bits(bits, BOOL_MASK);
};

constraints_t abstract_not_eq(constraints_t lhs, constraints_mask_t lhs_cm, 
                              constraints_t rhs, constraints_mask_t rhs_cm,
                              bool sign_diff)
{
    // For now, require both types be the same size.
    assert(lhs_cm.mask == rhs_cm.mask);

    if(lhs.is_top(lhs_cm) || rhs.is_top(rhs_cm))
        return constraints_t::top();

    if(sign_diff || lhs_cm.signed_ != rhs_cm.signed_)
    {
        fixed_uint_t const sign_bit = high_bit_only(lhs_cm.mask);
        assert(sign_bit == high_bit_only(rhs_cm.mask));

        // If either sign bit is set, the values don't compare equal.
        if((lhs.bits.known1 | rhs.bits.known1) & sign_bit)
            return constraints_t::bool_(true);
    }

    if(lhs.is_top(lhs_cm) || rhs.is_top(rhs_cm))
        return constraints_t::top();
    if(lhs.bits.known0 & rhs.bits.known1)
        return constraints_t::bool_(true);
    if(lhs.bits.known1 & rhs.bits.known0)
        return constraints_t::bool_(true);
    if(lhs.bounds.min > rhs.bounds.max || lhs.bounds.max < rhs.bounds.min)
        return constraints_t::bool_(true);
    if(lhs.is_const() && rhs.is_const() && lhs.get_const() == rhs.get_const())
        return constraints_t::bool_(false);
    return constraints_t::any_bool();
}

ABSTRACT(SSA_not_eq) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(cv[0].cm.mask == cv[1].cm.mask);

    if(handle_top(cv, argn, result))
        return;
    
    assert(cv[0].cm.mask == cv[1].cm.mask);

    result[0] = abstract_not_eq(cv[0][0], cv[0].cm, cv[1][0], cv[1].cm);
};

ABSTRACT(SSA_multi_not_eq) = ABSTRACT_FN
{
    assert(argn % 2 == 0 && result.vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;

    known_bits_t bits = known_bits_t::bool_(false);
    for(unsigned i = 0; i < argn; i += 2)
        bits = abstract_or(bits, abstract_not_eq(cv[i][0], cv[i].cm, cv[i+1][0], cv[i+1].cm, i == argn - 2).bits, BOOL_MASK.mask);

    result[0].bits = bits;
    result[0].bounds = from_bits(bits, BOOL_MASK);
};

constraints_t abstract_lt(constraints_t lhs, constraints_mask_t lhs_cm, 
                          constraints_t rhs, constraints_mask_t rhs_cm)
{
    if(lhs.is_top(lhs_cm) || rhs.is_top(rhs_cm))
        return constraints_t::top();
    if(lhs.bounds.max < rhs.bounds.min)
        return constraints_t::bool_(true);
    if(rhs.bounds.max <= lhs.bounds.min)
        return constraints_t::bool_(false);
    return constraints_t::any_bool();
}

ABSTRACT(SSA_lt) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;
    
    result[0] = abstract_lt(cv[0][0], cv[0].cm, cv[1][0], cv[1].cm);
};

ABSTRACT(SSA_multi_lt) = ABSTRACT_FN
{
    // It's quite difficult to implement multi_lt, so for now we'll skip it.
    // TODO: Properly implement
    result[0] = constraints_t::any_bool();
};

constraints_t abstract_lte(constraints_t lhs, constraints_mask_t lhs_cm, 
                           constraints_t rhs, constraints_mask_t rhs_cm)
{
    if(lhs.is_top(lhs_cm) || rhs.is_top(rhs_cm))
        return constraints_t::top();
    if(lhs.bounds.max <= rhs.bounds.min)
        return constraints_t::bool_(true);
    if(rhs.bounds.max < lhs.bounds.min)
        return constraints_t::bool_(false);
    return constraints_t::any_bool();
}

ABSTRACT(SSA_lte) = ABSTRACT_FN
{
    assert(argn == 2 && result.vec.size() >= 1);

    if(handle_top(cv, argn, result))
        return;
    
    result[0] = abstract_lte(cv[0][0], cv[0].cm, cv[1][0], cv[1].cm);
};

ABSTRACT(SSA_multi_lte) = ABSTRACT_FN
{
    // It's quite difficult to implement multi_lte, so for now we'll skip it.
    // TODO: Implement
    result[0] = constraints_t::any_bool();
};

ABSTRACT(SSA_init_array) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    passert(result.vec.size() == argn || result.vec.size() == 0, result.vec.size(), argn);

    for(unsigned i = 0; i < result.vec.size(); ++i)
    {
        assert(cv[i].vec.size() == 1);
        result[i] = cv[i][0];
    }
};

auto const read_array = ABSTRACT_FN
{
    using namespace ssai::array;

    if(handle_top(cv, argn, result))
        return;

    auto& input_array = cv[ARRAY];

    if(input_array.vec.empty())
        return abstract_bottom(cv, argn, result);

    std::int16_t const offset = static_cast<std::int16_t>(cv[OFFSET][0].get_const());
    bounds_t const index = cv[INDEX][0].bounds;

    fixed_sint_t const min_bound = (index.min >> fixed_t::shift) + offset;
    fixed_sint_t const max_bound = (index.max >> fixed_t::shift) + offset;
    fixed_sint_t const iter_to = std::min<fixed_sint_t>(max_bound + 1, input_array.vec.size());

    result[0] = constraints_t::top();
    for(auto i = std::max<fixed_sint_t>(min_bound, 0); i < iter_to; ++i)
        if(index(fixed_t::whole(i).value, cv[INDEX].cm))
            result[0] = union_(input_array[i], result[0]);
};

ABSTRACT(SSA_read_array8) = read_array;
ABSTRACT(SSA_read_array16) = read_array;

auto const write_array = ABSTRACT_FN
{
    using namespace ssai::array;

    if(handle_top(cv, argn, result))
        return;

    auto& input_array = cv[ARRAY];

    if(input_array.vec.empty())
        return abstract_bottom(cv, argn, result);

    std::int16_t const offset = static_cast<std::int16_t>(cv[OFFSET][0].get_const());
    bounds_t const index = cv[INDEX][0].bounds;
    constraints_t const value = cv[ASSIGNMENT][0];

    assert(input_array.vec.size() == result.vec.size());

    result.vec = input_array.vec;

    if(index.is_const())
    {
        fixed_sint_t const i = (index.min >> fixed_t::shift) + offset;
        if(i >= 0 && i < fixed_sint_t(result.vec.size()))
            result[i] = value;
    }
    else
    {
        fixed_sint_t const min_bound = (index.min >> fixed_t::shift) + offset;
        fixed_sint_t const max_bound = (index.max >> fixed_t::shift) + offset;
        fixed_sint_t const iter_to = std::min<fixed_sint_t>(max_bound + 1, input_array.vec.size());

        for(auto i = std::max<fixed_sint_t>(min_bound, 0); i < iter_to; ++i)
            if(index(fixed_t::whole(i).value, cv[INDEX].cm))
                result[i] = union_(result[i], value);
    }
};

ABSTRACT(SSA_write_array8) = write_array;
ABSTRACT(SSA_write_array16) = write_array;

ABSTRACT(SSA_resize_array) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    auto& input_array = cv[0].vec;

    if(result.vec.size() < input_array.size())
        std::copy_n(input_array.begin(), result.vec.size(), result.vec.begin());
    else
    {
        std::copy(input_array.begin(), input_array.end(), result.vec.begin());
        for(unsigned i = input_array.size(); i < result.vec.size(); ++i)
            result[i] = constraints_t::const_(0, result.cm);
    }
};

ABSTRACT(SSA_shl) = ABSTRACT_FN
{
    assert(argn == 2);
    passert(result.cm.mask == cv[0].cm.mask, result.cm.mask, cv[0].cm.mask);
    assert((cv[1].cm.mask & numeric_bitmask(TYPE_U)) == cv[1].cm.mask);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    fixed_uint_t const mask = result.cm.mask;
    constraints_t const L = cv[0][0];
    constraints_t const R = cv[1][0];

    assert(L.is_normalized(cv[0].cm));
    assert(R.is_normalized(cv[1].cm));

    // Convert R to whole
    assert(R.bounds.min >= 0 && R.bounds.max >= 0);
    fixed_uint_t const R_min = R.bounds.min >> fixed_t::shift;
    fixed_uint_t const R_max = R.bounds.max >> fixed_t::shift;
    assert(R_min < 256 && R_max < 256);
    assert(R_min <= R_max);

    // Calc known bits
    known_bits_t bits = L.bits;
    bits.known0 <<= R_min;
    bits.known0 |= ~(mask << R_min);
    bits.known1 <<= R_min;

    for(unsigned i = R_min; i < R_max; ++i)
    {
        bits.known0 &= (bits.known0 << 1ull);
        bits.known1 &= (bits.known1 << 1ull);
    }

    // Calc the output carry:
    fixed_uint_t const carry_out_bit = high_bit_only(mask) << 1;
    result[1].bits = known_bits_t::bottom(CARRY_MASK);
    if(bits.known0 & carry_out_bit)
        result[1].bits.known0 |= 1ull << fixed_t::shift;
    if(bits.known1 & carry_out_bit)
        result[1].bits.known1 |= 1ull << fixed_t::shift;
    result[1].bounds = from_bits(result[1].bits, CARRY_MASK);

    bits.known0 |= ~mask;
    bits.known1 &= mask;

    // Calc bounds
    bounds_t bounds;
    bounds.min = std::min(L.bounds.min << R_min, L.bounds.min << R_max);
    bounds.max = std::max(L.bounds.max << R_max, L.bounds.max << R_min);

    if(signed_clz(L.bounds.min) + R_min + result.cm.signed_ >= builtin::clz(mask))
        bounds = from_bits(bits, result.cm);
    else if(signed_clz(L.bounds.max) + R_max + result.cm.signed_ >= builtin::clz(mask))
        bounds.max = from_bits(bits, result.cm).max;

    result[0] = apply_mask(normalize({ bounds, bits }, result.cm), result.cm);

};

ABSTRACT(SSA_shr) = ABSTRACT_FN
{
    assert(argn == 2);
    assert(result.cm.mask == cv[0].cm.mask);
    assert((cv[1].cm.mask & numeric_bitmask(TYPE_U)) == cv[1].cm.mask);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    fixed_uint_t const mask = result.cm.mask;
    constraints_t const L = cv[0][0];
    constraints_t const R = cv[1][0];

    assert(L.is_normalized(cv[0].cm));
    assert(R.is_normalized(cv[1].cm));

    // Convert R to whole
    assert(R.bounds.min >= 0 && R.bounds.max >= 0);
    fixed_uint_t const R_min = R.bounds.min >> fixed_t::shift;
    fixed_uint_t const R_max = R.bounds.max >> fixed_t::shift;
    assert(R_min < 256 && R_max < 256);
    assert(R_min <= R_max);

    // Calc known bits

    known_bits_t bits = L.bits;
    bits.known0 = ((bits.known0 & mask) << 1) >> R_min;
    bits.known1 = ((bits.known1 & mask) << 1) >> R_min;

    if(L.bounds.min >= 0)
        bits.known0 |= (~(mask >> R_min) & mask);
    else if(L.bounds.max < 0)
        bits.known1 |= (~(mask >> R_min) & mask);

    for(unsigned i = R_min; i < R_max; ++i)
    {
        bits.known0 &= (bits.known0 >> 1ull);
        bits.known1 &= (bits.known1 >> 1ull);
    }

    bits.known0 |= ~mask;
    bits.known1 &= mask;

    // Calc bounds
    bounds_t bounds;
    bounds.min = std::min(L.bounds.min >> R_min, L.bounds.min >> R_max);
    bounds.max = std::max(L.bounds.max >> R_max, L.bounds.max >> R_min);
    bounds.min &= supermask(mask);
    bounds.max &= supermask(mask);
    assert(bounds.min <= bounds.max);

    result[0] = apply_mask(normalize({ bounds, bits }, result.cm), result.cm);

    // For now, don't calculate the carry.
    // TODO: calculate carry.
    result[1] = constraints_t::bottom(CARRY_MASK);
};

ABSTRACT(SSA_rol) = ABSTRACT_FN
{
    assert(argn == 2);
    assert(result.cm.mask == cv[0].cm.mask);
    assert(CARRY_MASK == cv[1].cm);
    assert(!result.cm.signed_);
    assert(result.vec.size() == 2);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    fixed_uint_t const mask = result.cm.mask;
    fixed_uint_t const C_mask = low_bit_only(mask);
    constraints_mask_t const V_cm = { cv[0].cm.mask, false };
    constraints_t V = cv[0][0];
    constraints_t const C = cv[1][0];
    constraints_t const shifted_C = constraints_t::shifted_carry(C.to_carry(), C_mask);

    if(cv[0].cm.signed_)
        V = normalize(apply_mask(V, V_cm), V_cm);

    assert(V.is_normalized(V_cm));
    assert(C.is_normalized(cv[1].cm));

    // Calc the output carry:
    fixed_uint_t const carry_out_bit = high_bit_only(mask);
    result[1].bits = known_bits_t::bottom(CARRY_MASK);
    if(V.bits.known0 & carry_out_bit)
        result[1].bits.known0 |= 1ull << fixed_t::shift;
    if(V.bits.known1 & carry_out_bit)
        result[1].bits.known1 |= 1ull << fixed_t::shift;
    result[1].bounds = from_bits(result[1].bits, CARRY_MASK);

    // Calc known bits
    known_bits_t bits = V.bits;
    bits.known0 <<= 1;
    bits.known1 <<= 1;
    bits.known0 &= ~CARRY_MASK.mask;
    bits.known1 &= ~CARRY_MASK.mask;
    bits.known0 |= shifted_C.bits.known0 & CARRY_MASK.mask;
    bits.known1 |= shifted_C.bits.known1 & CARRY_MASK.mask;
    bits.known0 |= ~mask;
    bits.known1 &= mask;

    // Calc bounds
    bounds_t bounds;
    bounds.min = (V.bounds.min << 1) + (!!C.bounds.min * C_mask);
    bounds.max = (V.bounds.max << 1) + (!!C.bounds.max * C_mask);

    assert(bounds.min >= 0 && bounds.max >= 0);

    if(V.bounds.min && builtin::clz(fixed_uint_t(V.bounds.min)) + 1 >= builtin::clz(mask))
        bounds = from_bits(bits, result.cm);
    else if(V.bounds.max && builtin::clz(fixed_uint_t(V.bounds.max)) + 1 >= builtin::clz(mask))
        bounds.max = from_bits(bits, result.cm).max;

    result[0] = apply_mask(normalize({ bounds, bits }, result.cm), result.cm);
};

ABSTRACT(SSA_ror) = ABSTRACT_FN
{
    assert(argn == 2);
    assert(result.cm.mask == cv[0].cm.mask);
    assert(CARRY_MASK == cv[1].cm);
    assert(!result.cm.signed_);
    assert(result.vec.size() == 2);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    fixed_uint_t const mask = result.cm.mask;
    fixed_uint_t const C_mask = high_bit_only(mask);
    constraints_mask_t const V_cm = { cv[0].cm.mask, false };
    constraints_t V = cv[0][0];
    constraints_t const C = cv[1][0];
    constraints_t const shifted_C = constraints_t::shifted_carry(C.to_carry(), C_mask);

    if(cv[0].cm.signed_)
        V = normalize(apply_mask(V, V_cm), V_cm);

    assert(V.is_normalized(V_cm));
    assert(C.is_normalized(cv[1].cm));

    // Calc the output carry:
    fixed_uint_t const carry_out_bit = low_bit_only(mask);
    result[1].bits = known_bits_t::bottom(CARRY_MASK);
    if(V.bits.known0 & carry_out_bit)
        result[1].bits.known0 |= 1ull << fixed_t::shift;
    if(V.bits.known1 & carry_out_bit)
        result[1].bits.known1 |= 1ull << fixed_t::shift;
    result[1].bounds = from_bits(result[1].bits, CARRY_MASK);

    // Calc known bits
    known_bits_t bits = V.bits;
    bits.known0 >>= 1;
    bits.known1 >>= 1;
    bits.known0 &= ~C_mask;
    bits.known1 &= ~C_mask;
    bits.known0 |= shifted_C.bits.known0 & C_mask;
    bits.known1 |= shifted_C.bits.known1 & C_mask;
    bits.known0 |= ~mask;
    bits.known1 &= mask;

    // Calc bounds
    bounds_t bounds;
    bounds.min = (V.bounds.min >> 1) + (!!C.bounds.min * C_mask);
    bounds.max = (V.bounds.max >> 1) + (!!C.bounds.max * C_mask);

    if(V.bounds.min && builtin::ctz(fixed_uint_t(V.bounds.min)) >= builtin::ctz(mask))
        bounds = from_bits(bits, result.cm);
    else if(V.bounds.max && signed_clz(fixed_uint_t(V.bounds.max)) >= builtin::ctz(mask))
        bounds.max = from_bits(bits, result.cm).max;

    result[0] = apply_mask(normalize({ bounds, bits }, result.cm), result.cm);
};


#if 0

/*

/* TODO
ABSTRACT(SSA_copy_array) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    auto& input_array = cv[2];
    assert(input_array.vec.size() == result.vec.size());
    result = input_array;
};

ABSTRACT(SSA_fill_array) = ABSTRACT_FN
{
    if(handle_top(cv, argn, result))
        return;

    constraints_t const value = cv[2][0];
    result.vec.assign(result.vec.size(), value);
};
*/


ABSTRACT(SSA_mul) = ABSTRACT_FN
{
    assert(false); // TODO: properly handle signed/unsigned!
    /*
    assert(argn == 2);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    fixed_uint_t const mask = result.mask;
    constraints_t const L = cv[0][0];
    constraints_t const R = cv[1][0];

    assert(L.is_normalized());
    assert(R.is_normalized());

    __int128 const min = builtin::mul128(L.bounds.min, R.bounds.min) >> __int128(fixed_t::shift);
    __int128 const max = builtin::mul128(L.bounds.max, R.bounds.max) >> __int128(fixed_t::shift);
    fixed_uint_t const masked_min = min & mask;
    fixed_uint_t const masked_max = max & mask;

    if(masked_min == masked_max)
        result[0] = constraints_t::const_(masked_min);
    else if(min > __int128(mask) || max > __int128(mask))
        result[0] = constraints_t::bottom(mask);
    else
    {
        assert(masked_min < masked_max);
        result[0].bounds = { .min = masked_min, .max = masked_max };
        result[0].bits = apply_mask(mask, from_bounds(result[0].bounds));
    }

    assert(result[0].bounds.max <= result.mask && result[0].is_normalized());
    */
};

ABSTRACT(SSA_shr) = ABSTRACT_FN
{
    assert(0);
    /*
    assert(argn == 2);
    assert(result.mask == cv[0].mask);
    assert((cv[1].mask & numeric_bitmask(TYPE_U)) == cv[1].mask);

    if(handle_top(cv, argn, result))
        return;

    // Inputs:
    fixed_uint_t const mask = result.mask;
    constraints_t const L = cv[0][0];
    constraints_t const R = cv[1][0];

    assert(L.is_normalized());
    assert(R.is_normalized());

    // Convert R to whole
    fixed_uint_t const R_min = R.bounds.min >> fixed_t::shift;
    fixed_uint_t const R_max = R.bounds.max >> fixed_t::shift;
    assert(R_min < 256 && R_max < 256);
    assert(R_min <= R_max);

    // Calc known bits

    known_bits_t bits = L.bits;
    bits.known0 >>= R_min;
    bits.known0 |= ~(mask >> R_min);
    bits.known1 >>= R_min;

    for(unsigned i = R_min; i < R_max; ++i)
    {
        bits.known0 &= (bits.known0 >> 1ull);
        bits.known1 &= (bits.known1 >> 1ull);
    }

    bits.known0 |= ~mask;
    bits.known1 &= mask;

    // Calc bounds
    bounds_t bounds;
    bounds.min = (L.bounds.min >> R_max) & mask;
    bounds.max = (L.bounds.max >> R_min) & mask;

    result[0] = apply_mask(mask, normalize({ bounds, bits }));
    */
};
#endif


static constexpr auto narrow_bottom = NARROW_FN
{
    constraints_t& input = cv[0][0];
    input.bits.known0 |= result[0].bits.known0 & cv[0].cm.mask;
    input.bits.known1 |= result[0].bits.known1 & cv[0].cm.mask;
};

NARROW(SSA_uninitialized) = narrow_bottom;
NARROW(SSA_fn_call) = narrow_bottom;
NARROW(SSA_cast) = narrow_bottom;

// TODO: implement
NARROW(SSA_get_byte) = narrow_bottom;
NARROW(SSA_array_get_byte) = narrow_bottom;
NARROW(SSA_replace_byte) = narrow_bottom;

NARROW(SSA_sign_extend) = NARROW_FN
{
    assert(argn == 1);

    if(!result[0].is_const())
        return;
    
    fixed_uint_t const sign_bit = high_bit_only(cv[0].cm.mask);

    if(result[0].get_const() == 0ull)
        cv[0][0].bits.known0 |= sign_bit;
    else if((result[0].get_const() & cv[0].cm.mask) == cv[0].cm.mask)
        cv[0][0].bits.known1 |= sign_bit;
};

NARROW(SSA_sign) = NARROW_FN
{
    assert(argn == 1);

    if(!result[0].is_const())
        return;
    
    fixed_uint_t const sign_bit = high_bit_only(cv[0].cm.mask);

    if(result[0].get_const())
        cv[0][0].bits.known1 |= sign_bit;
    else
        cv[0][0].bits.known0 |= sign_bit;
};

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
        assert(cv[j].cm.mask == result.cm.mask);
        cv[j][i] = intersect(cv[j][i], result[i]);
    }
};

NARROW(SSA_and) = NARROW_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(cv[0].cm == result.cm);
    assert(cv[1].cm == result.cm);

    if(result[0].is_top(result.cm))
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
    assert(cv[0].cm == result.cm);
    assert(cv[1].cm == result.cm);

    if(result[0].is_top(result.cm))
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
    assert(cv[0].cm == result.cm);
    assert(cv[1].cm == result.cm);

    if(result[0].is_top(result.cm))
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

template<bool Add>
void narrow_add_sub(constraints_def_t* cv, unsigned argn, constraints_def_t const& result)
{
    assert(argn == 3 && result.vec.size() >= 2);
    assert(result.cm == cv[0].cm);
    assert(result.cm == cv[1].cm);
    assert(CARRY_MASK == cv[2].cm);
    assert(cv[0].vec.size() >= 1);
    assert(cv[1].vec.size() >= 1);
    assert(cv[2].vec.size() >= 1);

    constraints_mask_t const cm = result.cm;

    if(any_top(result))
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];
    constraints_t& C = cv[2][0];

    std::cout << "NAR OG" << Add << std::endl;
    std::cout << L << std::endl;
    std::cout << R << std::endl;
    std::cout << C << std::endl;
    std::cout << result[0] << std::endl;

    known_bits_t R_bits = R.bits;
    if(!Add)
        std::swap(R_bits.known0, R_bits.known1);

    assert(L.is_normalized(cm) && R.is_normalized(cm));

    // We use an approximation approach.
    // We can solve bit equations of the form KNOWN ^ KNOWN ^ UNKNOWN = KNOWN
    // (Three arguments because of carries).

    // Determine some of the carried bits:
    fixed_uint_t carry0 = ((L.bits.known0 & R_bits.known0 & cm.mask) << 1ull) & cm.mask;
    fixed_uint_t carry1 = ((L.bits.known1 & R_bits.known1 & cm.mask) << 1ull) & cm.mask;

    fixed_uint_t const carry_i = ~cm.mask ? (~(cm.mask << 1) & cm.mask) : 1;
    
    // First do the carry. If we know the lowest bit of L, R, and result
    // we can infer the required carry.
    if(result[0].bits.known() & L.bits.known() & R_bits.known() & carry_i)
    {
        if((result[0].bits.known1 ^ L.bits.known1 ^ R_bits.known1) & carry_i)
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
    
    fixed_uint_t const solvable = result[0].bits.known() & (carry0 | carry1);
    fixed_uint_t const lsolvable = R_bits.known() & solvable;
    fixed_uint_t const rsolvable = L.bits.known() & solvable;


    if(Add)
    {
        L.bits.known1 |= ((carry1 ^ R_bits.known1 ^ result[0].bits.known1) & lsolvable);
        R.bits.known1 |= ((carry1 ^ L.bits.known1 ^ result[0].bits.known1) & rsolvable);
        L.bits.known0 |= ~L.bits.known1 & lsolvable;
        R.bits.known0 |= ~R.bits.known1 & rsolvable;
    }
    else
    {
        L.bits.known1 |= ((carry1 ^ R_bits.known1 ^ result[0].bits.known1) & lsolvable);
        R.bits.known0 |= ((carry1 ^ L.bits.known1 ^ result[0].bits.known1) & rsolvable);
        L.bits.known0 |= ~L.bits.known1 & lsolvable;
        R.bits.known1 |= ~R.bits.known1 & rsolvable;
    }

    std::cout << "NAR" << std::endl;
    std::cout << L << std::endl;
    std::cout << R << std::endl;

    // Move the bounds in after calculating bits.
    L.bounds = intersect(L.bounds, from_bits(L.bits, cm));
    R.bounds = intersect(R.bounds, from_bits(R.bits, cm));

    std::cout << "NAR" << std::endl;
    std::cout << L << std::endl;
    std::cout << R << std::endl;

    L.normalize(cm);
    R.normalize(cm);

    // Keep moving the bounds in even further, if possible.
    //if(cm.signed_)
        //return;

    constraints_t value = result[0];
    bounds_t const bb = bounds_t::bottom(cm);
    constraints_t const shifted_C = constraints_t::shifted_carry(C.to_carry(), cm.mask);

    if(Add)
    {
        fixed_sint_t const min_sum = L.bounds.min + R.bounds.min + shifted_C.bounds.min;
        fixed_sint_t const max_sum = L.bounds.max + R.bounds.max + shifted_C.bounds.max;
        fixed_uint_t const span = max_sum - min_sum;

        if(max_sum > bb.max)
        {
            if(min_sum <= bb.max)
                return;

            value.bounds.min += high_bit_only(cm.mask) << 1;
            value.bounds.max += high_bit_only(cm.mask) << 1;
        }
        else if(min_sum < bb.min)
        {
            assert(cm.signed_);

            if(max_sum >= bb.min)
                return;

            value.bounds.min -= high_bit_only(cm.mask) << 1;
            value.bounds.max -= high_bit_only(cm.mask) << 1;
        }

        // If the result's max is less than expected, 
        // try lowering L and R's max bound.
        L.bounds.max = std::min(L.bounds.max, value.bounds.max - R.bounds.min - shifted_C.bounds.min);
        R.bounds.max = std::min(R.bounds.max, value.bounds.max - L.bounds.min - shifted_C.bounds.min);

        // If the result's min is greater than expected,
        // try raising L and R's min bound.
        if(value.bounds.min > R.bounds.max + shifted_C.bounds.max)
            L.bounds.min = std::max(L.bounds.min, value.bounds.min - R.bounds.max - shifted_C.bounds.max);
        if(value.bounds.min > L.bounds.max + shifted_C.bounds.max)
            R.bounds.min = std::max(R.bounds.min, value.bounds.min - L.bounds.max - shifted_C.bounds.max);
    }
    else
    {
        fixed_sint_t const one = static_cast<fixed_sint_t>(low_bit_only(cm.mask));
        fixed_sint_t const min_sum = L.bounds.min - R.bounds.max - (one - shifted_C.bounds.min);
        fixed_sint_t const max_sum = L.bounds.max - R.bounds.max - (one - shifted_C.bounds.max);
        fixed_uint_t const span = max_sum - min_sum;

        if(max_sum > bb.max)
        {
            assert(cm.signed_);

            if(min_sum <= bb.max)
                return;

            value.bounds.min += high_bit_only(cm.mask) << 1;
            value.bounds.max += high_bit_only(cm.mask) << 1;
        }
        else if(min_sum < bb.min)
        {
            if(max_sum >= bb.min)
                return;

            value.bounds.min -= high_bit_only(cm.mask) << 1;
            value.bounds.max -= high_bit_only(cm.mask) << 1;
        }

        // If the result's max is less than expected, 
        // try lowering L and R's max bound.
        L.bounds.max = std::min(L.bounds.max, value.bounds.max + R.bounds.max + (one - shifted_C.bounds.max));
        R.bounds.max = std::min(R.bounds.max, L.bounds.min - value.bounds.min - (one - shifted_C.bounds.min));

        // If the result's min is greater than expected, 
        // try raising L and R's min bound.
        if(value.bounds.min > R.bounds.max + shifted_C.bounds.max)
            L.bounds.min = std::max(L.bounds.min, value.bounds.min + R.bounds.min + (one - shifted_C.bounds.min));
        if(value.bounds.min > L.bounds.max + shifted_C.bounds.max)
            R.bounds.min = std::max(R.bounds.min, L.bounds.max - value.bounds.max - (one - shifted_C.bounds.max));
    }
};

NARROW(SSA_add) = narrow_add_sub<true>;
NARROW(SSA_sub) = narrow_add_sub<false>;

NARROW(SSA_mul) = narrow_bottom;

template<bool Eq>
static void narrow_eq(constraints_def_t* cv, unsigned argn, constraints_def_t const& result)
{
    assert(result.vec.size() >= 1);
    assert(argn == 2);

    if(!result[0].is_const())
        return;

    if(!!result[0].get_const() == Eq)
        cv[0][0] = cv[1][0] = intersect(cv[0][0], cv[1][0]);
    else
    {
        assert(result[0].get_const() == fixed_t::whole(!Eq).value);
        for(unsigned i = 0; i < 2; ++i)
        {
            constraints_t& a = cv[i][0];
            constraints_t& b = cv[1 - i][0];

            if(!a.is_const())
                continue;

            fixed_uint_t const const_ = a.get_const();

            if(b.bounds.umin() == const_)
                ++b.bounds.min;
            if(b.bounds.umax() == const_)
                --b.bounds.max;
        }
    }
}

NARROW(SSA_trace) = NARROW_FN {};

NARROW(SSA_eq) = narrow_eq<true>;
NARROW(SSA_not_eq) = narrow_eq<false>;

NARROW(SSA_multi_lt) = NARROW_FN {};
NARROW(SSA_multi_lte) = NARROW_FN {};

template<bool Eq>
static void narrow_multi_eq(constraints_def_t* cv, unsigned argn, constraints_def_t const& result)
{
    assert(result.vec.size() >= 1);
    assert(argn % 2 == 0);
    assert(argn >= 2);

    if(!result[0].is_const() || !!result[0].get_const() != Eq)
        return;

    for(unsigned i = 0; i < argn; i += 2)
        cv[i][0] = cv[i+1][0] = intersect(cv[i][0], cv[i+1][0]);

    // Last comparison cares about sign.
    if(cv[argn-1][0].bits.known1 & cv[argn-2][0].bits.known1 & high_bit_only(cv[argn-1].cm.mask))
        cv[argn-1][0] = cv[argn-2][0] = constraints_t::top();
}

NARROW(SSA_multi_eq) = narrow_multi_eq<true>;
NARROW(SSA_multi_not_eq) = narrow_multi_eq<false>;

NARROW(SSA_lt) = NARROW_FN
{
    assert(result.vec.size() >= 1);
    assert(argn == 2);

    if(!result[0].is_const())
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];

    if(result[0].get_const())
    {
        L.bounds.max = std::min(L.bounds.max, R.bounds.max - static_cast<fixed_sint_t>(low_bit_only(cv[0].cm.mask)));
        R.bounds.min = std::max(R.bounds.min, L.bounds.min + static_cast<fixed_sint_t>(low_bit_only(cv[1].cm.mask)));
    }
    else
    {
        L.bounds.min = std::max(L.bounds.min, R.bounds.min);
        R.bounds.max = std::min(R.bounds.max, L.bounds.max);
    }
};

NARROW(SSA_lte) = NARROW_FN
{
    assert(result.vec.size() >= 1);
    assert(argn == 2);

    if(!result[0].is_const())
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];

    if(result[0].get_const())
    {
        L.bounds.max = std::min(L.bounds.max, R.bounds.max);
        R.bounds.min = std::max(R.bounds.min, L.bounds.min);
    }
    else
    {
        L.bounds.min = std::max(L.bounds.min, R.bounds.min + static_cast<fixed_sint_t>(low_bit_only(cv[0].cm.mask)));
        R.bounds.max = std::min(R.bounds.max, L.bounds.max - static_cast<fixed_sint_t>(low_bit_only(cv[1].cm.mask)));
    }
};


NARROW(SSA_shl) = NARROW_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(result.cm.mask == cv[0].cm.mask);
    assert((cv[1].cm.mask & numeric_bitmask(TYPE_U)) == cv[1].cm.mask);

    if(result[0].is_top(result.cm))
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];
    fixed_uint_t const mask = result.cm.mask;

    // We can narrow L if 'R' is constant
    if(R.is_const())
    {
        unsigned const Rc = R.get_const() >> fixed_t::shift;

        L.bits.known0 |= (result[0].bits.known0 >> Rc) & mask & (mask >> Rc);
        L.bits.known1 |= (result[0].bits.known1 >> Rc) & mask & (mask >> Rc);
    }
    else
    {
        // Narrow 'R' by comparing trailing bits.

        if(~L.bits.known0)
        {
            int r;
            if(~result[0].bits.known0)
                r = builtin::ctz(~result[0].bits.known0);
            else
                r = builtin::rclz(mask);

            int const diff = r - (int)builtin::ctz(~L.bits.known0);
            
            if(diff > 0)
                R.bounds.min = std::max<fixed_uint_t>(R.bounds.min, (fixed_uint_t)diff << fixed_t::shift);
        }

        if(result[0].bits.known1)
        {
            int const r = builtin::ctz(result[0].bits.known1);
            int const diff = r - (int)builtin::ctz(mask);
            R.bounds.max = std::min<fixed_uint_t>(R.bounds.max, (fixed_uint_t)diff << fixed_t::shift);
        }
    }
};

NARROW(SSA_shr) = NARROW_FN
{
    assert(argn == 2 && result.vec.size() >= 1);
    assert(result.cm.mask == cv[0].cm.mask);
    assert((cv[1].cm.mask & numeric_bitmask(TYPE_U)) == cv[1].cm.mask);

    if(result[0].is_top(result.cm))
        return;

    constraints_t& L = cv[0][0];
    constraints_t& R = cv[1][0];
    fixed_uint_t const mask = result.cm.mask;

    // We can narrow L if 'R' is constant
    if(R.is_const())
    {
        unsigned const Rc = R.get_const() >> fixed_t::shift;

        L.bits.known0 |= (result[0].bits.known0 << Rc) & mask & (mask << Rc);
        L.bits.known1 |= (result[0].bits.known1 << Rc) & mask & (mask << Rc);
    }
    else if(!result.cm.signed_)
    {
        // Narrow 'R' by comparing leading bits.

        if(~L.bits.known0)
        {
            int r;
            if(~result[0].bits.known0)
                r = builtin::clz(~result[0].bits.known0);
            else
                r = builtin::rctz(mask);

            int const diff = r - (int)builtin::clz(~L.bits.known0);
            
            if(diff > 0)
                R.bounds.min = std::max<fixed_uint_t>(R.bounds.min, (fixed_uint_t)diff << fixed_t::shift);
        }

        if(result[0].bits.known1)
        {
            int const r = builtin::clz(result[0].bits.known1);
            int const diff = r - (int)builtin::clz(mask);
            R.bounds.max = std::min<fixed_uint_t>(R.bounds.max, (fixed_uint_t)diff << fixed_t::shift);
        }
    }
};

NARROW(SSA_rol) = NARROW_FN
{
    // We can use abtract 'ror' to implement this.

    constraints_def_t args[2] = { { result.cm, { result[0] }}, { CARRY_MASK, { result[1] }} };
    args[0][0].normalize(args[0].cm);
    args[1][0].normalize(args[1].cm);

    constraints_def_t ror_result;
    ror_result.cm = result.cm;
    ror_result.vec.resize(2);

    abstract_fn_table[SSA_ror](args, 2, ror_result);

    cv[0][0] = intersect(cv[0][0], ror_result[0]);
    cv[1][0] = intersect(cv[1][0], ror_result[1]);
};

NARROW(SSA_ror) = NARROW_FN
{
    // We can use abtract 'rol' to implement this.

    constraints_def_t args[2] = { { result.cm, { result[0] }}, { CARRY_MASK, { result[1] }} };
    args[0][0].normalize(args[0].cm);
    args[1][0].normalize(args[1].cm);

    constraints_def_t rol_result;
    rol_result.cm = result.cm;
    rol_result.vec.resize(2);

    abstract_fn_table[SSA_rol](args, 2, rol_result);

    cv[0][0] = intersect(cv[0][0], rol_result[0]);
    cv[1][0] = intersect(cv[1][0], rol_result[1]);
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
