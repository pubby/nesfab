#include "catch/catch.hpp"
#include "constraints.hpp"

#include <algorithm>
#include <cstdlib>
#include <ctime>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "alloca.hpp"
#include "type_mask.hpp"

namespace bc = ::boost::container;

constexpr unsigned TEST_ITER = 1000;

SSA_VERSION(1);

known_bits_t random_bits(constraints_mask_t cm, bool allow_top = false)
{
    known_bits_t bits;
    do
        bits = { rand() | ~cm.mask, rand() & cm.mask };
    while(!allow_top && bits.is_top());
    return bits;
}

constraints_t random_constraint(constraints_mask_t cm, bool allow_top = false)
{
    if(rand() & 1)
    {
        bounds_t bounds;
        do
        {
            bounds = { rand() & cm.mask, rand() & cm.mask };
            if(cm.signed_ && (bounds.min & high_bit_only(cm.mask)))
                bounds.min |= above_mask(cm.mask); 
            if(cm.signed_ && (bounds.max & high_bit_only(cm.mask)))
                bounds.max |= above_mask(cm.mask); 
        }
        while(allow_top || bounds.is_top());
        constraints_t c = { bounds, from_bounds(bounds, cm) };
        c.bits.known0 |= ~cm.mask;
        constraints_t ret = normalize(c, cm);
        if(ret.is_top(cm))
        {
            std::cout << "TOP:" << std::endl;
            std::cout << c << std::endl;
            std::cout << ret << std::endl;
            std::cout << ret.bits.is_top() << std::endl;
            std::cout << ret.bounds.is_top() << std::endl;
            std::cout << (ret.bits.min(cm) > ret.bounds.max) << std::endl;
            std::cout << (ret.bits.max(cm) < ret.bounds.min) << std::endl;
            assert(0);
        }
        return ret;
    }
    else
    {
        known_bits_t bits = random_bits(cm, allow_top);
        constraints_t c = { from_bits(bits, cm), bits };
        constraints_t ret = normalize(c, cm);
        bool const b = ret.is_top(cm);
        bool const b2 = ret.is_top(cm);
        if(b)
        {
            std::cout << "TOP2:" << std::endl;
            std::cout << b << std::endl;
            std::cout << b2 << std::endl;
            std::cout << ret.is_top(cm) << std::endl;
            std::cout << c << std::endl;
            std::cout << ret << std::endl;
            std::cout << ret.is_top(cm) << std::endl;
            std::cout << ret.bits.is_top() << std::endl;
            std::cout << ret.bounds.is_top() << std::endl;
            std::cout << (ret.bits.min(cm) > ret.bounds.max) << std::endl;
            std::cout << (ret.bits.max(cm) < ret.bounds.min) << std::endl;
            //std::cout << std::hex << ret.bits.min(cm) << std::endl;
            std::cout << static_cast<fixed_sint_t>(ret.bits.min(cm)) << std::endl;
            assert(0);
        }
        return ret;
    }
}

constraints_t random_subset(constraints_t c, constraints_mask_t cm)
{
    if(c.is_top(cm))
        return c;

    unsigned tries = 0;
    constraints_t r;
    do
    {
        r = c;
        if(r.bounds.min != r.bounds.max)
            r.bounds.min += (rand() % (r.bounds.max - r.bounds.min)) & cm.mask;
        if(r.bounds.min != r.bounds.max)
            r.bounds.max -= (rand() % (r.bounds.max - r.bounds.min)) & cm.mask;

        r.bits = intersect(r.bits, from_bounds(r.bounds, cm));
        r = apply_mask(r, cm);

        ++tries;
        if(tries > 10)
            return c;
    }
    while(r.is_top(cm));
    return r;
}

TEST_CASE("const_", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned b = 0; b < 2; ++b)
    {
        constraints_mask_t const cm = { 0xF << 24, b };

        for(unsigned i = 0; i < TEST_ITER; ++i)
        {
            int rnum = std::rand() & 0xF;
            if(b)
                rnum -= 8;

            constraints_t constraint = constraints_t::whole(rnum, cm);

            //std::cout << "rnum =  " << rnum << std::endl;
            //std::cout << "whole =  " << constraint << std::endl;

            constraints_t masked = apply_mask(constraint, cm);
            REQUIRE(constraint.bit_eq(masked));

            //std::cout << "masked = " << masked << std::endl;
            //std::cout << constraint.bounds.umin() << ' ' << constraint.bounds.umax() << std::endl;

            REQUIRE(constraint.is_const());
            REQUIRE(!constraint.bits.is_top());
            REQUIRE(!constraint.bounds.is_top());
            REQUIRE(!constraint.is_top(cm));

            //bounds_t bounds = from_bits(constraint.bits, mask, b);
            //known_bits_t bits = from_bounds(constraint.bounds, mask, b);

            //std::cout << constraint << std::endl;
            //std::cout << constraints_t{ bounds, bits } << std::endl;
            //std::cout << std::endl;

            REQUIRE(from_bits(constraint.bits, cm).bit_eq(constraint.bounds));
            REQUIRE(from_bounds(constraint.bounds, cm).bit_eq(constraint.bits));
        }
    }
}

TEST_CASE("top", "[constraints]")
{
    {
        constraints_mask_t const cm = { 0xF << 24, false };

        REQUIRE(bounds_t::top().is_top());
        REQUIRE(known_bits_t::top().is_top());
        REQUIRE(constraints_t::top().is_top(cm));
        REQUIRE(!known_bits_t::top().is_const());
        REQUIRE(!constraints_t::top().is_const());
        REQUIRE(!constraints_t::whole(10, cm).is_top(cm));
        REQUIRE(!constraints_t::whole(5, cm).is_top(cm));
    }

    {
        constraints_mask_t const cm = { 0xF << 24, true };
        REQUIRE(!constraints_t::whole(-5, cm).is_top(cm));
    }
}

TEST_CASE("from_bits", "[constraints]")
{
    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    for(int imin = signed_ ? -8 : 0; imin < (signed_ ? 8 : 16); ++imin)
    for(int imax = imin; imax < (signed_ ? 8 : 16); ++imax)
    {
        constraints_mask_t const cm = { 0xF << 4, signed_ };

        bounds_t bounds = { imin << 4, imax << 4 };
        bounds = apply_mask(bounds, cm);
        known_bits_t bits = from_bounds(bounds, cm);

        for(unsigned i = 0; i < 16; ++i)
            if(bounds(i << 4, cm))
                REQUIRE(bits(i << 4, cm));

        known_bits_t test = known_bits_t::const_(imin << 4, cm);
        for(int i = imin; i <= imax; ++i)
        {
            test.known0 &= known_bits_t::const_(i << 4, cm).known0;
            test.known1 &= known_bits_t::const_(i << 4, cm).known1;
        }
        //std::cout << signed_ << std::endl;
        //std::cout << bits << std::endl;
        bits = apply_mask(bits, cm);

        //std::cout << bits << std::endl;
        //std::cout << test << std::endl;
        //std::cout << std::endl << std::endl;

        REQUIRE(test.known0 == bits.known0);
        REQUIRE(test.known1 == bits.known1);
    }
}

TEST_CASE("from_bounds", "[constraints]")
{
    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    for(int i0 = signed_ ? -8 : 0; i0 < (signed_ ? 8 : 16); ++i0)
    for(int i1 = i0; i1 < (signed_ ? 8 : 16); ++i1)
    {
        constraints_mask_t const cm = { 0xF << 4, signed_ };

        known_bits_t bits = { i0 << 4, i1 << 4 };
        bits = apply_mask(bits, cm);
        if(bits.is_top())
            continue;

        bounds_t bounds = from_bits(bits, cm);

        //std::cout << i0 << ' ' << i1 << std::endl;
        //std::cout << bits << std::endl;
        //std::cout << bounds << std::endl;
        //std::cout << bounds.umin() << ' ' << bounds.umax() << std::endl;
        //std::cout << std::endl;

        for(int i = signed_ ? -8 : 0; i < (signed_ ? 8 : 16); ++i)
            if(bits(i << 4, cm))
                REQUIRE(bounds(i << 4, cm));

        continue;

        bounds_t test = { 15 << 4, 0 };
        for(fixed_sint_t i = 0; i <= 15; ++i)
        {
            if(bits(i << 4, cm))
            {
                test.min = std::min(test.min, i << 4);
                test.max = std::max(test.max, i << 4);
            }
        }

        REQUIRE(test.min == bounds.min);
        REQUIRE(test.max == bounds.max);
    }
}

TEST_CASE("intersect", "[constraints]")
{
    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_mask_t const cm = { 0xF << 24, signed_ };

        constraints_t c1 = random_constraint(cm);
        constraints_t c2 = random_constraint(cm);
        for(fixed_uint_t i = 0; i <= 15; ++i)
        {
            constraints_t in = intersect(c1, c2);
            REQUIRE((c1(i<<24, cm) && c2(i<<24, cm)) == in(i<<24, cm));
        }
    }

    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    {
        constraints_mask_t const cm = { 0xF << 24, signed_ };

        REQUIRE(intersect(random_constraint(cm), constraints_t::top()).is_top(cm));
        REQUIRE(intersect(constraints_t::top(), random_constraint(cm)).is_top(cm));
        REQUIRE(intersect(constraints_t::top(), constraints_t::top()).is_top(cm));
    }
}

TEST_CASE("union_", "[constraints]")
{
    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_mask_t const cm = { 0xF << 4, signed_ };

        constraints_t c1 = random_constraint(cm);
        constraints_t c2 = random_constraint(cm);
        for(fixed_uint_t i = 0; i <= 15; ++i)
        {
            constraints_t un = union_(c1, c2);
            if(c1(i<<4, cm) || c2(i<<4, cm))
                REQUIRE(un(i<<4, cm));
        }
        REQUIRE(union_(c1, constraints_t::top()).bit_eq(c1));
        REQUIRE(union_(constraints_t::top(), c1).bit_eq(c1));
        REQUIRE(union_(constraints_t::top(), constraints_t::top()).is_top(cm));
    }
}

TEST_CASE("tighten_bounds", "[constraints]")
{
    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_mask_t const cm = { 0xFF, signed_ };

        known_bits_t bits = random_bits(cm);
        constraints_t c  = { bounds_t::bottom(cm), bits };
        REQUIRE(!c.is_top(cm));
        bounds_t bounds = tighten_bounds(c, cm);
        //std::cout << std::endl;
        //std::cout << c << std::endl;
        //std::cout << bounds << std::endl;
        REQUIRE(!bounds.is_top());
        //std::cout << std::endl;
        //std::cout << bounds << std::endl;
        //std::cout << from_bits(bits, 0xFF, signed_) << std::endl;
        REQUIRE(is_subset(from_bits(bits, cm), bounds));
        //std::cout << cm.mask << std::endl;
        //std::cout << c << std::endl;
        //std::cout << bounds << std::endl;
        REQUIRE(bits(bounds.umin() & cm.mask, cm));
        REQUIRE(bits(bounds.umax() & cm.mask, cm));
        for(int j = signed_ ? -128 : 0; j < (signed_ ? 128 : 256); ++j)
        {
            if(bits(j, cm))
                REQUIRE(bounds(j, cm));
        }
    }
}

TEST_CASE("normalize", "[constraints]")
{
    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    {
        {
            constraints_mask_t const cm = { 0xFF, signed_ };

            REQUIRE(normalize({{ 3, 9 }, { 0b1, 0 }}, cm).bounds.bit_eq({ 4, 8 }));
            REQUIRE(normalize({{ 3, 9 }, { 0b11, 0 }}, cm).bounds.bit_eq({ 4, 8 }));
            REQUIRE(normalize({{ 3, 9 }, { 0b10, 0 }}, cm).bounds.bit_eq({ 4, 9 }));
        }

        for(unsigned i = 0; i < TEST_ITER; ++i)
        {
            constraints_mask_t const cm = { 0xFFF << 4, signed_ };

            constraints_t c = random_constraint(cm);
            REQUIRE(!c.is_top(cm));
            REQUIRE(normalize(c, cm).bit_eq(normalize(normalize(c, cm), cm)));
            REQUIRE(normalize(c, cm).is_normalized(cm));
        }
    }
}

TEST_CASE("random_subset", "[constraints]")
{
    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    {
        constraints_mask_t const cm = { 0xF << 24, signed_ };

        std::srand(std::time(nullptr));
        for(unsigned i = 0; i < TEST_ITER; ++i)
        {
            constraints_t c = random_constraint(cm);
            //std::cout << "c = " << c << std::endl;
            constraints_t s = random_subset(c, cm);
            //std:
            REQUIRE(is_subset(s, c, cm));
            for(unsigned j = 0; j < 256; ++j)
                if(s(j, cm))
                    REQUIRE(c(j, cm));
        }
    }
}

struct int_cm_t
{
    static constraints_mask_t cm(bool signed_) { return { 0xF << 24, signed_ }; }
};

struct uint_cm_t
{
    static constraints_mask_t cm(bool signed_) { return { 0xF << 24, false }; }
};

struct bool_cm_t
{
    static constraints_mask_t cm(bool signed_) { return CARRY_MASK; }
};

//template<int Argn, bool SignRhs = true, typename Fn>
template<typename Result, typename... Args, typename Fn>
void test_op(ssa_op_t op, Fn fn, bool debug = false)
{
    constexpr int Argn = sizeof...(Args);

    for(unsigned signed_ = 0; signed_ < 2; ++signed_)
    {
        std::array<constraints_def_t, Argn> cv = {{ constraints_def_t{ Args::cm(signed_) }... }};
        for(auto& cd : cv)
        {
            cd.vec = { random_constraint(cd.cm) };
            assert(!cd[0].is_top(cd.cm));
        }

        constraints_def_t result;
        result.cm = Result::cm(signed_);
        result.vec.resize(2);

        if(!!signed_ != !!result.cm.signed_)
            continue;

        REQUIRE(abstract_fn_table[op]);
        abstract_fn_table[op](cv.data(), Argn, result);
        constraints_t r = result[0];
        constraints_t rc = result[1];

        if(debug)
        {
            std::cout << "args:" << std::endl;
            for(auto& cd : cv)
                std::cout << cd[0] << std::endl;
            std::cout << "result:\n" << r << std::endl;
        }

        REQUIRE(!r.is_top(result.cm));

        for(int i = 0; i < 16; ++i)
        for(int j = 0; j < 16; ++j)
        {
            fixed_sint_t a[2] = { i, j };
            if(signed_)
            {
                a[0] -= 8;
                a[1] -= 8;
            }
            for(int i = 0; i < Argn; ++i)
                if(!cv[i][0](a[i] << 24, result.cm))
                    goto next_iter;
            fixed_uint_t o;
            o = fn(a);
            o &= 0xF;
            if(signed_ && (o & 0b1000))
                o |= ~0xF;
            o <<= 24;
            if(debug)// && (!r.bounds(o, result.cm) || !r.bits(o, result.cm)))
            {
                std::cout << "signed: " << signed_ << std::endl;
                std::cout << "i j o: " << a[0] << ' ' << a[1] << ' ' << (fixed_sint_t(o) >> fixed_t::shift) << std::endl;
                std::cout << "args:" << std::endl;
                for(auto& cd : cv)
                    std::cout << cd[0] << std::endl;
                std::cout << "result:\n" << r << std::endl;
                std::cout << (a[0]) << std::endl;
                std::cout << (a[1]) << std::endl;
                std::cout << (o >> 24) << std::endl;
            }
            REQUIRE(r.bounds(o, result.cm));
            REQUIRE(r.bits(o, result.cm));
            REQUIRE(r.in_mask(result.cm));
            REQUIRE(r.is_normalized(result.cm));

            // If the inputs are const, the output should be as well.
            for(int i = 0; i < Argn; ++i)
                if(!cv[i][0].is_const())
                    goto not_const;
            REQUIRE(r.is_const());
        not_const:

        next_iter:;
        }

        // Now test narrowing
        bc::small_vector<constraints_def_t, 16> cvn(Argn);
        for(int i = 0; i < Argn; ++i)
        {
            cvn[i] = cv[i];
            REQUIRE(!cvn[i][0].is_top(cvn[i].cm));
        }

        // First check to make sure it preserves the original inputs:
        REQUIRE(narrow_fn_table[op]);
        narrow_fn_table[op](cvn.data(), Argn, result);

        for(int i = 0; i < Argn; ++i)
            REQUIRE(!cvn[i][0].is_top(cvn[i].cm));

        for(int i = 0; i < Argn; ++i)
        {
            //std::cout << std::endl;
            //std::cout << cvn[i][0] << std::endl;
            //std::cout << cv[i][0] << std::endl;
            REQUIRE(cvn[i][0].bit_eq(cv[i][0]));
            REQUIRE(!cvn[i][0].is_top(cvn[i].cm));
        }

        constraints_def_t result2;
        result2.cm = result.cm;
        result2.vec.resize(2);

        abstract_fn_table[op](cvn.data(), Argn, result2);

        REQUIRE(r.bit_eq(result2[0]));
        REQUIRE(rc.bit_eq(result2[1]));

        // Then check with a subset result:
        constraints_t n = random_subset(r, result.cm);
        //std::cout << std::endl;
        //std::cout << r << std::endl;
        //std::cout << n << std::endl;
        REQUIRE(is_subset(n, r, result.cm));
        REQUIRE(!n.is_top(result.cm));
        result2.vec = {{ n, result[1] }};

        narrow_fn_table[op](cvn.data(), Argn, result2);

        for(int i = 0; i < Argn; ++i)
        {
            cvn[i][0].normalize(cvn[i].cm);
            if(cvn[i][0].is_top(cvn[i].cm))
                goto next_iter2;
            REQUIRE(!cvn[i][0].is_top(cvn[i].cm));
        }

        for(int i = 0; i < Argn; ++i)
            REQUIRE(is_subset(cvn[i][0], cv[i][0], cvn[i].cm));

        abstract_fn_table[op](cvn.data(), Argn, result2);

        REQUIRE(is_subset(result2[0], r, result.cm));
    next_iter2:;
    }
}

TEST_CASE("abstract_cast", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t>(SSA_cast, [](fixed_sint_t* c){ return c[0]; });
}

TEST_CASE("abstract_and", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t, int_cm_t>(SSA_and, [](fixed_sint_t* c)
            { return c[0] & c[1]; });
}

TEST_CASE("abstract_or", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t, int_cm_t>(SSA_or, [](fixed_sint_t* c)
            { return c[0] | c[1]; });
}

TEST_CASE("abstract_xor", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t, int_cm_t>(SSA_xor, [](fixed_sint_t* c)
            { return c[0] ^ c[1]; });
}

TEST_CASE("abstract_add", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t, int_cm_t, bool_cm_t>(SSA_add, [](fixed_sint_t* c) { return c[0] + c[1]; });
}

TEST_CASE("abstract_eq", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<bool_cm_t, int_cm_t, int_cm_t>(SSA_eq, [](fixed_sint_t* c) { return c[0] == c[1]; });
}

TEST_CASE("abstract_not_eq", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<bool_cm_t, int_cm_t, int_cm_t>(SSA_not_eq, [](fixed_sint_t* c) { return c[0] != c[1]; });
}


TEST_CASE("abstract_lt", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<bool_cm_t, int_cm_t, int_cm_t>(SSA_lt, [](fixed_sint_t* c) { return c[0] < c[1]; });
}

TEST_CASE("abstract_lte", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<bool_cm_t, int_cm_t, int_cm_t>(SSA_lte, [](fixed_sint_t* c) { return c[0] <= c[1]; });
}

TEST_CASE("abstract_shl", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t, uint_cm_t>(SSA_shl, [](fixed_sint_t* c) { return c[0] << c[1]; });
}

TEST_CASE("abstract_shr", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t, uint_cm_t>(SSA_shr, [](fixed_sint_t* c) { return c[0] >> c[1]; });
}

TEST_CASE("abstract_rol", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<uint_cm_t, uint_cm_t, bool_cm_t>(SSA_rol, [](fixed_sint_t* c) { return (c[0] << 1) | c[1]; });
}

TEST_CASE("abstract_ror", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<uint_cm_t, uint_cm_t, bool_cm_t>(SSA_ror, [](fixed_sint_t* c) { return (c[0] >> 1) | (c[1] << 3); });
}

TEST_CASE("abstract_sign_extend", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<int_cm_t, int_cm_t>(SSA_sign_extend, [](fixed_sint_t* c) { return c[0] & 0b1000 ? 0xFF : 0; });
}

TEST_CASE("abstract_sign_to_carry", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<bool_cm_t, int_cm_t>(SSA_sign_to_carry, [](fixed_sint_t* c) { return !!(c[0] & 0b1000); });
}

/*
TEST_CASE("abstract_rol", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2, false>(SSA_shl, [](fixed_sint_t* c) { return c[0] << c[1]; }, true);
}
*/

/*
TEST_CASE("abstract_shr", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_shr, [](fixed_uint_t* c) { return c[0] >> c[1]; }, true);
}
*/

