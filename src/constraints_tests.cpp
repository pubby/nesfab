#include "catch/catch.hpp"
#include "constraints.hpp"

#include <algorithm>
#include <cstdlib>
#include <ctime>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "alloca.hpp"

namespace bc = ::boost::container;

constexpr unsigned TEST_ITER = 1000;

SSA_VERSION(1);

known_bits_t random_bits(fixed_uint_t mask, bool allow_top = false)
{
    known_bits_t bits;
    do
        bits = { rand() | ~mask, rand() & mask };
    while(allow_top || bits.is_top());
    return bits;
}

constraints_t random_constraint(fixed_uint_t mask, bool allow_top = false)
{
    if(rand() & 1)
    {
        bounds_t bounds;
        do
            bounds = { rand() & mask, rand() & mask };
        while(allow_top || bounds.is_top());
        constraints_t c = { bounds, from_bounds(bounds) };
        c.bits.known0 |= ~mask;
        return normalize(c);
    }
    else
    {
        known_bits_t bits = random_bits(mask, allow_top);
        return normalize({ from_bits(bits), bits });
    }
}

constraints_t random_subset(constraints_t c)
{
    if(c.is_top())
        return c;

    unsigned tries = 0;
    constraints_t r;
    do
    {
        r = c;
        if(r.bounds.min != r.bounds.max)
            r.bounds.min += rand() % (r.bounds.max - r.bounds.min);
        if(r.bounds.min != r.bounds.max)
            r.bounds.max -= rand() % (r.bounds.max - r.bounds.min);

        r.bits = intersect(r.bits, from_bounds(r.bounds));;

        ++tries;
        if(tries > 10)
            return c;
    }
    while(r.is_top());
    return r;
}

TEST_CASE("const_", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_t constraint = constraints_t::const_(std::rand());
        REQUIRE(constraint.is_const());
        REQUIRE(!constraint.is_top());

        REQUIRE(from_bits(constraint.bits).bit_eq(constraint.bounds));
        REQUIRE(from_bounds(constraint.bounds).bit_eq(constraint.bits));
    }
}

TEST_CASE("top", "[constraints]")
{
    REQUIRE(bounds_t::top().is_top());
    REQUIRE(known_bits_t::top().is_top());
    REQUIRE(constraints_t::top().is_top());
    REQUIRE(!known_bits_t::top().is_const());
    REQUIRE(!constraints_t::top().is_const());
}

TEST_CASE("from_bits", "[constraints]")
{
    for(unsigned imin = 0; imin < 16; ++imin)
    for(unsigned imax = imin; imax < 16; ++imax)
    {
        bounds_t bounds = { imin << 4, imax << 4 };
        bounds = apply_mask(0xF << 4, bounds);
        known_bits_t bits = from_bounds(bounds);

        for(unsigned i = 0; i < 16; ++i)
            if(bounds(i << 4))
                REQUIRE(bits(i << 4));

        known_bits_t test = known_bits_t::const_(imin << 4);
        for(unsigned i = imin; i <= imax; ++i)
        {
            test.known0 &= known_bits_t::const_(i << 4).known0;
            test.known1 &= known_bits_t::const_(i << 4).known1;
        }
        bits = apply_mask(0xF << 4, bits);

        REQUIRE(test.known0 == bits.known0);
        REQUIRE(test.known1 == bits.known1);
    }
}

TEST_CASE("from_bounds", "[constraints]")
{
    for(unsigned i0 = 0; i0 < 16; ++i0)
    for(unsigned i1 = 0; i1 < 16; ++i1)
    {
        known_bits_t bits = { i0 << 4, i1 << 4 };
        bits = apply_mask(0x4 << 4, bits);
        if(bits.is_top())
            continue;

        bounds_t bounds = from_bits(bits);

        for(unsigned i = 0; i < 16; ++i)
            if(bits(i << 4))
                REQUIRE(bounds(i << 4));

        bounds_t test = { 15 << 4, 0 };
        for(fixed_uint_t i = 0; i <= 15; ++i)
        {
            if(bits(i << 4))
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
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_t c1 = random_constraint(0xF << 4);
        constraints_t c2 = random_constraint(0xF << 4);
        for(fixed_uint_t i = 0; i <= 15; ++i)
        {
            constraints_t in = intersect(c1, c2);
            REQUIRE((c1(i<<4) && c2(i<<4)) == in(i<<4));
        }
    }
    REQUIRE(intersect(random_constraint(0xF), constraints_t::top()).is_top());
    REQUIRE(intersect(constraints_t::top(), random_constraint(0xF)).is_top());
    REQUIRE(intersect(constraints_t::top(), constraints_t::top()).is_top());
}

TEST_CASE("union_", "[constraints]")
{
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_t c1 = random_constraint(0xF << 4);
        constraints_t c2 = random_constraint(0xF << 4);
        for(fixed_uint_t i = 0; i <= 15; ++i)
        {
            constraints_t un = union_(c1, c2);
            if(c1(i<<4) || c2(i<<4))
                REQUIRE(un(i<<4));
        }
        REQUIRE(union_(c1, constraints_t::top()).bit_eq(c1));
        REQUIRE(union_(constraints_t::top(), c1).bit_eq(c1));
        REQUIRE(union_(constraints_t::top(), constraints_t::top()).is_top());
    }
}

TEST_CASE("tighten_bounds", "[constraints]")
{
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        known_bits_t bits = random_bits(0xFF);
        constraints_t c  = {{ 0, 0xFF }, bits };
        bounds_t bounds = tighten_bounds(c);
        REQUIRE(!bounds.is_top());
        REQUIRE(is_subset(from_bits(bits), bounds));
        REQUIRE(bits(bounds.min));
        REQUIRE(bits(bounds.max));
        for(unsigned j = 0; j < 256; ++j)
        {
            if(bits(j))
                REQUIRE(bounds(j));
        }
    }
}

TEST_CASE("normalize", "[constraints]")
{
    REQUIRE(normalize({{ 3, 9 }, { 0b1, 0 }}).bounds.bit_eq({ 4, 8 }));
    REQUIRE(normalize({{ 3, 9 }, { 0b11, 0 }}).bounds.bit_eq({ 4, 8 }));
    REQUIRE(normalize({{ 3, 9 }, { 0b10, 0 }}).bounds.bit_eq({ 4, 9 }));

    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_t c = random_constraint(0xFFF << 4, false);
        REQUIRE(normalize(c).bit_eq(normalize(normalize(c))));
        REQUIRE(normalize(c).is_normalized());
    }
}

template<int Argn, typename Fn>
void test_op(ssa_op_t op, Fn fn, bool debug = false)
{
    static_assert(Argn <= 3 && Argn > 0);
    std::array<constraints_def_t, 3> cv;
    cv[0] = { 0xF << 24, { random_constraint(0xF << 24) }};
    cv[1] = { 0xF << 24, { random_constraint(0xF << 24) }};
    cv[2] = { 0xF << 24, { random_constraint(1 << 24) }}; // Carry

    constraints_def_t result;
    result.mask = 0xF << 24;
    result.vec.resize(2);

    abstract_fn_table[op](cv.data(), Argn, result);
    constraints_t r = result[0];
    constraints_t rc = result[1];
    REQUIRE(!r.is_top());

    for(unsigned i = 0; i < 16; ++i)
    for(unsigned j = 0; j < 16; ++j)
    {
        fixed_uint_t a[2] = { i, j };
        fixed_uint_t o;
        for(int i = 0; i < Argn; ++i)
            if(!cv[i][0](a[i] << 24))
                goto next_iter;
        o = fn(a);
        o &= 0xF;
        o <<= 24;
        if(debug && (!r.bounds(o) || !r.bits(o)))
        {
            std::cout << cv[0][0] << std::endl;
            std::cout << cv[1][0] << std::endl;
            std::cout << cv[2][0] << std::endl;
            std::cout << r << std::endl;
            std::cout << (a[0]) << std::endl;
            std::cout << (a[1]) << std::endl;
            std::cout << (o >> 24) << std::endl;
        }
        REQUIRE(r.bounds(o));
        REQUIRE(r.bits(o));

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
        REQUIRE(!cvn[i][0].is_top());
    }

    // First check to make sure it preserves the original inputs:
    narrow_fn_table[op](cvn.data(), Argn, result);

    for(int i = 0; i < Argn; ++i)
        REQUIRE(!cvn[i][0].is_top());

    for(int i = 0; i < Argn; ++i)
        REQUIRE(cvn[i][0].bit_eq(cv[i][0]));

    constraints_def_t result2;
    result2.mask = 0xF << 24;
    result2.vec.resize(2);

    abstract_fn_table[op](cvn.data(), Argn, result2);

    REQUIRE(r.bit_eq(result2[0]));
    REQUIRE(rc.bit_eq(result2[1]));


    // Then check with a subset result:
    constraints_t n = random_subset(r);
    REQUIRE(is_subset(n, r));
    REQUIRE(!n.is_top());
    result2.vec = {{ n, result[1] }};

    narrow_fn_table[op](cvn.data(), Argn, result2);

    for(int i = 0; i < Argn; ++i)
    {
        if(cvn[i][0].is_top())
            return;
        cvn[i][0].normalize();
    }

    for(int i = 0; i < Argn; ++i)
        REQUIRE(is_subset(cvn[i][0], cv[i][0]));

    abstract_fn_table[op](cvn.data(), Argn, result2);

    REQUIRE(is_subset(result2[0], r));

}

TEST_CASE("random_subset", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
    {
        constraints_t c = random_constraint(0xF);
        constraints_t s = random_subset(c);
        REQUIRE(is_subset(s, c));
        for(unsigned j = 0; j < 256; ++j)
            if(s(j))
                REQUIRE(c(j));
    }
}

TEST_CASE("abstract_cast", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<1>(SSA_cast, [](fixed_uint_t* c){ return c[0]; });
}

TEST_CASE("abstract_add", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<3>(SSA_add, [](fixed_uint_t* c) { return c[0] + c[1]; });
}

TEST_CASE("abstract_shl", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_shl, [](fixed_uint_t* c) { return c[0] << c[1]; });
}

TEST_CASE("abstract_shr", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_shr, [](fixed_uint_t* c) { return c[0] >> c[1]; }, true);
}

TEST_CASE("abstract_and", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_and, [](fixed_uint_t* c)
            { return c[0] & c[1]; });
}

TEST_CASE("abstract_or", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_or, [](fixed_uint_t* c)
            { return c[0] | c[1]; });
}

TEST_CASE("abstract_xor", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_xor, [](fixed_uint_t* c)
            { return c[0] ^ c[1]; });
}

TEST_CASE("abstract_eq", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_eq, [](fixed_uint_t* c)
            { return c[0] == c[1]; });
}

TEST_CASE("abstract_not_eq", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_not_eq, [](fixed_uint_t* c)
            { return c[0] != c[1]; });
}

TEST_CASE("abstract_lt", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_lt, [](fixed_uint_t* c)
            { return c[0] < c[1]; });
}

TEST_CASE("abstract_lte", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < TEST_ITER; ++i)
        test_op<2>(SSA_lte, [](fixed_uint_t* c)
            { return c[0] <= c[1]; });
}

