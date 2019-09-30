#include "catch/catch.hpp"
#include "constraints.hpp"

#include <algorithm>
#include <cstdlib>
#include <ctime>

#include "alloca.hpp"

known_bits_t random_bits(fixed_t::int_type mask, bool allow_top = false)
{
    known_bits_t bits;
    do
        bits = { rand() | ~mask, rand() & mask };
    while(allow_top || bits.is_top());
    return bits;
}

constraints_t random_constraint(fixed_t::int_type mask, bool allow_top = false)
{
    if(rand() & 1)
    {
        bounds_t bounds;
        do
            bounds = { rand() & mask, rand() & mask };
        while(allow_top || bounds.is_top());
        return { bounds, from_bounds(bounds) };
    }
    else
    {
        known_bits_t bits = random_bits(mask, allow_top);
        return { from_bits(bits), bits };
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
    for(unsigned i = 0; i < 1000; ++i)
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
        for(fixed_t::int_type i = 0; i <= 15; ++i)
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
    for(unsigned i = 0; i < 1000; ++i)
    {
        constraints_t c1 = random_constraint(0xF << 4);
        constraints_t c2 = random_constraint(0xF << 4);
        for(fixed_t::int_type i = 0; i <= 15; ++i)
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
    for(unsigned i = 0; i < 1000; ++i)
    {
        constraints_t c1 = random_constraint(0xF << 4);
        constraints_t c2 = random_constraint(0xF << 4);
        for(fixed_t::int_type i = 0; i <= 15; ++i)
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
    for(unsigned i = 0; i < 1000; ++i)
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

    for(unsigned i = 0; i < 1000; ++i)
    {
        constraints_t c = random_constraint(0xFFF << 4, false);
        REQUIRE(normalize(c).bit_eq(normalize(normalize(c))));
        REQUIRE(normalize(c).is_normalized());
    }
}

template<typename Fn>
void test_op(ssa_op_t op, Fn fn)
{
    int argn = ssa_argn(op);
    if(argn < 0)
        argn = 3;
    REQUIRE(argn > 0);
    constraints_t* c = ALLOCA_T(constraints_t, argn);
    for(int i = 0; i < argn; ++i)
        c[i] = random_constraint(0xF << 24);
    constraints_t r = abstract_fn_table[op](0xF << 24, c, argn);
    REQUIRE(!r.is_top());

    if(argn > 3)
        return;

    for(unsigned i = 0; argn >= 3 && i < 16; ++i)
    for(unsigned j = 0; argn >= 2 && j < 16; ++j)
    for(unsigned k = 0; argn >= 1 && k < 16; ++k)
    {
        fixed_int_t a[3] = { i, j, k };
        fixed_int_t o;
        for(int i = 0; i < argn; ++i)
            if(!c[i](a[i] << 24))
                goto next_iter;
        o = fn(a, argn);
        o &= 0xF;
        o <<= 24;
        REQUIRE(r.bounds(o));
        REQUIRE(r.bits(o));
    next_iter:;
    }

    // Now test narrowing
    constraints_t* c_narrowed = ALLOCA_T(constraints_t, argn);
    for(int i = 0; i < argn; ++i)
        c_narrowed[i] = c[i];

    constraints_t n = random_subset(r);
    REQUIRE(!n.is_top());

    narrow_fn_table[op](0xF << 24, r, c_narrowed, argn);

    for(int i = 0; i < argn; ++i)
        if(c_narrowed[i].is_top())
            return;

    for(int i = 0; i < argn; ++i)
        REQUIRE(is_subset(c_narrowed[i], c[i]));

    for(unsigned i = 0; argn >= 3 && i < 16; ++i)
    for(unsigned j = 0; argn >= 2 && j < 16; ++j)
    for(unsigned k = 0; argn >= 1 && k < 16; ++k)
    {
        fixed_int_t a[3] = { i, j, k };
        fixed_int_t o;
        for(int i = 0; i < argn; ++i)
            if(!c_narrowed[i](a[i] << 24))
                goto next_iter2;
        o = fn(a, argn);
        o &= 0xF;
        o <<= 24;
        REQUIRE(n.bounds(o));
        REQUIRE(n.bits(o));
    next_iter2:;
    }
}

TEST_CASE("random_subset", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
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
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_cast, [](fixed_int_t* c, unsigned argn)
            { return c[0]; });
}

TEST_CASE("abstract_add", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_add, [](fixed_int_t* c, unsigned argn)
            { return c[0] + c[1]; });
}

TEST_CASE("abstract_and", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_and, [](fixed_int_t* c, unsigned argn)
            { return c[0] & c[1]; });
}

TEST_CASE("abstract_or", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_or, [](fixed_int_t* c, unsigned argn)
            { return c[0] | c[1]; });
}

TEST_CASE("abstract_xor", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_xor, [](fixed_int_t* c, unsigned argn)
            { return c[0] ^ c[1]; });
}

TEST_CASE("abstract_eq", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_eq, [](fixed_int_t* c, unsigned argn)
            { return c[0] == c[1]; });
}

TEST_CASE("abstract_not_eq", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_not_eq, [](fixed_int_t* c, unsigned argn)
            { return c[0] != c[1]; });
}

TEST_CASE("abstract_lt", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_lt, [](fixed_int_t* c, unsigned argn)
            { return c[0] < c[1]; });
}

TEST_CASE("abstract_lte", "[constraints]")
{
    std::srand(std::time(nullptr));
    for(unsigned i = 0; i < 1000; ++i)
        test_op(SSA_lte, [](fixed_int_t* c, unsigned argn)
            { return c[0] <= c[1]; });
}

