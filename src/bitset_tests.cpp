#include "catch/catch.hpp"
#include "bitset.hpp"

#include <cstdlib>
#include <iostream>

void test_fill(bitset_t& bs, unsigned start, unsigned size)
{
    bs.clear_all();
    REQUIRE(bs.all_clear());
    bitset_set_n(bs.size(), bs.data(), start, size);

    for(unsigned bit = 0; bit < bs.num_bits(); ++bit)
    {
        INFO("bit = " << bit);
        REQUIRE(bs.test(bit) == (bit >= start && bit < start + size));
    }
}

TEST_CASE("bitset_set_n", "[bitset]")
{
    bitset_t bs(8);
    REQUIRE(bs.all_clear());

    test_fill(bs, 0, 64);
    test_fill(bs, 1, 64);
    test_fill(bs, 110, 64);
    test_fill(bs, 200, 64);

    test_fill(bs, 0, 7);
    test_fill(bs, 1, 7);
    test_fill(bs, 110, 7);
    test_fill(bs, 200, 7);

    test_fill(bs, 0, 300);
    test_fill(bs, 1, 300);
    test_fill(bs, 110, 300);
    test_fill(bs, 200, 300);

    test_fill(bs, 0, 0);
    test_fill(bs, 200, 0);
}

