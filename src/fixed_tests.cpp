#include "catch/catch.hpp"
#include "fixed.hpp"

TEST_CASE("arithmetic_bitmask", "[fixed]")
{
    REQUIRE(arithmetic_bitmask(TYPE_BYTE) == 0xFFull << 24);
    REQUIRE(arithmetic_bitmask(TYPE_INT) == 0xFFFFFFull << 24);
    REQUIRE(arithmetic_bitmask(TYPE_arithmetic(0, 1)) == 0xFFull << 16);
    REQUIRE(arithmetic_bitmask(TYPE_arithmetic(1, 1)) == 0xFFFFull << 16);
    REQUIRE(arithmetic_bitmask(TYPE_arithmetic(1, 3)) == 0xFFFFFFFFull);
}

