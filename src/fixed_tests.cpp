#include "catch/catch.hpp"
#include "fixed.hpp"

TEST_CASE("numeric_bitmask", "[fixed]")
{
    REQUIRE(whole_bytes(TYPE_BYTE) == 1);
    REQUIRE(frac_bytes(TYPE_BYTE) == 0);

    REQUIRE(numeric_bitmask(TYPE_BYTE) == 0xFFull << 24);
    REQUIRE(numeric_bitmask(TYPE_INT) == 0xFFFFFFull << 24);
    REQUIRE(numeric_bitmask(TYPE_arithmetic(0, 1)) == 0xFFull << 16);
    REQUIRE(numeric_bitmask(TYPE_arithmetic(1, 1)) == 0xFFFFull << 16);
    REQUIRE(numeric_bitmask(TYPE_arithmetic(1, 3)) == 0xFFFFFFFFull);
}

