#include "catch/catch.hpp"
#include "fixed.hpp"

TEST_CASE("numeric_bitmask", "[fixed]")
{
    REQUIRE(whole_bytes(TYPE_U) == 1);
    REQUIRE(frac_bytes(TYPE_U) == 0);

    REQUIRE(numeric_bitmask(TYPE_U) == 0xFFull << 24);
    REQUIRE(numeric_bitmask(TYPE_U30) == 0xFFFFFFull << 24);
    REQUIRE(numeric_bitmask(type_f(1)) == 0xFFull << 16);
    REQUIRE(numeric_bitmask(type_u(1, 1)) == 0xFFFFull << 16);
    REQUIRE(numeric_bitmask(type_s(1, 3)) == 0xFFFFFFFFull);
}

