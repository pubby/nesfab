#ifndef CARRY_HPP
#define CARRY_HPP

#include <string>

enum carry_t : unsigned char
{
    CARRY_BOTTOM = 0b00,
    CARRY_CLEAR  = 0b01,
    CARRY_SET    = 0b10,
    CARRY_TOP    = 0b11,
};

std::string to_string(carry_t carry);

constexpr bool carry_const(carry_t cr)
{
    return cr == CARRY_CLEAR || cr == CARRY_SET;
}

constexpr carry_t carry_intersect(carry_t a, carry_t b)
{
    return carry_t(a | b);
}

#endif


