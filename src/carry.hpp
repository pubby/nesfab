#ifndef CARRY_HPP
#define CARRY_HPP

#include <string>

enum carry_t : char
{
    CARRY_BOTTOM,
    CARRY_CLEAR,
    CARRY_SET,
    CARRY_TOP,
};

std::string to_string(carry_t carry);

constexpr bool carry_const(carry_t cr)
{
    return cr == CARRY_CLEAR || cr == CARRY_SET;
}

#endif


