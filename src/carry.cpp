#include "carry.hpp"

// For debugging mostly
std::string to_string(carry_t carry)
{
    switch(carry)
    {
    case CARRY_BOTTOM: return "CARRY BOTTOM";
    case CARRY_CLEAR:  return "CARRY CLEAR";
    case CARRY_SET:    return "CARRY SET";
    case CARRY_TOP:    return "CARRY TOP";
    default:        return "BAD CARRY";
    }
}

