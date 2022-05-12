#include "type_mask.hpp"

#include "builtin.hpp"

fixed_sint_t to_signed(fixed_uint_t f, fixed_uint_t bitmask)
{
    assert(bitmask);

    fixed_uint_t const high_bit = bitmask & ~(bitmask >> 1ull);

    if(f & high_bit)
    {
        f |= ~0ull << builtin::rclz(bitmask);
        assert(static_cast<fixed_sint_t>(f) < 0ll);
    }

    return static_cast<fixed_sint_t>(f);
}

fixed_sint_t to_signed(fixed_uint_t f, type_name_t type_name)
{
    if(is_signed(type_name))
        return to_signed(f, numeric_bitmask(type_name));
    return static_cast<fixed_sint_t>(f);
}
