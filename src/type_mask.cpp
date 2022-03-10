#include "type_mask.hpp"

#include "builtin.hpp"

fixed_lut_t<fixed_int_t> const numeric_bitmask_table = []()
{
    fixed_lut_t<fixed_int_t> table;
    for(int i = TYPE_FIRST_SCALAR; i <= TYPE_LAST_SCALAR; ++i)
    {
        type_name_t type_name = (type_name_t)i;

        fixed_int_t v = 0;

        if(type_name == TYPE_BOOL)
            v = 1ull << fixed_t::shift;
        else
        {
            for(unsigned j = 0; j < frac_bytes(type_name); ++j)
                v |= 0xFFull << (8 * (2 - j));
            for(unsigned j = 0; j < whole_bytes(type_name); ++j)
                v |= 0xFFull << (8 * (3 + j));
        }

        table[i - TYPE_FIRST_SCALAR] = v;
    }
    return table;
}();

fixed_lut_t<fixed_int_t> const numeric_sub_bitmask_table = []()
{
    fixed_lut_t<fixed_int_t> table;
    for(int i = TYPE_FIRST_SCALAR; i <= TYPE_LAST_SCALAR; ++i)
    {
        type_name_t type_name = (type_name_t)i;
        table[i - TYPE_FIRST_SCALAR] = (1ull << (8 * (3 - frac_bytes(type_name)))) - 1ull;
    }
    return table;
}();

fixed_lut_t<fixed_int_t> const numeric_super_bitmask_table = []()
{
    fixed_lut_t<fixed_int_t> table;
    for(int i = TYPE_FIRST_SCALAR; i <= TYPE_LAST_SCALAR; ++i)
    {
        type_name_t type_name = (type_name_t)i;
        table[i - TYPE_FIRST_SCALAR] = ~((1ull << (8 * (3 + whole_bytes(type_name)))) - 1ull);
    }
    return table;
}();

sfixed_int_t to_signed(fixed_int_t f, fixed_int_t bitmask)
{
    assert(bitmask);

    fixed_int_t const high_bit = bitmask & ~(bitmask >> 1ull);

    if(f & high_bit)
    {
        f |= ~0ull << builtin::rclz(bitmask);
        assert(static_cast<sfixed_int_t>(f) < 0ll);
    }

    return static_cast<sfixed_int_t>(f);
}

sfixed_int_t to_signed(fixed_int_t f, type_name_t type_name)
{
    if(is_signed(type_name))
        return to_signed(f, numeric_bitmask(type_name));
    return static_cast<sfixed_int_t>(f);
}
