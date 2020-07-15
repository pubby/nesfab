#include "fixed.hpp"

fixed_lut_t<fixed_t::int_type> arithmetic_bitmask_table = []()
{
    fixed_lut_t<fixed_t::int_type> table;
    for(int i = TYPE_FIRST_ARITH; i <= TYPE_LAST_ARITH; ++i)
    {
        type_name_t type_name = (type_name_t)i;
        fixed_t::int_type v = 0;
        for(unsigned j = 0; j < frac_bytes(type_name); ++j)
            v |= 0xFFull << (8 * (2 - j));
        for(unsigned j = 0; j < whole_bytes(type_name); ++j)
            v |= 0xFFull << (8 * (3 + j));
        table[i - TYPE_FIRST_ARITH] = v;
    }
    return table;
}();

