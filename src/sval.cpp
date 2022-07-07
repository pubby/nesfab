#include "sval.hpp"

#include "locator.hpp"

bool is_ct(sval_t const& sval)
{
    for(auto const& v : sval)
    {
        if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
        {
            if(ssa->holds_ref())
                return false;
        }
        else if(!std::holds_alternative<ct_array_t>(v))
            return false;
    }

    return true;
}

