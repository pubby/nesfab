#include "stmt.hpp"

std::string to_string(stmt_name_t stmt_name)
{
    switch(stmt_name)
    {
    default:
        if(is_var_init(stmt_name))
            return ("STMT_VAR_INIT " 
                    + std::to_string(get_local_var_i(stmt_name)));
        else
            return "bad stmt_name_t";

#define X(x) case x: return #x;
    STMT_XENUM
#undef X
    }
}
