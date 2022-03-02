#include "stmt.hpp"

bool has_expression(stmt_name_t stmt_name)
{
    switch(stmt_name)
    {
    default: return false;
#define X(x, e) case x: return e;
    STMT_XENUM
#undef X
    }
}

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

#define X(x, e) case x: return #x;
    STMT_XENUM
#undef X
    }
}

token_t const* stmt_t::new_expr(token_t const* begin, token_t const* end)
{
    return expr_pool.insert(begin, end);
}

void fn_def_t::dethunkify()
{
    for(auto& decl : local_vars)
        decl.type = ::dethunkify(decl.type);
}
