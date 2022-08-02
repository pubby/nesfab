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

pstring_t fn_def_t::find_global(global_t const* global) const
{
    for(stmt_t const& stmt : stmts)
    {
        if(!has_expression(stmt.name))
            continue;
        for(token_t const* token = stmt.expr; token->type; ++token)
            if(token->type == TOK_global_ident && token->ptr<global_t>() == global)
                return token->pstring;
    }
    return {};
}
