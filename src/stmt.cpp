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

label_t* stmt_t::new_label()
{
    std::lock_guard<std::mutex> lock(label_pool_mutex);
    return &label_pool.emplace();
}

token_t const* stmt_t::new_expr(token_t const* begin, token_t const* end)
{
    std::lock_guard<std::mutex> lock(expr_pool_mutex);
    return expr_pool.insert(begin, end);
}

