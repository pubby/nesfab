#include "token.hpp"

#include "format.hpp"

std::string token_t::to_string(char const* source) const
{
    return fmt("{ %, %, % }", token_name(type), value, pstring.view(source));
}

token_t const* token_t::new_expr(token_t const* begin, token_t const* end)
{
    static thread_local array_pool_t<token_t> expr_pool;
    return expr_pool.insert(begin, end);
}

