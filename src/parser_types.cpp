#include "parser_types.hpp"

#include "format.hpp"

array_pool_t<token_t> expr_pool;

std::string token_t::to_string() const
{
    return fmt("{ %, %, % }", token_name(type), value, pstring.view());
}

