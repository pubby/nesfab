#include "parser_types.hpp"

#include "format.hpp"

std::string token_t::to_string() const
{
    return fmt("{ %, %, % }", token_name(type), value, pstring.view());
}

