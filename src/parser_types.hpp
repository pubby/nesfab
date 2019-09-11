#ifndef PARSER_TYPES_HPP
#define PARSER_TYPES_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "array_pool.hpp"
#include "lex_tables.hpp"
#include "pstring.hpp"
#include "types.hpp"

namespace bc = boost::container;

struct token_t
{
    using int_type = std::uint32_t;

    token_type_t type;
    pstring_t pstring;
    int_type value;

    // Used for debugging and logging.
    std::string to_string() const;
};

struct var_decl_t
{
    type_t type;
    pstring_t name;
};

using expr_temp_t = bc::small_vector<token_t, 16>;

inline token_t* expr_end(token_t* expr)
{
    while(expr->type)
        ++expr;
    return expr;
}

inline pstring_t expr_pstring(token_t* expr)
{
    assert(expr);
    assert(expr->type);
    return concat(expr->pstring, (expr_end(expr) - 1)->pstring);
}

extern array_pool_t<token_t> expr_pool;

#endif
