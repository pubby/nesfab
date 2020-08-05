#ifndef STMT_HPP
#define STMT_HPP

#include <string>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "ir_decl.hpp"
#include "handle.hpp"
#include "parser_types.hpp"
#include "pstring.hpp"

namespace bc = ::boost::container;

using stmt_ht = handle_t<unsigned, struct stmt_ht_tag, ~0>;

#define STMT_XENUM \
    X(STMT_END_BLOCK)\
    X(STMT_EXPR)\
    X(STMT_IF)\
    X(STMT_ELSE)\
    X(STMT_WHILE)\
    X(STMT_DO)\
    X(STMT_RETURN)\
    X(STMT_BREAK)\
    X(STMT_CONTINUE)\
    X(STMT_LABEL)\
    X(STMT_GOTO)

// Negative values represent var inits, where the negated value 
// holds the bitwise negated index of the fn variable.
// (See 'get_local_var_i')
enum stmt_name_t : int
{
    STMT_MIN_VAR_DECL = INT_MIN,
    STMT_MAX_VAR_DECL = -1,
#define X(x) x,
    STMT_XENUM
#undef X
};

std::string to_string(stmt_name_t);

constexpr bool is_var_init(stmt_name_t stmt_name)
{
    return stmt_name < STMT_END_BLOCK;
}

constexpr unsigned get_local_var_i(stmt_name_t stmt_name)
{
    assert(is_var_init(stmt_name));
    return ~static_cast<unsigned>(stmt_name);
}

struct label_t
{
    cfg_ht node;
    stmt_ht stmt_h;
    unsigned goto_count;
    bc::small_vector<cfg_ht, 2> inputs;
};

struct stmt_t
{
    stmt_name_t name;
    pstring_t pstring;
    union
    {
        token_t const* expr;
        label_t* label;
    };
};

std::string to_string(stmt_name_t stmt_name);

#endif
