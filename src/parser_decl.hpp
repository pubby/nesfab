#ifndef PARSER_DECL_HPP
#define PARSER_DECL_HPP

#include <boost/container/small_vector.hpp>

#include "token.hpp"
#include "pstring.hpp"
#include "type.hpp"

namespace bc = boost::container;

template<typename Policy>
class parser_t;

struct var_decl_t
{
    type_t type = TYPE_VOID;
    pstring_t name;
};

using expr_temp_t = bc::small_vector<token_t, 16>;

#endif
