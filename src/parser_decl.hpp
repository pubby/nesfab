#ifndef PARSER_DECL_HPP
#define PARSER_DECL_HPP

#include <string>

#include <boost/container/small_vector.hpp>

#include "token.hpp"
#include "pstring.hpp"
#include "type.hpp"

namespace bc = boost::container;

template<typename Policy>
class parser_t;

struct var_decl_t
{
    src_type_t src_type;
    pstring_t name;
};

struct string_literal_t
{
    std::string string;
    pstring_t pstring;
};

// TODO
//using expr_temp_t = bc::small_vector<token_t, 16>;
//using expr_vec_t = std::vector<token_t>;

#endif
