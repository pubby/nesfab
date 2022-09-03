#ifndef AST_HPP
#define AST_HPP

#include <cstdint>

#include "token.hpp"

struct ast_node_t
{
    using int_type = std::uint64_t; // Should match fixed_uint_t

    token_t token = {};
    ast_node_t* children = nullptr;

    unsigned num_children() const;
};

#endif
