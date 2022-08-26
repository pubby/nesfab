#ifndef LT_HPP
#define LT_HPP

// LT = linktime

#include <deque>
#include <mutex>

#include "type.hpp"
#include "handle.hpp"
#include "parser_decl.hpp"
#include "ast.hpp"

struct lt_value_t
{
    static constexpr compiler_phase_t impl_deque_phase = PHASE_COMPILE;

    type_t type;
    ast_node_t ast;
};

lt_ht alloc_lt_value(type_t type, ast_node_t const& expr);

#endif
