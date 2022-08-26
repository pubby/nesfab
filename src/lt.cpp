#include "lt.hpp"

lt_ht alloc_lt_value(type_t type, ast_node_t const& expr)
{
    lt_value_t* ptr;
    return lt_ht::pool_emplace(ptr, lt_value_t{ type, expr });
}
