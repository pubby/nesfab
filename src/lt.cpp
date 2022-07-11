#include "lt.hpp"

lt_ht alloc_lt_value(type_t type, expr_vec_t const& expr)
{
    lt_value_t* ptr;
    return { impl_deque_alloc(ptr, lt_value_t{ type, expr }) };
}
