#include "lt.hpp"

lt_ht alloc_lt_value(lt_value_t&& lt)
{
    lt_value_t* ptr;
    return { impl_deque_alloc(ptr, std::move(lt)) };
}
