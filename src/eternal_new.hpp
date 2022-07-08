#ifndef ETERNAL_NEW_HPP
#define ETERNAL_NEW_HPP

// Let's you allocate data that persists until program termination.
// (This could simply be 'return new T', but that triggers leak detectors.)

#include "array_pool.hpp"

template<typename T>
inline thread_local array_pool_t<T, 4096> eternal_new_pool;

template<typename T>
T const* eternal_new(T const* begin, T const* end)
{
    return eternal_new_pool<T>.insert(begin, end);
}

template<typename T, typename... Args>
T const* eternal_emplace(Args&&... args)
{
    return &eternal_new_pool<T>.emplace(std::forward<Args>(args)...);
}

#endif
