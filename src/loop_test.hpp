#ifndef LOOP_TEST_HPP
#define LOOP_TEST_HPP

#include <utility>

// This is used inside "for_each" style functions.
// It converts functions returning void into functions returning 'true'.

template<typename Return>
struct loop_test_t
{
    template<typename Fn, typename... T>
    static bool call(Fn const& fn, T&&... t) { return fn(std::forward<T>(t)...); } 
};


template<>
struct loop_test_t<void>
{
    template<typename Fn, typename... T>
    static bool call(Fn const& fn, T&&... t) { fn(std::forward<T>(t)...); return true; } 
};

#define LOOP_TEST(FN, ...) ::loop_test_t<decltype((FN)(__VA_ARGS__))>::call((FN), __VA_ARGS__)

#endif
