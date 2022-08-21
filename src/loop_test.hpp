#ifndef LOOP_TEST_HPP
#define LOOP_TEST_HPP

#include <utility>

// This is used inside "for_each" style functions.
// It converts functions returning void into functions returning 'true'.

template<typename Fn, typename Return>
struct loop_test_t
{
    Fn const& fn;

    template<typename... T>
    bool call(T&&... t) { return fn(std::forward<T>(t)...); } 
};


template<typename Fn>
struct loop_test_t<Fn, void>
{
    Fn const& fn;

    template<typename... T>
    bool call(T&&... t) { fn(std::forward<T>(t)...); return true; } 
};

#define LOOP_TEST(FN, ...) (::loop_test_t<decltype(FN), decltype((FN)(__VA_ARGS__))>{FN}.call(__VA_ARGS__))

#endif
