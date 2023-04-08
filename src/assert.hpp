#ifndef ASSERT_HPP
#define ASSERT_HPP

#include <cassert>
#include <cstdio>
#include <cstdlib>

#include "format.hpp"

// passert is like assert, but lets you print more information on failure.
#ifdef NDEBUG
#define passert(C, ...) ((void) 0)
#else
#define passert(C, ...) (void)((C) || (_passert_impl(#C, __FILE__, __LINE__, __PRETTY_FUNCTION__, __VA_ARGS__), 0))
#endif

template<typename... Args>
void _passert_impl(char const* expr, char const* file, long long line, char const* fn, Args const&... args)
{
    std::fflush(stdout);
    std::fprintf(stderr, "%s\n", 
        ezcat(" ", "assert: ", file, ':', line, ':', fn, ": Assertion `", expr, 
              "' failed. Additional info below.\n", args...).c_str());
    std::abort();
}

#endif
