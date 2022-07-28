#ifndef DEBUG_PRINT_HPP
#define DEBUG_PRINT_HPP

#include <cstdio>

#define DEBUG_PRINT

#ifdef DEBUG_PRINT
#define debug_printf(...) std::printf(__VA_ARGS__)
#else
#define debug_printf(...) ((void)0)
#endif

#endif
