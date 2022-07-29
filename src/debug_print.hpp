#ifndef DEBUG_PRINT_HPP
#define DEBUG_PRINT_HPP

#include <cstdio>
#include <ostream>

#include "format.hpp"

#define DEBUG_PRINT

#ifdef DEBUG_PRINT
#define debug_printf(...) std::printf(__VA_ARGS__)
#define dprint(stream, ...) ((void)((stream) ? ((*(stream) << ::ezcat(__VA_ARGS__) << std::endl), 0) : 0))
#else
#define debug_printf(...) ((void)0)
#define dprint(...) ((void)0)
#endif

#endif
