#ifndef ROM_ALLOC_HPP
#define ROM_ALLOC_HPP

#include <ostream>

#include "debug_print.hpp"

struct span_t;
class span_allocator_t;

void alloc_rom(log_t* log, span_allocator_t allocator);

void print_rom(std::ostream& o);

#endif
