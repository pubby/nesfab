#ifndef ROM_ALLOC_HPP
#define ROM_ALLOC_HPP

#include <ostream>

#include "debug_print.hpp"

struct span_t;
class span_allocator_t;
//class asm_proc_t; // TODO

void alloc_rom(log_t* log, span_allocator_t allocator, unsigned num_banks);

void print_rom(std::ostream& o);

#endif
