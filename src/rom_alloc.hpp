#ifndef ROM_ALLOC_HPP
#define ROM_ALLOC_HPP

#include <ostream>

struct span_t;
class span_allocator_t;
//class asm_proc_t; // TODO

void alloc_rom(std::ostream* log, span_allocator_t allocator, unsigned num_banks);

#endif
