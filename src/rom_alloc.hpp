#ifndef ROM_ALLOC_HPP
#define ROM_ALLOC_HPP

struct span_t;
class span_allocator_t;
//class asm_proc_t; // TODO

void alloc_rom(span_allocator_t allocator, unsigned num_banks);

#endif
