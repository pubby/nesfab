#ifndef ROM_ALLOC_HPP
#define ROM_ALLOC_HPP

#include "span.hpp"
#include "span_allocator.hpp"

void alloc_rom(span_allocator_t allocator, unsigned num_banks);

#endif
