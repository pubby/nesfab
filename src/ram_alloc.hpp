#ifndef RAM_ALLOC_HPP
#define RAM_ALLOC_HPP

#include <ostream>

#include "ram.hpp"

void alloc_ram(ram_bitset_t const& initial);

void print_ram(std::ostream& o);

#endif
