#ifndef RAM_ALLOC_HPP
#define RAM_ALLOC_HPP

#include <ostream>

#include "debug_print.hpp"
#include "ram.hpp"

void alloc_ram(log_t* log, ram_bitset_t const& initial);

void print_ram(std::ostream& o);

#endif
