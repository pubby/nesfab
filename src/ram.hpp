#ifndef RAM_HPP
#define RAM_HPP

#include <cstdint>

#include "addr16.hpp"
#include "bitset.hpp"

constexpr addr16_t ram_size = 2048;
using ram_bitset_t = aggregate_bitset_t<bitset_uint_t, bitset_size<>(ram_size)>;
constexpr ram_bitset_t zp_bitset = { ~0ull, ~0ull, ~0ull, ~0ull };
constexpr ram_bitset_t stack_bitset = { 0ull, 0ull, 0ull, 0ull, ~0ull, ~0ull, ~0ull, ~0ull };

#endif
