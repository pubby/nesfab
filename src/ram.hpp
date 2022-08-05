#ifndef RAM_HPP
#define RAM_HPP

#include <cstdint>

#include "bitset.hpp"

constexpr std::uint16_t ram_size = 2048;
using ram_bitset_t = static_bitset_t<ram_size>;
using page_bitset_t = static_bitset_t<256>;
constexpr ram_bitset_t zp_bitset = { ~0ull, ~0ull, ~0ull, ~0ull };
constexpr ram_bitset_t stack_bitset = { 0ull, 0ull, 0ull, 0ull, ~0ull, ~0ull, ~0ull, ~0ull };

#endif
