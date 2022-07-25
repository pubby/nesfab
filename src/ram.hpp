#ifndef RAM_HPP
#define RAM_HPP

#include <cstdint>

#include "addr16.hpp"
#include "bitset.hpp"
#include "span.hpp"

class locator_t;

// ds = data segment, i.e. all read/writable global variables.
// (Note that BSS doesn't exist. Just use DS for BSS vars.)

constexpr addr16_t ram_size = 2048;
using ram_bitset_t = aggregate_bitset_t<bitset_uint_t, bitset_size<>(ram_size)>;
constexpr ram_bitset_t zp_bitset = { ~0ull, ~0ull, ~0ull, ~0ull };

constexpr addr16_t usable_ram_size = ram_size - 512;
using usable_ram_bitset_t = aggregate_bitset_t<bitset_uint_t, bitset_size<>(usable_ram_size)>;

constexpr addr16_t page_size = 256;
using page_bitset_t = aggregate_bitset_t<bitset_uint_t, bitset_size<>(page_size)>;

/* TODO
// RAM variables that will exist at all times.
namespace std_ram
{
    enum var_t
    {
#define STD_RAM(name, size, alignment, zp) STD_RAM_##name,
#include "std_ram.inc"
#undef STD_RAM
        NUM_VARS,
    };

    std::array<span_t, NUM_VARS> const& vars_array();

    span_t ram_span(var_t v);
    locator_t ram_locator(var_t v, std::uint16_t offset);
}
*/

#endif
