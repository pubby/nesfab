#ifndef RAM_HPP
#define RAM_HPP

#include <cstdint>

#include "addr16.hpp"
#include "bitset.hpp"
#include "span.hpp"

// ds = data segment, i.e. all read/writable global variables.
// (Note that BSS doesn't exist. Just use DS for BSS vars.)

constexpr addr16_t ram_size = 2048;
using ram_bitset_t = aggregate_bitset_t<bitset_uint_t, bitset_size<>(ram_size)>;
constexpr ram_bitset_t zp_bitset = { ~0ull, ~0ull, ~0ull, ~0ull };

constexpr addr16_t usable_ram_size = ram_size - 512;
using usable_ram_bitset_t = aggregate_bitset_t<bitset_uint_t, bitset_size<>(usable_ram_size)>;

constexpr addr16_t page_size = 256;
using page_bitset_t = aggregate_bitset_t<bitset_uint_t, bitset_size<>(page_size)>;

/*
struct ram_region_t
{
    addr16_t offset;
    addr16_t size;

    explicit operator bool() const { return size; }
};

constexpr bool is_zp(addr16_t addr) { return addr < 256; }
constexpr bool is_zp(ram_region_t region)
{ 
    return region.offset < 256 && (region.offset + region.size) <= 256; 
}
*/

// Modifies 'ram' to only include addresses that can hold a contiguous span of 'size' bytes
// (This is useful for allocating multi-byte regions like arrays and pointers)
void ram_for_size(ram_bitset_t& ram, std::size_t size);
span_t alloc_ram(ram_bitset_t const& usable_ram, std::size_t size, bool zp_only);

// TODO: remove?
// Returns the first unused memory position of size 'size',
// and modifies 'rbs' with the allocation.
//ram_region_t alloc_ram(ram_bitset_t& rbs, addr16_t size);

// TODO: use this ? or delete
/*
class ram_allocator_t
{
public:
    explicit ram_allocator_t(ram_region_t region) : m_region(region) {}

    bool alloc(ram_region_t& region, addr16_t size)
    {
        if(m_region.size < size)
            return false;
        region = { m_region.offset, size };
        m_region.offset += size;
        m_region.size -= size;
        return true;
    }

private:
    ram_region_t m_region;
};
*/

#endif
