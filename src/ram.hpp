#ifndef RAM_HPP
#define RAM_HPP

#include <cstdint>

#include "bitset.hpp"

// ds = data segment, i.e. all read/writable global variables.
// (Note that BSS doesn't exist. Just use DS for BSS vars.)

using addr16_t = std::uint16_t;

constexpr addr16_t ram_size = 2048;
using ram_bitset_t = aggregate_bitset_t<std::uint64_t, (ram_size + 63)/ 64>;

struct ram_region_t
{
    addr16_t offset;
    addr16_t size;
};

constexpr bool is_zp(addr16_t addr) { return addr < 256; }
constexpr bool is_zp(ram_region_t region)
{ 
    return region.offset < 256 && (region.offset + region.size) <= 256; 
}

// TODO: use this
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


#endif
