#ifndef RAM_HPP
#define RAM_HPP

#include <cstdint>

#include "bitset.hpp"

// ds = data segment, i.e. all read/writable global variables.
// (Note that BSS doesn't exist. Just use DS for BSS vars.)

struct ds_region_t
{
    std::uint16_t short offset;
    std::uint16_t short size;
};

constexpr std::size_t ds_size = 1024;
using ds_bitset_t = aggregate_bitset_t<std::uint64_t, (ds_size + 63)/ 64>;

class ds_manager_t
{
public:
    ds_region_t alloc(std::uint16_t size)
    {
        ds_region_t ret = { used, size };
        used += size;
        // TODO: better error message.
        if(used > ds_size)
            throw std::runtime_error("Data segment overflow.");
        return ret;
    }

private:
    unsigned used = 0;
};

#endif
