#ifndef CG_ARRAY_HPP
#define CG_ARRAY_HPP

#include <cstdint>
#include <functional>
#include <mutex>
#include <vector>

#include "robin/set.hpp"

#include "ir.hpp"

struct rom_array_t
{
    std::vector<locator_t> data;

    auto operator<=>(rom_array_t const&) const = default;
};

template<>
struct std::hash<rom_array_t>
{
    std::size_t operator()(rom_array_t const& a) const noexcept
    {
        std::hash<locator_t> lh;

        // Include array size in the hash:
        std::size_t h = a.data.size();

        // Only hash the first 4 locators, for speed:
        unsigned const n = std::min<unsigned>(a.data.size(), 4);
        for(unsigned i = 0; i < n; ++i)
            h = rh::hash_combine(h, lh(a.data[i]));

        // Also hash the last locator:
        h = rh::hash_combine(h, lh(a.data.back()));

        return h;
    }
};

using rom_array_set_type = rh::batman_set<rom_array_t>;
extern std::mutex rom_array_set_mutex;
extern rom_array_set_type rom_array_set;

// Orders CFG basic blocks, trying to find an optimal layout for branches.
void build_rom_arrays(ir_t& ir);

#endif
