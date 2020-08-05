#ifndef LOCATOR_HPP
#define LOCATOR_HPP

// Handles the use of global variables inside fn IR.

// Each read_global and write_global op take a locator_ht as their [1] input.
// This locator defines a set of global variables the read/writes apply to.

#include <cassert>
#include <cstdint>
#include <vector>

#include "robin/map.hpp"

#include "array_pool.hpp"
#include "bitset.hpp"
#include "handle.hpp"
#include "types.hpp"

struct global_t;

using locator_ht = handle_t<unsigned, struct locator_ht_tag, ~0u>;

class locator_manager_t
{
public:
    void setup(global_t const& global);

    // If the global was mentioned by name in the fn, it is called a single.
    bool is_single(locator_ht loc) const 
        { return loc.value < num_singles(); }

    global_t const& get_single(locator_ht loc) const
    {
        assert(is_single(loc));
        return *static_cast<global_t const*>(sets[loc.value]);
    }

    bitset_uint_t const* get_set(locator_ht loc) const
    {
        assert(!is_single(loc));
        return static_cast<bitset_uint_t const*>(sets[loc.value]);
    }

    type_t type(locator_ht loc) const;

    std::size_t size() const { return sets.size(); }
    std::size_t num_singles() const { return m_num_singles; }

    locator_ht get(global_t const& global) const
    { 
        if(auto const* pair = handle_map.find(&global))
            return pair->second;
        return {locator_ht::null};
    }

private:
    array_pool_t<bitset_uint_t> bitset_pool;
    std::vector<void const*> sets;
    unsigned m_num_singles = 0;
    rh::robin_map<global_t const*, locator_ht> handle_map;
};

#endif
