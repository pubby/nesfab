#ifndef GVAR_LOC_MANAGER_HPP
#define GVAR_LOC_MANAGER_HPP

#include <functional>
#include <vector>

#include "robin/map.hpp"

#include "array_pool.hpp"
#include "bitset.hpp"
#include "locator.hpp"
#include "handle.hpp"

class type_t;

// This class exists to manage LOC_GVAR_SETs.
// It puts gvars into these sets, then allows queries on what each set contains.
class gvar_loc_manager_t
{
public:
    using index_t = handle_t<unsigned, struct gvar_loc_tag, ~0>;

    void init(fn_ht fn);

    // Number of unique locators this manages.
    std::size_t num_unique_locators() const { return singletons.size() + gvar_sets.size(); }

    // Returns a bitset containing every gvar belonging to 'loc'.
    bitset_uint_t const* get_set(locator_t loc) const
    {
        assert(loc.lclass() == LOC_GVAR_SET);
        assert(loc.data() < gvar_sets.size());
        return gvar_sets[loc.data()];
    }

    // Returns an index in the range [starting_index, starting_index + num_unique_locators()),
    // uniquely identifying the gvar set.
    index_t index(gvar_ht gvar) const
    { 
        auto const* pair = map.find(gvar);
        assert(pair);
        return { pair->second };
    }

    // Returns a locator describing the gvar:
    locator_t locator(gvar_ht gvar) const;
    locator_t locator(index_t i) const;

    // Returns the type of the gvar/gvar set.
    type_t type(index_t index) const;

    template<typename Fn>
    void for_each_singleton(Fn const& fn)
    {
        for(unsigned i = 0; i < singletons.size(); ++i)
        {
            gvar_ht gvar = singletons[i];
            fn(gvar, index_t{i});
        }
    }

    template<typename Fn>
    void for_each_set(Fn const& fn)
    {
        unsigned j = singletons.size();
        for(unsigned i = 0; i < gvar_sets.size(); ++i, ++j)
        {
            bitset_uint_t const* bitset = gvar_sets[i];
            fn(bitset, index_t{j});
        }
    }

    template<typename Fn>
    void for_each_locator(Fn const& fn)
    {
        unsigned const size = num_unique_locators();
        for(unsigned i = 0; i < size; ++i)
            fn(locator(index_t{i}), index_t{i});
    }

    static std::size_t bitset_size();

private:
    fn_ht fn;
    array_pool_t<bitset_uint_t> bitset_pool;
    std::vector<gvar_ht> singletons;
    std::vector<bitset_uint_t*> gvar_sets;
    rh::robin_map<gvar_ht, unsigned> map;
    unsigned first_set = 0;
};

#endif
