#ifndef GMEMBER_LOC_MANAGER_HPP
#define GMEMBER_LOC_MANAGER_HPP

#include <functional>
#include <vector>

#include "robin/map.hpp"
#include "flat/small_set.hpp"

#include "array_pool.hpp"
#include "bitset.hpp"
#include "decl.hpp"
#include "locator.hpp"
#include "handle.hpp"

class type_t;

// This class exists to manage LOC_GMEMBER_SETs.
// It puts gmembers into these sets, then allows queries on what each set contains.
class gmanager_t
{
public:
    struct index_t : public handle_t<index_t, unsigned, ~0> {};

    void init(fn_ht fn);

    index_t var_i(gvar_ht gvar) const;
    index_t var_i(gmember_ht gmember) const;
    //index_t ptr_i(group_vars_ht group_vars) const;
    //index_t ptr_i(type_t const& type) const;

    std::size_t num_gvar_locators() const { return gvar_set.size(); }
    std::size_t num_gmember_set_locators() const { return gmember_sets.size(); }
    std::size_t num_locators() const { return num_gvar_locators() + num_gmember_set_locators(); }
    //std::size_t num_vars() const { return num_locators() + group_vars_map.size(); }

    // Returns a bitset containing every gmember belonging to 'loc'.
    bitset_uint_t const* get_set(locator_t loc) const
    {
        assert(loc.lclass() == LOC_GMEMBER_SET);
        assert(loc.data() < gmember_sets.size());
        return gmember_sets[loc.data()];
    }

    /* TODO
    // Returns a locator describing the gmember:
    locator_t locator(gmember_ht gmember) const;
    locator_t locator(index_t i) const;

    // Returns the type of the gmember/gmember set.
    type_t type(index_t index) const;
    */

    template<typename Fn>
    void for_each_gvar(Fn const& fn) const
    {
        for(unsigned i = 0; i < gvar_set.size(); ++i)
            fn(gvar_set.container[i], index_t{i});
    }

    template<typename Fn>
    void for_each_gmember(fn_ht fn, Fn const& callback) const
    {
        for(auto const& pair : gmember_sets_map)
            callback(pair.first, index_t{ pair.second + num_gvar_locators() }, locator_t::gmember_set(fn, pair.second));
    }

    template<typename Fn>
    void for_each_gmember_set(fn_ht fn, Fn const& callback) const
    {
        for(unsigned i = 0; i < gmember_sets.size(); ++i)
            callback(gmember_sets[i], index_t{ i + num_gvar_locators() }, locator_t::gmember_set(fn, i));
    }

    static std::size_t bitset_size();

private:
    fn_ht fn;
    fc::small_set<gvar_ht, 8> gvar_set;
    rh::batman_map<gmember_ht, unsigned> gmember_sets_map;
    std::vector<bitset_uint_t const*> gmember_sets;
    //rh::batman_map<group_vars_ht, index_t> group_vars_map;
    std::vector<type_t> m_types;
    array_pool_t<bitset_uint_t> bitset_pool;
    //unsigned first_set = 0; TODO
};

#endif
