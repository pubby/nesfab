#include "gmanager.hpp"

#include <boost/container/small_vector.hpp>

#include "globals.hpp"
#include "group.hpp"
#include "type.hpp"

void gmanager_t::init(fn_ht fn)
{
    //////////////
    // GMEMBERS //
    //////////////

    {
        assert(!this->fn);
        this->fn = fn;

        unsigned const set_size = gmember_ht::bitset_size();

        // 'named_set' will hold all the members of gvars mentioned by name 
        // inside this fn's code, ignoring what happens in other functions.
        // NOTE: 'named_set' doesn't track preserved groups from 'goto mode's.
        bitset_uint_t* const named_set = bitset_pool.alloc(set_size);
        assert(bitset_all_clear(set_size, named_set));

        auto const add_gvar = [&](gvar_ht gvar)
        {
            gvar_set.insert(gvar);
            bitset_set_n(set_size, named_set, gvar->begin().id, gvar->num_members());
        };

        fn->for_each_inlined([&](fn_t const& fn)
        {
            for(auto const& pair : fn.precheck_tracked().gvars_used)
                add_gvar(pair.first);

            if(fn.mods() && (fn.mods()->explicit_lists & MODL_EMPLOYS))
            {
                fn.mods()->for_each_list_vars(MODL_EMPLOYS, [&](group_vars_ht gv, pstring_t)
                {
                    for(gvar_ht gvar : (*gv)->vars()->gvars())
                        add_gvar(gvar);
                });
            }
        });

        // Start out with a single equivalence class, containing all the
        // globals possibly used in this fn (including fn calls).
        // Then, this equivalence class will be broken up into multiple 
        // disjoint sublocs.

        bitset_uint_t* const initial_set = bitset_pool.alloc(set_size);
        assert(bitset_all_clear(set_size, initial_set));

        fn->for_each_inlined([&](fn_t const& fn)
        {
            for(auto const& pair : fn.precheck_tracked().calls)
            {
                fn_t const& call = *pair.first;

                if(call.fclass == FN_CT)
                    continue;

                assert(call.fclass != FN_MODE);
                assert(call.global.compiled());

                if(call.always_inline())
                    continue;

                assert(set_size == call.ir_reads().size());
                bitset_or(set_size, initial_set, call.ir_reads().data());
                bitset_or(set_size, initial_set, call.ir_writes().data());
            }

            for(auto const& pair : fn.precheck_tracked().goto_modes)
            {
                if(mods_t const* mods = pair.second.mods)
                {
                    mods->for_each_list_vars(MODL_PRESERVES, [&](group_vars_ht gv, pstring_t)
                    {
                        bitset_or(set_size, initial_set, (*gv)->vars()->gmembers().data());
                    });
                }
            }

        });

        // The eq classes won't involve any global named in the fn.
        bitset_difference(set_size, initial_set, named_set);

        if(bitset_all_clear(set_size, initial_set))
            return;

        // Now break equivalent classes apart:
        std::vector<bitset_uint_t*> eq_classes = { initial_set };
        std::vector<bitset_uint_t*> new_eq_classes;

        auto const split = [&](unsigned bs_size, bitset_uint_t const* a, bitset_uint_t const* b)
        {
            bitset_uint_t any_in = 0;
            bitset_uint_t any_comp = 0;
            bitset_uint_t* comp_set = bitset_pool.alloc(bs_size);

            for(bitset_uint_t* in_set : eq_classes)
            {
                // Split the eq class set into two locs:
                // - One which has the intersection of 'in_set' and the idep's reads and writes
                // - The complement of that ('comp')

                for(unsigned i = 0; i < bs_size; ++i)
                {
                    bitset_uint_t rw = a[i] | b[i];
                    any_comp |= (comp_set[i] = in_set[i] & ~rw);
                    any_in |= (in_set[i] &= rw);
                }

                assert(any_in || any_comp);

                if(any_in)
                    new_eq_classes.push_back(in_set);
                if(any_comp)
                {
                    new_eq_classes.push_back(comp_set);
                    comp_set = any_in ? bitset_pool.alloc(bs_size) : in_set;
                }
            }

            eq_classes.swap(new_eq_classes);
            new_eq_classes.clear();
        };

        fn->for_each_inlined([&](fn_t const& fn)
        {
            // Split calls.
            for(auto const& pair : fn.precheck_tracked().calls)
            {
                fn_t const& call = *pair.first;

                if(call.fclass == FN_CT)
                    continue;

                assert(call.fclass != FN_MODE);
                assert(call.global.compiled());

                if(call.always_inline())
                    continue;

                split(set_size, call.ir_reads().data(), call.ir_writes().data());

                // If it waits on an NMI, split using the NMI's reads and writes.
                if(call.precheck_fences())
                    split(set_size, call.fence_rw().data(), call.fence_rw().data());
            }

            // Split goto modes.
            for(auto const& pair : fn.precheck_tracked().goto_modes)
            {
                if(mods_t const* mods = pair.second.mods)
                {
                    mods->for_each_list_vars(MODL_PRESERVES, [&](group_vars_ht gv, pstring_t)
                    {
                        split(set_size, (*gv)->vars()->gmembers().data(), (*gv)->vars()->gmembers().data());
                    });
                }
            }
        });

        assert(eq_classes.size() >= 1);

        // OK! The equivalence classes are built.
        // Now associate each variable with its eq class.

        assert(gmember_sets.empty());
        assert(gmember_sets_map.empty());

        gmember_sets.reserve(gmember_sets.size() + eq_classes.size());
        for(unsigned i = 0; i < eq_classes.size(); ++i)
        {
            bitset_for_each(set_size, eq_classes[i], [this](unsigned bit)
            {
                gmember_sets_map.insert({ gmember_ht{ bit }, gmember_sets.size() });
            });
            gmember_sets.push_back(eq_classes[i]);
        }
    }
}

auto gmanager_t::var_i(gvar_ht gvar) const -> index_t
{ 
    auto it = gvar_set.find(gvar);
    if(it != gvar_set.end())
        return { it - gvar_set.begin() };
    return {};
}

auto gmanager_t::var_i(gmember_ht gmember) const -> index_t
{
    auto it = gvar_set.find(gmember->gvar.handle());
    if(it != gvar_set.end())
        return { it - gvar_set.begin() };
    else if(auto const* pair = gmember_sets_map.lookup(gmember))
        return { pair->second + num_gvar_locators() };
    return {};
}

