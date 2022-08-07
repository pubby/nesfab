#include "gmanager.hpp"

#include <boost/container/small_vector.hpp>

#include "globals.hpp"
#include "group.hpp"
#include "type.hpp"

std::size_t gmanager_t::bitset_size() { return gmember_ht::bitset_size(); }

void gmanager_t::init(fn_ht fn)
{
    //////////////
    // GMEMBERS //
    //////////////

    {
        assert(!this->fn);
        this->fn = fn;

        unsigned const set_size = bitset_size();

        // 'named_set' will hold all the members of gvars mentioned by name 
        // inside this fn's code, ignoring what happens in other functions.
        // NOTE: 'named_set' doesn't track preserved groups from 'goto mode's.
        bitset_uint_t* const named_set = bitset_pool.alloc(set_size);
        assert(bitset_all_clear(set_size, named_set));

        // TODO: handle inlining
        for(auto const& pair : fn->precheck_tracked().gvars_used)
        {
            gvar_ht gvar = pair.first;
            gvar_set.container.push_back(gvar);
            bitset_set_n(set_size, named_set, gvar->begin().id, gvar->num_members());
        }

        // Start out with a single equivalence class, containing all the
        // globals possibly used in this fn (including fn calls).
        // Then, this equivalence class will be broken up into multiple 
        // disjoint sublocs.

        bitset_uint_t* const initial_set = bitset_pool.alloc(set_size);
        assert(bitset_all_clear(set_size, initial_set));

        for(auto const& pair : fn->precheck_tracked().calls)
        {
            fn_t const& call = *pair.first;

            if(call.fclass == FN_CT)
                continue;
            assert(call.fclass != FN_MODE);

            assert(set_size == call.ir_reads().size());
            bitset_or(set_size, initial_set, call.ir_reads().data());
            bitset_or(set_size, initial_set, call.ir_writes().data());
        }

        for(auto const& pair : fn->precheck_tracked().goto_modes)
        {
            mods_t const* mods = fn->def().mods_of(pair.second);

            if(!mods)
                continue;

            mods->for_each_group_vars([&](group_vars_ht gv)
            {
                bitset_or(set_size, initial_set, gv->gmembers().data());
            });
        }

        // Needed as we used 'push_back' to generate the set:
        std::sort(gvar_set.container.begin(), gvar_set.container.end());

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

        // Split calls.
        for(auto const& pair : fn->precheck_tracked().calls)
        {
            fn_t const& call = *pair.first;

            if(call.fclass == FN_CT)
                continue;
            assert(call.fclass != FN_MODE);

            split(set_size, call.ir_reads().data(), call.ir_writes().data());
        }

        // Split goto modes.
        for(auto const& pair : fn->precheck_tracked().goto_modes)
        {
            mods_t const* mods = fn->def().mods_of(pair.second);

            if(!mods)
                continue;

            mods->for_each_group_vars([&](group_vars_ht gv)
            {
                split(set_size, gv->gmembers().data(), gv->gmembers().data());
            });
        }

        assert(eq_classes.size() >= 1);

        // OK! The equivalence classes are built.
        // Now associate each variable with its eq class.

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

    //////////////////
    // DEREF GROUPS //
    //////////////////

    {
        rh::batman_map<group_vars_ht, group_vars_ht> union_map;

        auto const get_head = [&](group_vars_ht a) -> group_vars_ht
        {
            while(group_vars_ht next = union_map[a])
                a = next;
            return a;
        };

        auto const share = [&](group_vars_ht head_a, group_vars_ht b)
        {
            assert(head_a == get_head(head_a));
            group_vars_ht head_b = get_head(b);

            if(head_a == head_b)
                return;

            union_map[head_b] = head_a;
            assert(get_head(b) == head_a);
        };

        for(type_t const& type : fn->precheck_tracked().deref_types)
        {
            group_vars_ht prev_head = {};

            unsigned const size = type.group_tail_size();
            for(unsigned i = 0; i < size; ++i)
            {
                if(type.group(i)->gclass() != GROUP_VARS)
                    continue;
                group_vars_ht const h = type.group(i)->handle<group_vars_ht>();

                if(prev_head)
                    share(prev_head, h);
                else
                    prev_head = get_head(h);
            }
        }

        rh::robin_map<group_vars_ht, unsigned> index_map;
        unsigned const offset = num_locators();

        for(auto const& pair : union_map)
        {
            auto result = index_map.insert({ get_head(pair.first), index_map.size() });
            group_vars_map.insert({ pair.first, index_t{ result.first->second + offset }});
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

auto gmanager_t::ptr_i(group_vars_ht group_vars) const -> index_t
{
    if(auto const* pair = group_vars_map.lookup(group_vars))
        return pair->second;
    return {};
}

auto gmanager_t::ptr_i(type_t const& type) const -> index_t
{
    unsigned const size = type.group_tail_size();
    for(unsigned i = 0; i < size; ++i)
        if(type.group(i)->gclass() == GROUP_VARS)
            return ptr_i(type.group(i)->handle<group_vars_ht>());
    return {};
}

/* TODO
locator_t gmanager_t::locator(gmember_ht gmember) const
{
    return locator(index(gmember));
}

locator_t gmanager_t::locator(index_t i) const
{
    if(i.value < singletons.size())
        return locator_t::gmember(singletons[i.value], 0);
    else
        return locator_t::gmember_set(fn, i.value - singletons.size());
}

type_t gmanager_t::type(index_t i) const
{
    if(i.value < singletons.size())
        return singletons[i.value]->type();
    else
        return TYPE_VOID;
}
*/
