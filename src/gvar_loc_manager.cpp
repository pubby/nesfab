#include "gvar_loc_manager.hpp"

#include <boost/container/small_vector.hpp>

#include "globals.hpp"
#include "types.hpp"

std::size_t gvar_loc_manager_t::bitset_size() { return impl_bitset_size<gvar_t>(); }

void gvar_loc_manager_t::init(fn_ht fn)
{
    assert(!this->fn);
    this->fn = fn;

    global_t const& global = fn->global;

    unsigned const set_size = bitset_size();

    // 'named_set' will hold all globals mentioned by name inside this fn's code,
    // ignoring what happens in other functions.
    bitset_uint_t* named_set = bitset_pool.alloc(set_size);
    assert(bitset_all_clear(set_size, named_set));

    // Start out with a single equivalence class, containing all the
    // globals possibly used in this fn (including fn calls).
    // Then, this equivalence class will be broken up into multiple 
    // disjoint sublocs.

    bitset_uint_t* initial_set = bitset_pool.alloc(set_size);
    assert(bitset_all_clear(set_size, initial_set));

    for(global_t const* idep : global.ideps())
    {
        if(idep->gclass() == GLOBAL_FN)
        {
            assert(set_size == idep->impl<fn_t>().ir_reads().size());
            bitset_or(set_size, initial_set, idep->impl<fn_t>().ir_reads().data());
            bitset_or(set_size, initial_set, idep->impl<fn_t>().ir_writes().data());
        }
        else if(idep->gclass() == GLOBAL_VAR)
        {
            // Setup 'named_set' while we're here.
            bitset_set(named_set, idep->index());

            gvar_ht const h = idep->handle<gvar_ht>();
            map.insert({ h, num_unique_locators() });
            singletons.push_back(h);
        }
    }

    // The eq classes won't involve any global named in the fn.
    bitset_difference(set_size, initial_set, named_set);

    if(bitset_all_clear(set_size, initial_set))
        return;

    // Now break equivalent classes apart:
    std::vector<bitset_uint_t*> eq_classes = { initial_set };
    std::vector<bitset_uint_t*> new_eq_classes;
    
    for(global_t const* idep : global.ideps())
    {
        if(idep->gclass() != GLOBAL_FN)
            continue;

        bitset_uint_t any_in  = 0;
        bitset_uint_t any_comp = 0;
        bitset_uint_t* comp_set = bitset_pool.alloc(set_size);

        for(bitset_uint_t* in_set : eq_classes)
        {
            // Split the eq class set into two locs:
            // - One which has the intersection of 'in_set' and the idep's reads and writes
            // - The complement of that ('comp')

            for(unsigned i = 0; i < set_size; ++i)
            {
                bitset_uint_t rw = idep->impl<fn_t>().ir_reads()[i] | idep->impl<fn_t>().ir_writes()[i];
                any_comp |= (comp_set[i] = in_set[i] & ~rw);
                any_in |= (in_set[i] &= rw);
            }

            assert(any_in || any_comp);

            if(any_in)
                new_eq_classes.push_back(in_set);
            if(any_comp)
            {
                new_eq_classes.push_back(comp_set);
                comp_set = any_in ? bitset_pool.alloc(set_size) : in_set;
            }
        }

        eq_classes.swap(new_eq_classes);
        new_eq_classes.clear();
    }

    assert(eq_classes.size() >= 1);

    // OK! The equivalence classes are built.
    // Now associate each variable with its eq class.

    gvar_sets.reserve(gvar_sets.size() + eq_classes.size());
    for(unsigned i = 0; i < eq_classes.size(); ++i)
    {
        bitset_for_each(set_size, eq_classes[i], 
        [this](unsigned bit)
        {
            map.insert({ gvar_ht{ bit }, num_unique_locators() });
        });
        gvar_sets.push_back(eq_classes[i]);
    }
}

locator_t gvar_loc_manager_t::locator(gvar_ht gvar) const
{
    return locator(index(gvar));
}

locator_t gvar_loc_manager_t::locator(index_t i) const
{
    if(i.value < singletons.size())
        return locator_t::gvar(singletons[i.value], 0);
    else
        return locator_t::gvar_set(fn, i.value - singletons.size());
}

type_t gvar_loc_manager_t::type(index_t i) const
{
    if(i.value < singletons.size())
        return singletons[i.value]->type();
    else
        return TYPE_VOID;
}
