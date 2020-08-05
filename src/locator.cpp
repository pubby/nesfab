#include "locator.hpp"

#include "globals.hpp"

type_t locator_manager_t::type(locator_ht loc) const
{
    return is_single(loc) ? get_single(loc).type() : type_t{};
}

void locator_manager_t::setup(global_t const& global)
{
    assert(global.gclass() == GLOBAL_FN);

    bitset_pool.clear();
    sets.clear();
    handle_map.clear();

    unsigned const set_size = bitset_size<>(global_t::num_vars());

    // 'named_set' will hold all globals mentioned by name inside this fn.
    bitset_uint_t* named_set = bitset_pool.alloc(set_size);
    assert(bitset_all_clear(set_size, named_set));

    // Start out with a single equivalence class, containing all the
    // globals possibly used in this fn.
    // Then, this equivalence class will be broken up into multiple 
    // disjoint subsets.

    bitset_uint_t* initial_set = bitset_pool.alloc(set_size);
    assert(bitset_all_clear(set_size, initial_set));

    for(global_t const* idep : global.ideps())
    {
        if(idep->gclass() == GLOBAL_FN)
        {
            bitset_or(set_size, initial_set, idep->fn().reads());
            bitset_or(set_size, initial_set, idep->fn().writes());
        }
        else if(idep->gclass() == GLOBAL_VAR)
        {
            // Setup 'named_set' while we're here.

            bitset_set(named_set, idep->var().value);

            handle_map.insert({ idep, sets.size() });
            sets.push_back(idep);
        }
    }

    m_num_singles = sets.size();

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
            // Split the eq class set into two sets:
            // - One which has everything in 'bitset' ('in')
            // - The complement of that ^ ('comp')

            for(unsigned i = 0; i < set_size; ++i)
            {
                bitset_uint_t rw = 
                    idep->fn().reads()[i] | idep->fn().writes()[i];
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

    sets.reserve(sets.size() + eq_classes.size());
    for(unsigned i = 0; i < eq_classes.size(); ++i)
    {
        bitset_for_each_bit(set_size, eq_classes[i], 
        [this](unsigned bit)
        {
            global_t& var = global_t::get_var({ bit });
            handle_map.insert({ &var, sets.size() });
        });
        sets.push_back(eq_classes[i]);
    }
}
