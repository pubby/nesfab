#include "locator.hpp"

#include "format.hpp"
#include "globals.hpp"

std::string to_string(locator_t loc)
{
    switch(loc.lclass())
    {
    case LCLASS_GLOBAL:
        return loc.global().name;
    case LCLASS_GLOBAL_SET:
        return fmt("gset %:%", loc.index(), loc.byte());
    case LCLASS_THIS_ARG:
        return fmt("arg %:%", loc.index(), loc.byte());
    case LCLASS_CALL_ARG:
        return fmt("call %:%", loc.index(), loc.byte());
    case LCLASS_RETURN:
        return fmt("ret %:%", loc.index(), loc.byte());
    case LCLASS_PHI:
        return fmt("phi %:%", loc.index(), loc.byte());
    case LCLASS_CFG_LABEL:
        return fmt("cfg label %", loc.index());
    case LCLASS_MINOR_LABEL:
        return fmt("minor label %", loc.index());
    default: return "unknown locator";
    }
}

std::ostream& operator<<(std::ostream& o, locator_t loc)
{
    o << to_string(loc);
    return o;
}

global_t& locator_t::global() const
{
    return global_t::lookup(gvar());
}

void locator_manager_t::setup(global_t const& global)
{
    assert(global.gclass() == GLOBAL_FN);

    bitset_pool.clear();
    locs.clear();
    map.clear();

    unsigned set_size = bitset_size<>(global_t::num_vars());

    // 'named_set' will hold all globals mentioned by name inside this fn.
    bitset_uint_t* named_set = bitset_pool.alloc(set_size);
    assert(bitset_all_clear(set_size, named_set));

    // Start out with a single equivalence class, containing all the
    // globals possibly used in this fn.
    // Then, this equivalence class will be broken up into multiple 
    // disjoint sublocs.

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

            map.insert({ idep, locs.size() });
            locs.push_back(idep);
        }
    }
    
    first_set = locs.size();

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

    locs.reserve(locs.size() + eq_classes.size());
    for(unsigned i = 0; i < eq_classes.size(); ++i)
    {
        bitset_for_each(set_size, eq_classes[i], 
        [this](unsigned bit)
        {
            global_t& var = global_t::get_var({ bit });
            map.insert({ &var, locs.size() });
        });
        locs.push_back(eq_classes[i]);
    }
}

locator_t locator_manager_t::locator(global_t const& global) const
{
    return locator(index(global));
}

locator_t locator_manager_t::locator(unsigned i) const
{
    locator_t loc;
    if(i < first_set)
    {
        loc.impl =
        { 
            .index = static_cast<global_t const*>(locs[i])->var().value,
            .lclass = LCLASS_GLOBAL
        };
    }
    else
    {
        loc.impl =
        { 
            .index = i,
            .lclass = LCLASS_GLOBAL_SET
        };
    }
    return loc;
}

type_t locator_manager_t::type(unsigned i) const
{
    if(i < first_set)
        return static_cast<global_t const*>(locs[i])->type();
    return TYPE_VOID;
}
