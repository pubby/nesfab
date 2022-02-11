#include "locator.hpp"

#include "format.hpp"
#include "globals.hpp"
#include "ir.hpp"

std::string to_string(locator_t loc)
{
    switch(loc.lclass())
    {
    case LOC_NONE:
        return "none";
    case LOC_IOTA:
        return fmt("iota %", loc.offset());
    case LOC_GVAR:
        return fmt("gvar %:%", loc.gvar()->global.name, (int)loc.offset());
    case LOC_GVAR_SET:
        return fmt("gset %:%", loc.handle(), (int)loc.offset());
    case LOC_FN:
        return fmt("fn %", loc.fn()->global.name);
    case LOC_THIS_ARG:
        return fmt("this arg % %:%", loc.fn()->global.name, loc.data(), (int)loc.offset());
    case LOC_CALL_ARG:
        return fmt("call arg % %:%", loc.fn()->global.name, loc.data(), (int)loc.offset());
    case LOC_RETURN:
        return fmt("ret % %", loc.fn()->global.name, (int)loc.offset());
    case LOC_PHI:
        return fmt("phi % %", loc.fn()->global.name, loc.data());
    case LOC_CFG_LABEL:
        return fmt("cfg label % %", loc.fn()->global.name, loc.data());
    case LOC_MINOR_LABEL:
        return fmt("minor label % %", loc.fn()->global.name, loc.data());
    case LOC_CONST_BYTE:
        return fmt("const byte %", loc.data());
    case LOC_SSA:
        return fmt("ssa %", loc.handle());
    default: 
        return "unknown locator";
    }
}

std::ostream& operator<<(std::ostream& o, locator_t loc)
{
    o << to_string(loc);
    return o;
}

locator_t locator_t::from_ssa_value(ssa_value_t v)
{
    if(v.holds_ref())
        return ssa(v.handle());
    else if(v.is_num())
        return const_byte(v.whole());
    else if(v.is_locator())
        return v.locator();
    else
        return null();
}

std::size_t locator_t::mem_size() const
{
    // TODO: implement
    throw std::runtime_error("Unimplemented locator_t::mem_size()");

    /*
    switch(lclass())
    {
    case LOC_CALL_ARG:
        fn_t const& f = *fn();
        return f.arg_loc_size(data());
        break;

    case LOC_GVAR:
        {
            type_t const type = fn()->type;
            if(type.name() == TYPE_ARRAY)a_value_t _trans = {};

        static void set(ssa_value_t v)
        {
            _node = v;
            _value = orig_def(v);
            _trans = asm_arg(v);
        }

        [[gnu::always_inline]]
        static ssa_value_t node() { return _node; }

        [[gnu::always_inline]]
        static ssa_value_t value() { return _value; }

        [[gnu::always_inline]]
        static ssa_value_t trans() { return _trans; }
    };

    template<typename Param>
    struct array_index
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(1); }


                return type.size();
            else if(type.name() == TYPE_)
        }
    }
    */
}

void gvar_locator_manager_t::setup(fn_t const& fn)
{
    global_t const& global = fn.global;

    bitset_pool.clear();
    locs.clear();
    map.clear();

    unsigned const set_size = fn_t::rw_bitset_size();

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
            bitset_or(set_size, initial_set, idep->impl<fn_t>().ir_reads());
            bitset_or(set_size, initial_set, idep->impl<fn_t>().ir_writes());
        }
        else if(idep->gclass() == GLOBAL_VAR)
        {
            // Setup 'named_set' while we're here.

            bitset_set(named_set, idep->index());

            map.insert({ idep, locs.size() });
            locs.push_back(idep);
        }
    }
    
    this->first_set = locs.size();

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

    locs.reserve(locs.size() + eq_classes.size());
    for(unsigned i = 0; i < eq_classes.size(); ++i)
    {
        bitset_for_each(set_size, eq_classes[i], 
        [this](unsigned bit)
        {
            global_t& var = gvar_ht{ bit }->global;
            map.insert({ &var, locs.size() });
        });
        locs.push_back(eq_classes[i]);
    }
}

locator_t gvar_locator_manager_t::locator(global_t const& global) const
{
    return locator(index(global));
}

locator_t gvar_locator_manager_t::locator(unsigned i) const
{
    locator_t loc;
    if(i < first_set)
    {
        loc.set_lclass(LOC_GVAR);
        loc.set_handle(static_cast<global_t const*>(locs[i])->index());
    }
    else
    {
        loc.set_lclass(LOC_GVAR_SET);
        loc.set_handle(i);
    }
    return loc;
}

type_t gvar_locator_manager_t::type(unsigned i) const
{
    if(i < first_set)
        return static_cast<global_t const*>(locs[i])->impl<gvar_t>().type;
    return TYPE_VOID;
}
