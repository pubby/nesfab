#include "rval.hpp"

#include <iostream> // TODO

#include "locator.hpp"
#include "compiler_error.hpp"
#include "lt.hpp"
#include "globals.hpp"
#include "text.hpp"

unsigned lval_t::ulabel() const
{ 
    if(label != ENTRY_LABEL && (flags & LVALF_IS_GLOBAL) && global().gclass() == GLOBAL_FN && global().impl<fn_t>().iasm)
        return global().impl<fn_t>().def().default_label;

    return label;
}

bool is_ct(rval_t const& rval)
{
    for(auto const& v : rval)
    {
        if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
        {
            if(!ssa->is_num())
                return false;
            //if(ssa->is_locator() && ssa->locator().lclass() == LOC_LT_EXPR)
            // TODO
            //if(ssa->is_locator())
                //return false;
        }
        // TODO
        //else if(!std::holds_alternative<ct_array_t>(v))
            //return false;
    }

    return true;
}

bool is_lt(rval_t const& rval)
{
    for(auto const& v : rval)
    {
        // TODO
        //if(std::holds_alternative<ast_node_t const*>(v))
            //return true;
        if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
        {
            //if(ssa->is_locator() && ssa->locator().lclass() == LOC_LT_EXPR)
            // TODO
            if(ssa->is_locator())
                return true;
        }
    }

    return false;
}

bool is_rt(rval_t const& rval)
{
    for(auto const& v : rval)
        if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
            if(!ssa->holds_ref())
                return false;

    return true;
}

void append_locator_bytes(std::vector<locator_t>& vec, rval_t const& rval, type_t const type, pstring_t pstring)
{
    std::size_t const total_size_of = type.size_of();

    if(total_size_of == 0)
        compiler_error(pstring, "Invalid type in pointer-addressable array. (Size of 0.)");

    vec.reserve(vec.size() + total_size_of);

    assert(rval.size());
    for(unsigned i = 0; i < rval.size(); ++i)
    {
        type_t const mt = ::member_type(type, i);

        // TODO
        //if(!is_scalar(mt.name()))
            //compiler_error(pstring, "Invalid type in pointer-addressable array. (Not scalar.)");

        auto const push_bytes = [&](ssa_value_t v, type_t subtype)
        {
            //passert(subtype == v.type(), subtype, v.type());

            if(!is_scalar(subtype.name()))
                compiler_error(pstring, "Invalid type in pointer-addressable array. (Not scalar subtype.)");

            unsigned const size_of = subtype.size_of();
            assert(size_of);
            unsigned const frac_shift = max_frac_bytes - frac_bytes(subtype.name());

            if(v.is_num())
            {
                for(unsigned i = 0; i < size_of; ++i)
                {
                    vec.push_back(locator_t::const_byte(v.fixed().value >> ((i + frac_shift) * 8)));
                }
            }
            else if(v.is_locator())
            {
                locator_t const loc = v.locator();

                if(loc.byteified())
                {
                    vec.push_back(loc);
                    return;
                }

                if(loc.is() == IS_PTR)
                {
                    vec.push_back(loc);
                    vec.push_back(loc.with_is(IS_PTR_HI));
                    return;
                }

                unsigned const member = loc.maybe_member();
                type_t const mt = ::member_type(subtype, member);
                unsigned const num_atoms = ::num_atoms(mt, member);
                assert(num_atoms);
                for(unsigned j = 0; j < num_atoms; ++j)
                {
                    unsigned const num_offsets = ::num_offsets(subtype);
                    assert(num_offsets);
                    for(unsigned k = 0; k < num_offsets; ++k)
                    {
                        locator_t loc = v.locator();
                        
                        if(!loc.byteified())
                        {
                            passert(loc.atom() == 0, loc);

                            loc.set_byteified(true);
                            if(has_arg_member_atom(loc.lclass()))
                                loc.set_atom(j);
                            loc.advance_offset(k);
                        }

                        vec.push_back(loc);
                    }
                }
            }
            else
                compiler_error(pstring, "Invalid value in pointer-addressable array.");
        };

        // Convert the scalar into bytes.
        ct_variant_t const& v = rval[i];

        if(ssa_value_t const* value = std::get_if<ssa_value_t>(&v))
            push_bytes(*value, mt);
        else if(ct_array_t const* array = std::get_if<ct_array_t>(&v))
        {
            type_t const elem_type = mt.elem_type();
            unsigned const length = mt.array_length();
            for(unsigned i = 0; i < length; ++i)
                push_bytes((*array)[i], elem_type);
        }
        /* TODO
        else if(ast_node_t const* const* ast = std::get_if<ast_node_t const*>(&v))
        {
            assert(false);
            // TODO
            //push_bytes(locator_t::lt_expr(alloc_lt_value(mt, *vec)), mt);
        }
        */
    }
}

fixed_t expr_value_t::fixed() const
{ 
    if(rval_t const* rval = is_rval())
        return ::fixed(*rval, type, pstring);
    else
        compiler_error(pstring, "Expecting rvalue.");
}

fixed_t expr_value_t::sfixed() const
{ 
    if(rval_t const* rval = is_rval())
        return ::sfixed(*rval, type, pstring);
    else
        compiler_error(pstring, "Expecting rvalue.");
}

fixed_t fixed(rval_t const& rval, type_t type, pstring_t pstring)
{ 
    ssa_value_t const* v;

    // TODO: remove?
    //if(rval.size() != 1)
        //goto not_cne;

    if(rval.size() < 1)
        goto not_cne;

    if(!(v = std::get_if<ssa_value_t>(&rval[0])))
        goto not_cne;

    if(!*v)
        compiler_error(pstring, "Value is uninitialized.");

    if(!v->is_num() || !is_scalar(type.name()))
        goto not_cne;

    assert(is_masked(v->fixed(), type.name()));
    return v->fixed();

not_cne:
    compiler_error(pstring, "Expecting compile-time constant numeric expression.");
}

fixed_t sfixed(rval_t const& rval, type_t type, pstring_t pstring)
{ 
    if(is_signed(type.name()))
        return { to_signed(fixed(rval, type, pstring).value, type.name()) };
    return fixed(rval, type, pstring);
}

std::string const& strval_t::get_string() const
{
    return sl_manager.get_string(&charmap->global, index, compressed);
}

rval_t default_init(type_t type, pstring_t at)
{
    unsigned const num_m = num_members(type);

    rval_t new_rval;
    new_rval.reserve(num_m);
    for(unsigned i = 0; i < num_m; ++i)
    {
        type_t const mt = member_type(type, i);

        if(mt.name() == TYPE_TEA)
        {
            unsigned const size = mt.size();

            if(size == 0)
                compiler_error(at, "Default initializing array of size 0.");

            if(!is_scalar(mt.elem_type().name()))
                compiler_error(at, "Unable to default initialize.");

            ct_array_t array = make_ct_array(size);
            for(unsigned i = 0; i < size; ++i)
                array[i] = ssa_value_t(0u, mt.elem_type().name());

            new_rval.push_back(std::move(array));
        }
        else if(is_scalar(mt.name()))
            new_rval.push_back({ ssa_value_t(0u, mt.name()) });
        else
            compiler_error(at, "Unable to default initialize.");
    }

    return new_rval;
}
