#include "rval.hpp"

#include "locator.hpp"
#include "compiler_error.hpp"
#include "lt.hpp"
#include "globals.hpp"
#include "text.hpp"

unsigned lval_t::ulabel() const
{ 
    if(label == ENTRY_LABEL && is_global() && global().gclass() == GLOBAL_FN && global().impl<fn_t>().iasm)
        return global().impl<fn_t>().def().default_label;

    return label;
}

value_time_t calc_time(type_t const& type, rval_t const& rval)
{
    value_time_t time = CT;

    auto const check_ssa = [&](ssa_value_t ssa)
    {
        if(ssa.is_locator())
            time = std::max(time, LT);
        else if(ssa.holds_ref())
            time = RT;
    };

    for(unsigned i = 0; i < rval.size(); ++i)
    {
        if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&rval[i]))
            check_ssa(*ssa);
        else if(ct_array_t const* array = std::get_if<ct_array_t>(&rval[i]))
        {
            type_t const mt = ::member_type(type, i);
            unsigned const length = mt.array_length();
            for(unsigned j = 0; j < length; ++j)
                check_ssa((*array)[j]);
        }
    }

    return time;
}

void append_locator_bytes(std::vector<locator_t>& vec, rval_t const& rval, type_t const type, pstring_t pstring)
{
    /* TODO
    if(is_tea(type.name()))
    {
        unsigned const length = type.array_length();
        for(unsigned i = 0; i < length; ++i)
        {
        }
    }
    */

    std::size_t const total_size_of = type.size_of();

    if(total_size_of == 0 && !is_vec(type.name()) && !is_tea(type.name()))
        compiler_error(pstring, fmt("Invalid type % in pointer-addressable array. (Size of 0.)", type));

    vec.reserve(vec.size() + total_size_of);

    assert(rval.size());
    for(unsigned i = 0; i < rval.size(); ++i)
    {
        type_t const mt = ::member_type(type, i);

        auto const push_bytes = [&](ssa_value_t v, type_t subtype)
        {
            if(!is_scalar(subtype.name()))
                compiler_error(pstring, fmt("Invalid type in pointer-addressable array. (% is not a scalar subtype.)", subtype));

            unsigned const size_of = subtype.size_of();
            assert(size_of);
            unsigned const frac_shift = max_frac_bytes - frac_bytes(subtype.name());

            if(v.is_num())
            {
                for(unsigned i = 0; i < size_of; ++i)
                    vec.push_back(locator_t::const_byte(v.fixed().value >> ((i + frac_shift) * 8)));
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
                unsigned const num_atoms = ::num_atoms(mt, 0);
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
        else if(auto const* vec_ptr = std::get_if<std::shared_ptr<vec_t>>(&v))
        {
            type_t const elem_type = mt.elem_type();
            for(rval_t const& rval : (*vec_ptr)->data)
                append_locator_bytes(vec, rval, elem_type, pstring);
        }
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

            if(!is_scalar(mt.elem_type().name()))
                compiler_error(at, "Unable to default initialize.");

            ct_array_t array = make_ct_array(size);
            for(unsigned i = 0; i < size; ++i)
                array[i] = ssa_value_t(0u, mt.elem_type().name());

            new_rval.push_back(std::move(array));
        }
        else if(mt.name() == TYPE_VEC)
            new_rval.push_back(std::make_shared<vec_t>());
        else if(is_scalar(mt.name()))
            new_rval.push_back({ ssa_value_t(0u, mt.name()) });
        else
            compiler_error(at, "Unable to default initialize.");
    }

    return new_rval;
}
