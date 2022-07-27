#include "sval.hpp"

#include "locator.hpp"
#include "compiler_error.hpp"
#include "lt.hpp"

bool is_ct(sval_t const& sval)
{
    for(auto const& v : sval)
    {
        if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
        {
            if(ssa->holds_ref())
                return false;
        }
        else if(!std::holds_alternative<ct_array_t>(v))
            return false;
    }

    return true;
}

bool is_lt(sval_t const& sval)
{
    for(auto const& v : sval)
    {
        if(std::holds_alternative<expr_vec_t>(v))
            return true;
        else if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
        {
            if(ssa->is_locator() && ssa->locator().lclass() == LOC_LT_EXPR)
                return true;
        }
    }

    return false;
}

void append_locator_bytes(std::vector<locator_t>& vec, sval_t const& sval, type_t const type, pstring_t pstring)
{
    std::size_t const total_size_of = type.size_of();

    if(total_size_of == 0)
        compiler_error(pstring, "Invalid type in pointer-addressable array.");

    vec.reserve(vec.size() + total_size_of);

    assert(sval.size());
    for(unsigned i = 0; i < sval.size(); ++i)
    {
        type_t const mt = ::member_type(type, i);

        if(!is_scalar(mt.name()))
            compiler_error(pstring, "Invalid type in pointer-addressable array.");

        auto const push_bytes = [&](ssa_value_t v, type_t subtype)
        {
            assert(subtype == v.type());

            if(!is_scalar(subtype.name()))
                compiler_error(pstring, "Invalid type in pointer-addressable array.");

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
                    vec.push_back(loc);

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
                            assert(loc.atom() == 0);
                            assert(loc.offset() == 0);

                            loc.set_byteified(true);
                            if(has_arg_member_atom(loc.lclass()))
                                loc.set_atom(j);
                            loc.set_offset(k);
                        }

                        vec.push_back(loc);
                    }
                }
            }
            else
                compiler_error(pstring, "Invalid value in pointer-addressable array.");
        };

        // Convert the scalar into bytes.
        ct_variant_t const& v = sval[i];

        if(ssa_value_t const* value = std::get_if<ssa_value_t>(&v))
            push_bytes(*value, mt);
        else if(ct_array_t const* array = std::get_if<ct_array_t>(&v))
        {
            type_t const elem_type = mt.elem_type();
            unsigned const length = mt.array_length();
            for(unsigned i = 0; i < length; ++i)
                push_bytes((*array)[i], elem_type);
        }
        else if(expr_vec_t const* vec = std::get_if<expr_vec_t>(&v))
            push_bytes(locator_t::lt_expr(alloc_lt_value(mt, *vec)), mt);
    }
}
