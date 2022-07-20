#include "locator.hpp"

#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "type.hpp"
#include "ir.hpp"
#include "lt.hpp"

std::string to_string(locator_t loc)
{
    std::string str;

    switch(loc.lclass())
    {
    default: 
        return "unknown locator";
    case LOC_NONE:
        return "none";
    case LOC_IOTA:
        str = "offset"; break;
    case LOC_GMEMBER:
        str = fmt("gmember % %", loc.gmember()->gvar.global.name, loc.gmember()->member()); break;
    case LOC_GMEMBER_SET:
        str = fmt("gset %", loc.handle()); break;
    case LOC_FN:
        str = fmt("fn %", loc.fn()->global.name); break;
    case LOC_ARG:
        str = fmt("arg %", loc.fn()->global.name); break;
    case LOC_RETURN:
        str = fmt("ret %", loc.fn()->global.name); break;
    case LOC_PHI:
        str = fmt("phi %", loc.fn()->global.name); break;
    case LOC_CFG_LABEL:
        str = fmt("cfg label %", loc.fn()->global.name); break;
    case LOC_MINOR_LABEL:
        str = fmt("minor label %", loc.fn()->global.name); break;
    case LOC_CONST_BYTE:
        str = "const byte"; break;
    case LOC_SSA:
        str = fmt("ssa %", loc.handle()); break;
    case LOC_MINOR_VAR:
        str = fmt("minor var %", loc.fn()->global.name); break;
    case LOC_ROM_ARRAY:
        str = "rom_array"; break;
    case LOC_LT_CONST_PTR:
        str = fmt("lt const ptr %", loc.const_()->global.name); break;
    case LOC_LT_CONST_PTR_BANK:
        str = fmt("lt const ptr bank %", loc.const_()->global.name); break;
    case LOC_LT_EXPR:
        str = fmt("lt expr % %", loc.handle(), loc.lt().safe().type); break;
    }

    if(has_arg_member_atom(loc.lclass()))
        str += fmt(" %.%:% (%)", (int)loc.arg(), (int)loc.member(), (int)loc.atom(), (int)loc.offset());
    else
        str += fmt(" [%] (%)", (int)loc.data(), (int)loc.offset());

    str += fmt(" {%}", (int)loc.byteified());

    return str;
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
        return none();
}

std::size_t locator_t::mem_size() const
{
    return type().size_of();
}

bool locator_t::mem_zp_only() const
{
    type_t const t = type();
    return is_ptr(t.name()) && atom() == 0;
}

type_t locator_t::type() const
{
    auto const byteify = [&](type_t type) -> type_t
    {
        if(byteified())
        {
            if(type.name() == TYPE_TEA)
            {
                if(type.elem_type().size_of() > 1)
                    return type_t::tea(TYPE_U, type.array_length());
            }
            else
            {
                assert(is_scalar(type.name()));
                if(type.size_of() > 1)
                    return TYPE_U;
            }
        }
        return type;
    };

    switch(lclass())
    {
    case LOC_LT_CONST_PTR:
        if(const_ht const c = const_())
            return byteify(type_t::ptr(c->group(), false));
        break;
    case LOC_LT_CONST_PTR_BANK:
        return TYPE_U;
    case LOC_LT_EXPR:
        assert(lt());
        return byteify(lt().safe().type);
    case LOC_IOTA:
        return type_t::tea(TYPE_U, 256);
    case LOC_GMEMBER: 
        return byteify(gmember()->type());
    case LOC_ARG:
        return byteify(fn().safe().type().type(arg()));
    case LOC_RETURN:
        return byteify(fn().safe().type().return_type());
    case LOC_CONST_BYTE:
        return TYPE_U;
    case LOC_SSA:
        assert(compiler_phase() == PHASE_COMPILE);
        return byteify(ssa_node()->type());
    default:
        break;
    }
    return TYPE_VOID;
}
