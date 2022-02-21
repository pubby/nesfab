#include "locator.hpp"

#include "format.hpp"
#include "globals.hpp"
#include "types.hpp"
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
        return fmt("gvar % %.%:%", loc.gvar()->global.name, (int)loc.arg(), (int)loc.field(), (int)loc.offset());
    case LOC_GVAR_SET:
        return fmt("gset %.%:%", loc.handle(), (int)loc.offset());
    case LOC_FN:
        return fmt("fn %", loc.fn()->global.name);
    case LOC_THIS_ARG:
        return fmt("this arg % %.%:%", loc.fn()->global.name, (int)loc.arg(), (int)loc.field(), (int)loc.offset());
    case LOC_CALL_ARG:
        return fmt("call arg % %.%:%", loc.fn()->global.name, (int)loc.arg(), (int)loc.field(), (int)loc.offset());
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

type_t locator_t::mem_type() const
{
    switch(lclass())
    {
    case LOC_IOTA:
        return type_t::array(TYPE_BYTE, 256);
    case LOC_GVAR: 
        return gvar()->type;
    case LOC_THIS_ARG:
    case LOC_CALL_ARG:
        return fn()->type.type(arg());
    case LOC_RETURN:
        return fn()->type.return_type();
    case LOC_LVAR:
        assert(false);
        throw 0; // TODO
    case LOC_CONST_BYTE:
        return TYPE_BYTE;
    case LOC_SSA:
        assert(compiler_phase() == PHASE_COMPILE);
        return ssa_node()->type();
    default:
        return TYPE_VOID;
    }
}

std::size_t locator_t::mem_size() const
{
    type_t const t = mem_type();

    switch(t.name())
    {
    case TYPE_ARRAY: 
        return t.size();
    case TYPE_PTR:
    case TYPE_BANKED_PTR:
        return field() == 0 ? 2 : 1;
    default:
        assert(!is_ptr(t.name()));
        return 1;
    }
}

bool locator_t::mem_zp_only() const
{
    type_t const t = mem_type();
    return is_ptr(t.name()) && field() == 0;
}
