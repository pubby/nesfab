#include "locator.hpp"

#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "type.hpp"
#include "ir.hpp"
#include "lt.hpp"
#include "rom.hpp"
#include "rom_array.hpp"

std::string to_string(locator_t loc)
{
    std::string str;

    switch(loc.lclass())
    {
    default: 
        return fmt("unknown locator %", (int)loc.lclass());
    case LOC_NONE:
        return "none";
    case LOC_IOTA:
        str = "iota offset"; break;
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
    case LOC_CFG_LABEL:
        str = fmt("cfg label %", loc.cfg_node()); break;
    case LOC_MINOR_LABEL:
        str = fmt("minor label"); break;
    case LOC_CONST_BYTE:
        str = "const byte"; break;
    case LOC_ADDR:
        str = "addr"; break;
    case LOC_SSA:
        str = fmt("ssa %", loc.handle()); break;
    case LOC_PHI:
        str = fmt("ssa %", loc.handle()); break;
    case LOC_MINOR_VAR:
        str = fmt("minor var %", loc.fn()->global.name); break;
    case LOC_ROM_ARRAY:
        str = "rom_array"; break;
    case LOC_LT_GMEMBER_PTR:
        str = fmt("gmember ptr % %", loc.gmember()->gvar.global.name, loc.gmember()->member()); break;
    case LOC_LT_CONST_PTR:
        str = fmt("lt const ptr %", loc.const_()->global.name); break;
    case LOC_LT_CONST_PTR_BANK:
        str = fmt("lt const ptr bank %", loc.const_()->global.name); break;
    case LOC_LT_EXPR:
        str = fmt("lt expr % %", loc.handle(), loc.lt().safe().type); break;
    case LOC_THIS_BANK:
        str = "this bank"; break;
    case LOC_MAIN_ENTRY:
        str = "main entry"; break;
    case LOC_MAIN_ENTRY_BANK:
        str = "main entry bank"; break;
    }

    if(has_arg_member_atom(loc.lclass()))
        str += fmt(" %.%:% (%)", (int)loc.arg(), (int)loc.member(), (int)loc.atom(), (int)loc.offset());
    else
        str += fmt(" [%] (%)", (int)loc.data(), (int)loc.offset());

    str += fmt(" {% %}", (int)loc.byteified(), (int)loc.high());

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
        return LOC_NONE;
}

std::size_t locator_t::mem_size() const
{
    return with_byteified(true).type().size_of();
}

bool locator_t::mem_zp_only() const
{
    type_t const t = with_byteified(false).type();
    return is_ptr(t.name()) && member() == 0;
}

type_t locator_t::type() const
{
    auto const byteify = [&](type_t type) -> type_t
    {
        if(is_banked_ptr(type.name()))
        {
            if(member() == 1)
                type = TYPE_U;
            else
            {
                assert(member() == 0);
                type.unsafe_set_name(remove_bank(type.name()));
            }
        }

        if(byteified())
        {
            if(type.name() == TYPE_TEA)
            {
                if(type.elem_type().size_of() > 1)
                    return type_t::tea(TYPE_U, type.array_length());
            }
            else if(is_scalar(type.name()))
            {
                if(type.size_of() > 1)
                    return TYPE_U;
            }
        }

        return type;
    };

    switch(lclass())
    {
    case LOC_LT_GMEMBER_PTR:
        if(gmember_ht const m = gmember())
            return byteify(type_t::ptr(m->gvar.group(), true, false));
        break;
    case LOC_LT_CONST_PTR:
        if(const_ht const c = const_())
            return byteify(type_t::ptr(c->group(), false, false));
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
    case LOC_PHI:
        assert(compiler_phase() == PHASE_COMPILE);
        return byteify(ssa_node()->type());
    default:
        break;
    }
    return TYPE_VOID;
}

locator_t locator_t::link(fn_ht fn_h, int bank) const
{
    assert(compiler_phase() == PHASE_LINK);

    auto const from_span = [&](span_t span) -> locator_t
    { 
        if(!span)
            return *this;
        return addr(span.addr, offset()).with_high(high()); 
    };

    auto const from_alloc = [&](rom_alloc_ht h) -> locator_t
    {
        if(rom_alloc_t* alloc = h.get())
            return from_span(alloc->span);
        return *this;
    };

    auto const from_alloc_bank = [&](rom_alloc_ht h) -> locator_t
    {
        int const bank = h.first_bank();
        if(bank < 0 || bank >= 256)
            return *this;
        return locator_t::const_byte(bank);
    };

    switch(lclass())
    {
    default:
        return *this;

    case LOC_FN:
        return from_alloc(fn()->rom_alloc());

    case LOC_GMEMBER:
        return from_span(gmember()->span(atom()));

    case LOC_ARG:
    case LOC_RETURN:
    case LOC_MINOR_VAR:
        return from_span(fn()->lvar_span(mem_head()));

    case LOC_SSA:
    case LOC_PHI:
        if(!fn_h)
            return *this;
        return from_span(fn_h->lvar_span(mem_head()));

    case LOC_ROM_ARRAY:
        return from_alloc(get_meta(rom_array()).alloc);

    case LOC_MAIN_ENTRY:
        return from_alloc(get_main_entry().rom_alloc());
    case LOC_MAIN_ENTRY_BANK:
        return from_alloc_bank(get_main_entry().rom_alloc());

    case LOC_THIS_BANK:
        if(bank >= 0 && bank < 256)
            return locator_t::const_byte(bank);
        return *this;

    case LOC_LT_GMEMBER_PTR:
        return from_span(gmember()->span(0));
    case LOC_LT_CONST_PTR:
        return from_alloc(get_meta(const_()->rom_array()).alloc);
    case LOC_LT_CONST_PTR_BANK:
        return from_alloc_bank(get_meta(const_()->rom_array()).alloc);
    };
}
