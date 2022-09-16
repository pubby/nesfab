#include "locator.hpp"

#include <iostream> // TODO

#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "type.hpp"
#include "ir.hpp"
#include "lt.hpp"
#include "rom.hpp"

std::string to_string(locator_t loc)
{
    std::string str;

    switch(loc.lclass())
    {
    default: 
        return fmt("unknown locator %", (int)loc.lclass());
    case LOC_NONE:
        return "none";
    case LOC_GMEMBER:
        str = fmt("gmember % %", loc.gmember()->gvar.global.name, loc.gmember()->member()); break;
    case LOC_GMEMBER_SET:
        str = fmt("gset %", loc.handle()); break;
    case LOC_PTR_SET:
        str = fmt("pset %", loc.handle()); break;
    case LOC_FN:
        str = fmt("fn %", loc.fn()->global.name); break;
    case LOC_STMT:
        str = fmt("stmt %", loc.handle()); break;
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
        str = "addr $" + to_hex_string(loc.data() + loc.offset()); break;
    case LOC_INDEX:
        str = "index $" + loc.data(); break;
    case LOC_SSA:
        str = fmt("ssa %", loc.handle()); break;
    case LOC_PHI:
        str = fmt("phi %", loc.handle()); break;
    case LOC_MINOR_VAR:
        str = fmt("minor var %", loc.fn()->global.name); break;
    case LOC_ASM_LOCAL_VAR:
        str = fmt("asm_local_var", loc.fn()->global.name); break;
    case LOC_ROM_ARRAY:
        str = "rom_array"; break;
    //case LOC_LT_GMEMBER_PTR:
        //str = fmt("gmember_ptr % %", loc.gmember()->gvar.global.name, loc.gmember()->member()); break;
    case LOC_GCONST:
        str = fmt("gconst %", loc.const_()->global.name); break;
    case LOC_LT_EXPR:
        str = fmt("lt_expr % %", loc.handle(), loc.lt().safe().type); break;
    case LOC_THIS_BANK:
        str = "this bank"; break;
    case LOC_MAIN_ENTRY:
        str = "main entry"; break;
    case LOC_RESET_GROUP_VARS:
        str = fmt("reset group vars %", loc.group_vars()->group.name); break;
    case LOC_RUNTIME_ROM:
        str = fmt("runtime_rom %", loc.runtime_rom()); break;
    case LOC_RUNTIME_RAM:
        str = fmt("runtime_ram %", loc.runtime_ram()); break;
    case LOC_NMI_INDEX:
        str = fmt("nmi_index %", loc.fn()->global.name); break;
    }

    if(has_arg_member_atom(loc.lclass()))
        str += fmt(" arg:% member:% atom:% offset:%", (int)loc.arg(), (int)loc.member(), (int)loc.atom(), (int)loc.offset());
    else
        str += fmt(" data:% offset:%", (int)loc.data(), (int)loc.offset());

    str += fmt(" byteified:% is:%", (int)loc.byteified(), (int)loc.is());

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

mods_t const* locator_t::mods() const
{
    if(lclass() == LOC_ASM_LOCAL_VAR)
        return fn().safe().def().local_vars[arg()].mods();
    if(has_const(lclass()))
        return const_()->mods();
    if(has_gmember(lclass()))
        return gmember()->gvar.mods();
    if(has_fn(lclass()))
        return fn()->mods();
    return nullptr;
}

bool locator_t::mem_zp_only() const
{
    if(!mem_zp_valid())
        return false;

    if(mod_test(mods(), MOD_zero_page))
        return true;

    if(lclass() == LOC_ROM_ARRAY || is_lt(lclass()))
        return false;

    type_t const t = with_byteified(false).with_is(IS_DEREF).type();
    return is_ptr(t.name()) && (!has_arg_member_atom(lclass()) || member() == 0);
}

bool locator_t::mem_zp_valid() const
{
    return !mod_test(mods(), MOD_zero_page, false);
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
                type = type.with_banked(false);
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

    switch(is())
    {
    case IS_PTR:
        return byteify(TYPE_APTR);
    case IS_PTR_HI:
    case IS_BANK:
        return TYPE_U;
    default:
        break;
    }

    switch(lclass())
    {
        /*
    case LOC_LT_GMEMBER_PTR:
        if(gmember_ht const m = gmember())
            return byteify(type_t::ptr(m->gvar.group(), true, false));
        break;
        */
    case LOC_GCONST:
        if(const_ht const c = const_())
            c->type();
        break;
    case LOC_ROM_ARRAY:
        if(rom_array_ht const a = rom_array())
            return type_t::tea(TYPE_U, a.safe().data().size()); // TODO: use a proper pointer, with groups
        break;
    case LOC_LT_EXPR:
        assert(lt());
        return byteify(lt().safe().type);
    case LOC_GMEMBER: 
        return byteify(gmember()->type());
    case LOC_ARG:
        return byteify(fn().safe().type().type(arg()));
    case LOC_RETURN:
        return byteify(fn().safe().type().return_type());
    case LOC_ASM_LOCAL_VAR:
        return byteify(fn().safe().def().local_vars[arg()].type());
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

locator_t locator_t::link(romv_t romv, fn_ht fn_h, int bank) const
{
    assert(compiler_phase() == PHASE_LINK);

    auto const from_span = [&](span_t span) -> locator_t
    { 
        if(!span)
            return *this;

        span.addr += offset();

        if(is() == IS_DEREF)
            return addr(span.addr); 
        else if(is() == IS_PTR)
            return const_byte(span.addr & 0xFF);
        else if(is() == IS_PTR_HI)
            return const_byte((span.addr >> 8) & 0xFF);

        return *this;
    };

    int span_offset = 0;

    switch(lclass())
    {
    case LOC_FN:
        // Functions with a known first bank must be called using that bank:
        if(is() == IS_BANK && fn()->first_bank_switch())
            return fn()->first_bank_switch().link(romv, fn_h, bank);

        {
            auto const& proc = fn()->rom_proc()->asm_proc();
            locator_t const label = 
                data() == ENTRY_LABEL ? proc.entry_label : locator_t::minor_label(data());

            if(auto const* info = proc.lookup_label(label))
                span_offset = info->offset;
            else // Likely a compiler bug:
                throw std::runtime_error(fmt("Missing label during link: % / % ", label, *this));
        }

        // fall-through
    default:
        if(rom_alloc_ht a = rom_alloc(romv))
        {
            if(is() == IS_BANK)
            {
                int const bank = a.first_bank();
                if(bank < 0 || bank >= 256)
                    return *this;
                return locator_t::const_byte(bank);
            }
            else if(rom_alloc_t* alloc = a.get())
                return from_span(offset_span(alloc->span, span_offset));
            return *this;
        }
        return *this;

    case LOC_ADDR: // Remove the offset.
        return locator_t::addr(data() + offset()).with_is(is());

    case LOC_GMEMBER:
        return from_span(gmember()->span(atom()));

    case LOC_SSA:
    case LOC_PHI:
        {
            if(!fn_h)
                return *this;
            span_t span = {};
            for(unsigned i = 0; !span && i < NUM_ROMV; ++i)
                span = fn_h->lvar_span(romv_t(i), mem_head());
            return from_span(fn_h->lvar_span(romv, mem_head()));
        }

    case LOC_ASM_LOCAL_VAR:
    case LOC_ARG:
    case LOC_RETURN:
    case LOC_MINOR_VAR:
        {
            span_t span = {};
            for(unsigned i = 0; !span && i < NUM_ROMV; ++i)
                span = fn()->lvar_span(romv_t(i), mem_head());
            return from_span(fn()->lvar_span(romv, mem_head()));
        }

    case LOC_THIS_BANK:
        if(bank >= 0 && bank < 256)
            return locator_t::const_byte(bank);
        return *this;

    //case LOC_LT_GMEMBER_PTR:
        //return from_span(gmember()->span(0));

    case LOC_RUNTIME_RAM:
        {
            if(is() == IS_BANK)
                return locator_t::const_byte(0);
            span_t span = {};
            for(unsigned i = 0; !span && i < NUM_ROMV; ++i)
                span = runtime_span(runtime_ram(), romv_t(i));
            return from_span(span);
        }

    case LOC_RUNTIME_ROM:
        {
            if(is() == IS_BANK)
                return locator_t::const_byte(0);
            span_t span = {};
            for(unsigned i = 0; !span && i < NUM_ROMV; ++i)
                span = runtime_span(runtime_rom(), romv_t(i));
            return from_span(span);
        }

    case LOC_NMI_INDEX:
        return locator_t::const_byte(fn()->nmi_index() + 1);
    };
}

rom_data_ht locator_t::rom_data() const
{
    switch(lclass())
    {
    default:
        return {};
    case LOC_FN:
        return fn()->rom_proc();
    case LOC_ROM_ARRAY:
        return rom_array();
    case LOC_MAIN_ENTRY:
        return get_main_entry().rom_proc();
    case LOC_GCONST:
        return const_()->rom_array();
    case LOC_RESET_GROUP_VARS:
        return group_vars()->init_proc();
    };
}

rom_alloc_ht locator_t::rom_alloc(romv_t romv) const
{
    if(rom_data_ht d = rom_data())
        return d.get()->find_alloc(romv);
    return {};
}
