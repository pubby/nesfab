#include "mods.hpp"

#include "compiler_error.hpp"
#include "group.hpp"

mod_flags_t parse_mod_flag(std::string_view sv)
{
    using namespace std::literals::string_view_literals;
#define MOD(bit, name) if(sv == (#name ""sv)) return MOD_##name;
#include "mods.inc"
#undef MOD
    return 0;
}

std::string_view to_string(mod_flags_t flag)
{
    using namespace std::literals::string_view_literals;
    switch(flag)
    {
#define MOD(bit, name) case MOD_##name: return (#name ""sv);
#include "mods.inc"
#undef MOD
    default: return "badflag";
    }
}

std::string_view mod_list_name(mod_list_t list)
{
    using namespace std::literals;
    switch(list)
    {
    case MODL_VARS: return "vars"sv;
    case MODL_DATA: return "data"sv;
    case MODL_EMPLOYS: return "employs"sv;
    case MODL_PRESERVES: return "preserves"sv;
    case MODL_STOWS: return "stows"sv;
    default: return "bad list modifier"sv;
    }
}

void mods_t::validate_groups() const
{
    for(auto const& pair : lists)
    {
        switch(pair.first->gclass())
        {
        case GROUP_DATA:
            if(pair.second.lists & ~(MODL_DATA | MODL_EMPLOYS | MODL_STOWS))
                goto error;
            break;

        case GROUP_VARS:
            if(pair.second.lists & ~(MODL_VARS | MODL_EMPLOYS | MODL_PRESERVES))
                goto error;
            break;

        default:
        error:
            throw compiler_error_t(
                fmt_error(pair.second.pstring, fmt("% is not compatible with this modifier.", pair.first->name))
                + fmt_note(pair.first->pstring(), fmt("% was declared here.", pair.first->name)));
        }
    }
}

void mods_t::for_each_list(mod_list_t listf, std::function<void(group_ht, pstring_t)> const& fn) const
{
    if(explicit_lists & listf)
        for(auto const& pair : lists)
            if(pair.second.lists & listf)
                fn(pair.first, pair.second.pstring);
}

void mods_t::for_each_list_vars(mod_list_t listf, std::function<void(group_vars_ht, pstring_t)> const& fn) const
{
    if(explicit_lists & listf)
        for(auto const& pair : lists)
            if(pair.second.lists & listf)
                if(pair.first->gclass() == GROUP_VARS)
                    fn(pair.first->handle<group_vars_ht>(), pair.second.pstring);
}

void mods_t::for_each_list_data(mod_list_t listf, std::function<void(group_data_ht, pstring_t)> const& fn) const
{
    if(explicit_lists & listf)
        for(auto const& pair : lists)
            if(pair.second.lists & listf)
                if(pair.first->gclass() == GROUP_DATA)
                    fn(pair.first->handle<group_data_ht>(), pair.second.pstring);
}

bool mods_t::in_lists(mod_list_t listf, group_ht g) const
{
    if(!(explicit_lists & listf))
        return false;

    auto it = lists.find(g);
    if(it == lists.end())
        return false;

    return it->second.lists & listf;
}

void mods_t::inherit(mods_t const& from)
{
    explicit_lists |= from.explicit_lists;

    mod_flags_t const flag_mask = (from.enable | from.disable) & ~(enable | disable);
    enable |= from.enable & flag_mask;
    disable |= from.disable & flag_mask;

    for(auto const& pair : from.lists)
    {
        auto& mapped = lists[pair.first];
        mapped.lists |= pair.second.lists;
        if(!mapped.pstring)
            mapped.pstring = pair.second.pstring;
    }

    if(from.nmi && !nmi)
    {
        nmi = from.nmi;
        nmi_pstring = from.nmi_pstring;
    }
}

void mods_t::validate(
    pstring_t pstring,
    mod_flags_t accepts_flags, 
    mod_list_t accepts_lists,
    bool accepts_nmi) const
{
    if(~accepts_lists & explicit_lists)
    {
        mod_list_t const wrong_flags = ~accepts_lists & explicit_lists;
        std::string wrong_str;

        bitset_for_each(wrong_flags, [&](unsigned bit)
        {
            wrong_str.push_back(' ');
            wrong_str += mod_list_name(1 << bit);
        });

        compiler_error(pstring, fmt("Unexpected modifiers:%.", wrong_str));
    }

    if(!accepts_nmi && nmi)
        compiler_error(pstring, "Unexpected nmi modifier.");

    mod_flags_t const bad_enable = enable & ~accepts_flags;
    mod_flags_t const bad_disable = disable & ~accepts_flags;

    if(bad_enable | bad_disable)
    {
        bitset_for_each(bad_enable, [&](unsigned bit)
        {
            compiler_warning(pstring, 
                fmt("Ignoring modifier +%.", to_string(mod_flags_t(1ull << bit))));
        });

        bitset_for_each(bad_disable, [&](unsigned bit)
        {
            compiler_warning(pstring, 
                fmt("Ignoring modifier -%.", to_string(mod_flags_t(1ull << bit))));
        });
    }
}

void inherit(std::unique_ptr<mods_t>& mods, std::unique_ptr<mods_t> const& from)
{
    if(!from)
        return;
    if(!mods)
        mods.reset(new mods_t());
    mods->inherit(*from);
}

bool mod_test(flag_mods_t const* mods, mod_flags_t flags, bool enabled) 
{ 
    if(!mods)
        return false;
    if(enabled)
        return flags & mods->enable;
    else
        return flags & mods->disable;
}

