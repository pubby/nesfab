#include "mods.hpp"

#include "compiler_error.hpp"
#include "group.hpp"

mod_flags_t parse_mod_flag(std::string_view sv)
{
    using namespace std::literals::string_view_literals;
#define MOD(name, bit) if(sv == (#name ""sv)) return MOD_##name;
#include "mods.inc"
#undef MOD
    return 0;
}

std::string_view to_string(mod_flags_t flag)
{
    using namespace std::literals::string_view_literals;
    switch(flag)
    {
#define MOD(name, bit) case MOD_##name: return (#name ""sv);
#include "mods.inc"
#undef MOD
    default: return "badflag";
    }
}

void mods_t::validate_groups() const
{
    auto const validate = [&](group_class_t gclass, auto const& pair)
    {
        if(pair.first->gclass() == gclass)
            return;

        pstring_t const mod_pstring = pair.second;
        pstring_t const def_pstring = pair.first->pstring();

        throw compiler_error_t(
            fmt_error(mod_pstring, fmt("% is not a % group.", pair.first->name, group_class_keyword(gclass)))
            + fmt_note(def_pstring, fmt("% was declared here.", pair.first->name)));
    };

    for(auto const& pair : group_vars)
        validate(GROUP_VARS, pair);
    for(auto const& pair : group_data)
        validate(GROUP_DATA, pair);
}

void mods_t::for_each_group_vars(std::function<void(group_vars_ht)> const& fn) const
{
    for(auto const& pair : group_vars)
        if(pair.first->gclass() == GROUP_VARS)
            fn(pair.first->handle<group_vars_ht>());
}

void mods_t::for_each_group_data(std::function<void(group_data_ht)> const& fn) const
{
    for(auto const& pair : group_data)
        if(pair.first->gclass() == GROUP_DATA)
            fn(pair.first->handle<group_data_ht>());
}
