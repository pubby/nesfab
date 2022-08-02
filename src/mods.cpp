#include "mods.hpp"

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

