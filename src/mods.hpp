#ifndef MODS_HPP
#define MODS_HPP

#include <cstdint>
#include <string_view>
#include <vector>

#include "flat/flat_map.hpp"

#include "decl.hpp"
#include "parser_decl.hpp"

struct pstring_t;

using mod_flags_t = std::uint64_t;

#define MOD(name, bit) constexpr mod_flags_t MOD_##name = 1 << (bit);
#include "mods.inc"
#undef MOD

struct src_group_t
{
    pstring_t pstring;
    group_ht group;
};

struct mods_t
{
    fc::vector_map<group_ht, pstring_t> group_vars;
    fc::vector_map<group_ht, pstring_t> group_data;

    bool explicit_group_vars = false;
    bool explicit_group_data = false;

    mod_flags_t enable = 0;
    mod_flags_t disable = 0;

    void remove_conflicting_flags()
    {
        mod_flags_t const conflicting = enable & disable;
        enable ^= conflicting;
        disable ^= conflicting;
    }
};

mod_flags_t parse_mod_flag(std::string_view sv);
std::string_view to_string(mod_flags_t flag);

#endif
