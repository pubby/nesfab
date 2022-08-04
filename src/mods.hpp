#ifndef MODS_HPP
#define MODS_HPP

#include <cstdint>
#include <string_view>
#include <vector>
#include <functional>

#include "flat/flat_map.hpp"

#include "decl.hpp"
#include "parser_decl.hpp"

struct pstring_t;

using mod_flags_t = std::uint32_t;

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
    bool defined = false;
    bool explicit_group_vars = false;
    bool explicit_group_data = false;
    bool explicit_flags = false;

    fc::vector_map<group_ht, pstring_t> group_vars;
    fc::vector_map<group_ht, pstring_t> group_data;

    mod_flags_t enable = 0;
    mod_flags_t disable = 0;

    constexpr explicit operator bool() const { return defined; }
    constexpr bool operator!() const { return !defined; }

    // Ensures groups match their group_class.
    void validate_groups() const;

    void remove_conflicting_flags()
    {
        mod_flags_t const conflicting = enable & disable;
        enable ^= conflicting;
        disable ^= conflicting;
    }

    void for_each_group_vars(std::function<void(group_vars_ht)> const& fn) const;
    void for_each_group_data(std::function<void(group_data_ht)> const& fn) const;
};

mod_flags_t parse_mod_flag(std::string_view sv);
std::string_view to_string(mod_flags_t flag);

#endif
