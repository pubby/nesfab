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

#define MOD(bit, name) constexpr mod_flags_t MOD_##name = 1 << (bit);
#include "mods.inc"
#undef MOD

struct src_group_t
{
    pstring_t pstring;
    group_ht group;
};

struct flag_mods_t
{
    mod_flags_t enable = 0;
    mod_flags_t disable = 0;

    void remove_conflicting_flags()
    {
        mod_flags_t const conflicting = enable & disable;
        enable ^= conflicting;
        disable ^= conflicting;
    }
};

struct mods_t : public flag_mods_t
{
    bool explicit_group_vars = false;
    bool explicit_group_data = false;

    fc::vector_map<group_ht, pstring_t> group_vars;
    fc::vector_map<group_ht, pstring_t> group_data;

    global_t const* nmi = nullptr;
    pstring_t nmi_pstring = {};

    // Ensures groups match their group_class.
    void validate_groups() const;

    void for_each_group_vars(std::function<void(group_vars_ht)> const& fn) const;
    void for_each_group_data(std::function<void(group_data_ht)> const& fn) const;

    void inherit(mods_t const& from);

    void validate(
        pstring_t at,
        mod_flags_t accepts_flags = 0, 
        bool accepts_vars = false, bool accepts_data = false, 
        bool accepts_nmi = false) const;
};

void inherit(std::unique_ptr<mods_t>& mods, std::unique_ptr<mods_t> const& from);

mod_flags_t parse_mod_flag(std::string_view sv);
std::string_view to_string(mod_flags_t flag);

// Used to test if a mod flag is enabled / disabled:
bool mod_test(flag_mods_t const* mods, mod_flags_t flags, bool enabled = true);

// (Base class to be inherited.)
class modded_t
{
public:
    explicit modded_t(std::unique_ptr<mods_t> m) 
    : m_mods(std::move(m))
    {}

    mods_t const* mods() const { return m_mods.get(); }
protected:
    std::unique_ptr<mods_t> m_mods; // Storing as a pointer saves memory
};

#endif
