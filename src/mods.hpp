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

    flag_mods_t() = default;

    explicit flag_mods_t(mod_flags_t enable, mod_flags_t disable = 0) 
    : enable(enable)
    , disable(disable)
    {}

    void remove_conflicting_flags()
    {
        mod_flags_t const conflicting = enable & disable;
        enable ^= conflicting;
        disable ^= conflicting;
    }
};

using mod_list_t = std::uint8_t;
// !!! Don't forget to update 'mod_list_name' when adding lists !!!
constexpr mod_list_t MODL_VARS         = 1 << 0;
constexpr mod_list_t MODL_DATA         = 1 << 1;
constexpr mod_list_t MODL_EMPLOYS      = 1 << 2;
constexpr mod_list_t MODL_EMPLOYS_VARS = 1 << 3;
constexpr mod_list_t MODL_EMPLOYS_DATA = 1 << 4;
constexpr mod_list_t MODL_PRESERVES    = 1 << 5;
constexpr mod_list_t MODL_STOWS        = 1 << 6;

constexpr mod_list_t MODL_EMPLOYS_ANY = MODL_EMPLOYS | MODL_EMPLOYS_VARS | MODL_EMPLOYS_DATA;

// Additional flags
using mod_details_t = std::uint8_t;
constexpr mod_details_t MODD_STOWS_OMNI = 1 << 0;

std::string_view mod_list_name(mod_list_t list);

struct mods_t : public flag_mods_t
{
    mod_list_t explicit_lists = 0;
    mod_details_t details = 0;

    struct list_mentioned_t
    {
        mod_list_t lists = 0;
        pstring_t pstring = {};
    };

    fc::vector_map<group_ht, list_mentioned_t> lists;

    global_t const* nmi = nullptr;
    global_t const* irq = nullptr;

    mods_t() = default;

    explicit mods_t(mod_flags_t enable, mod_flags_t disable = 0) 
    : flag_mods_t(enable, disable)
    {}

    // Ensures groups match their group_class.
    void validate_groups() const;

    void for_each_list(mod_list_t lists, std::function<void(group_ht, pstring_t)> const& fn) const;
    void for_each_list_vars(mod_list_t lists, std::function<void(group_vars_ht, pstring_t)> const& fn) const;
    void for_each_list_data(mod_list_t lists, std::function<void(group_data_ht, pstring_t)> const& fn) const;
    void for_each_employs_vars(std::function<void(group_vars_ht, pstring_t)> const& fn) const;
    void for_each_employs_data(std::function<void(group_data_ht, pstring_t)> const& fn) const;

    bool in_lists(mod_list_t lists, group_ht g) const;

    void inherit(mods_t const& from);

    void validate(
        pstring_t at,
        mod_flags_t accepts_flags = 0, 
        mod_list_t accepts_lists = 0,
        bool accepts_nmi_irq = false) const;
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
