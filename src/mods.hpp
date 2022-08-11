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
    bool explicit_group_vars = false;
    bool explicit_group_data = false;

    fc::vector_map<group_ht, pstring_t> group_vars;
    fc::vector_map<group_ht, pstring_t> group_data;

    mod_flags_t enable = 0;
    mod_flags_t disable = 0;

    global_t const* nmi = nullptr;
    pstring_t nmi_pstring = {};

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

    void inherit(mods_t const& from);
};

void inherit(std::unique_ptr<mods_t>& mods, std::unique_ptr<mods_t> const& from);

mod_flags_t parse_mod_flag(std::string_view sv);
std::string_view to_string(mod_flags_t flag);

// Base class
class modded_t
{
public:
    explicit modded_t(std::unique_ptr<mods_t> mods) 
    : m_mods(std::move(mods))
    {
        assert(!m_mods || (m_mods->enable & m_mods->disable) == 0);
        if(m_mods)
        {
            m_mflags = m_mods->enable & ~m_mods->disable;
            m_mflags_known = m_mods->enable | m_mods->disable;
        }
    }

    mods_t const* mods() const { return m_mods.get(); }
    mod_flags_t mflags() const { return m_mflags; }
    mod_flags_t mflags_known() const { return m_mflags_known; }
protected:
    std::unique_ptr<mods_t> m_mods; // Storing as a pointer saves memory
    mod_flags_t m_mflags;
    mod_flags_t m_mflags_known;
};

#endif
