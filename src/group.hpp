#ifndef GROUP_HPP
#define GROUP_HPP

#include <string>
#include <utility>

#include "robin/collection.hpp"
#include "robin/set.hpp"

#include "bitset.hpp"
#include "decl.hpp"
#include "pstring.hpp"
#include "rom_decl.hpp"
#include "ident_map.hpp"

class group_vars_t
{
public:
    using handle_t = group_vars_ht;

    void add_gvar(gvar_ht v)
    {
        assert(compiler_phase() <= PHASE_PARSE);
        std::lock_guard<std::mutex> lock(m_gvars_mutex);
        m_gvars.push_back(v);
    }

    std::vector<gvar_ht> const& gvars() const { assert(compiler_phase() > PHASE_PARSE); return m_gvars; }
    auto const& gmembers() const { assert(compiler_phase() > PHASE_GROUP_MEMBERS); return m_gmembers; }

    bool has_init() const { assert(compiler_phase() > PHASE_PARSE_CLEANUP); return m_has_init; }
    void determine_has_init();
    rom_proc_ht init_proc() const { assert(compiler_phase() > PHASE_INITIAL_VALUES); return m_init_proc; }
    void assign_init_proc(rom_proc_ht h) { assert(compiler_phase() == PHASE_INITIAL_VALUES); m_init_proc = h; }

    void group_members();

private:
    std::mutex m_gvars_mutex; // Used during parsing only.
    std::vector<gvar_ht> m_gvars;

    bool m_has_init = false;
    rom_proc_ht m_init_proc = {};

    xbitset_t<gmember_ht> m_gmembers;
};

class group_data_t
{
public:
    using handle_t = group_data_ht;

    void add_const(const_ht c)
    {
        assert(compiler_phase() <= PHASE_PARSE);
        std::lock_guard<std::mutex> lock(m_consts_mutex);
        m_consts.push_back(c);
    }

    std::vector<const_ht> const& consts() const { assert(compiler_phase() > PHASE_PARSE); return m_consts; }

private:
    std::mutex m_consts_mutex; // Used during parsing only.
    std::vector<const_ht> m_consts;
};

class group_t
{
public:
    std::string const name;

private:
    std::mutex m_define_mutex;
    pstring_t m_pstring = {};
    std::unique_ptr<group_vars_t> m_vars;
    std::unique_ptr<group_data_t> m_data;
    std::unique_ptr<group_data_t> m_omni;

    group_vars_ht m_vars_h = {};
    group_data_ht m_data_h = {};

    group_ht m_handle = {};

public:
    pstring_t pstring() const { return m_pstring; }
    group_ht handle() const { return m_handle; }

    group_vars_t* vars() { assert(compiler_phase() > PHASE_PARSE); return m_vars.get(); }
    group_data_t* data() { assert(compiler_phase() > PHASE_PARSE); return m_data.get(); }
    group_data_t* omni() { assert(compiler_phase() > PHASE_PARSE); return m_omni.get(); }
    group_data_t* data(bool get_omni) { return get_omni ? omni() : data(); }

    group_vars_t const* vars() const { assert(compiler_phase() > PHASE_PARSE); return m_vars.get(); }
    group_data_t const* data() const { assert(compiler_phase() > PHASE_PARSE); return m_data.get(); }
    group_data_t const* omni() const { assert(compiler_phase() > PHASE_PARSE); return m_omni.get(); }
    group_data_t const* data(bool get_omni) const { return get_omni ? omni() : data(); }
    bool any_data() const { return data() || omni(); }

    auto vars_handle() const { assert(compiler_phase() > PHASE_PARSE); return m_vars_h; }
    auto data_handle() const { assert(compiler_phase() > PHASE_PARSE); return m_data_h; }

    bool using_vars() const { return vars() && !vars()->gvars().empty(); }
    bool using_data() const { return data() && !data()->consts().empty(); }
    bool using_omni() const { return omni() && !omni()->consts().empty(); }
    bool using_any_data() const { return using_data() || using_omni(); }

    bool undefined() const { return !vars() && !data() && !omni(); }

    defined_group_vars_t define_vars(pstring_t pstring);
    defined_group_data_t define_data(pstring_t pstring, bool omni);

    static group_t* lookup(char const* source, pstring_t name)
        { return lookup_sourceless(name, name.view(source)); }
    static group_t* lookup_sourceless(pstring_t name, std::string_view key)
        { assert(key.empty() || key[0] == '/'); return key.empty() ? nullptr : &group_pool_map.lookup(name, key); }
    static group_t* lookup_sourceless(std::string_view view)
        { assert(view.empty() || view[0] == '/'); return view.empty() ? nullptr : group_pool_map.lookup(view); }

    
    template<typename Fn>
    void for_each_const(Fn const& fn) 
    {
        if(data())
            for(const_ht c : data()->consts())
                fn(c);

        if(omni())
            for(const_ht c : omni()->consts())
                fn(c);
    }

    group_t(pstring_t pstring, std::string_view view, unsigned handle)
    : name(view)
    , m_pstring(pstring)
    , m_handle{ handle }
    {}

    static void group_members();

private:
    inline static ident_map_t<group_ht> group_pool_map;
};

#endif
