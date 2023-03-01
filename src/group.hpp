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

class group_t
{
public:
    std::string const name;
private:
    std::mutex m_define_mutex;
    group_class_t m_gclass = GROUP_UNDEFINED;
    pstring_t m_pstring = {};

    group_ht m_handle = {};

    // An index into some storage that holds the group's implementation data
    unsigned m_impl_id = ~0;

public:
    group_class_t gclass() const { return m_gclass; }

    pstring_t pstring() const { return m_pstring; }

    unsigned impl_id() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_impl_id; 
    }

    group_ht handle() const { return m_handle; }

    template<typename T>
    T handle() const
    {
        static_assert(is_handle<T>::value);
        assert(gclass() == T::value_type::group_class);
        assert(compiler_phase() > PHASE_PARSE);
        return { m_impl_id };
    }

    template<typename T>
    T& impl() const
    {
        assert(gclass() == T::group_class);
        assert(compiler_phase() > PHASE_PARSE);
        return *handle<typename T::handle_t>();
    }

    std::pair<group_vars_t*, group_vars_ht> define_vars(pstring_t pstring);
    std::pair<group_data_t*, group_data_ht> define_data(pstring_t pstring, bool once);

    static group_t* lookup(char const* source, pstring_t name);
    static group_t* lookup_sourceless(pstring_t at, std::string_view key);
    static group_t* lookup_sourceless(std::string_view name);

    group_t(pstring_t pstring, std::string_view view, unsigned handle)
    : name(view)
    , m_pstring(pstring)
    , m_handle{ handle }
    {}

    static void group_members();

private:

    unsigned define(pstring_t pstring, group_class_t gclass, 
                    std::function<bool(group_t&)> valid_same,
                    std::function<unsigned(group_t&)> create_impl);

    inline static rh::robin_auto_table<group_t*> group_map;
};

class group_vars_t
{
public:
    static constexpr group_class_t group_class = GROUP_VARS;
    using handle_t = group_vars_ht;

    group_t& group;

    explicit group_vars_t(group_t& group)
    : group(group)
    {}

    group_vars_ht handle() const { return group.handle<group_vars_ht>(); }

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
    static constexpr group_class_t group_class = GROUP_DATA;
    using handle_t = group_data_ht;

    group_t& group;
    bool const once;

    group_data_t(group_t& group, bool once)
    : group(group)
    , once(once)
    {}

    group_data_ht handle() const { return group.handle<group_data_ht>(); }

    bool banked_ptrs() const { return once; }

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

#endif
