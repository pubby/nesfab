#ifndef GROUP_HPP
#define GROUP_HPP

#include <string>
#include <utility>

#include "robin/collection.hpp"
#include "robin/set.hpp"

#include "bitset.hpp"
#include "decl.hpp"
#include "pstring.hpp"

class group_t
{
public:
    static constexpr compiler_phase_t impl_deque_phase = PHASE_PARSE;
    std::string const name;
private:
    std::mutex m_define_mutex;
    group_class_t m_gclass = GROUP_UNDEFINED;
    pstring_t m_pstring = {};

    group_ht m_handle = {};

    // An index into some storage that holds the group's implementation data
    unsigned m_impl_index = ~0;

public:
    group_class_t gclass() const { return m_gclass; }

    pstring_t pstring() const { return m_pstring; }

    unsigned index() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_impl_index; 
    }

    group_ht handle() const { return m_handle; }

    template<typename T>
    T handle() const
    {
        static_assert(is_group_handle<T>::value);
        assert(gclass() == T::gclass);
        assert(compiler_phase() > PHASE_PARSE);
        return { m_impl_index };
    }

    template<typename T>
    T& impl() const
    {
        static_assert(is_group_impl<T>::value);
        assert(gclass() == T::gclass);
        assert(compiler_phase() > PHASE_PARSE);
        return impl_deque<T>[m_impl_index];
    }

    std::pair<group_vars_t*, group_vars_ht> define_vars(pstring_t pstring);
    std::pair<group_data_t*, group_data_ht> define_data(pstring_t pstring, bool once);

    static group_t& lookup(char const* source, pstring_t name);

    group_t(pstring_t pstring, char const* source, unsigned handle)
    : name(pstring.view(source))
    , m_pstring(pstring)
    , m_handle{ handle }
    {}

    // Call after parsing
    static void parse_cleanup();

private:

    unsigned define(pstring_t pstring, group_class_t gclass, 
                    std::function<bool(group_t&)> valid_same,
                    std::function<unsigned(group_t&)> create_impl);

    inline static rh::robin_auto_table<group_t*> group_map;
};

class group_vars_t
{
public:
    static constexpr compiler_phase_t impl_deque_phase = PHASE_PARSE;
    using group_impl_tag = void;
    static constexpr group_class_t gclass = GROUP_VARS;

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

private:
    std::mutex m_gvars_mutex; // Used during parsing only.
    std::vector<gvar_ht> m_gvars;

    //std::mutex m_interfering_groups_mutex;
    //bitset_t m_interfering_groups;
};

class group_data_t
{
public:
    static constexpr compiler_phase_t impl_deque_phase = PHASE_PARSE;
    using group_impl_tag = void;
    static constexpr group_class_t gclass = GROUP_DATA;

    group_t& group;
    bool const once;

    group_data_t(group_t& group, bool once)
    : group(group)
    , once(once)
    {}

    group_data_ht handle() const { return group.handle<group_data_ht>(); }

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
