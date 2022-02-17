#ifndef GROUP_HPP
#define GROUP_HPP

#include <string>
#include <string>

#include "robin/collection.hpp"
#include "robin/set.hpp"

#include "bitset.hpp"
#include "decl.hpp"
#include "pstring.hpp"

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
    unsigned m_impl_index = ~0;

public:
    group_class_t gclass() const { return m_gclass; }

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

    group_vars_t& define_vars(pstring_t pstring);
    group_data_t& define_data(pstring_t pstring);

    static group_t& lookup(char const* source, pstring_t name);

    group_t(pstring_t pstring, char const* source, unsigned handle)
    : name(pstring.view(source))
    , m_pstring(pstring)
    , m_handle{ handle }
    {}
private:

    unsigned define(pstring_t pstring, group_class_t gclass, 
                    std::function<unsigned(group_t&)> create_impl);

    inline static rh::robin_auto_table<group_t*> group_map;
};

class group_vars_t
{
public:
    using group_impl_tag = void;
    static constexpr group_class_t gclass = GROUP_DATA;

    group_t& group;

    explicit group_vars_t(group_t& group)
    : group(group)
    {}

    group_vars_ht handle() const { return group.handle<group_vars_ht>(); }

    void add_gvar(gvar_ht v)
    {
        assert(compiler_phase() <= PHASE_PARSE);
        std::lock_guard<std::mutex> lock(gvars_mutex);
        gvars.push_back(v);
    }

    void add_interferences(bitset_uint_t const* other_groups);
    bitset_t const& interfering_group_vars() { return m_interfering_group_vars; }

private:
    std::mutex gvars_mutex; // Used during parsing only.
    std::vector<gvar_ht> gvars;

    // Tracks 'group vars' that are live while this group is live.
    bitset_t m_interfering_group_vars;
};

class group_data_t
{
public:
    using group_impl_tag = void;
    static constexpr group_class_t gclass = GROUP_DATA;

    group_t& group;

    explicit group_data_t(group_t& group)
    : group(group)
    {}

    group_data_ht handle() const { return group.handle<group_data_ht>(); }

    void add_const(const_ht c)
    {
        assert(compiler_phase() <= PHASE_PARSE);
        std::lock_guard<std::mutex> lock(consts_mutex);
        consts.push_back(c);
    }

private:
    std::mutex consts_mutex; // Used during parsing only.
    std::vector<const_ht> consts;
};

#endif
