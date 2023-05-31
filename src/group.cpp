#include "group.hpp"

#include "fnv1a.hpp"
#include "format.hpp"
#include "compiler_error.hpp"
#include "globals.hpp"

group_t* group_t::lookup(char const* source, pstring_t name)
{
    return lookup_sourceless(name, name.view(source));
}

group_t* group_t::lookup_sourceless(pstring_t at, std::string_view key)
{
    if(key.empty())
        return nullptr;

    std::uint64_t const hash = fnv1a<std::uint64_t>::hash(key.data(), key.size());

    return group_ht::with_pool([&, hash, key](auto& pool)
    {
        rh::apair<group_t**, bool> result = group_map.emplace(hash,
            [key](group_t* ptr) -> bool
            {
                return std::equal(key.begin(), key.end(), ptr->name.begin(), ptr->name.end());
            },
            [&pool, at, key]() -> group_t*
            { 
                return &pool.emplace_back(at, key, pool.size());
            });

        return *result.first;
    });
}

group_t* group_t::lookup_sourceless(std::string_view view)
{
    if(view.empty())
        return nullptr;

    std::uint64_t const hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    return group_ht::with_const_pool([&, hash, view](auto const&)
    {
        auto result = group_map.lookup(hash,
            [view](group_t* ptr) -> bool
            {
                return std::equal(view.begin(), view.end(), ptr->name.begin(), ptr->name.end());
            });

        return result.second ? *result.second : nullptr;
    });
}

defined_group_vars_t group_t::define_vars(pstring_t pstring)
{
    std::lock_guard<std::mutex> lock(m_define_mutex);
    if(!m_pstring)
        m_pstring = pstring;
    if(!m_vars)
        m_vars.reset(new group_vars_t());
    if(!m_vars_h)
        m_vars_h = group_vars_ht::pool_make(this);
    return { this, m_vars.get(), m_vars_h };
}

defined_group_data_t group_t::define_data(pstring_t pstring, bool omni)
{
    std::lock_guard<std::mutex> lock(m_define_mutex);
    if(!m_pstring)
        m_pstring = pstring;
    auto& ptr = omni ? m_omni : m_data;
    if(!ptr)
        ptr.reset(new group_data_t());
    if(!m_data_h)
        m_data_h = group_data_ht::pool_make(this);
    return { this, ptr.get(), m_data_h };
}

void group_t::group_members()
{
    assert(compiler_phase() == PHASE_GROUP_MEMBERS);

    for(group_t* group : group_vars_ht::values())
        group->vars()->group_members();
}

///////////////////
// group_gvars_t //
///////////////////

void group_vars_t::group_members()
{
    assert(compiler_phase() == PHASE_GROUP_MEMBERS);

    // Init the gmembers bitset
    m_gmembers.alloc();
    for(gvar_ht gv : gvars())
        m_gmembers.set_n(gv->begin().id, gv->num_members());
}

void group_vars_t::determine_has_init()
{
    assert(compiler_phase() == PHASE_PARSE_CLEANUP);
    m_has_init = false;
    for(gvar_ht gvar : gvars())
    {
        if(gvar->init_expr)
        {
            m_has_init = true;
            break;
        }
    }
}
