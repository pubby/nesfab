#include "group.hpp"

#include "fnv1a.hpp"
#include "format.hpp"
#include "compiler_error.hpp"
#include "globals.hpp"

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
    auto& h = omni ? m_omni_h : m_data_h;
    if(!m_data_h)
        h = group_data_ht::pool_make(this);
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
