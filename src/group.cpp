#include "group.hpp"

#include "fnv1a.hpp"
#include "format.hpp"
#include "compiler_error.hpp"
#include "globals.hpp"

group_t& group_t::lookup(char const* source, pstring_t name)
{
    auto& group = lookup_sourceless(name, name.view(source));
    return group;
}

group_t& group_t::lookup_sourceless(pstring_t at, std::string_view key)
{
    std::uint64_t const hash = fnv1a<std::uint64_t>::hash(key.data(), key.size());

    return *group_ht::with_pool([&, hash, key](auto& pool)
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

unsigned group_t::define(pstring_t pstring, group_class_t gclass, 
                         std::function<bool(group_t&)> valid_same,
                         std::function<unsigned(group_t&)> create_impl)
{
    assert(compiler_phase() <= PHASE_PARSE);
    unsigned ret;
    {
        std::lock_guard<std::mutex> group_lock(m_define_mutex);

        if(m_gclass == gclass && valid_same(*this)) // Groups can have multiple definition points.
        {
            if(!m_pstring)
                m_pstring = pstring;
            return m_impl_id;
        }

        if(m_gclass != GROUP_UNDEFINED)
        {
            if(pstring && m_pstring)
            {
                file_contents_t file(pstring.file_i);
                throw compiler_error_t(
                    fmt_error(pstring, fmt("Group identifier % already in use.", 
                                           pstring.view(file.source())), &file)
                    + fmt_note(m_pstring, "Previous definition here:"));
            }
            else
                throw compiler_error_t(fmt("Group identifier % already in use.", name));
        }

        m_gclass = gclass;
        if(pstring)
            m_pstring = pstring; // Not necessary but useful for error reporting.
        m_impl_id = ret = create_impl(*this);
    }
    return ret;
}

std::pair<group_vars_t*, group_vars_ht> group_t::define_vars(pstring_t pstring)
{
    group_vars_t* ptr = nullptr;

    group_vars_ht h = { define(pstring, GROUP_VARS, 
    [](group_t& g) { return true; },
    [&ptr](group_t& g)
    { 
        return group_vars_ht::pool_emplace(ptr, g).id;
    })};

    return std::make_pair(ptr ? ptr : &h.safe(), h);
}

std::pair<group_data_t*, group_data_ht> group_t::define_data(pstring_t pstring, bool once)
{
    group_data_t* ptr = nullptr;

    group_data_ht h = { define(pstring, GROUP_DATA, 
    [once](group_t& g)
    {
        group_data_t& data = group_data_ht{ g.m_impl_id }.safe();
        return data.once == once;
    },
    [&ptr, once](group_t& g)
    { 
        return group_data_ht::pool_emplace(ptr, g, once).id; 
    })};

    return std::make_pair(ptr ? ptr : &h.safe(), h);
}

void group_t::group_members()
{
    assert(compiler_phase() == PHASE_GROUP_MEMBERS);

    for(group_vars_t& gv : group_vars_ht::values())
        gv.group_members();
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
