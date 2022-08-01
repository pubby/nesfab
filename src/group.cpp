#include "group.hpp"

#include "fnv1a.hpp"
#include "format.hpp"
#include "compiler_error.hpp"

group_t& group_t::lookup(char const* source, pstring_t name)
{
    assert(compiler_phase() <= PHASE_PARSE);

    std::string_view view = name.view(source);
    auto const hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    return *group_ht::with_pool([&, hash, view, source](auto& pool)
    {
        rh::apair<group_t**, bool> result = group_map.emplace(hash,
            [view](group_t* ptr) -> bool
            {
                return std::equal(view.begin(), view.end(), 
                                  ptr->name.begin(), ptr->name.end());
            },
            [&pool, name, source]() -> group_t*
            { 
                return &pool.emplace_back(name, source, pool.size());
            });

        return *result.first;
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
            return m_impl_id;

        if(m_gclass != GROUP_UNDEFINED)
        {
            if(pstring && m_pstring)
            {
                file_contents_t file1(pstring.file_i);
                file_contents_t file2(m_pstring.file_i);
                throw compiler_error_t(
                    fmt_error(file1, pstring, 
                              fmt("Group identifier % already in use.", 
                                  pstring.view(file1.source())))
                    + fmt_error(file2, m_pstring, 
                                "Previous definition here:"));
            }
            else
                throw compiler_error_t(fmt("Group identifier % already in use.", name));
        }

        m_gclass = gclass;
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

void group_t::parse_cleanup()
{
    assert(compiler_phase() > PHASE_PARSE);
    for(group_t const& group : group_ht::values())
        if(group.gclass() == GROUP_UNDEFINED)
            compiler_error(group.pstring(), "Group name not in scope.");
}
