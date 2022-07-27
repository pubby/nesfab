#include "group.hpp"

#include "fnv1a.hpp"
#include "format.hpp"
#include "compiler_error.hpp"

group_t& group_t::lookup(char const* source, pstring_t name)
{
    assert(compiler_phase() <= PHASE_PARSE);

    std::string_view view = name.view(source);
    auto const hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    std::lock_guard<std::mutex> lock(impl_deque_mutex<group_t>);
    rh::apair<group_t**, bool> result = group_map.emplace(hash,
        [view](group_t* group) -> bool
        {
            return std::equal(view.begin(), view.end(), group->name.begin());
        },
        [name, source]() -> group_t*
        { 
            return &impl_deque<group_t>.emplace_back(name, source, impl_deque<group_t>.size());
        });

    assert(result.first);
    return **result.first;
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
            return m_impl_index;

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
        m_impl_index = ret = create_impl(*this);
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
        return impl_deque_alloc<group_vars_t>(ptr, g); 
    })};

    return std::make_pair(ptr ? ptr : &h.safe(), h);
}

std::pair<group_data_t*, group_data_ht> group_t::define_data(pstring_t pstring, bool once)
{
    group_data_t* ptr = nullptr;

    group_data_ht h = { define(pstring, GROUP_DATA, 
    [once](group_t& g)
    {
        group_data_t& data = group_data_ht{ g.m_impl_index }.safe();
        return data.once == once;
    },
    [&ptr, once](group_t& g)
    { 
        return impl_deque_alloc<group_data_t>(ptr, g, once); 
    })};

    return std::make_pair(ptr ? ptr : &h.safe(), h);
}

void group_t::parse_cleanup()
{
    assert(compiler_phase() > PHASE_PARSE);
    for(group_t const& group : impl_deque<group_t>)
        if(group.gclass() == GROUP_UNDEFINED)
            compiler_error(group.pstring(), "Group name not in scope.");
}
