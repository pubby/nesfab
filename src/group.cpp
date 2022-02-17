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
                         std::function<unsigned(group_t&)> create_impl)
{
    assert(compiler_phase() <= PHASE_PARSE);
    unsigned ret;
    {
        std::lock_guard<std::mutex> group_lock(m_define_mutex);

        if(m_gclass == gclass) // Groups can have multiple definition points.
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

group_vars_t& group_t::define_vars(pstring_t pstring)
{
    group_vars_t* ret;

    define(pstring, GROUP_VARS, [&ret](group_t& g)
    { 
        ret = &impl_deque_alloc<group_vars_t>(g); 
        return impl_deque<group_vars_t>.size() - 1;
    });

    return *ret;
}

group_data_t& group_t::define_data(pstring_t pstring)
{
    group_data_t* ret;

    define(pstring, GROUP_DATA, [&ret](group_t& g)
    { 
        ret = &impl_deque_alloc<group_data_t>(g); 
        return impl_deque<group_data_t>.size() - 1;
    });

    return *ret;
}

//////////////////
// group_vars_t //
//////////////////

void group_vars_t::add_interferences(bitset_uint_t const* other_groups)
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);

    if(!m_interfering_group_vars)
        m_interfering_group_vars.reset(impl_bitset_size<group_vars_t>());

    bitset_or(m_interfering_group_vars.size(), 
              m_interfering_group_vars.data(), 
              other_groups);
    // Always interfere with itself:
    m_interfering_group_vars.set(handle().value);
}

