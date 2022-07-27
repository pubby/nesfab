#include "span_allocator.hpp"

#include <cassert>

span_t span_allocator_t::alloc_at(span_t span)
{
    if(!span)
        return {};

    auto it = treap.upper_bound<std::uint16_t>(span.addr, upper_func{});

    if(it != treap.end() && it->span.contains(span))
        return did_alloc(it, span);

    return {};
}

span_t span_allocator_t::alloc(std::uint16_t size, std::uint16_t prefer_alignment)
{
    if(!size || treap.empty())
        return {};

    auto it = treap.top(); // Select the node with the largest size.
    if(it->span.size < size)
        return {};

    span_t alloc;
    if(!(alloc = aligned(it->span, size, prefer_alignment)))
        alloc = { .addr = it->span.addr, .size = size };
    return did_alloc(it, alloc);
}

void span_allocator_t::free(span_t span)
{
    auto const pre  = treap.upper_bound<std::uint16_t>(span.addr, upper_func{});
    auto const post = treap.lower_bound<std::uint16_t>(span.addr, lower_func{});

    // Update the treap

    assert(pre == treap.end() || (std::next(pre) == post));

    if(pre != treap.end())
    {
        // combine 'pre' and 'span'
        assert(pre->span.end() == span.addr);
        span = { pre->span.addr, span.end() - pre->span.addr };
        m_bytes_free -= pre->span.size;
        pool.free(*pre);
        treap.erase(*pre);
    }

    if(post != treap.end())
    {
        // combine 'post' and 'span'
        assert(span.end() == post->span.addr);
        span.size = post->span.end() - span.addr;
        m_bytes_free -= post->span.size;
        pool.free(*post);
        treap.erase(*post);
    }

    m_bytes_free += span.size;
    treap.insert(pool.alloc().assign(span));

    // Update the bitset

    unsigned const start = ((span.addr - m_initial.addr) * bitset_t::num_bits + m_initial.size - 1) / m_initial.size;
    unsigned const end = ((span.end() - m_initial.addr) * bitset_t::num_bits + m_initial.size - 1) / m_initial.size;
    unsigned const size = end - start;
    assert(size > 0);
    assert(span.contains(span_t{ start * m_initial.size / bitset_t::num_bits + m_initial.addr, 
                                 size * m_initial.size / bitset_t::num_bits }));
    m_allocated_bs -= bitset_t::filled(size, start);

    assert_valid();
}

span_t span_allocator_t::did_alloc(treap_t::iterator it, span_t alloc)
{
    assert(alloc.size > 0);

    // Update the bitset

    unsigned const start = (alloc.addr - m_initial.addr) * bitset_t::num_bits / m_initial.size;
    unsigned const end = ((alloc.end() - m_initial.addr) * bitset_t::num_bits + m_initial.size - 1) / m_initial.size;
    unsigned const size = end - start;
    assert(size > 0);
    assert((span_t{ start * m_initial.size / bitset_t::num_bits + m_initial.addr ,
                    size * m_initial.size / bitset_t::num_bits }.contains(alloc)));
    m_allocated_bs |= bitset_t::filled(size, start);

    // Update the treap

    span_t pre  = { .addr = it->span.addr, .size = alloc.addr - it->span.addr };
    if(pre.size < min_alloc_size)
    {
        alloc.addr -= pre.size;
        alloc.size += pre.size;
        pre.size = 0;
    }

    span_t post = { .addr = alloc.end(),   .size = it->span.end() - alloc.end() };
    if(post.size < min_alloc_size)
    {
        alloc.size += post.size;
        post.size = 0;
    }

    pool.free(*it);
    treap.erase(it);

    m_bytes_free -= alloc.size;

    if(pre.size > 0)
        treap.insert(pool.alloc().assign(pre));

    if(post.size > 0)
        treap.insert(pool.alloc().assign(post));

    assert_valid();

    return alloc;
}

span_t span_allocator_t::unallocated_span_at(std::uint16_t addr) const
{
    auto const node = treap.upper_bound<std::uint16_t>(addr, upper_func{});
    if(node == treap.end())
        return {};

    assert(node->span.contains(addr));
    return node->span;
}
