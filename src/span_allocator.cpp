#include "span_allocator.hpp"

#include "assert.hpp"

span_allocation_t span_allocator_t::alloc_at(span_t span)
{
    if(!span)
        return {};
    assert(span.size > 0);

    auto it = treap.upper_bound<std::uint16_t>(span.addr, upper_func{});
    if(it == treap.begin())
        return {};
    --it;

    if(it->span.contains(span))
        return did_alloc(it, span);

    return {};
}

span_allocation_t span_allocator_t::alloc(std::uint16_t size, std::uint16_t alignment, bool insist_alignment)
{
    if(treap.empty())
        return {};

    if(!size)
        size = 1;

    auto it = treap.top(); // Select the node with the largest size.
    if(it->span.size < size)
        return {};

    span_t alloc = aligned(it->span, size, alignment);
    if(!alloc)
    {
        if(insist_alignment)
            return {};
        alloc = { .addr = it->span.addr, .size = size };
    }
    assert(it->span.contains(alloc));
    return did_alloc(it, alloc);
}

span_allocation_t span_allocator_t::alloc_linear(std::uint16_t size, std::uint16_t alignment, unsigned after)
{
    if(!after)
        return alloc(size, alignment);

    for(auto it = treap.begin(); it != treap.end(); ++it)
    {
        if(it->span.end() < after)
            continue;

        span_t span = it->span;
        if(span.addr < after)
            span = { after, span.end() - after };

        if(span.size >= size)
        {
            span_t alloc = aligned(span, size, alignment);
            if(!alloc)
                continue;

            assert(it->span.contains(alloc));
            return did_alloc(it, alloc);
        }
    }

    return {};
}

void span_allocator_t::free(span_t span)
{
    if(!span.size)
        return;

    auto const post = treap.upper_bound<std::uint16_t>(span.addr, upper_func{});
    auto const pre  = post == treap.begin() ? treap.end() : std::prev(post);

    // Update the treap

    assert(pre == treap.end() || (std::next(pre) == post));

    if(pre != treap.end() && pre->span.end() == span.addr)
    {
        // combine 'pre' and 'span'
        m_bytes_free -= pre->span.size;
        pool.free(*pre);
        treap.erase(*pre);
    }

    if(post != treap.end() && span.end() == post->span.addr)
    {
        // combine 'post' and 'span'
        span.size = post->span.end() - span.addr;
        m_bytes_free -= post->span.size;
        pool.free(*post);
        treap.erase(*post);
    }

    assert(span.size > 0);

#ifndef NDEBUG
    for(auto const& node : treap)
        assert(!node.span.intersects(span));
#endif

    m_bytes_free += span.size;
    treap.insert(pool.alloc().assign(span));

    // Update the bitset

    unsigned const start = ((span.addr - m_initial.addr) * bitset_t::num_bits) / m_initial.size;
    unsigned const end = ((span.end() - m_initial.addr) * bitset_t::num_bits + m_initial.size - 1) / m_initial.size;
    unsigned const size = end - start;
    passert(size > 0, span, m_initial);
#ifndef NDEBUG
    span_t c = { start * m_initial.size / bitset_t::num_bits + m_initial.addr, 
                 size * m_initial.size / bitset_t::num_bits };
    passert(c.contains(span), span, c);
#endif
    m_allocated_bs -= bitset_t::filled(start, size);

    assert_valid();
}

span_allocation_t span_allocator_t::did_alloc(treap_t::iterator it, span_t const object)
{
    assert(object.size > 0);
    assert(it->span.contains(object));

    span_t alloc = object;

    // Update the bitset

    unsigned const start = (alloc.addr - m_initial.addr) * bitset_t::num_bits / m_initial.size;
    unsigned const end = ((alloc.end() - m_initial.addr) * bitset_t::num_bits + m_initial.size - 1) / m_initial.size;
    unsigned const size = end - start;
    assert(size > 0);
    assert((span_t{ start * m_initial.size / bitset_t::num_bits + m_initial.addr ,
                    size * m_initial.size / bitset_t::num_bits }.contains(alloc)));
    m_allocated_bs |= bitset_t::filled(start, size);

    // Update the treap

    assert(it->span.addr <= alloc.addr);
    span_t pre  = { .addr = it->span.addr, .size = alloc.addr - it->span.addr };
    if(pre.size < min_alloc_size)
    {
        alloc.addr -= pre.size;
        alloc.size += pre.size;
        pre.size = 0;
    }

    assert(it->span.end() >= alloc.end());
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

    return { alloc, object };
}

span_t span_allocator_t::unallocated_span_at(std::uint16_t addr) const
{
    auto node = treap.upper_bound<std::uint16_t>(addr, upper_func{});
    if(node != treap.begin())
    {
        --node;
        if(node->span.contains(addr))
            return node->span;
    }

    return {};
}
