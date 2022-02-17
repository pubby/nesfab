#include "span_allocator.hpp"

#include <cassert>

span_t span_allocator_t::alloc_at(span_t span)
{
    span.size = std::max(span.size, min_alloc_size);

    auto it = treap.upper_bound<std::uint16_t>(span.addr, upper_func{});

    if(it != treap.end() && it->span.contains(span))
    {
        did_alloc(it, span);
        return span;
    }

    return {};
}

span_t span_allocator_t::alloc(std::uint16_t size, std::uint16_t prefer_alignment)
{
    if(treap.empty())
        return {};

    size = std::max(size, min_alloc_size);

    auto it = treap.top(); // Select the node with the largest size.
    if(it->span.size < size)
        return {};

    span_t alloc;
    if(!(alloc = aligned(it->span, size, prefer_alignment)))
        alloc = { .addr = it->span.addr, .size = size };
    did_alloc(it, alloc);
    return alloc;
}

// Tries to allocate in the highest availible address,
// but not intersecting any spans in 'avoid'.
// (Used to allocate a 'many')
span_t span_allocator_t::alloc_highest_addr(
    std::uint16_t size, std::uint16_t prefer_alignment,
    span_t const* avoid, std::size_t avoid_count)
{
    for(auto it = treap.rbegin(); it != treap.rend(); ++it)
    {
        if(it->span.size < size)
            continue;

        span_t alloc;
        if(!(alloc = aligned_reverse(it->span, size, prefer_alignment)))
            alloc = { .addr = it->span.end() - size, .size = size };

        for(std::size_t i = 0; i < avoid_count; ++i)
        {
            if(alloc.intersects(avoid[i]))
            {
                // Re-seat alloc, if possible.
                alloc.addr = avoid[i].end() - size;
                if(alloc.addr < it->span.addr)
                    goto fail;
            }
        }

        assert(&*it == &*std::next(it).base());
        did_alloc(std::next(it).base(), alloc);
        return alloc;
    fail:;
    }

    return {};
}

void span_allocator_t::free(span_t span)
{
    auto pre  = treap.upper_bound<std::uint16_t>(span.addr, upper_func{});
    auto post = treap.lower_bound<std::uint16_t>(span.addr, lower_func{});

    assert(pre == treap.end() || (std::next(pre) == post));

    if(pre != treap.end() 
       && std::abs((int)pre->span.end() - (int)span.addr) < min_alloc_size)
    {
        // combine 'pre' and 'span'
        span = { pre->span.addr, span.end() - pre->span.addr };
        m_bytes_free -= pre->span.size;
        pool.free(*pre);
        treap.erase(*pre);
    }

    if(post != treap.end() 
       && std::abs((int)span.end() - (int)post->span.addr) < min_alloc_size)
    {
        // combine 'post' and 'span'
        span.size = post->span.end() - span.addr;
        m_bytes_free -= post->span.size;
        pool.free(*post);
        treap.erase(*post);
    }

    m_bytes_free += span.size;
    treap.insert(pool.alloc().assign(span));

    assert_valid();
}

void span_allocator_t::did_alloc(treap_t::iterator it, span_t alloc)
{
    span_t pre  = { .addr = it->span.addr, .size = alloc.addr - it->span.addr };
    span_t post = { .addr = alloc.end(),   .size = it->span.end() - alloc.end() };

    pool.free(*it);
    treap.erase(it);

    m_bytes_free -= alloc.size;

    if(pre.size >= min_alloc_size)
        treap.insert(pool.alloc().assign(pre));
    else
        m_bytes_free -= pre.size;

    if(post.size >= min_alloc_size)
        treap.insert(pool.alloc().assign(post));
    else
        m_bytes_free -= post.size;

    assert_valid();
}
