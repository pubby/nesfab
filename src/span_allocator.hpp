#ifndef SPAN_ALLOCATOR_HPP
#define SPAN_ALLOCATOR_HPP

#include <cstdint>

#include <boost/intrusive/treap_set.hpp>

#include "object_pool.hpp"
#include "span.hpp"

namespace bi = boost::intrusive;

struct treap_node_t : public bi::bs_set_base_hook<>
{
    span_t span;

    treap_node_t& assign(span_t f) { span = f; return *this; }

    constexpr bool operator<(treap_node_t const& o) const
        { return span.addr < o.span.addr; }
};

inline constexpr bool priority_order(treap_node_t const& lhs, treap_node_t const& rhs)
{
    return lhs.span.size > rhs.span.size;
}

// Allocates spans inside a larger set of spans.
// Created to allocate ROM.

class span_allocator_t
{
private:
    using treap_t = bi::treap_set<treap_node_t>;
     object_pool_t<treap_node_t> pool;
     treap_t treap;
     std::uint16_t m_bytes_free;
     std::uint16_t m_initial_bytes_free;

     struct upper_func
     {
         bool operator()(std::uint16_t lhs, treap_node_t const& rhs) const
             { return lhs > rhs.span.addr; }
     };

     struct lower_func
     {
         bool operator()(treap_node_t const& lhs, std::uint16_t rhs) const
             { return lhs.span.addr < rhs; }
     };

public:
    // Stop tracking spans once their size is less than this:
    static constexpr std::uint16_t min_alloc_size = 8;

    explicit span_allocator_t(span_t initial)
    : m_bytes_free(initial.size)
    , m_initial_bytes_free(initial.size)
    {
        treap.insert(pool.alloc().assign(initial));
        assert_valid();
    }

    span_allocator_t(span_allocator_t const& o)
    : m_bytes_free(o.m_bytes_free)
    , m_initial_bytes_free(o.m_bytes_free)
    {
        for(treap_node_t const& node : o.treap)
            treap.insert(pool.alloc().assign(node.span));
    }

    span_t alloc_at(span_t span);

    span_t alloc(std::uint16_t size, std::uint16_t prefer_alignment = 1);

    // Tries to allocate in the highest availible address,
    // but not intersecting any spans in 'avoid'.
    // (Used to allocate a 'many')
    span_t alloc_highest_addr(std::uint16_t size, std::uint16_t prefer_alignment,
                              span_t const* avoid, std::size_t avoid_count);

    void free(span_t span);

    std::uint16_t bytes_free() const { return m_bytes_free; }
    std::uint16_t initial_bytes_free() const { return m_initial_bytes_free; }
    std::size_t spans_free() const { return treap.size(); }

private:
    /* TODO: remove?
    span_t alloc_in_node(treap_t::iterator it, std::uint16_t size, bool page_align)
    {
        assert(it->span.size >= size);

        std::uint16_t addr = it->span.addr;

        if(page_align)
        {
            span_t aligned = aligned(it->span);
            if(aligned.size >= size)
                addr = aligned.addr;
        }

        span_t alloc = { .addr = addr, .size = size };
        did_alloc(it, alloc);
        return alloc;
    }
    */

    void did_alloc(treap_t::iterator it, span_t alloc);

    void assert_valid()
    {
#ifndef NDEBUG
        std::uint16_t free_count = 0;
        for(auto const& node : treap)
        {
            free_count += node.span.size;
            assert(node.span.size >= min_alloc_size);
        }

        assert(free_count == m_bytes_free);
#endif
    }
};

#endif
