#ifndef SPAN_ALLOCATOR_HPP
#define SPAN_ALLOCATOR_HPP

#include <cstdint>

#include <boost/intrusive/treap_set.hpp>

#include "object_pool.hpp"
#include "span.hpp"
#include "bitset.hpp"

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
public:
    using bitset_t = aggregate_bitset_t<bitset_uint_t, 4>;
private:
    using treap_t = bi::treap_set<treap_node_t>;

    object_pool_t<treap_node_t> pool;
    treap_t treap;
    span_t m_initial;
    std::uint16_t m_bytes_free;
    std::uint16_t m_initial_bytes_free;
    unsigned m_bytes_per_bit;
    bitset_t m_allocated_bs = {};

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
    : m_initial(initial)
    , m_bytes_free(initial.size)
    , m_initial_bytes_free(initial.size)
    {
        treap.insert(pool.alloc().assign(initial));
        assert_valid();
    }

    span_allocator_t(span_allocator_t const& o)
    : m_initial(o.m_initial)
    , m_bytes_free(o.m_bytes_free)
    , m_initial_bytes_free(o.m_bytes_free)
    , m_allocated_bs(o.m_allocated_bs)
    {
        for(treap_node_t const& node : o.treap)
            treap.insert(pool.alloc().assign(node.span));
        assert_valid();
    }

    span_t alloc_at(span_t span);

    span_t alloc(std::uint16_t size, std::uint16_t alignment = 1, bool insist_alignment = false);

    void free(span_t span);

    span_t unallocated_span_at(std::uint16_t addr) const;

    std::uint16_t bytes_free() const { return m_bytes_free; }
    std::uint16_t initial_bytes_free() const { return m_initial_bytes_free; }
    std::size_t spans_free() const { return treap.size(); }

    span_t initial() const { return m_initial; }
    bitset_t const& allocated_bitset() const { return m_allocated_bs; }

    static std::size_t bytes_per_bit(span_t initial) { return initial.size / bitset_t::num_bits; }
    std::size_t bytes_per_bit() const { return bytes_per_bit(m_initial); }


private:
    span_t did_alloc(treap_t::iterator it, span_t alloc);

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
