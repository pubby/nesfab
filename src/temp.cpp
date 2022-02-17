#include <iostream> // TODO: remove
#include <ostream>
#include <cmath>
#include <boost/container/small_vector.hpp>

#include "robin/map.hpp"

#include "alloca.hpp"
#include "bitset.hpp"
#include "format.hpp"
#include "handle.hpp"
#include "object_pool.hpp"
#include "phase.hpp"

namespace bc = boost::container;

int main()
{
    span_allocator_t a({ .addr = 0, .size = 512 });
    span_t span;
    std::cout << a.alloc_at({ 10, 100 }) << std::endl;
    std::cout << a.alloc_at({ 10, 100 }) << std::endl;
    std::cout << (span = a.alloc(10)) << std::endl;
    std::cout << (span = a.alloc(15, true)) << std::endl;
    std::cout << (span = a.alloc(20)) << std::endl;
    std::cout << (a.alloc(30)) << std::endl;
    a.free(span);
    std::cout << (span = a.alloc(20)) << std::endl;
    a.free(span);
    std::cout << (span = a.alloc(20)) << std::endl;
    std::cout << (span = a.alloc(50)) << std::endl;
}

/*
struct
{
    std::uint16_t desired_alignment;
    std::uint16_t desired_size;
    span_t allocated_span;
};
*/


class rom_allocator_t
{
public:
    rom_allocator_t(unsigned num_banks)
    {
        constexpr span_t rom_span = { .addr = 0x8000, .size = 0x8000 };
        span_allocator_t allocator(rom_span);

        // Allocate static_addrs
        for(rom_static_t& sa : rom_deque<rom_static_t>)
        {
            if(sa.span)
            {
                if(!allocator.alloc_at(sa.span))
                    throw std::runtime_error(fmt("Unable to allocate static address % - % (out of ROM space).", 
                                                 sa.span.addr, sa.span.end() - 1));
            }
            else if(!(sa.span = allocator.alloc(sa.size, sa.alignment)))
                throw std::runtime_error("Unable to allocate static address (out of ROM space).");
        }

        // Use 'allocator' to fill banks.
        banks.clear();
        for(unsigned i = 0; i < num_banks; ++i)
            banks.emplace_back(allocator);
        ranks.resize(num_banks);

        struct once_rank_t
        {
            float score;
            rom_once_t* once;

            constexpr auto operator<=>(once_rank_t const&) const = default;
        };

        // Order 'onces'
        std::vector<once_rank_t> ordered_onces;
        ordered_onces.reserve(rom_deque<rom_once_t>.size());
        for(rom_once_t& once : rom_deque<rom_once_t>)
            ordered_onces.push_back({ rank(once), &once });
        std::sort(ordered_onces.begin(), ordered_onces.end(), std::greater<>{});

        // Allocate onces
        for(once_rank_t const& rank : ordered_onces)
            alloc(*rank.once);
    }

private:
    struct bank_rank_t
    {
        float score;
        unsigned bank_index;

        constexpr auto operator<=>(bank_rank_t const& o) const = default;
    };

    std::vector<bank_t> banks;
    std::vector<bank_rank_t> ranks;

    static float rank(rom_once_t const& once)
    {
        int many_size = 0;
        bitset_for_each(many_bitset_size(), once.required_manys.get(), [&](unsigned i)
        {
            rom_many_t const& many = *rom_many_ht{ i };
            if(many.static_addr)
                many_size += many.size * 8;
            else
                many_size += many.size;
        });

        int related = bitset_popcount(once_bitset_size(), once.related_onces.get());

        return many_size + once.size + related;
    }

    static float rank(bank_t const& bank, rom_once_t const& once)
    {
        bitset_uint_t* unallocated_manys = ALLOCA_T(bitset_uint_t, many_bitset_size());
        bitset_copy(many_bitset_size(), unallocated_manys, once.required_manys.get());
        bitset_difference(many_bitset_size(), unallocated_manys, bank.allocated_manys.get());

        int unallocated_many_size = 0;
        bitset_for_each(many_bitset_size(), unallocated_manys, [&](unsigned i)
        {
            rom_many_t const& many = *rom_many_ht{ i };
            unallocated_many_size += many.size;
        });

        bitset_uint_t* onces = ALLOCA_T(bitset_uint_t, once_bitset_size());
        bitset_copy(once_bitset_size(), onces, once.related_onces.get());
        bitset_and(once_bitset_size(), onces, bank.allocated_onces.get());
        int const related = bitset_popcount(once_bitset_size(), onces);
        bitset_xor(once_bitset_size(), onces, bank.allocated_onces.get());
        int const unrelated = bitset_popcount(once_bitset_size(), onces);

        float const r = bank.allocator.initial_bytes_free() * std::sqrt((float)bank.allocator.spans_free());
        return unallocated_many_size + related - (unrelated * 0.25f) + (bank.allocator.bytes_free() / r);
    }

    void rank_for(rom_once_t const& once)
    {
        assert(ranks.size() == banks.size());

        for(unsigned i = 0; i < banks.size(); ++i)
            ranks[i] = { rank(banks[i], once), i };

        std::sort(ranks.begin(), ranks.end(), std::greater<>{});
    }

    void alloc(rom_once_t& once)
    {
        struct many_span_t
        {
            rom_many_ht many;
            span_t span;
        };

        bc::small_vector<many_span_t, 32> many_spans;

        rank_for(once);
        for(bank_rank_t const& r : ranks)
        {
            bank_t& bank = banks[r.bank_index];

            // 1. try to allocate manys
            // 2. then try to allocate 'once'

            many_spans.clear();

            bool const allocated_manys = 
            bitset_for_each_test(once_bitset_size(), once.required_manys.get(), [&](unsigned i)
            {
                rom_many_ht h = rom_many_ht{ i };
                rom_many_t& many = *h;

                if(!bank.many_spans.find(h))
                {
                    span_t span = bank.alloc_many_span(many);
                    if(!span)
                        return false;
                    many_spans.push_back({ h, span });
                }

                return true;
            });

            // Now try to allocate 'once's span:
            if(!allocated_manys || !(once.span = bank.allocator.alloc(once.size, once.alignment)))
            {
                // If we fail, free allocated 'many' memory.
                for(many_span_t const& ms : many_spans)
                    bank.allocator.free(ms.span);
                continue;
            }

            // If we succeed, record the results:

            for(many_span_t const& ms : many_spans)
            {
                ms.many->span = ms.span;
                bank.many_spans.insert({ ms.many, ms.span });
            }

            return;
        }

        throw std::runtime_error("Unable to allocate address (out of ROM space).");
    }
};

/*
struct rom_many_t
{
    std::uint16_t size;
};

struct static_rom_many_t : public rom_many_t
{
    span_t span;
};

struct reloc_rom_many_t : public rom_many_t
{
    bank_bitset_t in_banks;
};
*/

class static_many_allocator_t
{
public:

private:
};


/*
class span_alloc_t
{
public:
    bool free_at(span_t span) const
    {
    }

    span_t alloc(std::uint16_t size, bool page_align = false)
    {
        assert(size > 0);

        if(m_free.empty())
            return {};

        if(page_align)
        {

            for(auto it = m_free.begin(); it != m_free.end(); ++it)
            {
                if(it->size < size)
                    break;

                span_t aligned = page_aligned(*it);
                if(aligned.size >= size)
                {
                    aligned.size = size;
                    did_alloc(it, aligned);
                    return aligned;
                }
            }
        }
        else
        {
            // Non-aligned allocations always use the beginning 
            // of the largest free spanment to allocate.

            if(m_free.front().size > size)
            {
                span_t alloc = { m_free.front().addr, size };
                did_alloc(m_free.begin(), alloc);
                return alloc;
            }
        }

        return {};
    }

private:

    struct free_t : public intrusive_t<free_t>
    {
        free_t(span_t span, span_alloc_t& owner, free_t* search_from)
        : span(span)
        {
            owner.add_to_list(search_from, *this);
        }

        span_t span;

        constexpr auto operator<=>(free_t const& o) const
            { return span.addr <=> o.addr; }
        constexpr auto operator<=>(std::uint16_t addr) const
            { return span.addr <=> addr; }
        using is_transparent = void;

    };

    span_list_t m_free;

    std::set<free_t> m_free_tree;
    intrusive_list_t<free_t> m_free_list;

    using list_iterator_t = intrusive_list_t<free_t>::iterator;

    list_iterator_t add_to_list(list_iterator_t search_from, free_t& free)
    {
        while(search_from != m_free_list.end() && search_from->size > span.size)
            ++search_from;
        return m_free_list.insert(search_from, free);
    }

    void did_alloc(seg_list_t::iterator it, span_t span)
    {
        assert(it->contains(span));

        if(it->addr == span.addr) // Allocated in the beginning of 'it'
        {
            auto node = m_free_tree.extract(it);
            auto list_it = m_free_list.erase(node.key());
            node.key().span.size -= alloc.size;

            if(node.key().span.size >= min_alloc_size)
            {
                node.key().span.addr += alloc.size;
                add_to_list(list_it, span);
                m_free_tree.insert(std::move(node));
            }
        }
        else if(it->end() == alloc.end()) // Allocated in the end of 'it'
        {
            auto node = m_free_tree.extract(it);
            auto list_it = m_free_list.erase(node.key());
            node.key().span.size -= alloc.size;

            if(node.key().span.size >= min_alloc_size)
            {
                add_to_list(list_it, span);
                m_free_tree.insert(std::move(node));
            }
        }
        else // Allocated in the middle of 'it'
        {
            // Split the span of 'it' in two

            span_t pre, post;

            pre.addr = it->addr;
            pre.size = alloc.addr - it->addr;

            post.addr = alloc.end();
            post.size = it->end() - alloc.end();

            if(post.size > pre.size)
                std::swap(pre, post);

            auto list_it = m_free_list.erase(it);

            if(pre.size > post.size)
            {
                it = add_span(it, pre);
                add_span(it, post);
            }
            else
            {
                it = add_span(it, post);
                add_span(it, pre);
            }
        }
    }
};

/*


struct rom_once_t
{
    std::vector<span_t> segs;

    fc::small_set<unsigned> static_manys;
    fc::small_set<unsigned> static_manys;
};

struct static_rom_many_t
{

    set<static_rom_many_ht> interfere;
};

struct reloc_rom_many_t
{

};

struct runner
{
    std::vector<rom_once_t> onces;
    std::vector<static_rom_many_t> static_manys;
    std::vector<reloc_rom_many_t> reloc_manys;



    //rom_once_t& operator[](rom_once_ht i) { return onces[i.value]; }
    //static_rom_many_t& operator[](rom_once_ht i) { return onces[i.value]; }
    //reloc_rom_many_t& operator[](rom_once_ht i) { return onces[i.value]; }
};

// Every 'once' we allocate comes with associated 'many's
// The more we spread 'once's out, the more 'many' wastes




// 3 types of data:
// 1. Once - exists in one address, one bank
// 2. Static Many - exists at one address, many banks
// 3. Relocatable Many - exists in many banks at potentially many addresses


// - build interference graph for static_manys
// - allocate each static_many to an address
//   - we may need to allocate these overlapping each other
//   - when they overlap, it adds interferences to dependent 'once' and 'relocatable_manys'
// - allocate onces that use static_manys
//   - minimize amount of banks used


- pick a once
- 


// Question: What are we optimizing
// - Priority #1: Fit everything in rom
// - Priority #2: Make it fast
*/
