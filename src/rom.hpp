#ifndef ROM_HPP
#define ROM_HPP

struct rom_static_t;
struct rom_many_t;
struct rom_once_t;
struct bank_t;

template<typename T> 
inline std::deque<T> rom_deque;
template<typename T> 
inline std::mutex rom_deque_mutex;

template<typename T>
struct rom_impl_ht : handle_t<unsigned, T, ~0>
{
    T* operator->() const { return &operator*(); }
    T& operator*() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return rom_deque<T>[this->value];
    }
};

struct rom_static_ht : rom_impl_ht<rom_static_t> {};
struct rom_many_ht : rom_impl_ht<rom_many_t> {};
struct rom_once_ht : rom_impl_ht<rom_once_t> {};

namespace std
{
    template<> struct hash<rom_static_ht> : handle_hash_t<rom_static_ht> {};
    template<> struct hash<rom_many_ht> : handle_hash_t<rom_many_ht> {};
    template<> struct hash<rom_once_ht> : handle_hash_t<rom_once_ht> {};
}

enum rom_alloc_class_t : char
{
    ROM_ONCE,
    ROM_MANY,
    ROM_STATIC,
};

struct rom_alloc_ht
{
    rom_alloc_class_t rclass;
    unsigned value;
};

//std::size_t many_bitset_size() { return 8; } // TODO
//std::size_t once_bitset_size() { return 8; } // TODO

struct rom_alloc_t
{
    std::uint16_t desired_alignment;
    std::uint16_t required_size;
    span_t span;
};

struct rom_static_t : public rom_alloc_t
{
};

struct rom_many_t : public rom_alloc_t
{
    bool require_static_addr;
    dynamic_bitset_t required_manys;
    dynamic_bitset_t interfering_static_manys;

};

struct rom_once_t : public rom_alloc_t
{
    dynamic_bitset_t required_manys;
    dynamic_bitset_t related_onces;
};

struct rom_bank_t
{
    span_allocator_t allocator;
    dynamic_bitset_t allocated_manys;
    dynamic_bitset_t allocated_onces;

    rh::robin_map<rom_many_ht, span_t> many_spans;

    explicit bank_t(span_allocator_t const& allocator)
    : allocator(allocator)
    {}

    span_t alloc_many_span(rom_many_t const& many)
    {
        if(!many.static_addr)
            return allocator.alloc(many.size, many.alignment);

        bc::small_vector<span_t, 16> avoid;

        bitset_for_each(many_bitset_size(), many.interfering_static_manys.get(), [&](unsigned i)
        {
            rom_many_t const& interfering = *rom_many_ht{i};
            assert(interfering.static_addr);

            if(interfering.span)
                avoid.push_back(interfering.span);
        });

        return allocator.alloc_highest_addr(many.size, many.alignment, avoid.data(), avoid.size());
    }
};

#endif
