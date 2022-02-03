#ifndef BITSET_HPP
#define BITSET_HPP

#include <algorithm>
#include <array>
#include <climits>
#include <cstdint>
#include <type_traits>

#include "array_pool.hpp"
#include "builtin.hpp"
#include "sizeof_bits.hpp"

using bitset_uint_t = std::uint64_t;
using bitset_pool_t = array_pool_t<bitset_uint_t>;

// A small-size optimized bitset.
// Uses 'uint' when size == 1, otherwise ptr.
union sso_bitset_t
{
    bitset_uint_t uint;
    bitset_uint_t* ptr;

    [[gnu::always_inline]] bitset_uint_t const* get(std::size_t size) const 
        { return size == 1 ? &uint : ptr; }

    [[gnu::always_inline]] bitset_uint_t* get(std::size_t size) 
        { return size == 1 ? &uint : ptr; }
};

template<std::size_t N>
sso_bitset_t bitset_alloc(array_pool_t<bitset_uint_t, N>& pool, 
                          std::size_t size)
{
    if(size == 1)
        return { .uint = 0 };
    return { .ptr = pool.alloc(size) };
}

// Gives the array size needed for a bitset containing 'bits_required' bits.
template<typename UInt = bitset_uint_t>
constexpr std::size_t bitset_size(std::size_t bits_required)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    return (bits_required + sizeof_bits<UInt> - 1) / sizeof_bits<UInt>;
}

template<typename UInt>
void bitset_and(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] &= rhs[i];
}

inline void bitset_and(std::size_t size, sso_bitset_t& lhs, sso_bitset_t rhs)
{ 
    if(size == 1)
        lhs.uint &= rhs.uint;
    else
        bitset_and(size, lhs.ptr, rhs.ptr);
}

template<typename UInt>
void bitset_difference(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] &= ~rhs[i];
}

inline void bitset_difference(std::size_t size, sso_bitset_t& lhs, 
                              sso_bitset_t rhs)
{ 
    if(size == 1)
        lhs.uint &= ~rhs.uint;
    else
        bitset_difference(size, lhs.ptr, rhs.ptr);
}

template<typename UInt>
void bitset_or(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] |= rhs[i];
}

inline void bitset_or(std::size_t size, sso_bitset_t& lhs, sso_bitset_t rhs)
{ 
    if(size == 1)
        lhs.uint |= rhs.uint;
    else
        bitset_or(size, lhs.ptr, rhs.ptr);
}

template<typename UInt>
void bitset_xor(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] ^= rhs[i];
}

inline void bitset_xor(std::size_t size, sso_bitset_t& lhs, sso_bitset_t rhs)
{ 
    if(size == 1)
        lhs.uint ^= rhs.uint;
    else
        bitset_or(size, lhs.ptr, rhs.ptr);
}

template<typename UInt>
void bitset_clear(UInt* bitset, std::size_t i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    bitset[byte_i] &= ~((UInt)1 << bit_i);
}

inline void bitset_clear(std::size_t size, sso_bitset_t& lhs, std::size_t i)
{ 
    if(size == 1)
        lhs.uint &= ~(1ull << i);
    else
        bitset_clear(lhs.ptr, i);
}

template<typename UInt>
void bitset_set(UInt* bitset, std::size_t i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    bitset[byte_i] |= (UInt)1 << bit_i;
}

inline void bitset_set(std::size_t size, sso_bitset_t& lhs, std::size_t i)
{ 
    if(size == 1)
        lhs.uint |= (1ull << i);
    else
        bitset_set(lhs.ptr, i);
}

template<typename UInt>
void bitset_set(UInt* bitset, std::size_t i, bool b)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    bitset[byte_i] &= ~((UInt)1 << bit_i);
    bitset[byte_i] |= (UInt)b << bit_i;
}

inline void bitset_set(std::size_t size, sso_bitset_t& lhs, 
                       std::size_t i, bool b)
{ 
    if(size == 1)
    {
        lhs.uint &= ~(1ull << i);
        lhs.uint |= (static_cast<bitset_uint_t>(b) << i);
    }
    else
        bitset_set(lhs.ptr, i, b);
}


template<typename UInt>
void bitset_flip(UInt* bitset, std::size_t i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    bitset[byte_i] ^= (UInt)1 << bit_i;
}

inline void bitset_flip(std::size_t size, sso_bitset_t& lhs, std::size_t i)
{ 
    if(size == 1)
        lhs.uint ^= (1ull << i);
    else
        bitset_flip(lhs.ptr, i);
}

template<typename UInt>
bool bitset_test(UInt const* bitset, std::size_t i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    return bitset[byte_i] & ((UInt)1 << bit_i);
}

inline bool bitset_test(std::size_t size, sso_bitset_t lhs, std::size_t i)
{ 
    if(size == 1)
        return lhs.uint & (1ull << i);
    else
        return bitset_test(lhs.ptr, i);
}

template<typename UInt>
void bitset_clear_all(std::size_t size, UInt* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::fill_n(bitset, size, (UInt)0);
}

inline void bitset_clear_all(std::size_t size, sso_bitset_t& lhs)
{ 
    if(size == 1)
        lhs.uint = 0;
    else
        bitset_clear_all(size, lhs.ptr);
}

template<typename UInt>
void bitset_set_all(std::size_t size, UInt* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::fill_n(bitset, size, ~(UInt)0);
}

template<typename UInt>
void bitset_set_n(std::size_t size, UInt* bitset, std::size_t n)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    assert(n <= size * sizeof_bits<UInt>);
    std::fill_n(bitset, n / sizeof_bits<UInt>, ~(UInt)0);
    if(UInt rem = n % sizeof_bits<UInt>)
        bitset[n / sizeof_bits<UInt>] |= ~((1 << (sizeof_bits<UInt> - rem)) - 1);
}
template<typename UInt>
void bitset_clear_n(std::size_t size, UInt* bitset, std::size_t n)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    assert(n <= size * sizeof_bits<UInt>);
    std::fill_n(bitset, n / sizeof_bits<UInt>, 0);
    if(UInt rem = n % sizeof_bits<UInt>)
        bitset[n / sizeof_bits<UInt>] &= ((1 << (sizeof_bits<UInt> - rem)) - 1);
}

inline void bitset_set_all(std::size_t size, sso_bitset_t& lhs)
{ 
    if(size == 1)
        lhs.uint = ~0ull;
    else
        bitset_set_all(size, lhs.ptr);
}

template<typename UInt>
void bitset_flip_all(std::size_t size, UInt* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        bitset[i] = ~bitset[i];
}

inline void bitset_flip_all(std::size_t size, sso_bitset_t& lhs)
{ 
    if(size == 1)
        lhs.uint = ~lhs.uint;
    else
        bitset_flip_all(size, lhs.ptr);
}

template<typename UInt>
bool bitset_all_set(std::size_t size, UInt const* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        if(bitset[i] != ~(UInt)0)
            return false;
    return true;
}

inline bool bitset_all_set(std::size_t size, sso_bitset_t lhs)
{ 
    if(size == 1)
        return lhs.uint == ~0ull;
    else
        return bitset_all_set(size, lhs.ptr);
}

template<typename UInt>
bool bitset_all_clear(std::size_t size, UInt const* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        if(bitset[i] != 0)
            return false;
    return true;
}

inline bool bitset_all_clear(std::size_t size, sso_bitset_t lhs)
{ 
    if(size == 1)
        return lhs.uint == 0ull;
    else
        return bitset_all_clear(size, lhs.ptr);
}

template<typename UInt>
std::size_t bitset_popcount(std::size_t size, UInt const* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::size_t count = 0;
    for(std::size_t i = 0; i < size; ++i)
        count += builtin::popcount(bitset[i]);
    return count;
}

inline std::size_t bitset_popcount(std::size_t size, sso_bitset_t lhs)
{ 
    if(size == 1)
        return builtin::popcount(lhs.uint);
    else
        return bitset_popcount(size, lhs.ptr);
}

template<typename UInt>
bool bitset_eq(std::size_t size, UInt const* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    return std::equal(lhs, lhs + size, rhs, rhs + size);
}

inline bool bitset_eq(std::size_t size, sso_bitset_t lhs, sso_bitset_t rhs)
{ 
    if(size == 1)
        return lhs.uint == rhs.uint;
    else
        return bitset_eq(size, lhs.ptr, rhs.ptr);
}

template<typename UInt>
void bitset_copy(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::copy_n(rhs, size, lhs);
}

inline void bitset_copy(std::size_t size, sso_bitset_t& lhs, sso_bitset_t rhs)
{ 
    if(size == 1)
        lhs.uint = rhs.uint;
    else
        bitset_copy(size, lhs.ptr, rhs.ptr);
}

// Calls 'fn' for each set bit of the bitset.
template<typename UInt, typename Fn>
void bitset_for_each(UInt bitset, Fn fn, unsigned span = 0)
{
    while(bitset)
    {
        unsigned bit = builtin::ctz(bitset);
        bitset ^= (UInt)1 << bit;
        fn(bit + span);
    }
}

// Calls 'fn' for each set bit of the bitset.
template<typename UInt, typename Fn>
void bitset_for_each(std::size_t size, UInt const* bitset, Fn fn)
{
    unsigned span = 0;
    for(std::size_t i = 0; i < size; ++i)
    {
        bitset_for_each(bitset[i], fn, span);
        span += sizeof_bits<UInt>;
    }
}

template<typename Fn>
void bitset_for_each(std::size_t size, sso_bitset_t bitset, Fn fn)
{ 
    if(size == 1)
        bitset_for_each(bitset.uint, std::move(fn), 0);
    else
        bitset_for_each(size, bitset.ptr, std::move(fn)); 
}

// Calls 'fn' for each set bit of the bitset.
template<typename UInt, typename Fn>
bool bitset_for_each_test(UInt bitset, Fn fn, unsigned span = 0)
{
    while(bitset)
    {
        unsigned bit = builtin::ctz(bitset);
        bitset ^= (UInt)1 << bit;
        if(!fn(bit + span))
            return false;
    }
    return true;
}

template<typename UInt, typename Fn>
bool bitset_for_each_test(std::size_t size, UInt* bitset, Fn fn)
{
    unsigned span = 0;
    for(std::size_t i = 0; i < size; ++i)
    {
        if(!bitset_for_each_test(bitset[i], fn, span))
            return false;
        span += sizeof_bits<UInt>;
    }
    return true;
}

template<typename Fn>
bool bitset_for_each_test(std::size_t size, sso_bitset_t bitset, Fn fn)
{ 
    if(size == 1)
        return bitset_for_each_test(bitset.uint, std::move(fn), 0);
    else
        return bitset_for_each_test(size, bitset.ptr, std::move(fn)); 
}

template<typename UInt>
void bitset_lshift(std::size_t size, UInt* bitset, std::size_t amount = 1)
{
    std::size_t const int_shifts = amount / sizeof_bits<UInt>;
    std::size_t const bit_shifts = amount % sizeof_bits<UInt>;
    std::size_t const ibit_shifts = sizeof_bits<UInt> - bit_shifts;

    std::size_t i = 0;
    if(bit_shifts == 0)
        for(; i < size - int_shifts; ++i)
            bitset[i] = bitset[i + int_shifts];
    else if(size > int_shifts)
    {
        for(; i < size - int_shifts - 1; ++i)
            bitset[i] = (bitset[i+int_shifts] << bit_shifts) | (bitset[i+int_shifts+1] >> ibit_shifts);
        bitset[i] = (bitset[i+int_shifts] << bit_shifts);
        ++i;
    }
    for(; i < size; ++i)
        bitset[i] = 0;
}

template<typename UInt>
void bitset_rshift(std::size_t size, UInt* bitset, std::size_t amount = 1)
{
    int const int_shifts = amount / sizeof_bits<UInt>;
    int const bit_shifts = amount % sizeof_bits<UInt>;
    int const ibit_shifts = sizeof_bits<UInt> - bit_shifts;

    int i = size - 1;
    if(bit_shifts == 0)
        for(; i > int_shifts - 1; --i)
            bitset[i] = bitset[i - int_shifts];
    else if(size > (std::size_t)int_shifts)
    {
        for(; i > int_shifts; --i)
            bitset[i] = (bitset[i-int_shifts] >> bit_shifts) | (bitset[i-int_shifts-1] << ibit_shifts);
        bitset[i] = (bitset[i-int_shifts] >> bit_shifts);
        --i;
    }
    for(; i >= 0; --i)
        bitset[i] = 0;
}


// A shitty bitset class that exists because std::bitset abstracts too much.
// 'aggregate_bitset_t' is an aggregate class and can use
// any unsigned integer type.
template<typename UInt, std::size_t N>
struct aggregate_bitset_t
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");

    using value_type = UInt;
    using array_type = std::array<UInt, N>;

    static constexpr std::size_t bits_per_int = sizeof(UInt) * CHAR_BIT;
    static constexpr std::size_t num_ints = N;
    static constexpr std::size_t num_bits = N * bits_per_int;

    array_type array;

    UInt const* data() const { return array.data(); }
    UInt* data() { return array.data(); }

    [[gnu::flatten]]
    void clear(UInt bit) { bitset_clear(array.data(), bit); }

    [[gnu::flatten]]
    void set(UInt bit) { bitset_set(array.data(), bit); }

    [[gnu::flatten]]
    void set(UInt bit, bool b) { bitset_set(array.data(), bit, b); }

    [[gnu::flatten]]
    void flip(UInt bit) { bitset_flip(array.data(), bit); }

    [[gnu::flatten]]
    bool test(UInt bit) const { return bitset_test(array.data(), bit); }

    [[gnu::flatten]]
    void clear_all() { bitset_clear_all(N, array.data()); }

    [[gnu::flatten]]
    void set_all() { bitset_set_all(N, array.data()); }

    [[gnu::flatten]]
    void flip_all() { bitset_flip_all(N, array.data()); }

    [[gnu::flatten]]
    std::size_t popcount() { return bitset_popcount(N, array.data()); }

    static aggregate_bitset_t filled(std::size_t size, std::size_t n = 0);
};

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator&=(aggregate_bitset_t<UInt, N>& lhs,
                                        aggregate_bitset_t<UInt, N> const& rhs)
{
    bitset_and(N, lhs.array.data(), rhs.array.data());
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator|=(aggregate_bitset_t<UInt, N>& lhs,
                                        aggregate_bitset_t<UInt, N> const& rhs)
{
    bitset_or(N, lhs.array.data(), rhs.array.data());
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator^=(aggregate_bitset_t<UInt, N>& lhs,
                                      aggregate_bitset_t<UInt, N> const& rhs)
{
    bitset_xor(N, lhs.array.data(), rhs.array.data());
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator<<=(aggregate_bitset_t<UInt, N>& lhs,
                                         std::size_t amount)
{
    bitset_lshift(N, lhs.array.data(), amount);
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator>>=(aggregate_bitset_t<UInt, N>& lhs,
                                         std::size_t amount)
{
    bitset_rshift(N, lhs.array.data(), amount);
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> operator&(aggregate_bitset_t<UInt, N> lhs,
                                      aggregate_bitset_t<UInt, N> const& rhs)
{
    lhs &= rhs;
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> operator|(aggregate_bitset_t<UInt, N> lhs,
                                      aggregate_bitset_t<UInt, N> const& rhs)
{
    lhs |= rhs;
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> operator^(aggregate_bitset_t<UInt, N> lhs,
                                      aggregate_bitset_t<UInt, N> const& rhs)
{
    lhs ^= rhs;
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> operator<<(aggregate_bitset_t<UInt, N> lhs,
                                       std::size_t amount)
{
    lhs <<= amount;
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> operator>>(aggregate_bitset_t<UInt, N> lhs,
                                       std::size_t amount)
{
    lhs >>= amount;
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> operator~(aggregate_bitset_t<UInt, N> lhs)
{
    lhs.flip_all();
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> 
aggregate_bitset_t<UInt, N>::filled(std::size_t size, std::size_t n)
{
    aggregate_bitset_t bs;
    bitset_set_n(num_ints, bs, size);
    return bs >> n;
}

#endif

