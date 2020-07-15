#ifndef BITSET_HPP
#define BITSET_HPP

#include <array>
#include <climits>
#include <cstdint>
#include <type_traits>

#include "builtin.hpp"
#include "sizeof_bits.hpp"

// Gives the array size needed for a bitset containing 'bits_required' bits.
template<typename UInt>
std::size_t bitset_size(std::size_t bits_required)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    return (bits_required + sizeof_bits<UInt> - 1) / sizeof_bits<Uint>;
}

template<typename UInt>
void bitset_and(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] &= rhs[i];
}

template<typename UInt>
void bitset_or(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] |= rhs[i];
}

template<typename UInt>
void bitset_xor(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] ^= rhs[i];
}

template<typename UInt>
void bitset_reset(UInt* bitset, UInt i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    bitset[byte_i] &= ~((UInt)1 << bit_i);
}

template<typename UInt>
void bitset_set(UInt* bitset, std::size_t i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    bitset[byte_i] |= (UInt)1 << bit_i;
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

template<typename UInt>
void bitset_flip(UInt* bitset, std::size_t i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    bitset[byte_i] ^= (UInt)1 << bit_i;
}

template<typename UInt>
bool bitset_test(UInt const* bitset, std::size_t i)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    UInt const byte_i = i / sizeof_bits<UInt>;
    UInt const bit_i = i % sizeof_bits<UInt>;
    return bitset[byte_i] & ((UInt)1 << bit_i);
}

template<typename UInt>
void bitset_reset_all(std::size_t size, UInt* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::fill_n(bitset, size, (Uint)0);
}

template<typename UInt>
void bitset_set_all(std::size_t size, UInt* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::fill_n(bitset, size, ~(Uint)0);
}

template<typename UInt>
void bitset_flip_all(std::size_t size, UInt* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        bitset[i] = ~bitset[i];
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


template<typename UInt>
bool bitset_all_reset(std::size_t size, UInt const* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        if(bitset[i] != 0)
            return false;
    return true;
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

template<typename UInt>
void bitset_eq(std::size_t size, UInt const* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    return std::equal(lhs, lhs + size, rhs, rhs + size);
}

template<typename UInt>
void bitset_copy(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    return std::copy_n(rhs, size, lhs);
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

    [[gnu::flatten]]
    void reset(UInt bit) { bitset_reset(array.data(), bit); }

    [[gnu::flatten]]
    void set(UInt bit) { bitset_set(array.data(), bit); }

    [[gnu::flatten]]
    void set(UInt bit, bool b) { bitset_set(array.data(), bit, b); }

    [[gnu::flatten]]
    void flip(UInt bit) { bitset_flip(array.data(), bit); }

    [[gnu::flatten]]
    bool test(UInt bit) const { return bitset_test(array.data(), bit); }

    [[gnu::flatten]]
    void reset_all() { bitset_reset_all(N, array.data()); }

    [[gnu::flatten]]
    void set_all() { bitset_set_all(N, array.data()); }

    [[gnu::flatten]]
    void flip_all() { bitset_flip_all(N, array.data()); }

    [[gnu::flatten]]
    std::size_t popcount() { return bitset_popcount(N, array.data()); }
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
aggregate_bitset_t<UInt, N> operator~(aggregate_bitset_t<UInt, N> lhs)
{
    lhs.flip_all();
    return lhs;
}

#endif

