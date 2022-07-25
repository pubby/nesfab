#ifndef BITSET_HPP
#define BITSET_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <climits>
#include <cstdint>
#include <type_traits>
#include <memory>

#include "alloca.hpp"
#include "builtin.hpp"
#include "sizeof_bits.hpp"

using bitset_uint_t = std::uint64_t;

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

template<typename UInt>
void bitset_difference(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] &= ~rhs[i];
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
void bitset_clear(UInt* bitset, std::size_t i)
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
void bitset_clear_all(std::size_t size, UInt* bitset)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::fill_n(bitset, size, (UInt)0);
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
    {
        assert(n / sizeof_bits<UInt> < size);
        bitset[n / sizeof_bits<UInt>] |= ((1ull << rem) - 1ull);
    }
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
bool bitset_all_clear(std::size_t size, UInt const* bitset)
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
bool bitset_eq(std::size_t size, UInt const* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    return std::equal(lhs, lhs + size, rhs, rhs + size);
}

template<typename UInt>
void bitset_copy(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    std::copy_n(rhs, size, lhs);
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

template<typename UInt>
int bitset_lowest_bit_set(std::size_t size, UInt* bitset)
{
    for(std::size_t i = 0; i < size; ++i)
        if(bitset[i])
            return i * sizeof_bits<UInt> + builtin::ctz(bitset[i]);
    return -1;
}

template<typename UInt>
int bitset_highest_bit_set(std::size_t size, UInt* bitset)
{
    for(int i = size-1; i >= 0; --i)
        if(bitset[i])
            return i * sizeof_bits<UInt> + builtin::rclz(bitset[i]) - 1;
    return -1;
}

template<typename UInt>
void bitset_rshift(std::size_t size, UInt* bitset, std::size_t amount = 1)
{
    if(amount == 0)
        return;

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
            bitset[i] = (bitset[i+int_shifts] >> bit_shifts) | (bitset[i+int_shifts+1] << ibit_shifts);
        bitset[i] = (bitset[i+int_shifts] >> bit_shifts);
        ++i;
    }
    for(; i < size; ++i)
        bitset[i] = 0;
}

template<typename UInt>
void bitset_lshift(std::size_t size, UInt* bitset, std::size_t amount = 1)
{
    if(amount == 0)
        return;

    int const int_shifts = amount / sizeof_bits<UInt>;
    int const bit_shifts = amount % sizeof_bits<UInt>;
    int const ibit_shifts = sizeof_bits<UInt> - bit_shifts;

    int i = size - 1;
    if(bit_shifts == 0)
        for(; i > int_shifts - 1; --i)
            bitset[i] = bitset[i - int_shifts];
    else if((int)size > int_shifts)
    {
        for(; i > int_shifts; --i)
            bitset[i] = (bitset[i-int_shifts] << bit_shifts) | (bitset[i-int_shifts-1] >> ibit_shifts);
        bitset[i] = (bitset[i-int_shifts] << bit_shifts);
        --i;
    }
    for(; i >= 0; --i)
        bitset[i] = 0;
}

// Used to find consecutive 1 bits set in the bitset of length 'consec_len'.
// The result will have a 1 set at the start of every span.
// (This can be used to track allocations using a bitset)
template<typename UInt>
void bitset_mark_consecutive(std::size_t size, UInt* bitset, std::size_t consec_len)
{
    if(consec_len <= 1)
        return;

    UInt* temp = ALLOCA_T(UInt, size);

    unsigned shift_by = 1;
    for(consec_len  -= 1; shift_by <= consec_len ; shift_by <<= 1)
    {
        bitset_copy(size, temp, bitset);
        bitset_rshift(size, temp, shift_by);
        bitset_and(size, bitset, temp);
    }

    bitset_copy(size, temp, bitset);
    bitset_rshift(size, temp, consec_len  - (shift_by >> 1));
    bitset_and(size, bitset, temp);
}

// A shitty bitset class that exists because std::bitset abstracts too much.
// 'aggregate_bitset_t' is an aggregate class and can use
// any unsigned integer type.
template<typename UInt, std::size_t N>
struct alignas(128) aggregate_bitset_t
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

    constexpr std::size_t size() const { return num_ints; }

    [[gnu::flatten]]
    void clear(UInt bit) { bitset_clear(data(), bit); }

    [[gnu::flatten]]
    void set(UInt bit) { bitset_set(data(), bit); }

    [[gnu::flatten]]
    void set(UInt bit, bool b) { bitset_set(data(), bit, b); }

    [[gnu::flatten]]
    void flip(UInt bit) { bitset_flip(data(), bit); }

    [[gnu::flatten]]
    bool test(UInt bit) const { return bitset_test(data(), bit); }

    [[gnu::flatten]]
    void clear_all() { bitset_clear_all(N, data()); }

    [[gnu::flatten]]
    void set_all() { bitset_set_all(N, data()); }

    bool all_clear() const { return bitset_all_clear(N, data()); }

    [[gnu::flatten]]
    constexpr void flip_all()
    { 
        for(UInt& i : array)
            i = ~i;
    }

    [[gnu::flatten]]
    std::size_t popcount() const { return bitset_popcount(N, data()); }

    int lowest_bit_set() const { return bitset_lowest_bit_set(N, data()); }

    static constexpr aggregate_bitset_t filled();
    static aggregate_bitset_t filled(std::size_t size, std::size_t n = 0);

    template<typename Fn>
    void for_each(Fn const& fn) const { bitset_for_each(size(), data(), fn); }
};

template<std::size_t Bits>
using static_bitset_t = aggregate_bitset_t<bitset_uint_t, (Bits + sizeof_bits<bitset_uint_t> - 1) / sizeof_bits<bitset_uint_t>>;

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator&=(aggregate_bitset_t<UInt, N>& lhs,
                                        aggregate_bitset_t<UInt, N> const& rhs)
{
    bitset_and(N, lhs.data(), rhs.data());
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator|=(aggregate_bitset_t<UInt, N>& lhs,
                                        aggregate_bitset_t<UInt, N> const& rhs)
{
    bitset_or(N, lhs.data(), rhs.data());
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator^=(aggregate_bitset_t<UInt, N>& lhs,
                                      aggregate_bitset_t<UInt, N> const& rhs)
{
    bitset_xor(N, lhs.data(), rhs.data());
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator-=(aggregate_bitset_t<UInt, N>& lhs,
                                        aggregate_bitset_t<UInt, N> const& rhs)
{
    bitset_difference(N, lhs.data(), rhs.data());
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator<<=(aggregate_bitset_t<UInt, N>& lhs,
                                         std::size_t amount)
{
    bitset_lshift(N, lhs.data(), amount);
    return lhs;
}

template<typename UInt, std::size_t N> [[gnu::flatten]]
aggregate_bitset_t<UInt, N>& operator>>=(aggregate_bitset_t<UInt, N>& lhs,
                                         std::size_t amount)
{
    bitset_rshift(N, lhs.data(), amount);
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
aggregate_bitset_t<UInt, N> operator-(aggregate_bitset_t<UInt, N> lhs,
                                      aggregate_bitset_t<UInt, N> const& rhs)
{
    lhs -= rhs;
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
constexpr aggregate_bitset_t<UInt, N> operator~(aggregate_bitset_t<UInt, N> lhs)
{
    lhs.flip_all();
    return lhs;
}

template<typename UInt, std::size_t N>
constexpr aggregate_bitset_t<UInt, N> 
aggregate_bitset_t<UInt, N>::filled()
{
    return ~aggregate_bitset_t<UInt, N>{};
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N> 
aggregate_bitset_t<UInt, N>::filled(std::size_t size, std::size_t n)
{
    aggregate_bitset_t bs = {};
    bitset_set_n(num_ints, bs.data(), size);
    assert(bs.popcount() == size);
    bs <<= n;
#ifndef NDEBUG
    for(unsigned i = 0; i < size; ++i)
        assert(bs.test(i + n));
#endif
    assert(bs.popcount() == size);
    return bs;
}

// Dynamically allocates its memory.
class bitset_t
{
public:
    using value_type = bitset_uint_t;
    static constexpr std::size_t bits_per_int = sizeof(value_type) * CHAR_BIT;

    bitset_t() = default;

    bitset_t(bitset_t const& o)
    : bitset_t(o.size())
    {
        assert(size() == o.size());
        std::copy(o.data(), o.data() + o.size(), data());
    }

    bitset_t(bitset_t&& o) = default;

    explicit bitset_t(std::size_t num_ints)
    : m_ptr(new value_type[num_ints]())
    , m_size(num_ints)
    {}

    bitset_t& operator=(bitset_t const& o)
    {
        reset(o.size());
        assert(size() == o.size());
        std::copy(o.data(), o.data() + o.size(), data());
        return *this;
    }

    bitset_t& operator=(bitset_t&& o) = default;

    bitset_uint_t const& operator[](std::size_t i) const { return data()[i]; }
    bitset_uint_t& operator[](std::size_t i) { return data()[i]; }

    value_type const* data() const { return m_ptr.get(); }
    value_type* data() { return m_ptr.get(); }

    constexpr std::size_t size() const { return m_size; }
    constexpr std::size_t num_bits() const { return m_size * bits_per_int; }

    constexpr explicit operator bool() const { return size(); }

    void reset(std::size_t num_ints)
    {
        m_ptr.reset(new value_type[num_ints]());
        m_size = num_ints;
    }

    [[gnu::flatten]]
    void clear(value_type bit) { assert(bit < num_bits()); bitset_clear(data(), bit); }

    [[gnu::flatten]]
    void set(value_type bit) { assert(bit < num_bits()); bitset_set(data(), bit); }

    [[gnu::flatten]]
    void set(value_type bit, bool b) { assert(bit < num_bits()); bitset_set(data(), bit, b); }

    [[gnu::flatten]]
    void flip(value_type bit) { assert(bit < num_bits()); bitset_flip(data(), bit); }

    [[gnu::flatten]]
    bool test(value_type bit) const { assert(bit < num_bits()); return bitset_test(data(), bit); }

    [[gnu::flatten]]
    void clear_all() { bitset_clear_all(size(), data()); }

    [[gnu::flatten]]
    void set_all() { bitset_set_all(size(), data()); }

    [[gnu::flatten]]
    void flip_all() { bitset_flip_all(size(), data()); }

    bool all_clear() const { return bitset_all_clear(size(), data()); }

    [[gnu::flatten]]
    std::size_t popcount() const { return bitset_popcount(size(), data()); }

    template<typename Fn>
    void for_each(Fn const& fn) const { bitset_for_each(size(), data(), fn); }

    template<typename Fn>
    bool for_each_test(Fn const& fn) const { return bitset_for_each_test(size(), data(), fn); }

private:
    std::unique_ptr<value_type[]> m_ptr;
    std::size_t m_size = 0; // in ints, NOT bits
};

[[gnu::flatten]]
inline bitset_t& operator&=(bitset_t& lhs, bitset_t const& rhs)
{
    assert(lhs.size() == rhs.size());
    bitset_and(lhs.size(), lhs.data(), rhs.data());
    return lhs;
}

[[gnu::flatten]]
inline bitset_t& operator|=(bitset_t& lhs, bitset_t const& rhs)
{
    assert(lhs.size() == rhs.size());
    bitset_or(lhs.size(), lhs.data(), rhs.data());
    return lhs;
}

[[gnu::flatten]]
inline bitset_t& operator^=(bitset_t& lhs, bitset_t const& rhs)
{
    assert(lhs.size() == rhs.size());
    bitset_xor(lhs.size(), lhs.data(), rhs.data());
    return lhs;
}

[[gnu::flatten]]
inline bitset_t& operator<<=(bitset_t& lhs, std::size_t amount)
{
    bitset_lshift(lhs.size(), lhs.data(), amount);
    return lhs;
}

[[gnu::flatten]]
inline bitset_t& operator>>=(bitset_t& lhs, std::size_t amount)
{
    bitset_rshift(lhs.size(), lhs.data(), amount);
    return lhs;
}

#endif

