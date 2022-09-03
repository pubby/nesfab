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
void bitset_flipped_difference(std::size_t size, UInt* lhs, UInt const* rhs)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    for(std::size_t i = 0; i < size; ++i)
        lhs[i] = rhs[i] & ~lhs[i];
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

template<typename UInt, typename Fn>
void _bitset_do_n(std::size_t size, UInt* bitset, std::size_t start, std::size_t n, Fn const& fn)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    assert(n <= size * sizeof_bits<UInt>);

    std::size_t const start_i = (start + sizeof_bits<UInt> - 1) / sizeof_bits<UInt>;
    std::size_t const end_i = (start + n) / sizeof_bits<UInt>;

    if(start_i <= end_i)
    {
        assert((end_i - start_i) <= size);
        std::fill_n(bitset + start_i, end_i - start_i, ~(UInt)0);

        if(UInt const rem = start % sizeof_bits<UInt>)
        {
            assert((end_i - start_i) < size);
            assert(start_i-1 == start / sizeof_bits<UInt>);
            fn(bitset[start_i-1], ~0ull << rem);
        }

        if(UInt const rem = (start + n) % sizeof_bits<UInt>)
        {
            assert((end_i - start_i) < size);
            fn(bitset[end_i], ((1ull << rem) - 1ull));
        }
    }
    else
        fn(bitset[end_i], ((1ull << n) - 1ull) << (start % sizeof_bits<UInt>));
}

template<typename UInt>
void bitset_set_n(std::size_t size, UInt* bitset, std::size_t start, std::size_t n)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    _bitset_do_n(size, bitset, start, n, [](UInt& v, UInt m) { v |= m; });
}

template<typename UInt>
void bitset_set_n(std::size_t size, UInt* bitset, std::size_t n)
{
    bitset_set_n(size, bitset, 0, n);
}

template<typename UInt>
void bitset_clear_n(std::size_t size, UInt* bitset, std::size_t n)
{
    bitset_clear_n(size, bitset, 0, n);
}

template<typename UInt>
void bitset_clear_n(std::size_t size, UInt* bitset, std::size_t start, std::size_t n)
{
    static_assert(std::is_unsigned<UInt>::value, "Must be unsigned.");
    _bitset_do_n(size, bitset, start, n, [](UInt& v, UInt m) { v &= ~m; });
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
template<typename Bit = unsigned, typename UInt, typename Fn>
void bitset_for_each(UInt bitset, Fn fn, unsigned span = 0)
{
    while(bitset)
    {
        std::size_t bit = builtin::ctz(bitset);
        bitset ^= 1ull << bit;
        fn(Bit{bit + span});
    }
}

// Calls 'fn' for each set bit of the bitset.
template<typename Bit = unsigned, typename UInt, typename Fn>
void bitset_for_each(std::size_t size, UInt const* bitset, Fn fn)
{
    unsigned span = 0;
    for(std::size_t i = 0; i < size; ++i)
    {
        bitset_for_each<Bit>(bitset[i], fn, span);
        span += sizeof_bits<UInt>;
    }
}

// Calls 'fn' for each set bit of the bitset.
template<typename Bit = unsigned, typename UInt, typename Fn>
bool bitset_for_each_test(UInt bitset, Fn fn, unsigned span = 0)
{
    while(bitset)
    {
        unsigned bit = builtin::ctz(bitset);
        bitset ^= (UInt)1 << bit;
        if(!fn(Bit{bit + span}))
            return false;
    }
    return true;
}

template<typename Bit = unsigned, typename UInt, typename Fn>
bool bitset_for_each_test(std::size_t size, UInt* bitset, Fn fn)
{
    unsigned span = 0;
    for(std::size_t i = 0; i < size; ++i)
    {
        if(!bitset_for_each_test<Bit>(bitset[i], fn, span))
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

template<typename Derived, typename value_type = bitset_uint_t>
struct bitset_base_t
{
    std::size_t size_in_bits() const { return d().size() * sizeof(value_type) * CHAR_BIT; }

    [[gnu::flatten]]
    void clear(value_type bit) { bitset_clear(d().data(), bit); }

    [[gnu::flatten]]
    void set(value_type bit) { bitset_set(d().data(), bit); }

    [[gnu::flatten]]
    void set(value_type bit, bool b) { bitset_set(d().data(), bit, b); }

    [[gnu::flatten]]
    void flip(value_type bit) { bitset_flip(d().data(), bit); }

    [[gnu::flatten]]
    bool test(value_type bit) const { return bitset_test(d().data(), bit); }

    [[gnu::flatten]]
    void set_n(std::size_t n) { bitset_set_n(d().size(), d().data(), n); }

    [[gnu::flatten]]
    void set_n(std::size_t start, std::size_t n) { bitset_set_n(d().size(), d().data(), start, n); }

    [[gnu::flatten]]
    void clear_all() { bitset_clear_all(d().size(), d().data()); }

    [[gnu::flatten]]
    void set_all() { bitset_set_all(d().size(), d().data()); }

    [[gnu::flatten]]
    bool all_clear() const { return bitset_all_clear(d().size(), d().data()); }

    [[gnu::flatten]]
    constexpr void flip_all() { bitset_flip_all(d().size(), d().data()); }

    [[gnu::flatten]]
    std::size_t popcount() const { return bitset_popcount(d().size(), d().data()); }

    [[gnu::flatten]]
    int lowest_bit_set() const { return bitset_lowest_bit_set(d().size(), d().data()); }

    template<typename Bit = unsigned, typename Fn>
    void for_each(Fn const& fn) const { bitset_for_each<Bit>(d().size(), d().data(), fn); }

    template<typename Bit = unsigned, typename Fn>
    bool for_each_test(Fn const& fn) const { return bitset_for_each_test<Bit>(d().size(), d().data(), fn); }

    [[gnu::flatten]]
    value_type const& operator[](std::size_t i) const { return d().data()[i]; }

    [[gnu::flatten]]
    value_type& operator[](std::size_t i) { return d().data()[i]; }

    [[gnu::flatten]]
    Derived& operator&=(Derived const& rhs)
    {
        assert(d().size() == rhs.size());
        bitset_and(d().size(), d().data(), rhs.data());
        return d();
    }

    [[gnu::flatten]]
    Derived& operator|=(Derived const& rhs)
    {
        assert(d().size() == rhs.size());
        bitset_or(d().size(), d().data(), rhs.data());
        return d();
    }

    [[gnu::flatten]]
    Derived& operator^=(Derived const& rhs)
    {
        assert(d().size() == rhs.size());
        bitset_xor(d().size(), d().data(), rhs.data());
        return d();
    }

    [[gnu::flatten]]
    Derived& operator-=(Derived const& rhs)
    {
        assert(d().size() == rhs.size());
        bitset_difference(d().size(), d().data(), rhs.data());
        return d();
    }

    [[gnu::flatten]]
    Derived& operator<<=(value_type rhs)
    {
        bitset_lshift(d().size(), d().data(), rhs);
        return d();
    }

    [[gnu::flatten]]
    Derived& operator>>=(value_type rhs)
    {
        bitset_rshift(d().size(), d().data(), rhs);
        return d();
    }

    auto begin() const { return d().data(); }
    auto end() const { return d().data() + d().size(); }
    auto begin() { return d().data(); }
    auto end() { return d().data() + d().size(); }

    auto operator<=>(bitset_base_t const& o) const = default;
    auto operator<=>(Derived const& o) const
    {
        return std::lexicographical_compare_three_way(
            begin(), end(), o.begin(), o.end());
    }
        
private:
    Derived const& d() const { return *static_cast<Derived const*>(this); }
    Derived& d() { return *static_cast<Derived*>(this); }
};

// A shitty bitset class that exists because std::bitset abstracts too much.
// 'aggregate_bitset_t' is an aggregate class and can use
// any unsigned integer type.
template<typename UInt, std::size_t N>
struct alignas(128) aggregate_bitset_t : public bitset_base_t<aggregate_bitset_t<UInt, N>, UInt>
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

    constexpr auto operator<=>(aggregate_bitset_t const& o) const = default;

    static constexpr aggregate_bitset_t filled();
    static aggregate_bitset_t filled(std::size_t start, std::size_t size);
};

template<std::size_t Bits>
using static_bitset_t = aggregate_bitset_t<bitset_uint_t, (Bits + sizeof_bits<bitset_uint_t> - 1) / sizeof_bits<bitset_uint_t>>;

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
aggregate_bitset_t<UInt, N>::filled(std::size_t start, std::size_t size)
{
    aggregate_bitset_t bs = {};
    bitset_set_n(num_ints, bs.data(), start, size);
    return bs;
}

// Dynamically allocates its memory.
class bitset_t : public bitset_base_t<bitset_t>
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
    : m_ptr(num_ints ? new value_type[num_ints]() : nullptr)
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

    value_type const* data() const { return m_ptr.get(); }
    value_type* data() { return m_ptr.get(); }
    constexpr std::size_t size() const { return m_size; }

    explicit operator bool() const { return m_ptr.get(); }

    void reset(std::size_t num_ints)
    {
        if(m_size == num_ints)
        {
            clear_all();
            return;
        }
        m_ptr.reset(num_ints ? new value_type[num_ints]() : nullptr);
        m_size = num_ints;
    }

private:
    std::unique_ptr<value_type[]> m_ptr;
    std::size_t m_size = 0; // in ints, NOT bits
};

// Dynamically allocates its memory.
template<typename SizeBase>
class xbitset_t : public bitset_base_t<xbitset_t<SizeBase>>
{
public:
    using size_base = SizeBase;
    using value_type = bitset_uint_t;
    static constexpr std::size_t bits_per_int = sizeof(value_type) * CHAR_BIT;

    xbitset_t() = default;
    explicit xbitset_t(std::nullptr_t) { alloc(); }

    xbitset_t(xbitset_t const& o)
    {
        if(o.data())
            alloc();
        assert(size() == o.size());
        std::copy(o.data(), o.data() + o.size(), data());
    }

    xbitset_t(xbitset_t&& o) = default;

    xbitset_t& operator=(xbitset_t const& o)
    {
        if(o.data())
            alloc();
        assert(!!data() == !!o.data());
        std::copy(o.data(), o.data() + o.size(), data());
        return *this;
    }

    xbitset_t& operator=(xbitset_t&& o) = default;

    value_type const* data() const { assert(m_ptr.get()); return m_ptr.get(); }
    value_type* data() { assert(m_ptr.get()); return m_ptr.get(); }

    constexpr std::size_t size() const { return SizeBase::bitset_size(); }
    constexpr std::size_t num_bits() const { return size() * bits_per_int; }

    explicit operator bool() const { return m_ptr.get(); }

    void alloc() { if(!m_ptr.get()) m_ptr.reset(new value_type[size()]()); }
    void free() { m_ptr.reset(nullptr); }

    template<typename Fn>
    void for_each(Fn const& fn) const { ::bitset_for_each<SizeBase>(size(), data(), fn); }

    template<typename Fn>
    bool for_each_test(Fn const& fn) const { return ::bitset_for_each_test<SizeBase>(size(), data(), fn); }


private:
    std::unique_ptr<value_type[]> m_ptr;
};

#endif

