#ifndef BITSET_HPP
#define BITSET_HPP

#include <array>
#include <climits>
#include <cstdint>
#include <type_traits>

#define BITSOF(T) (sizeof(T) * CHAR_BIT)

// A shitty bitset class that exists because std::bitset abstracts too much.
// 'aggregate_bitset_t' is an aggregate class and can use
// any unsigned integer type.

template<typename UInt, std::size_t N>
struct aggregate_bitset_t
{
    static_assert(std::is_unsigned<UInt>::value, "must be unsigned");

    using value_type = UInt;
    using array_type = std::array<UInt, N>;

    static constexpr std::size_t bits_per_int = sizeof(UInt) * CHAR_BIT;
    static constexpr std::size_t num_ints = N;
    static constexpr std::size_t num_bits = N * bits_per_int;

    array_type array;

    void reset(int bit)
    {
        if(N == 1)
            array[0] &= ~((UInt)1 << bit);
        else
            array[bit / bits_per_int] &= ~((UInt)1 << (bit % bits_per_int));
    }

    void set(int bit)
    {
        if(N == 1)
            array[0] |= (UInt)1 << bit;
        else
            array[bit / bits_per_int] |= (UInt)1 << (bit % bits_per_int);
    }

    void set(int bit, bool b)
    {
        reset(bit);
        if(b)
            set(bit);
    }

    void flip(int bit)
    {
        if(N == 1)
            array[0] ^= (UInt)1 << bit;
        else
            array[bit / bits_per_int] ^= (UInt)1 << (bit % bits_per_int);
    }

    bool test(int bit) const
    {
        return array[bit / bits_per_int] & 1 << (bit % bits_per_int);
    }

    static constexpr aggregate_bitset_t make_all_false()
    {
        return {{{ 0 }}};
    }

    static constexpr aggregate_bitset_t make_all_true()
    {
        aggregate_bitset_t bitset = make_all_false();
        for(std::size_t i = 0; i < N; ++i)
            bitset.array[i] = ~bitset.array[i];
        return bitset;
    }

    void flip_every_bit()
    {
        for(std::size_t i = 0; i < N; ++i)
            array[i] = ~array[i];
    }

    void reset_every_bit()
    {
        *this = make_all_false();
    }

    void set_every_bit()
    {
        *this = make_all_false();
    }

    void set_all(bool b)
    {
        if(b)
            set_every_bit();
        else
            reset_every_bit();
    }
};

template<typename UInt, std::size_t N>
bool every_bit_set(aggregate_bitset_t<UInt, N> const& bitset)
{
    for(std::size_t i = 0; i < N; ++i)
    {
        if(bitset.array[i] != ~static_cast<UInt>(0))
            return false;
    }
    return true;
}

template<typename UInt, std::size_t N>
bool no_bits_set(aggregate_bitset_t<UInt, N> const& bitset)
{
    for(std::size_t i = 0; i < N; ++i)
    {
        if(bitset.array[i] != 0)
            return false;
    }
    return true;
}

template<typename UInt, std::size_t N>
bool any_bit_set(aggregate_bitset_t<UInt, N> const& bitset)
{
    return !no_bits_set(bitset);
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N>& operator&=(aggregate_bitset_t<UInt, N>& lhs,
                                        aggregate_bitset_t<UInt, N> const& rhs)
{
    for(std::size_t i = 0; i < N; ++i)
        lhs.array[i] &= rhs.array[i];
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N>& operator|=(aggregate_bitset_t<UInt, N>& lhs,
                                        aggregate_bitset_t<UInt, N> const& rhs)
{
    for(std::size_t i = 0; i < N; ++i)
        lhs.array[i] |= rhs.array[i];
    return lhs;
}

template<typename UInt, std::size_t N>
aggregate_bitset_t<UInt, N>& operator^=(aggregate_bitset_t<UInt, N>& lhs,
                                      aggregate_bitset_t<UInt, N> const& rhs)
{
    for(std::size_t i = 0; i < N; ++i)
        lhs.array[i] |= rhs.array[i];
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
    lhs.flip_every_bit();
    return lhs;
}

template<typename Func, typename UInt, std::size_t N>
void iterate_set_bit_positions(aggregate_bitset_t<UInt, N> const& bitset,
                               Func func)
{
    constexpr std::size_t bpi = aggregate_bitset_t<UInt, N>::bits_per_int;
    for(std::size_t i = 0; i != N; ++i)
    {
        UInt x = bitset.array[i];
        for(std::size_t j = 0; j != bpi; ++j)
        {
            if(x & 1)
                func(i * bpi + j);
            x >>= 1;
        }
    }
}

#endif

