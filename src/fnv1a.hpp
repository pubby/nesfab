#ifndef FNV1A_HPP
#define FNV1A_HPP

#include <cassert>
#include <cstdint>
#include <string_view>

// A decently-fast, endian-agnostic, non-cryptographic hash function
// that is good for small strings. For big strings, check out smhasher.
// Hyperlinks:
//   http://create.stephan-brumme.com/fnv-hash/
//   http://isthe.com/chongo/tech/comp/fnv/

template<typename T>
struct fnv1a_constants {};

template<>
struct fnv1a_constants<std::uint32_t> 
{
    static constexpr std::uint32_t prime = 16777619ul;
    static constexpr std::uint32_t seed  = 2166136261ul;
};

template<>
struct fnv1a_constants<std::uint64_t> 
{
    static constexpr std::uint64_t prime = 1099511628211ull;
    static constexpr std::uint64_t seed  = 14695981039346656037ull;
};

template<typename T = std::uint64_t>
struct fnv1a
{
    static constexpr T prime = fnv1a_constants<T>::prime;
    static constexpr T seed  = fnv1a_constants<T>::seed;

    [[gnu::always_inline]]
    static constexpr inline T hash(unsigned char byte, T hashval = seed)
    {
        return (byte ^ hashval) * prime;
    }

    static constexpr T hash(char const* data, std::size_t size, T hashval = seed)
    {
        assert(data);
        while(size--)
            hashval = hash(*data++, hashval);
        return hashval;
    }

    static constexpr T hash(char const* begin, char const* end, T hashval = seed)
    {
        assert(begin && end);
        while(begin < end)
            hashval = hash(*begin++, hashval);
        return hashval;
    }

    static constexpr T hash(std::string_view view, T hashval = seed)
    {
        return hash(view.data(), view.size(), hashval);
    }
};

#endif
