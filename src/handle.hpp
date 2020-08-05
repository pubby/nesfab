#ifndef HANDLE_HPP
#define HANDLE_HPP

// handle_t is a 'strong typedef' for integer types.
// It supports the same operations that pointers support.

// EXAMPLE
//   Instead of doing:
//     using my_index_type = int;
//   Do:
//     using my_index_type = handle_t<int, struct some_unique_tag>;

#include <functional>
#include <ostream>

template<typename Int, typename Tag, Int Null = 0>
struct handle_t
{
    using int_type = Int;
    using tag_type = Tag;
    int_type value = Null;

    static constexpr Int null = Null;

    constexpr explicit operator bool() const { return value != Null; }
    constexpr bool operator==(handle_t o) const { return value == o.value; }
    constexpr bool operator!=(handle_t o) const { return value != o.value; }
    constexpr bool operator<=(handle_t o) const { return value <= o.value; }
    constexpr bool operator>=(handle_t o) const { return value >= o.value; }
    constexpr bool operator<(handle_t o) const { return value < o.value; }
    constexpr bool operator>(handle_t o) const { return value > o.value; }

    constexpr bool operator!() const { return !value; }

    handle_t& operator++() { ++value; return *this; }
    handle_t operator++(int) { handle_t h = *this; ++value; return h; }
    handle_t& operator--() { --value; return *this; }
    handle_t operator--(int) { handle_t h = *this; --value; return h; }

    handle_t& operator+=(int_type b) { value += b; return *this; }
    handle_t& operator-=(int_type b) { value -= b; return *this; }

    friend constexpr handle_t operator+(handle_t a, int_type b) 
        { return { a.value + b }; }
    friend constexpr handle_t operator+(int_type a, handle_t b) 
        { return { a + b.value }; }

    friend constexpr handle_t operator-(handle_t a, int_type b) 
        { return { a.value - b }; }
    friend constexpr handle_t operator-(int_type a, handle_t b)
        { return { a - b.value }; }
    friend constexpr int_type operator-(handle_t a, handle_t b) 
        { return { a.value - b.value }; }
};

template<typename Int, typename Tag>
std::ostream& operator<<(std::ostream& os, handle_t<Int, Tag> handle)
{
    os << "{" << handle.value << "}";
    return os;
}

namespace std
{
    template<typename Int, typename Tag>
    struct hash<handle_t<Int, Tag>>
    {
        using argument_type = handle_t<Int, Tag>;
        using result_type = std::size_t;
        result_type operator()(argument_type const& handle_t) const noexcept
        {
            std::hash<typename argument_type::int_type> hasher;
            return hasher(handle_t.value);
        }
    };
}

#endif
