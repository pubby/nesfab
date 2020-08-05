#ifndef TYPES_HPP
#define TYPES_HPP

#include <climits>
#include <cstdint>
#include <ostream>
#include <mutex>
#include <string_view>
#include <vector>

#include "robin/collection.hpp"

#include "array_pool.hpp"

enum type_name_t : std::uint8_t // Keep unsigned.
{
    // Have void be the zeroth/default value.
    TYPE_VOID = 0,

    // Arithmetic types, which include fixed point numbers and bools.
    // The enum is laid out with a bit representation corresponding to
    // the number of bytes each type uses. The bit format is: FFWW,
    // where FF is two bits storing the size of the fractional part in bytes,
    // and WW is two bits storing the size of the whole part in bytes.
    // Pointers are slightly special and come last.
    TYPE_BYTE  = 1,
    TYPE_FIRST_ARITH = TYPE_BYTE,
    TYPE_SHORT = 2,
    TYPE_INT   = 3,
    TYPE_FIRST_FIXED = 4,
    TYPE_LARGEST_FIXED = 0b1111,
    TYPE_LAST_FIXED  = 0b1111,
    TYPE_PTR  = 0b10010, // Bottom 2 bits must equal 2.
    TYPE_FIRST_COMPOSITE = TYPE_PTR,
    TYPE_LAST_ARITH = TYPE_PTR,


    // A composite type is one that holds smaller types.
    // These types use the 'tail_i' field in 'type_t'.
    // e.g. fn types or pointer types.
    TYPE_TABLE,
    TYPE_ARRAY,
    TYPE_FN,
    TYPE_LAST_COMPOSITE = TYPE_FN,

    // Bools aren't considered arithmetic or composite.
    TYPE_BOOL,
};

class type_t
{
public:
    static constexpr unsigned max_frac_bytes = 3;
    static constexpr unsigned max_whole_bytes = 3;
    static constexpr unsigned max_total_bytes = 6;

    constexpr type_t() = default;
    constexpr type_t(type_name_t name) : m_name(name) {}

    constexpr type_name_t name() const { return m_name; }
    constexpr std::size_t size() const { return m_size; }
    constexpr type_t const* tail() const { return m_tail; }
    type_t operator[](unsigned i) const { return tail()[i]; }

    std::size_t num_params() const
        { assert(name() == TYPE_FN); return size() - 1; }

    type_t return_type() const
        { assert(name() == TYPE_FN); return tail()[size() - 1]; }

    bool operator==(type_t o) const
    {
        return (m_name == o.m_name
                && m_size == o.m_size
                && m_tail == o.m_tail);
    }
    bool operator!=(type_t o) const { return !operator==(o); }

    std::size_t size_of() const;

    // Type creation functions.
    static type_t array(type_t elem_type, unsigned size);
    static type_t ptr(type_t pointed_to_type);
    static type_t fn(type_t* begin, type_t* end);

    static void clear_all();

private:
    type_name_t m_name = TYPE_VOID;
    // Overloaded; Holds tail size for fns and array size for arrays.
    std::uint16_t m_size = 0;
    type_t const* m_tail = nullptr;

    type_t(type_name_t name, std::uint16_t size, type_t const* tail) 
    : m_name(name)
    , m_size(size)
    , m_tail(tail)
    {}

    static type_t const* get_tail(type_t const& type);
    static type_t const* get_tail(type_t const* begin, type_t const* end);

    struct map_elem_t
    {
        std::uint16_t size;
        type_t const* tail;
    };

    inline static std::mutex tail_mutex; // Protects the objects below:
    inline static rh::robin_auto_table<map_elem_t> tail_map;
    static array_pool_t<type_t> tails;
};

inline bool operator==(type_t lhs, type_name_t rhs)
    { return (lhs.name() == rhs && lhs.size() == 0); }
inline bool operator==(type_name_t lhs, type_t rhs)
    { return operator==(rhs, lhs); }

inline bool operator!=(type_t lhs, type_name_t rhs)
    { return !operator==(lhs, rhs); }
inline bool operator!=(type_name_t lhs, type_t rhs)
    { return !operator==(lhs, rhs); }

constexpr bool is_composite(type_name_t type_name)
    { return (type_name >= TYPE_FIRST_COMPOSITE 
              && type_name <= TYPE_LAST_COMPOSITE); }
constexpr bool is_composite(type_t type)
    { return type.size() == 0 && is_composite(type.name()); }

constexpr bool is_arithmetic(type_name_t type_name)
    { return type_name >= TYPE_FIRST_ARITH && type_name <= TYPE_LAST_ARITH; }
constexpr bool is_arithmetic(type_t type)
    { return type.size() == 0 && is_arithmetic(type.name()); }

constexpr bool is_fixed(type_name_t type_name)
    { return type_name >= TYPE_FIRST_FIXED && type_name <= TYPE_LAST_FIXED; }
constexpr bool is_fixed(type_t type)
    { return type.size() == 0 && is_fixed(type.name()); }

constexpr unsigned whole_bytes(type_name_t type_name)
{
    assert(is_arithmetic(type_name));
    return type_name & 0b11;
}

constexpr unsigned frac_bytes(type_name_t type_name)
{
    assert(is_arithmetic(type_name));
    return (type_name >> 2) & 0b11;
}

constexpr unsigned total_bytes(type_name_t type_name)
{
    return whole_bytes(type_name) + frac_bytes(type_name);
}

constexpr type_name_t TYPE_arithmetic(unsigned w, unsigned f)
{
    assert(w <= 3);
    assert(f <= 3);
    return type_name_t((f << 2) | w);
}

constexpr type_name_t promote_arithmetic(type_name_t a, type_name_t b)
{
    assert(is_arithmetic(a));
    assert(is_arithmetic(b));

    return TYPE_arithmetic(std::max(whole_bytes(a), whole_bytes(b)), 
                           std::max(frac_bytes(a), frac_bytes(b)));
}


constexpr unsigned begin_byte(type_name_t type_name)
{
    return type_t::max_frac_bytes - frac_bytes(type_name);
}

constexpr unsigned end_byte(type_name_t type_name)
{
    return type_t::max_frac_bytes + whole_bytes(type_name);
}

std::string to_string(type_t type);
std::ostream& operator<<(std::ostream& ostr, type_t const& type);

enum cast_result_t : char
{
    CAST_FAIL,
    CAST_NOP,
    CAST_OP,
    CAST_BOOLIFY,
};

cast_result_t can_cast(type_t const& from, type_t const& to);

#endif
