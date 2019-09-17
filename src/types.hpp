#ifndef TYPES_HPP
#define TYPES_HPP

#include <climits>
#include <cstdint>
#include <ostream>
#include <string_view>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "robin/collection.hpp"
#include "robin/set.hpp"

namespace bc = boost::container;

enum type_name_t : std::uint8_t // Keep unsigned.
{
    // Have void be the zeroth/default value.
    TYPE_VOID = 0,

    // Arithmetic types, which include fixed point numbers and bools.
    // The enum is laid out with a bit representation corresponding to
    // the number of bytes each type uses. The bit format is: FFWW,
    // where FF is two bits storing the size of the fractional part in bytes,
    // and WW is two bits storing the size of the whole part in bytes.
    // Bool are slightly special and come last.
    TYPE_BYTE  = 1,
    TYPE_FIRST_ARITH = TYPE_BYTE,
    TYPE_SHORT = 2,
    TYPE_INT   = 3,
    TYPE_FIRST_FIXED = 4,
    TYPE_LAST_FIXED  = 0b1111,
    TYPE_BOOL = 0b10001, // 0th bit must be a 1.
    TYPE_LAST_ARITH = TYPE_BOOL,

    // A composite type is one that holds smaller types.
    // These types use the 'tail_i' field in 'type_t'.
    // e.g. fn types or pointer types.
    TYPE_FIRST_COMPOSITE,
    TYPE_TABLE = TYPE_FIRST_COMPOSITE,
    TYPE_ARRAY,
    TYPE_PTR,
    TYPE_FN,
};

constexpr bool is_composite(type_name_t type_name)
    { return type_name >= TYPE_FIRST_COMPOSITE; }

constexpr bool is_arithmetic(type_name_t type_name)
    { return type_name >= TYPE_FIRST_ARITH && type_name <= TYPE_LAST_ARITH; }

constexpr bool is_fixed(type_name_t type_name)
    { return type_name >= TYPE_FIRST_FIXED && type_name <= TYPE_LAST_FIXED; }

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

    if(a == TYPE_BOOL)
        return b;
    if(b == TYPE_BOOL)
        return a;

    return TYPE_arithmetic(std::max(whole_bytes(a), whole_bytes(b)), 
                           std::max(frac_bytes(a), frac_bytes(b)));
}

struct type_t
{
    type_name_t name;
    // Overloaded; Holds tail size for fns and array size for arrays.
    std::uint16_t size;
    std::uint32_t tail_i;

    type_t operator[](unsigned i) const { return tails[tail_i + i]; }
    type_t const* tail() const { return &tails[tail_i]; }

    std::size_t num_params() const
    {
        assert(name == TYPE_FN);
        return size - 1;
    }

    type_t return_type() const
    {
        assert(name == TYPE_FN);
        return tail()[size - 1];
    }

    bool operator==(type_t o) const
        { return name == o.name && size == o.size && tail_i == o.tail_i; }
    bool operator!=(type_t o) const
        { return !operator==(o); }

    std::size_t sizeof_() const
    {
        if(is_arithmetic(name))
            return whole_bytes(name) + frac_bytes(name);

        switch(name)
        {
        default: assert(false); return 0;
        case TYPE_PTR:   return 2;
        case TYPE_ARRAY: return size * tail()[0].sizeof_();
        }
    }

    // Type creation functions.
    static type_t array(type_t elem_type, unsigned size);
    static type_t ptr(type_t pointed_to_type);
    static type_t fn(type_t* begin, type_t* end);

    static void clear_all();

private:
    struct map_elem_t
    {
        std::uint16_t size;
        std::uint32_t tail_i;
    };

    static rh::robin_auto_table<map_elem_t> tail_map;
    static std::vector<type_t> tails;
    static unsigned get_tail_i(type_t const* begin, type_t const* end);
};


std::string type_string(type_t type);

std::ostream& operator<<(std::ostream& ostr, type_t const&);

enum cast_result_t : char
{
    CAST_FAIL,
    CAST_NOP,
    CAST_OP,
};

cast_result_t can_cast(type_t const& from, type_t const& to);

#endif
