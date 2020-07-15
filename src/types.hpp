#ifndef TYPES_HPP
#define TYPES_HPP

#include <climits>
#include <cstdint>
#include <ostream>
#include <string_view>
#include <vector>

#include "robin/collection.hpp"
#include "robin/set.hpp"

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

constexpr bool is_composite(type_name_t type_name)
    { return (type_name >= TYPE_FIRST_COMPOSITE 
              && type_name <= TYPE_LAST_COMPOSITE); }

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

struct type_t
{
    static constexpr unsigned max_frac_bytes = 3;
    static constexpr unsigned max_whole_bytes = 3;
    static constexpr unsigned max_total_bytes = 6;

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

constexpr unsigned begin_byte(type_name_t type_name)
{
    return type_t::max_frac_bytes - frac_bytes(type_name);
}

constexpr unsigned end_byte(type_name_t type_name)
{
    return type_t::max_frac_bytes + whole_bytes(type_name);
}

std::string type_string(type_t type);

std::ostream& operator<<(std::ostream& ostr, type_t const&);

enum cast_result_t : char
{
    CAST_FAIL,
    CAST_NOP,
    CAST_OP,
    CAST_BOOLIFY,
};

cast_result_t can_cast(type_t const& from, type_t const& to);

#endif
