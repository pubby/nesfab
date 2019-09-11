#ifndef TYPES_HPP
#define TYPES_HPP

#include <algorithm>
#include <climits>
#include <cstdint>
#include <ostream>
#include <string_view>
#include <variant>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "robin/collection.hpp"
#include "robin/set.hpp"

#include "handle.hpp"

namespace bc = boost::container;

// Most types are very simple: just a name and a pointer value.
// However, fn types are tuples which complicates things.
// To implement fn types, a separate storage object is used: 'fn_type_map_t'.
// This object tracks the parameters and return values of fn types.
// An index into this map is then stored in 'type_t' as a negative 'type_name'.

enum type_name_t : short
{
    TYPE_VOID = 0,

    TYPE_FIRST_COMPOSITE,
    TYPE_TABLE = TYPE_FIRST_COMPOSITE,
    TYPE_ARRAY,
    TYPE_PTR,
    TYPE_FN,

    TYPE_LAST_COMPOSITE = TYPE_FN,

    // Integral types.
    // Keep in order of smallest to largest.
    TYPE_FIRST_INTEGER,
    TYPE_BOOL = TYPE_FIRST_INTEGER,
    TYPE_BYTE,
    TYPE_SHORT,
};

// A composite type is one that holds smaller types.
// These types use the 'tail_i' field in 'type_t'.
// e.g. fn types or pointer types.
constexpr bool is_composite(type_name_t type_name)
{
    return (type_name >= TYPE_FIRST_COMPOSITE 
            && type_name <= TYPE_LAST_COMPOSITE);
}

constexpr bool is_integer(type_name_t type_name)
{
    return type_name >= TYPE_FIRST_INTEGER;
}

struct type_t
{
    type_name_t name;
    unsigned short size;
    unsigned tail_i;

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
        switch(name)
        {
        default: assert(false); return 0;
        case TYPE_BOOL:  return 1;
        case TYPE_BYTE:  return 1;
        case TYPE_SHORT: return 2;
        case TYPE_PTR:   return 2;
        case TYPE_ARRAY: return size * tail()[0].sizeof_();
        }
    }

    // Type creation functions.
    static type_t array(type_t elem_type, unsigned size);
    static type_t ptr(type_t pointed_to_type);
    static type_t fn(type_t* begin, type_t* end);

private:
    struct map_elem_t
    {
        unsigned short size;
        unsigned tail_i;
    };

    static rh::robin_auto_table<map_elem_t> tail_map;
    static std::vector<type_t> tails;
    static unsigned get_tail_i(type_t const* begin, type_t const* end);
};


char const* type_name_string(type_name_t type_name);
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
