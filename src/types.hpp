#ifndef TYPES_HPP
#define TYPES_HPP

#include <climits>
#include <cstdint>
#include <ostream>
#include <mutex>
#include <string_view>
#include <vector>

#include "robin/collection.hpp"
#include "robin/hash.hpp"

#include "array_pool.hpp"
#include "decl.hpp"

#define FIXED_X \
    FIXED(0,0) FIXED(0,1) FIXED(0,2) FIXED(0,3)\
    FIXED(1,0) FIXED(1,1) FIXED(1,2) FIXED(1,3)\
    FIXED(2,0) FIXED(2,1) FIXED(2,2) FIXED(2,3)\
    FIXED(3,0) FIXED(3,1) FIXED(3,2) FIXED(3,3)\

// ARRAYS:
// - Maximum size 256 bytes
// - Can't be referenced using pointers
// - Single dimension only - no multidimensional arrays
// Syntax: TYPE NAME[SIZE]

// BUFFERS:
// - Maximum size 65536
// - Can be referenced using pointers
// - Single dimension only - no multidimensonal arrays
// Syntax: buffer NAME[SIZE]

// RAM PTR:
// Can only reference a buffer that exists in RAM.
// Syntax: ram{RAM_BLOCKS...}
// (or ram{} for global block ptrs)

// ROM PTR:
// Can only reference a buffer that exists in ROM.
// Syntax: rom{BANK}
// (or rom{} for global bank ptrs)

// FN PTR:
// Syntax: fn(types) rtype {}

// STRUCTS:
// No maximum size
// Can't hold arrays or buffers

enum type_name_t : std::uint8_t // Keep unsigned.
{
    // Have void be the zeroth/default value.
    TYPE_VOID = 0,

    TYPE_ARRAY,
    TYPE_BUFFER,
    TYPE_STRUCT,
    TYPE_PTR,
    TYPE_FIRST_NUM = TYPE_PTR,
    TYPE_FIRST_PTR = TYPE_PTR,
    TYPE_BANKED_PTR,
    TYPE_FN, // should be named FN_PTR, but whatever
    TYPE_LAST_PTR = TYPE_FN,

    // Bools aren't considered arithmetic or composite, but they are numeric.
    TYPE_BOOL,
    TYPE_FIRST_BOOLEAN = TYPE_BOOL,
    // Carry is a more specific version of bool:
    TYPE_CARRY,
    TYPE_LAST_BOOLEAN = TYPE_CARRY,
#define FIXED(whole, frac) TYPE_FIXED_##whole##frac,
    FIXED_X
#undef FIXED
    TYPE_BYTE = TYPE_FIXED_10,
    TYPE_SHORT = TYPE_FIXED_20,
    TYPE_INT = TYPE_FIXED_30,

    TYPE_FIRST_ARITH = TYPE_FIXED_01,
    TYPE_LAST_ARITH = TYPE_FIXED_33,
    TYPE_LAST_NUM   = TYPE_FIXED_33,
    TYPE_LARGEST_FIXED = TYPE_FIXED_33,
};

constexpr bool is_arithmetic(type_name_t type_name)
    { return type_name >= TYPE_FIRST_ARITH && type_name <= TYPE_LAST_ARITH; }
constexpr bool is_numeric(type_name_t type_name)
    { return type_name >= TYPE_FIRST_NUM && type_name <= TYPE_LAST_NUM; }
constexpr bool is_boolean(type_name_t type_name)
    { return (type_name >= TYPE_FIRST_BOOLEAN 
              && type_name <= TYPE_LAST_BOOLEAN); }
constexpr bool is_ptr(type_name_t type_name)
    { return (type_name >= TYPE_FIRST_PTR && type_name <= TYPE_LAST_PTR); }

constexpr bool has_type_tail(type_name_t name)
    { return name == TYPE_ARRAY || name == TYPE_FN; }
constexpr bool has_group_tail(type_name_t name)
    { return is_ptr(name); }
constexpr bool has_tail(type_name_t name)
    { return has_type_tail(name) || has_group_tail(name); }

struct group_ht;

// Implementation detail!
// This takes a range of types and returns a pointer to allocated memory
// that contains the same data.
// The point being, it's faster to pass a pointer around than the actual range.
template<typename T>
class tails_manager_t
{
    struct map_elem_t
    {
        std::uint16_t size;
        T const* tail;
    };

    std::mutex mutex; // Protects the objects below:
    rh::robin_auto_table<map_elem_t> map;
    array_pool_t<T> tails;

public:
    T const* get(T const* begin, T const* end)
    {
        if(end - begin == 0)
            return nullptr;

        // Hash the range.

        std::size_t size = end - begin;
        std::size_t hash = size;

        for(T const* it = begin; it < end; ++it)
        {
            std::hash<T> hasher;
            hash = rh::hash_combine(hash, hasher(*it));
        }

        // Now insert into the map:

        std::lock_guard<std::mutex> const lock(mutex);

        rh::apair<map_elem_t*, bool> result = map.emplace(
            hash,
            [begin, end, size](map_elem_t elem) -> bool
            {
                return (elem.size == size && std::equal(begin, end, elem.tail));
            },
            [this, begin, end, size]() -> map_elem_t
            { 
                return { size, tails.insert(begin, end) };
            });

        assert(std::equal(begin, end, result.first->tail));

        return result.first->tail;
    }

    T const* get(T const& t) { return get(&t, &t+1); }

    void clear()
    {
        std::lock_guard<std::mutex> const lock(mutex);
        map.clear();
        tails.clear();
    }
};


class type_t
{
friend type_t arg_struct(type_t fn_type);
public:
    static constexpr unsigned max_frac_bytes = 3;
    // Arithmetic types, which include fixed point numbers and bools.
    // The enum is laid out with a bit representation corresponding to
    // the number of bytes each type uses. The bit format is: FFWW,
    // where FF is two bits storing the size of the fractional part in bytes,
    // and WW is two bits storing the size of the whole part in bytes.
    // Pointers are slightly special and come last.
    static constexpr unsigned max_whole_bytes = 3;
    static constexpr unsigned max_total_bytes = 6;

    constexpr type_t() = default;
    constexpr type_t(type_name_t name) : m_name(name) {}

    constexpr type_name_t name() const { return m_name; }
    constexpr std::size_t size() const { return m_size; }

    type_t const* types() const 
        { assert(has_type_tail(name())); return static_cast<type_t const*>(m_tail); }
    group_ht const* groups() const 
        { assert(has_group_tail(name())); return static_cast<group_ht const*>(m_tail); }

    type_t type(unsigned i) const { return types()[i]; }
    type_t elem_type() const { return type(0); }
    group_ht group(unsigned i) const;

    /* TODO: remove
    type_t const* begin() const { return types(); }
    type_t const* end() const { return types() + size(); }
    */

    std::size_t num_params() const { assert(name() == TYPE_FN); return size() - 1; }
    type_t return_type() const { assert(name() == TYPE_FN); return types()[size() - 1]; }

    bool operator==(type_t o) const;
    bool operator!=(type_t o) const { return !operator==(o); }

    std::size_t size_of() const;

    std::size_t hash() const;

    // Type creation functions.
    static type_t buffer(unsigned size);
    static type_t array(type_t elem_type, unsigned size);
    static type_t ptr(group_ht const* begin, group_ht const* end, bool banked);
    static type_t fn(type_t* begin, type_t* end);

private:
    type_name_t m_name = TYPE_VOID;

    // Overloaded; 
    // - Holds tail size for fns and ptrs
    // - Array size for arrays
    std::uint16_t m_size = 0;

    // Holds types or groups, depending on 'm_name'.
    void const* m_tail = nullptr;

    type_t(type_name_t name, std::uint16_t size, void const* tail = nullptr)
    : m_name(name), m_size(size), m_tail(tail) {}

    static tails_manager_t<type_t> type_tails;
    static tails_manager_t<group_ht> group_tails;
};


namespace std
{
    template<>
    struct hash<type_t>
    {
        std::size_t operator()(type_t const& type) const
        {
            return type.hash();
        }
    };
}

/* TODO: remove
inline bool operator==(type_t lhs, type_name_t rhs)
    { return (lhs.name() == rhs && lhs.size() == 0); }
inline bool operator==(type_name_t lhs, type_t rhs)
    { return operator==(rhs, lhs); }

inline bool operator!=(type_t lhs, type_name_t rhs)
    { return !operator==(lhs, rhs); }
inline bool operator!=(type_name_t lhs, type_t rhs)
    { return !operator==(lhs, rhs); }
    */

constexpr bool is_arithmetic(type_t type)
    { return type.size() == 0 && is_arithmetic(type.name()); }

constexpr bool is_numeric(type_t type)
    { return type.size() == 0 && is_numeric(type.name()); }

constexpr bool is_boolean(type_t type)
    { return type.size() == 0 && is_boolean(type.name()); }

constexpr bool is_ptr(type_t type)
    { return is_ptr(type.name()); }

constexpr unsigned whole_bytes(type_name_t type_name)
{
    switch(type_name)
    {
    default: return 0;
    case TYPE_CARRY: return 1;
    case TYPE_BOOL:  return 1;
    case TYPE_PTR:   return 2;
    case TYPE_BANKED_PTR:  return 3;
#define FIXED(whole, frac) case TYPE_FIXED_##whole##frac: return whole;
    FIXED_X
#undef FIXED
    }
}

constexpr unsigned frac_bytes(type_name_t type_name)
{
    switch(type_name)
    {
    default: return 0;
#define FIXED(whole, frac) case TYPE_FIXED_##whole##frac: return frac;
    FIXED_X
#undef FIXED
    }
}

constexpr unsigned total_bytes(type_name_t type_name)
{
    return whole_bytes(type_name) + frac_bytes(type_name);
}

constexpr type_name_t TYPE_arithmetic(unsigned w, unsigned f)
{
    assert(w <= 3);
    assert(f <= 3);
    return type_name_t(TYPE_FIXED_00 + w*4 + f);
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

constexpr bool valid_array_member(type_t type)
{
    return type.name() == TYPE_STRUCT || is_numeric(type);
}

/* TODO
constexpr bool valid_struct_member(type_t type)
{
    return type.name() == TYPE_STRUCT || is_numeric(type);
}
*/

type_name_t smallest_representable(struct fixed_t fixed);

unsigned num_fields(type_t type);

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

/* TODO
type_t arg_struct(type_t fn_type);
std::size_t struct_size(type_t type);
type_t struct_index(type_t type, unsigned i);
void struct_fill(type_t type, type_t* vec);
*/



#endif
