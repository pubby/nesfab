#ifndef TYPE_HPP
#define TYPE_HPP

#include <climits>
#include <cstdint>
#include <ostream>
#include <mutex>
#include <string_view>
#include <vector>

#include "decl.hpp"
#include "type_name.hpp"
#include "pstring.hpp"

struct token_t;
struct tea_thunk_t;
struct paa_thunk_t;
struct src_type_t;
class eval_t;

class type_t
{
friend type_t arg_struct(type_t fn_type);
public:
    constexpr type_t() = default;
    constexpr type_t(type_name_t name) : m_name(name) {}

    constexpr type_name_t name() const { return m_name; }
    constexpr std::size_t size() const { return m_size; }

    std::size_t type_tail_size() const
    {
        if(!has_type_tail(name()))
            return 0;
        if(name() == TYPE_TEA)
            return 1;
        return size();
    }

    std::size_t group_tail_size() const
    {
        if(!has_group_tail(name()))
            return 0;
        return size();
    }

    void unsafe_set_name(type_name_t name) { m_name = name; }

    type_t const* types() const 
        { assert(has_type_tail(name())); return static_cast<type_t const*>(m_tail); }
    group_ht const* groups() const 
        { assert(has_group_tail(name())); return static_cast<group_ht const*>(m_tail); }

    type_t type(unsigned i) const { assert(has_type_tail(name())); assert(i < type_tail_size()); assert(types()); return types()[i]; }
    type_t elem_type() const;
    group_ht group(unsigned i = 0) const;

    global_t const& global() const { assert(name() == TYPE_STRUCT_THUNK); return *static_cast<global_t const*>(m_tail); }
    struct_t const& struct_() const { assert(name() == TYPE_STRUCT); return *static_cast<struct_t const*>(m_tail); }
    tea_thunk_t const& tea_thunk() const
        { assert(name() == TYPE_TEA_THUNK || name() == TYPE_PAA_THUNK); return *static_cast<tea_thunk_t const*>(m_tail); }
    paa_thunk_t const& paa_thunk() const
        { assert(name() == TYPE_PAA_THUNK || name() == TYPE_PAA_THUNK); return *static_cast<paa_thunk_t const*>(m_tail); }

    std::size_t num_params() const { assert(name() == TYPE_FN); return size() - 1; }
    type_t return_type() const { assert(name() == TYPE_FN); return types()[size() - 1]; }

    bool operator==(type_t o) const;
    bool operator!=(type_t o) const { return !operator==(o); }

    std::size_t size_of() const;
    std::size_t array_length() const;
    void set_array_length(std::size_t size);
    void set_array_length(std::int64_t size, pstring_t pstring);
    bool is_unsized_array() const { return is_array(name()) && !is_thunk(name()) && m_size == 0; }

    std::size_t hash() const;

    // Type creation functions.
    static type_t paa(unsigned size, group_ht group);
    static type_t paa(std::int64_t size, group_ht group, pstring_t);
    static type_t paa_thunk(pstring_t pstring, token_t const* tokens, group_ht group);
    static type_t tea(type_t elem_type, unsigned size);
    static type_t tea(type_t elem_type, std::int64_t size, pstring_t);
    static type_t tea_thunk(pstring_t pstring, type_t elem_type, token_t const* tokens);
    static type_t ptr(group_ht group, bool banked);
    static type_t ptr(group_ht const* begin, group_ht const* end, bool banked);
    static type_t fn(type_t* begin, type_t* end);
    static type_t struct_thunk(global_t const& global);
    static type_t struct_(struct_t const& s);

    type_t set_banked(bool banked) const;

    // Allocates 'type' in storage, until program termination.
    static type_t const* new_type(type_t const& type);

private:
    type_name_t m_name = TYPE_VOID;
    //std::uint8_t m_unused; // TODO

    // Overloaded; 
    // - Holds tail size for fns and ptrs
    // - Array size for arrays
    std::uint16_t m_size = 0;

    // Holds types, groups, globals, or thunks, depending on 'm_name'.
    void const* m_tail = nullptr;

    type_t(type_name_t name, std::uint16_t size, void const* tail = nullptr)
    : m_name(name), m_size(size), m_tail(tail) {}
};

namespace std
{
    template<>
    struct hash<type_t>
    {
        std::size_t operator()(type_t const& type) const noexcept { return type.hash(); }
    };
}

struct tea_thunk_t
{
    pstring_t pstring;
    type_t elem_type;
    token_t const* expr;
};

struct paa_thunk_t
{
    pstring_t pstring;
    token_t const* expr;
    group_ht group;
};

// Pairs a pstring with a type.
struct src_type_t
{
    type_t type;
    pstring_t pstring;
};

inline type_t type_t::elem_type() const
{ 
    assert(is_tea(name()));
    if(name() == TYPE_TEA)
        return type(0); 
    assert(name() == TYPE_TEA_THUNK);
    return tea_thunk().elem_type;
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

// TODO: remove these
constexpr bool is_arithmetic(type_t type)
    { return type.size() == 0 && is_arithmetic(type.name()); }

constexpr bool is_numeric(type_t type)
    { return type.size() == 0 && is_numeric(type.name()); }

constexpr bool is_ptr(type_t type)
    { return is_ptr(type.name()); }

inline type_name_t promote_arithmetic(type_name_t a, type_name_t b)
{
    assert(false);
    return TYPE_VOID; // TODO
    //assert(is_arithmetic(a));
    //assert(is_arithmetic(b));

    //return TYPE_arithmetic(std::max(whole_bytes(a), whole_bytes(b)), 
                           //std::max(frac_bytes(a), frac_bytes(b)));
}
constexpr bool valid_array_member(type_t type)
{
    return type.name() == TYPE_STRUCT || is_numeric(type);
}

    */

/* TODO
constexpr bool valid_struct_member(type_t type)
{
    return type.name() == TYPE_STRUCT || is_numeric(type);
}
*/

//type_name_t smallest_representable(struct fixed_t fixed);

std::string to_string(type_t type);
std::ostream& operator<<(std::ostream& ostr, type_t const& type);

bool is_ct(type_t type);

// unsigned calc_num_members(type_t type); // TODO: remove?
unsigned num_members(type_t type);
unsigned num_atoms(type_t type);
unsigned num_offsets(type_t type, unsigned atom);

unsigned member_index(type_t const& type, unsigned i);
type_t member_type(type_t const& type, unsigned i);
type_t strip_array(type_t const& type);
bool has_tea(type_t const& type);

enum cast_result_t : char
{
    CAST_FAIL,
    CAST_NOP,
    CAST_TRUNCATE,
    CAST_PROMOTE,
    CAST_BOOLIFY,
    CAST_ROUND_REAL,
    CAST_CONVERT_INT,
    CAST_INTIFY_PTR,
};

bool can_size_unsized_array(type_t const& sized, type_t const& unsized);
cast_result_t can_cast(type_t const& from, type_t const& to, bool implicit);

// Converts THUNKs to regular types.
// If 'full' is true, the type will be fully stripped of thunks.
// Otherwise, only thunks necessary for counting members will be changed.
type_t dethunkify(src_type_t src_type, bool full, eval_t* env = nullptr);

/* TODO
type_t arg_struct(type_t fn_type);
std::size_t struct_size(type_t type);
type_t struct_index(type_t type, unsigned i);
void struct_fill(type_t type, type_t* vec);
*/

#endif
