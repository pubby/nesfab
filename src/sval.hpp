#ifndef SVAL_HPP
#define SVAL_HPP

#include <functional>
#include <memory>
#include <variant>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "robin/map.hpp"

#include "decl.hpp"
#include "type.hpp"
#include "ir_edge.hpp"
#include "parser_decl.hpp"

namespace bc = boost::container;

using ct_array_t = std::shared_ptr<ssa_value_t[]>;
using ct_variant_t = std::variant<ssa_value_t, ct_array_t, expr_vec_t /* (for LT) */>;

inline ct_array_t make_ct_array(unsigned size)
{
    // TODO: Change this to 'make_shared' when std library updates to c++20.
    return ct_array_t(new ssa_value_t[size]());
}

inline ssa_value_t const* ct_array(ct_variant_t const& variant)
{
    if(ct_array_t const* array = std::get_if<ct_array_t>(&variant))
        return array->get();
    return nullptr;
}

// Struct value
using sval_t = bc::small_vector<ct_variant_t, 1>;

// Creates a sval from a single ssa value
inline sval_t make_sval(ssa_value_t v) { return { v }; }

bool is_ct(sval_t const& sval);
bool is_lt(sval_t const& sval);

struct spair_t
{
    sval_t value;
    type_t type;
};

struct ct_pair_t
{
    ct_array_t array;
    type_t type;

    bool operator==(ct_pair_t const& o) const { return array == o.array && type == o.type; }
    bool operator!=(ct_pair_t const& o) const { return !operator==(o); }
};

template<>
struct std::hash<ct_pair_t>
{
    std::size_t operator()(ct_pair_t const& pair) const noexcept
    {
        std::size_t h = std::hash<ct_array_t>{}(pair.array);
        return rh::hash_combine(h, pair.type.hash());
    }
};

// Appends 'sval' onto 'vec'
void append_locator_bytes(std::vector<locator_t>& vec, sval_t const& sval, type_t type, pstring_t pstring);

#endif
