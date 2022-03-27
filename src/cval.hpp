#ifndef CVAL_HPP
#define CVAL_HPP

#include <memory>
#include <variant>

#include <boost/container/small_vector.hpp>

#include "type.hpp"
#include "ir_edge.hpp"

namespace bc = boost::container;

using ct_array_t = std::shared_ptr<ssa_value_t[]>;
using ct_variant_t = std::variant<ssa_value_t, ct_array_t>;

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
using sval_t = bc::small_vector<ct_variant_t, 8>;

// Creates a sval from a single ssa value
inline sval_t make_sval(ssa_value_t v) { return { v }; }

struct spair_t
{
    sval_t value;
    type_t type;
};

/* TODO
template<>
struct std::hash<spair_t>
{
    std::size_t operator()(spair_t const& s) const noexcept
    {
        std::size_t h = std::hash<type_t>{}(s.type);
        for(auto const& v : s.value)
            h = rh::hash_combine(h, v.target());
        return h;
    }
};
*/

// TODO: remove
//spair_t const* new_spair(cpair_t const& cpair);
//cpair_t const* new_cpair(cpair_t&& cpair);

#endif
