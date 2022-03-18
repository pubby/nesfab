#ifndef CVAL_HPP
#define CVAL_HPP

#include <boost/container/small_vector.hpp>

#include "type.hpp"
#include "ir_edge.hpp"

namespace bc = boost::container;

// compile-time-value
struct cval_t : public bc::small_vector<bc::small_vector<ssa_value_t, 1>, 1>;

struct cpair_t
{
    cval_t value;
    type_t type;
};

template<>
struct std::hash<cpair_t>
{
    std::size_t operator()(S const& s) const noexcept
    {
        std::size_t h = std::hash<type_t>{}(s.type);
        for(auto const& vec : s.value)
            for(ssa_value_t const& v : vec)
                h = rh::hash_combine(h, v.target());
        return h;
    }
};

cpair_t const* new_cpair(cpair_t const& cpair);
cpair_t const* new_cpair(cpair_t&& cpair);

#endif
