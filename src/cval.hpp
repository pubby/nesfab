#ifndef CVAL_HPP
#define CVAL_HPP

#include <boost/container/small_vector.hpp>

#include "type.hpp"
#include "ir_edge.hpp"

namespace bc = boost::container;

// compile-time-value
using cval_t = bc::small_vector<bc::small_vector<ssa_value_t, 1>, 1>;

struct cpair_t
{
    cval_t value;
    type_t type;
};

#endif
