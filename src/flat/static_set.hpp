#ifndef LIB_FLAT_STATIC_SET_HPP
#define LIB_FLAT_STATIC_SET_HPP

#include "flat_set.hpp"
#include <boost/container/static_vector.hpp>

namespace fc
{

template<typename T, std::size_t N, typename... Args>
using static_set = flat_set<
    ::boost::container::static_vector<T, N>, Args...>;

}

#endif
