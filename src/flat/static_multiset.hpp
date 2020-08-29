#ifndef LIB_FLAT_STATIC_MULTISET_HPP
#define LIB_FLAT_STATIC_MULTISET_HPP

#include "flat_multiset.hpp"
#include <boost/container/static_vector.hpp>

namespace fc
{

template<typename T, std::size_t N, typename... Args>
using static_multiset = flat_multiset<
    ::boost::container::static_vector<T, N>, Args...>;

}

#endif
