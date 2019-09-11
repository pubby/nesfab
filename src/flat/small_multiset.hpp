#ifndef LIB_FLAT_SMALL_MULTISET_HPP
#define LIB_FLAT_SMALL_MULTISET_HPP

#include "flat_multiset.hpp"
#include <boost/container/small_vector.hpp>

namespace fc
{

template<typename T, std::size_t N, typename Compare = std::less<void>>
using small_multiset = flat_multiset<
    ::boost::container::small_vector<T, N>, Compare>;

}

#endif
