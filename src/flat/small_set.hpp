#ifndef LIB_FLAT_SMALL_SET_HPP
#define LIB_FLAT_SMALL_SET_HPP

#include "flat_set.hpp"
#include <boost/container/small_vector.hpp>

namespace fc
{

template<typename T, std::size_t N, 
         typename Compare = std::less<T>, typename... Args>
using small_set = flat_set<
    ::boost::container::small_vector<T, N, Args...>, Compare>;

}

#endif
