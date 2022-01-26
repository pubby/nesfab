#ifndef LIB_FLAT_SMALL_MULTIMAP_HPP
#define LIB_FLAT_SMALL_MULTIMAP_HPP

#include "flat_multimap.hpp"
#include <boost/container/small_vector.hpp>

namespace fc
{

template<typename Key, typename Mapped, std::size_t N, 
         typename Compare = std::less<Key>, typename... Args>
using small_multimap = flat_multimap<
    ::boost::container::small_vector<std::pair<Key, Mapped>, N, Args...>, 
    Compare>;

}

#endif
