#ifndef LIB_FLAT_SMALL_MAP_HPP
#define LIB_FLAT_SMALL_MAP_HPP

#include "flat_map.hpp"
#include <boost/container/small_vector.hpp>

namespace fc
{

template<typename Key, typename Mapped, 
         std::size_t N, typename Compare = std::less<void>>
using small_map = flat_map<
    ::boost::container::small_vector<std::pair<Key, Mapped>, N>, Compare>;

}

#endif
