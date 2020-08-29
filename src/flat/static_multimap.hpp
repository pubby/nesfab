#ifndef LIB_FLAT_STATIC_MULTIMAP_HPP
#define LIB_FLAT_STATIC_MULTIMAP_HPP

#include "flat_multimap.hpp"
#include <boost/container/static_vector.hpp>

namespace fc
{

template<typename Key, typename Mapped, std::size_t N, typename... Args>
using static_multimap = flat_map<
    ::boost::container::static_vector<std::pair<Key, Mapped>, N>, Args...>;

}

#endif
