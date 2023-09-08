#ifndef IDENT_MAP_HPP
#define IDENT_MAP_HPP

#include <algorithm>
#include <cstdint>

#include "robin/collection.hpp"
#include "robin/table.hpp"

#include "fnv1a.hpp"
#include "handle.hpp"
#include "pstring.hpp"

// Maps identif
template<typename Handle>
class ident_map_t
{
public:
    using handle_type = Handle;
    using value_type = typename Handle::value_type;

    template<typename PString>
    value_type& lookup(PString name, std::string_view key)
    {
        std::uint64_t const hash = fnv1a<std::uint64_t>::hash(key.data(), key.size());

        return *Handle::with_pool([&, hash, key](auto& pool)
        {
            rh::apair<value_type**, bool> result = map.emplace(hash,
                [key](value_type* ptr) -> bool
                {
                    return std::equal(key.begin(), key.end(), ptr->name.begin(), ptr->name.end());
                },
                [&pool, name, key]() -> value_type*
                { 
                    return &pool.emplace_back(name, key, pool.size());
                });

            return *result.first;
        });
    }

    value_type* lookup(std::string_view view)
    {
        std::uint64_t const hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

        return Handle::with_const_pool([&, hash, view](auto const&)
        {
            auto result = map.lookup(hash,
                [view](value_type* ptr) -> bool
                {
                    return std::equal(view.begin(), view.end(), ptr->name.begin(), ptr->name.end());
                });

            return result.second ? *result.second : nullptr;
        });
    }
private:
    rh::robin_auto_table<value_type*> map;
};

#endif
