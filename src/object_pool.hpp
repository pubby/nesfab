#ifndef OBJECT_POOL_HPP
#define OBJECT_POOL_HPP

#include <memory>
#include <boost/container/deque.hpp>
#include <vector>

// A basic memory pool that supports alloc and free.
// NOTE: Objects are not destructed when 'free' is called!
template<typename T, typename Int = std::uint32_t>
class object_pool_t
{
public:
    object_pool_t() = default;
    object_pool_t(object_pool_t const&) = delete;
    object_pool_t& operator=(object_pool_t const&) = delete;

    using int_type = Int;
private:
    boost::container::deque<T> storage;
    std::vector<T*> free_list;
public:
    T& alloc() 
    {
        if(free_list.empty())
            return storage.emplace_back();
        else
        {
            T& ret = *free_list.back();
            free_list.pop_back();
            return ret;
        }
    }

    void free(T& t)
    {
        free_list.push_back(&t);
    }

    void clear() noexcept
    {
        storage.clear();
        free_list.clear();
    }
};

#endif
