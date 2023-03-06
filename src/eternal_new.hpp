#ifndef ETERNAL_NEW_HPP
#define ETERNAL_NEW_HPP

// Let's you allocate data that persists until program termination.
// (This could simply be 'return new T', but that triggers leak detectors.)

#include <mutex>

#include "array_pool.hpp"
#include "thread.hpp"

template<typename T, typename Tag = void>
class eternal_new_pool_t : public array_pool_t<T> 
{
public:
    using array_pool_t<T>::array_pool_t;

    ~eternal_new_pool_t() 
    { 
        std::lock_guard<std::mutex> lock(mutex);
        eternal.splice(*this);
    }

    static void free_parent()
    {
        std::lock_guard<std::mutex> lock(mutex);
        eternal.clear();
        eternal.shrink_to_fit();
    }

private:
    inline static std::mutex mutex;
    inline static array_pool_t<T> eternal;
};

// This has to be a function to get around a GCC bug
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81880
template<typename T, typename Tag = void>
inline eternal_new_pool_t<T>& eternal_new_pool()
{
    static TLS eternal_new_pool_t<T, Tag> pool;
    return pool;
}

template<typename T, typename Tag = void>
T* eternal_new(std::size_t size)
{
    return eternal_new_pool<T, Tag>().alloc(size);
}

template<typename T, typename Tag = void>
T* eternal_new(T const* begin, T const* end)
{
    return eternal_new_pool<T, Tag>().insert(begin, end);
}

template<typename T, typename Tag = void, typename... Args>
T* eternal_emplace(Args&&... args)
{
    return &eternal_new_pool<T, Tag>().emplace(std::forward<Args>(args)...);
}

#endif
