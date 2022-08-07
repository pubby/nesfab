#ifndef ETERNAL_NEW_HPP
#define ETERNAL_NEW_HPP

#include <mutex>

// Let's you allocate data that persists until program termination.
// (This could simply be 'return new T', but that triggers leak detectors.)

#include "array_pool.hpp"

template<typename T>
class eternal_new_pool_t : public array_pool_t<T> 
{
public:
    using array_pool_t<T>::array_pool_t;

    ~eternal_new_pool_t() 
    { 
        std::lock_guard<std::mutex> lock(mutex);
        eternal.splice(*this);
    }

private:
    inline static std::mutex mutex;
    inline static array_pool_t<T> eternal;
};

// This has to be a function to get around a GCC bug
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81880
template<typename T>
inline eternal_new_pool_t<T>& eternal_new_pool()
{
    thread_local eternal_new_pool_t<T> pool;
    return pool;
}

template<typename T>
T const* eternal_new(T const* begin, T const* end)
{
    return eternal_new_pool<T>().insert(begin, end);
}

template<typename T, typename... Args>
T const* eternal_emplace(Args&&... args)
{
    return &eternal_new_pool<T>().emplace(std::forward<Args>(args)...);
}

#endif
