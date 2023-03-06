#ifndef STATIC_POOL_HPP
#define STATIC_POOL_HPP

#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <utility>
#include <vector>
#include <type_traits>

#include "c_delete.hpp"
#include "handle.hpp"
#include "intrusive_pool.hpp"
#include "thread.hpp"

template<typename T, typename Tag = T>
class static_intrusive_pool_t;

// This pool can hold any type, but only 1 type at a time (you must call
// 'clear' before changing the type).
// It's used for allocating transient node data - the type of data attached 
// to nodes that's unique to each pass.
template<typename Tag = void>
class static_any_pool_t
{
private:
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MINGW64__)
    static auto& storage() 
    { 
        static TLS std::unique_ptr<char, c_delete> _storage = {};
        return _storage; 
    }

    static auto& data_ptr() 
    { 
        static TLS char* _data_ptr; // Keep this pointing to 'storage.data()'
        return _data_ptr; 
    }

    static auto& bytes_capacity() 
    { 
        static TLS std::size_t _bytes_capacity = 0;
        return _bytes_capacity; 
    }

    static auto& allocated_size() 
    { 
        static TLS std::size_t _allocated_size = 0;
        return _allocated_size; 
    }
#else
    inline static TLS std::unique_ptr<char, c_delete> _storage = {};
    inline static TLS char* _data_ptr; // Keep this pointing to 'storage.data()'
    inline static TLS std::size_t _bytes_capacity = 0;
    inline static TLS std::size_t _allocated_size = 0;

    static auto& storage() { return _storage; }
    static auto& data_ptr() { return _data_ptr; }
    static auto& bytes_capacity() { return _bytes_capacity; }
    static auto& allocated_size() { return _allocated_size; }
#endif
public:
    template<typename T> [[gnu::always_inline]]
    static T* data() 
        { assert(storage()); return reinterpret_cast<T*>(data_ptr()); }

    template<typename T> [[gnu::always_inline]]
    static T& get(std::size_t i) 
        { assert(i < allocated_size()); return data<T>()[i]; } 

    template<typename T>
    static void resize(std::size_t new_size)
    {
        if(new_size <= allocated_size())
            return;

        if(new_size * sizeof(T) > bytes_capacity())
        {
            std::size_t const new_capacity = new_size * sizeof(T) * 2;
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MINGW64__)
            std::unique_ptr<char, c_delete> new_storage(
                (char*)std::malloc(new_capacity));
#else
            std::unique_ptr<char, c_delete> new_storage(
                (char*)std::aligned_alloc(64, new_capacity));
#endif
            if(!new_storage)
                throw std::bad_alloc();
            for(std::size_t i = 0; i < allocated_size(); ++i)
                new (reinterpret_cast<T*>(new_storage.get()) + i) T(
                    std::move(data<T>()[i]));
            if(!std::is_trivially_destructible<T>::value)
                for(std::size_t i = 0; i < allocated_size(); ++i)
                    get<T>(i).~T();
            storage() = std::move(new_storage);
            bytes_capacity() = new_capacity;
            data_ptr() = storage().get();
        }

        if(!std::is_trivially_constructible<T>::value)
            for(std::size_t i = allocated_size(); i < new_size; ++i)
                new (data<T>() + i) T();

        allocated_size() = new_size;
    }

    template<typename T>
    static void clear()
    {
        if(!std::is_trivially_destructible<T>::value)
            for(std::size_t i = 0; i < allocated_size(); ++i)
                data<T>()[i].~T();
        allocated_size() = 0;
    }

    static std::size_t array_size() { return allocated_size(); }
    static bool empty() { return allocated_size() == 0; }

    template<typename T>
    struct scope_guard_t 
    { 
        scope_guard_t() { assert(empty()); }
        explicit scope_guard_t(std::size_t size) 
            { assert(empty()); resize<T>(size); }
        scope_guard_t(scope_guard_t const&) = delete;
        scope_guard_t(scope_guard_t&) = delete;
        scope_guard_t& operator=(scope_guard_t const&) = delete;
        scope_guard_t& operator=(scope_guard_t&) = delete;
        ~scope_guard_t() { clear<T>(); } 
    };
};

template<typename T, typename Tag>
class static_intrusive_pool_t
{
public:
    struct handle_t : public intrusive_pool_t<T>::handle_t
    {
        T& operator*() const { assert(valid()); return this->get(*pool_ptr()); }
        T* operator->() const { assert(valid()); return &this->get(*pool_ptr()); }

        handle_t& operator++() { *this = next(); return *this; }
        handle_t operator++(int) { auto x = *this; operator++(); return x; }
        handle_t& operator--() { *this = prev(); return *this; }
        handle_t operator--(int) { auto x = *this; operator--(); return x; }

        handle_t next() const { assert(valid()); return { intrusive_pool_t<T>::handle_t::next(*pool_ptr()).id }; }
        handle_t prev() const { assert(valid()); return { intrusive_pool_t<T>::handle_t::prev(*pool_ptr()).id }; }

        template<typename U>
        U& data() const { assert(this->id < pool().array_size()); return static_any_pool_t<Tag>::template get<U>(this->id); }

        static bool valid() { return &pool() == pool_ptr(); }
    };
private:
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MINGW64__)
    static auto& pool() 
    { 
        static TLS intrusive_pool_t<T> _pool;
        return _pool; 
    }

    static auto& pool_ptr() 
    { 
        static TLS intrusive_pool_t<T>* _pool_ptr;
        return _pool_ptr; 
    }
#else
    inline static TLS intrusive_pool_t<T> _pool;

    // Points to 'pool'. Call 'init' to set this.
    // (This exists to reduce penalty of __tls_init)
    inline static TLS intrusive_pool_t<T>* _pool_ptr;

    static auto& pool() { return _pool; }
    static auto& pool_ptr() { return _pool_ptr; }
#endif
public:
    static void init() { pool_ptr() = &pool(); (void)pool().data(); }
    static handle_t alloc() { return { pool().alloc().id }; }
    static void free(handle_t h) { pool().free({ h.id }); }
    static void clear() { pool().clear(); }

    static std::size_t size() { return pool().size(); }
    static std::size_t array_size() { return pool().array_size(); }
    static T* data() { return pool().data(); }
};

#endif
