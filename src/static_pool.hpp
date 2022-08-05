#ifndef STATIC_POOL_HPP
#define STATIC_POOL_HPP

#include <cassert>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>
#include <type_traits>

#include "c_delete.hpp"
#include "handle.hpp"

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
    inline static thread_local std::unique_ptr<char, c_delete> storage = {};
    inline static thread_local std::size_t bytes_capacity = 0;
    inline static thread_local std::size_t allocated_size = 0;
public:
    template<typename T> [[gnu::always_inline]]
    static T* data() 
        { assert(storage); return reinterpret_cast<T*>(storage.get()); }

    template<typename T> [[gnu::always_inline]]
    static T& get(std::size_t i) 
        { assert(i < allocated_size); return data<T>()[i]; } 

    template<typename T>
    static void resize(std::size_t new_size)
    {
        if(new_size <= allocated_size)
            return;

        if(new_size * sizeof(T) > bytes_capacity)
        {
            std::size_t const new_capacity = new_size * sizeof(T) * 2;
            std::unique_ptr<char, c_delete> new_storage(
                (char*)std::aligned_alloc(64, new_capacity));
            if(!new_storage)
                throw std::bad_alloc();
            for(std::size_t i = 0; i < allocated_size; ++i)
                new (reinterpret_cast<T*>(new_storage.get()) + i) T(
                    std::move(data<T>()[i]));
            if(!std::is_trivially_destructible<T>::value)
                for(std::size_t i = 0; i < allocated_size; ++i)
                    get<T>(i).~T();
            storage = std::move(new_storage);
            bytes_capacity = new_capacity;
        }

        if(!std::is_trivially_constructible<T>::value)
            for(std::size_t i = allocated_size; i < new_size; ++i)
                new (data<T>() + i) T();

        allocated_size = new_size;
    }

    template<typename T>
    static void clear()
    {
        if(!std::is_trivially_destructible<T>::value)
            for(std::size_t i = 0; i < allocated_size; ++i)
                data<T>()[i].~T();
        allocated_size = 0;
    }

    static std::size_t array_size() { return allocated_size; }
    static bool empty() { return allocated_size == 0; }

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

// A simple pool based on a single vector, providing handles (indexes) into
// the vector instead of pointers.
// Pointers are invalidated upon allocation, but handles aren't.
// 'T' must derive from 'intrusive_t', which provides an intrusive
// linked-list interface for handling freed nodes.
template<typename T, typename Tag>
class static_intrusive_pool_t
{
public:
    struct handle_t : public ::handle_t<handle_t, std::uint32_t, 0u>
    {
        T& operator*() const 
            { assert(this->operator bool()); assert(this->id < storage.size()); return storage[this->id]; }
        T* operator->() const 
            { assert(this->operator bool()); assert(this->id < storage.size()); return storage.data() + this->id; }

        handle_t& operator++() { *this = storage[this->id].next; return *this; }
        handle_t operator++(int) { auto x = *this; operator++(); return x; }
        handle_t& operator--() { *this = storage[this->id].prev; return *this; }
        handle_t operator--(int) { auto x = *this; operator--(); return x; }

        handle_t next() const { return storage[this->id].next; }
        handle_t prev() const { return storage[this->id].prev; }

        template<typename U>
        U& data() const { assert(this->id < storage.size()); return static_any_pool_t<Tag>::template get<U>(this->id); }
    };
private:
    inline static thread_local std::vector<T> storage = std::vector<T>(1);
    inline static thread_local handle_t free_head = {};
    inline static thread_local std::size_t used_size = 0;
public:
    static handle_t alloc() 
    {
        assert(storage.size() > 0);
        handle_t ret;
        if(free_head)
        {
            ret = free_head;
            free_head = free_head->next;
        }
        else
        {
            ret = { storage.size() };
            storage.emplace_back();
        }
        ++used_size;
        assert(ret);
        assert(ret.id < storage.size());
        return ret;
    }

    static void free(handle_t h)
    {
        assert(h);
        h->next = free_head;
        free_head = h;
        --used_size;
    }

    static void clear()
    {
        storage.resize(1);
        free_head = {};
        used_size = 0;
    }

    static std::size_t size() { return used_size; }
    static std::size_t array_size() { return storage.size(); }
    static T* data() { return storage.data(); }
};

template<typename Handle>
class intrusive_t
{
    template<typename T, typename Tag>
    friend class static_intrusive_pool_t;
protected:
    Handle next;
    Handle prev;
};

#endif
