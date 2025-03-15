#ifndef ARRAY_POOL_HPP
#define ARRAY_POOL_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <memory>
#include <vector>
#include <boost/container/deque.hpp>
#include <list>

// A simple allocator that only supports allocation, not free.
// Memory is still cleared on pool destruction or the calling of 'clear()'.
template<typename T, std::size_t ChunkSize = 256>
class array_pool_t
{
public:
    array_pool_t() = default;
    array_pool_t(array_pool_t const&) = delete;
    array_pool_t(array_pool_t&&) = default;
    array_pool_t& operator=(array_pool_t const&) = delete;
    array_pool_t& operator=(array_pool_t&&) = default;

    T* alloc(std::size_t size = 1)
    {
        if(size == 0)
            return nullptr;

        used_size += size;

        if(use_oversized(size))
            return oversized.emplace_back(size).data();

        reserve(size);
        storage_t* storage = used->data + used->size;
        for(std::size_t i = 0; i < size; ++i)
        {
            new(used->data + used->size) T();
            ++used->size; // Do it this way for exception safety.
        }

        return reinterpret_cast<T*>(storage);
    }

    template<typename... Args>
    T& emplace(Args&&... args)
    {
        used_size += 1;
        reserve(1);
        assert(used);
        storage_t* storage = used->data + used->size;
        new(storage) T(std::forward<Args>(args)...);
        ++used->size;
        return *reinterpret_cast<T*>(storage);
    }

    T& insert(T const& t) { return emplace(t); }
    T& insert(T&& t) { return emplace(std::move(t)); }

    template<typename It>
    T* insert(It begin, It end)
    {
        std::size_t size = std::distance(begin, end);
        if(size == 0)
            return nullptr;

        used_size += size;

        if(use_oversized(size))
            return oversized.emplace_back(begin, end).data();

        reserve(size);
        storage_t* storage = used->data + used->size;
        for(It it = begin; it != end; ++it)
        {
            new(used->data + used->size) T(*it);
            ++used->size; // Do it this way for exception safety.
        }

        return reinterpret_cast<T*>(storage);
    }

    template<typename... Args>
    T* insert_v(Args&&... args)
    {
        std::size_t size = sizeof...(Args);
        if(size == 0)
            return nullptr;

        used_size += size;

        if(use_oversized(size))
        {
            std::vector<T> vec;
            (vec.emplace_back(std::forward<Args>(args)), ...);
            return oversized.emplace_back(std::move(vec)).data();
        }

        reserve(size);
        storage_t* storage = used->data + used->size;

        ((new(used->data + used->size) T(std::forward<Args>(args)), 
          ++used->size), ...);

        return reinterpret_cast<T*>(storage);
    }

    void clear()
    {
        oversized.clear();
        used_size = 0;

        if(used)
        {
            for(buffer_t* buf = used.get(); buf; buf = buf->prev)
                buf->destruct_owned();
        }

        // Move the used list onto the free list.
        if(first_free)
            first_free->set_prev(used.release());
        else
            free = std::move(used);

        first_free = first_used;
        first_used = nullptr;
    }

    void shrink_to_fit()
    {
        free.reset();
    }

    // Calls Func on every allocated value.
    template<typename Func>
    void for_each(Func func)
    {
        for(buffer_t* buffer = used.get(); buffer; buffer = buffer->prev.get())
            for(std::size_t i = 0; i < buffer->size; ++i)
                func(reinterpret_cast<T&>(buffer->data[i]));

        for(auto& vec : oversized)
            for(T& t : vec)
                func(t);
    }

    std::size_t size() const { return used_size; }

    // Steals all the allocations from 'other'.
    // (Does not steal the free list)
    void splice(array_pool_t& other)
    {
        if(!other.first_used)
            return;

        if(first_used)
            first_used->set_prev(other.used.release());
        else
            used = std::move(other.used);

        first_used = other.first_used;
        other.first_used = nullptr;

        oversized.splice(oversized.begin(), other.oversized);

        used_size += other.used_size;
        other.used_size = 0;
    }

private:
    using storage_t = 
        typename std::aligned_storage<sizeof(T), alignof(T)>::type;

    void reserve(std::size_t size)
    {
        if(!used || (used->size + size) > ChunkSize)
        {
            std::unique_ptr<buffer_t, deleter_t> old_buffer = std::move(used);

            // Allocate the memory
            if(free)
            {
                used = std::move(free);
                free.reset(used->release_prev());
                if(!free)
                    first_free = nullptr;
            }
            else
                used.reset(new buffer_t());

            assert(!used->prev);

            // Zero the size.
            used->size = 0;

            // Update the pointers
            used->set_prev(old_buffer.release());
            if(!first_used)
                first_used = used.get();
        }

        assert(used && used->size + size <= ChunkSize);
    }

    bool use_oversized(std::size_t size) const
    {
        if(size > ChunkSize)
            return true;

        // Avoid large amounts of internal fragmentation with this:
        if(used)
        {
            return ((std::ptrdiff_t)size + (std::ptrdiff_t)used->size
                    > (std::ptrdiff_t)(ChunkSize * 3 / 2));
        }

        return false;
    }

    struct buffer_t
    {
        ~buffer_t()
        {
            destruct_owned();
        }

        void destruct_owned()
        {
            for(unsigned i = 0; i < size; ++i)
                reinterpret_cast<T&>(data[i]).~T();
            size = 0;
        }

        void set_prev(buffer_t* new_prev)
        {
            if(prev)
                delete prev;
            prev = new_prev;
        }

        buffer_t* release_prev()
        {
            buffer_t* ret = prev;
            prev = nullptr;
            return ret;
        }

        storage_t data[ChunkSize];
        std::size_t size = 0;
        buffer_t* prev = nullptr;
    };

    struct deleter_t
    {
        void operator()(buffer_t* buf)
        {
            // Implement non-recursively, to avoid stack overflows
            while(buf)
            {
                buffer_t* prev = buf->prev;
                delete buf;
                buf = prev;
            }
        }
    };

    std::unique_ptr<buffer_t, deleter_t> used;
    std::unique_ptr<buffer_t, deleter_t> free;
    std::list<std::vector<T>> oversized;
    std::size_t used_size = 0;
    buffer_t* first_used = nullptr;
    buffer_t* first_free = nullptr;
};

#endif
