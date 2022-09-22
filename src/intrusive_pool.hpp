#ifndef INTRUSIVE_POOL
#define INTRUSIVE_POOL

#include <cassert>
#include <cstdint>
#include <memory>
#include <utility>
#include <vector>
#include <type_traits>

#include "debug_print.hpp"
#include "handle.hpp"

// A simple pool based on a single vector, providing handles (indexes) into
// the vector instead of pointers.
// Pointers are invalidated upon allocation, but handles aren't.
// 'T' must derive from 'intrusive_t', which provides an intrusive
// linked-list interface for handling freed nodes.
template<typename T>
class intrusive_pool_t
{
public:
    struct handle_t : public ::handle_t<handle_t, std::uint32_t, 0u>
    {
        T const& get(intrusive_pool_t const& pool) const 
        { 
            assert(this->operator bool()); 
            passert(this->id < pool.storage.size(), this->id, pool.storage.size()); 
            return pool.storage[this->id]; 
        }

        T& get(intrusive_pool_t& pool) const 
        { 
            assert(this->operator bool()); 
            passert(this->id < pool.storage.size(), this->id, pool.storage.size()); 
            return pool.storage[this->id]; 
        }

        handle_t next(intrusive_pool_t const& pool) const { return pool.storage[this->id].next; }
        handle_t prev(intrusive_pool_t const& pool) const { return pool.storage[this->id].prev; }
    };
private:
    std::vector<T> storage = std::vector<T>(1);
    handle_t free_head = {};
    std::size_t used_size = 0;
public:
    handle_t alloc() 
    {
        assert(storage.size() > 0);
        handle_t ret;
        if(free_head)
        {
            ret = free_head;
            free_head = free_head.get(*this).next;
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

    void free(handle_t h)
    {
        assert(h);
        h.get(*this).next.id = free_head.id;
        free_head = h;
        --used_size;
    }

    void clear()
    {
        storage.resize(1);
        free_head = {};
        used_size = 0;
    }

    void reserve(std::size_t size) { storage.reserve(size); }

    std::size_t size() { return used_size; }
    std::size_t array_size() { return storage.size(); }
    T* data() { return storage.data(); }
};

template<typename Handle>
class intrusive_t
{
    template<typename T>
    friend class intrusive_pool_t;
protected:
    Handle next;
    Handle prev;
};


#endif
