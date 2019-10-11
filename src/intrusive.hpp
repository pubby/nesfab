#ifndef INTRUSIVE_POOL_HPP
#define INTRUSIVE_POOL_HPP

#include <memory>

// A very basic implementation of intrusive list containers.
template<typename T>
class intrusive_list_t
{
public:
    intrusive_list_t() = default;
    intrusive_list_t(intrusive_list_t const&) = delete;
    intrusive_list_t(intrusive_list_t&&) = default;
    intrusive_list_t& operator=(intrusive_list_t const&) = delete;
    intrusive_list_t& operator=(intrusive_list_t&&) = default;

    void insert(T& node)
    {
        if(head)
            head->prev = &node;
        node.next = head;
        node.prev = nullptr;
        head = &node;
    }

    void erase(T& node)
    {
        if(node.prev)
            node.prev->next = node.next;
        else
        {
            assert(&node == head);
            head = node.next;
        }

        if(node.next)
            node.next->prev = node.prev;
    }

    bool empty() const { return !head; }
    void clear() { head = nullptr; }

    template<typename Fn>
    void foreach(Fn const& fn)
    {
        for(T* node = head; node; node = node->next)
            fn(*node);
    }

private:
    T* head = nullptr;
};

// Inherit from this to make a type intrusive.
template<typename T>
class intrusive_t
{
    template<typename, std::size_t>
    friend class intrusive_pool_t;
    friend class intrusive_list_t<T>;
protected:
    T* next;
    T* prev;
};

// A very basic pool that uses T's 'next' member as an intrusive list.
// This is 'POD' style; objects are allocated when blocks are allocated,
// and aren't destructed until the list_pool is destructed.
template<typename T, std::size_t ChunkSize = 32>
class intrusive_pool_t
{
public:
    T& alloc()
    {
        if(!free_head)
        {
            std::unique_ptr<buffer_t> new_buffer;
            if(free_buffers_head)
            {
                new_buffer = std::move(free_buffers_head);
                free_buffers_head = std::move(new_buffer->next);
            }
            else
                new_buffer.reset(new buffer_t());
            assert(new_buffer);
            new_buffer->next = std::move(buffer_head);
            buffer_head = std::move(new_buffer);
            free_buffer(*buffer_head);
        }
        assert(free_head);
        T* ret = free_head;
        free_head = free_head->next;
        ++allocated_size;
        return *ret;
    }

    void free(T& t)
    {
        t.next = free_head;
        free_head = &t;
        --allocated_size;
    }

    void clear() 
    { 
        std::unique_ptr<buffer_t>* end = &free_buffers_head;
        while(*end)
            end = &(*end)->next;
        *end = std::move(buffer_head);
        allocated_size = 0;
    }

    std::size_t size() const { return allocated_size; }

private:
    struct buffer_t
    {
        T data[ChunkSize];
        std::unique_ptr<buffer_t> next;
    };

    void free_buffer(buffer_t& buffer)
    {
        for(std::size_t i = 0; i < ChunkSize-1; ++i)
            buffer.data[i].next = &buffer.data[i+1];
        buffer.data[ChunkSize-1].next = free_head;
        free_head = &buffer.data[0];
    }

    std::unique_ptr<buffer_t> buffer_head;
    std::unique_ptr<buffer_t> free_buffers_head;
    T* free_head = nullptr;
    std::size_t allocated_size = 0;
};

#endif
