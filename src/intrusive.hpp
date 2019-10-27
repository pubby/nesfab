#ifndef INTRUSIVE_POOL_HPP
#define INTRUSIVE_POOL_HPP

#include <memory>

template<typename T>
class intrusive_iterator_t
{
public:
    T* ptr;

    using value_type = T;
    using reference = T&;
    using pointer = T*;
    using difference_type = std::size_t;
    using iterator_category = std::forward_iterator_tag;

    intrusive_iterator_t() : ptr(nullptr) {}
    intrusive_iterator_t(T* ptr) : ptr(ptr) {}

    T& operator*() const { return *ptr; }
    T* operator->() const { return ptr; }

    intrusive_iterator_t& operator++() { ptr = ptr->next; return *this; }
    intrusive_iterator_t operator++(int) 
        { intrusive_iterator_t ret = *this; operator++(); return ret; }

    explicit operator bool() const { return ptr; }

    bool operator==(intrusive_iterator_t o) const { return ptr == o.ptr; }
    bool operator!=(intrusive_iterator_t o) const { return ptr != o.ptr; }
    bool operator<(intrusive_iterator_t o) const { return ptr < o.ptr; }
};

// A very basic implementation of intrusive list containers.
template<typename T>
class intrusive_list_t
{
protected:
    T* _head_ptr = nullptr;
public:
    using iterator = intrusive_iterator_t<T>;

    intrusive_list_t() = default;
    intrusive_list_t(intrusive_list_t const&) = delete;
    intrusive_list_t(intrusive_list_t&&) = default;
    intrusive_list_t& operator=(intrusive_list_t const&) = delete;
    intrusive_list_t& operator=(intrusive_list_t&&) = default;

    void insert(T& node)
    {
        if(_head_ptr)
            _head_ptr->prev = &node;
        node.next = _head_ptr;
        node.prev = nullptr;
        _head_ptr = &node;
    }

    T* erase(T& node)
    {
        T* ret = node.next;

        if(node.prev)
            node.prev->next = node.next;
        else
        {
            assert(&node == _head_ptr);
            _head_ptr = node.next;
        }

        if(node.next)
            node.next->prev = node.prev;

        return ret;
    }

    bool empty() const { return _head_ptr == nullptr; }
    void clear() { _head_ptr = nullptr; }
    T* head() const { return _head_ptr; }

    template<typename Fn>
    void foreach(Fn const& fn)
    {
        for(T* node = _head_ptr; node; node = node->next)
            fn(*node);
    }

    iterator begin() const { return head(); }
    iterator end() const { return iterator(); }
};

// Like a regular intrusive list, but allows tracking a subset of the list
// and iterating over this subset.
// For example, it's used to keep all phi nodes together so that phi nodes
// can be iterated efficiently.
template<typename T>
class partitioned_intrusive_list_t : public intrusive_list_t<T>
{
    T* end_ptr = nullptr; // Tracks the end of the non-partitioned range.
    T* partition_ptr = nullptr; // Tracks the start of the partitioned range.
public:
    using iterator = intrusive_iterator_t<T>;

    partitioned_intrusive_list_t() = default;
    partitioned_intrusive_list_t(partitioned_intrusive_list_t const&) = delete;
    partitioned_intrusive_list_t(partitioned_intrusive_list_t&&) = default;
    partitioned_intrusive_list_t& 
        operator=(partitioned_intrusive_list_t const&) = delete;
    partitioned_intrusive_list_t& 
        operator=(partitioned_intrusive_list_t&&) = default;

    void insert(T& node)
    {
        if(!end_ptr)
            end_ptr = &node;
        intrusive_list_t<T>::insert(node);
    }
    
    void partition_insert(T& node)
    {
        if(!this->_head_ptr)
            this->_head_ptr = &node;
        if(end_ptr)
            end_ptr->next = &node;
        if(partition_ptr)
            partition_ptr->prev = &node;
        node.prev = end_ptr;
        node.next = partition_ptr;
        partition_ptr = &node;
    }

    T* erase(T& node)
    {
        if(&node == end_ptr)
            end_ptr = node.prev;
        if(&node == partition_ptr)
            partition_ptr = node.next;
        return intrusive_list_t<T>::erase(node);
    }

    void clear() { intrusive_list_t<T>::clear(); partition_ptr = nullptr; }
    T* partition() const { return partition_ptr; }

    iterator partition_begin() const { return partition(); }
};

// A very basic implementation of intrusive list containers.
// TODO: remove?
/*
template<typename T>
class bi_intrusive_list_t
{
    T* first_ptr = nullptr;
    T* last_ptr = nullptr;
public:
    intrusive_list_t() = default;
    intrusive_list_t(intrusive_list_t const&) = delete;
    intrusive_list_t(intrusive_list_t&&) = default;
    intrusive_list_t& operator=(intrusive_list_t const&) = delete;
    intrusive_list_t& operator=(intrusive_list_t&&) = default;

    void push_front(T& node)
    {
        if(first_ptr)
            first_ptr->prev = &node;
        else
        {
            assert(!last_ptr);
            last_ptr = &node;
        }
        node.next = first_ptr;
        node.prev = nullptr;
        first_ptr = &node;
        if(!last_ptr)
            last_ptr = &node;
    }

    void push_back(T& node)
    {
        if(last_ptr)
            last_ptr->next = &node;
        else
        {
            assert(!first_ptr);
            first_ptr = &node;
        }
        node.next = nullptr;
        node.prev = last_ptr;
        last_ptr = &node;
    }

    T* erase(T& node)
    {
        T* ret = node.next;

        if(node.prev)
            node.prev->next = node.next;
        else
        {
            assert(&node == first_ptr);
            first_ptr = node.next;
        }

        if(node.next)
            node.next->prev = node.prev;
        else
        {
            assert(&node == last_ptr);
            last_ptr = node.prev;
        }

        return ret;
    }

    bool empty() const { return !first_ptr; }
    void clear() { first_ptr = last_ptr = nullptr; }
    T* first() const { return first_ptr; }
    T* last() const { return last_ptr; }

    template<typename Fn>
    void foreach(Fn const& fn)
    {
        for(T* node = first_ptr; node; node = node->next)
            fn(*node);
    }

    class iterator
    {
    public:
        T* ptr;

        using value_type = T;
        using reference = T&;
        using pointer = T*;
        using difference_type = std::size_t;
        using iterator_category = std::forward_iterator_tag;

        iterator() : ptr(nullptr) {}
        iterator(T* ptr) : ptr(ptr) {}

        T& operator*() const { return *ptr; }
        T* operator->() const { return ptr; }

        iterator& operator++() { ptr = ptr->next; return *this; }
        iterator operator++(int) 
            { iterator ret = *this; operator++(); return ret; }

        explicit operator bool() const { return ptr; }

        bool operator==(iterator o) const { return ptr == o.ptr; }
        bool operator!=(iterator o) const { return ptr != o.ptr; }
        bool operator<(iterator o) const { return ptr < o.ptr; }
    };

    iterator begin() const { return first(); }
    iterator end() const { return iterator(); }
};
*/

// Inherit from this to make a type intrusive.
template<typename T>
class intrusive_t
{
    template<typename, std::size_t>
    friend class intrusive_pool_t;
    friend class intrusive_iterator_t<T>;
    friend class intrusive_list_t<T>;
    friend class partitioned_intrusive_list_t<T>;
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

    // Returns the allocated object to the pool, but its storage won't
    // be reused until 'reclaim' is called. This can be used to keep
    // pointers valid.
    void prune(T& t)
    {
        t.next = prune_head;
        prune_head = &t;
        --allocated_size;
    }

    void reclaim()
    {
        if(T* ptr = prune_head)
        {
            while(ptr->next)
                ptr = ptr->next;
            ptr->next = free_head;
            free_head = prune_head;
            prune_head = nullptr;
        }
    }

    void clear_and_reclaim() 
    { 
        std::unique_ptr<buffer_t>* end = &free_buffers_head;
        while(*end)
            end = &(*end)->next;
        *end = std::move(buffer_head);
        allocated_size = 0;
        reclaim();
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
    T* prune_head = nullptr;
    std::size_t allocated_size = 0;
};

#endif
