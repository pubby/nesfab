#ifndef ROBIN_HOOD_COLLECTION_HPP
#define ROBIN_HOOD_COLLECTION_HPP

// Takes the low-level data structure from "table.hpp" and wraps it into
// a nicer to use container.

// This form is very generalized (thus powerful) and uses policies to 
// determine behavior.
// See map.hpp and set.hpp to get what you're used to.

#include <memory>
#include <vector>

#include "table.hpp"

#define LIKELY(b) __builtin_expect(bool(b), true)
#define UNLIKELY(b) __builtin_expect(bool(b), false)

// Policy expects these members functions:
//  static std::size_t hash(T)
//  static bool equal(T, T)

namespace rh
{

template<typename Policy>
class robin_collection
{
public:
    using value_type = typename Policy::value_type;
    using table_type = robin_auto_table<value_type>;
    using hash_type = typename table_type::hash_type;
    using policy_type = Policy;

    robin_collection() = default;
    explicit robin_collection(hash_type size) { reserve(size); }
    robin_collection(robin_collection const&) = default;
    robin_collection(robin_collection&&) = default;
    robin_collection& operator=(robin_collection const&) = default;
    robin_collection& operator=(robin_collection&&) = default;

    apair<value_type*, bool> insert(value_type const& t)
    { 
        return emplace(t, [&]() -> value_type const& { return t; }); 
    }

    apair<value_type*, bool> insert(value_type&& t)
    { 
        return emplace(t, [&]() -> value_type&& { return std::move(t); }); 
    }

    template<typename K, typename C>
    apair<value_type*, bool> emplace(K const& key, C&& construct)
    { 
        return table.emplace(
            Policy::hash(key),
            eq<K>{key},
            std::forward<C>(construct));
    }

    template<typename K>
    value_type const* find(K const& k) const
    { 
        return table.find(Policy::hash(k), eq<K>{k}).second;
    }

    template<typename K>
    value_type* find(K const& k)
    { 
        return const_cast<value_type*>(const_this()->find(k));
    }

    void erase(value_type const& v)
    {
        table.erase({ table.get_hash(std::addressof(v)), std::addressof(v) });
    }

    template<typename K>
    bool remove(K const& k)
    { 
        auto pair = table.find(Policy::hash(k), eq<K>{k});
        if(!pair.first)
            return false;
        table.erase(pair);
        return true;
    }

    template<typename K>
    bool count(K const& k) const { return find(k); }

    void clear() { table.clear(); }
    void reset() { table.reset(); }

    void swap(robin_collection& o) noexcept 
    { 
        table.swap(o.table);
    }

    friend void swap(robin_collection& a, robin_collection& b) noexcept 
    {
        a.swap(b);
    }

    std::size_t size() const { return table.size(); }
    bool empty() const { return size() == 0; }
    void reserve(hash_type size) { table.reserve(size); }

private:
    robin_collection const* const_this() const { return this; }

    template<typename K>
    struct eq
    {
        [[gnu::always_inline]]
        inline bool operator()(value_type const& v) const 
        { 
            return Policy::equal(key, v); 
        }
        K const& key;
    };

    table_type table;
};

// Stores data in a vector, and uses the hash table to 
// store indices into this vector.
// Because the data is in vector, it can be iterated efficiently.
template<typename Policy>
class batman_collection
{
public:
    using value_type = typename Policy::value_type;
    using index_type = std::uint32_t;
    using table_type = robin_auto_table<index_type>;
    using hash_type = typename table_type::hash_type;
    using policy_type = Policy;

    using iterator = value_type*;
    using const_iterator = value_type const*;

    batman_collection() = default;
    explicit batman_collection(hash_type size) { reserve(size); }
    batman_collection(batman_collection const&) = default;
    batman_collection(batman_collection&&) = default;
    batman_collection& operator=(batman_collection const&) = default;
    batman_collection& operator=(batman_collection&&) = default;

    apair<value_type*, bool> insert(value_type const& t)
    { 
        return emplace(t, [&]() -> value_type const& { return t; }); 
    }

    apair<value_type*, bool> insert(value_type&& t)
    { 
        return emplace(t, [&]() -> value_type&& { return std::move(t); }); 
    }

    template<typename K, typename C>
    apair<value_type*, bool> emplace(K const& key, C construct)
    { 
        apair<index_type*, bool> result = table.emplace(
            Policy::hash(key),
            eq<K>{ key, data() },
            [&]()-> index_type 
            { 
                index_type const index = m_data.size();
                m_data.emplace_back(construct()); 
                return index;
            });
        return { data() + *result.first, result.second };
    }

    // Returns nullptr on failure, NOT cend()!!
    template<typename K>
    value_type const* find(K const& k) const
    { 
        index_type const* ptr = table.find(
            Policy::hash(k), eq<K>{k, data()}).second;
        return ptr ? data() + *ptr : nullptr;
    }

    template<typename K>
    value_type* find(K const& k)
    { 
        return const_cast<value_type*>(const_this()->find(k));
    }

    // For values we know are in the container.
    void erase(value_type const& v)
    {
        erase_impl(table.find(Policy::hash(v), 
                              eq_i{ std::addressof(v) - data() }));
    }

    // For values that might be in the container.
    template<typename K>
    bool remove(K const& k)
    { 
        auto pair = table.find(Policy::hash(k), eq<K>{ k, data() });
        if(!pair.second)
            return false;
        erase_impl(pair);
        return true;
    }

    template<typename K>
    bool count(K const& k) const
    { 
        return table.find(Policy::hash(k), eq<K>{k, data()}).second;
    }

    void clear() { table.clear(); m_data.clear(); }
    void reset() { table.reset(); m_data.clear(); m_data.shrink_to_fit(0); }

    void swap(batman_collection& o) noexcept 
    { 
        table.swap(o.table);
        m_data.swap(o.m_data);
    }

    friend void swap(batman_collection& a, batman_collection& b) noexcept 
    {
        a.swap(b);
    }

    std::size_t size() const { return table.size(); }
    bool empty() const { return size() == 0; }

    void reserve(hash_type size) { table.reserve(size); }

    const_iterator cbegin() const noexcept { return data(); }
    const_iterator begin() const noexcept { return data(); }
    iterator begin() noexcept { return data(); }
    const_iterator cend() const noexcept { return data() + m_data.size(); }
    const_iterator end() const noexcept { return data() + m_data.size(); }
    iterator end() noexcept { return data() + m_data.size(); }

    value_type* data() { return m_data.data(); }
    value_type const* data() const { return m_data.data(); }

private:
    batman_collection const* const_this() const { return this; }

    void erase_impl(apair<hash_type*, index_type*> pair)
    {
        using std::swap;
        if(*pair.second != m_data.size() - 1)
        {
            *table.find(
                Policy::hash(m_data.back()), 
                eq<value_type>{m_data.back(), data()}).second = *pair.second;
            swap(data()[*pair.second], m_data.back());
        }
        m_data.pop_back();
        table.erase(pair);
    }

    template<typename K>
    struct eq
    {
        [[gnu::always_inline]]
        inline bool operator()(index_type index) const 
        { 
            return Policy::equal(key, data[index]);
        }
        K const& key;
        value_type const* data;
    };

    struct eq_i
    {
        [[gnu::always_inline]]
        inline bool operator()(index_type index) const 
        { 
            return index == i;
        }
        index_type i;
    };

    table_type table;
    std::vector<value_type> m_data;
};

// Let's say you have a collection<T*>,
// but you want to treat it like collection<T>.
// That's where ptr_policy comes into play.
template<typename Policy, typename Ptr = typename Policy::value_type const*>
struct ptr_policy
{
    using pointed_to = typename Policy::value_type;
    using value_type = Ptr;

    template<typename T>
    static std::size_t hash(T&& t) 
        { return Policy::hash(std::forward<T>(t)); }

    template<typename T>
    static std::size_t equal(T&& t, value_type const& ptr) 
        { return Policy::equal(std::forward<T>(t), *ptr); }
};

template<typename T>
class joker_iterator
{
public:
    using difference_type = std::ptrdiff_t;
    using value_type = T;
    using pointer = value_type*;
    using reference = value_type&;
    using iterator_category = std::random_access_iterator_tag;

    using unique_ptr_type = std::unique_ptr<std::remove_const_t<value_type>>;

    joker_iterator() : ptr(nullptr) {}
    explicit joker_iterator(unique_ptr_type* ptr) : ptr(ptr) {}

    joker_iterator& operator+=(difference_type i) { ptr += i; return *this; }
    joker_iterator& operator-=(difference_type i) { ptr -= i; return *this; }
    difference_type operator-(joker_iterator it) const { return ptr - it.ptr; }
    value_type& operator[](difference_type i) const { return *ptr[i]; }
    bool operator==(joker_iterator it) const { return ptr == it.ptr; }
    bool operator!=(joker_iterator it) const { return ptr != it.ptr; }
    bool operator>=(joker_iterator it) const { return ptr >= it.ptr; }
    bool operator<=(joker_iterator it) const { return ptr <= it.ptr; }
    bool operator<(joker_iterator it) const { return ptr < it.ptr; }
    bool operator>(joker_iterator it) const { return ptr > it.ptr; }
    value_type& operator*() const { return **ptr; }
    value_type* operator->() const { return ptr->get(); }
    joker_iterator& operator++() { ++ptr; return *this; }
    joker_iterator operator++(int) { return joker_iterator(ptr++); }
    joker_iterator& operator--() { --ptr; return *this; }
    joker_iterator& operator--(int) { return joker_iterator(ptr--); }

private:
    unique_ptr_type* ptr;
};

template<typename T>
joker_iterator<T> operator+(joker_iterator<T> it, std::ptrdiff_t i) 
{ 
    it += i;
    return it;
}

template<typename T>
joker_iterator<T> operator+(std::ptrdiff_t i, joker_iterator<T> it) 
{ 
    it += i;
    return it;
}

// Like batman_collection, but it stores unique_ptrs behind the scenes.
// This means pointers will never be invalidated when elements are added.
// On the downside, 'erase' does not exist. You have to use 'remove'.
template<typename Policy>
class joker_collection
{
public:
    using value_type = typename Policy::value_type;
    using index_type = std::uint32_t;
    using collection_type = 
        batman_collection<ptr_policy<Policy, std::unique_ptr<value_type>>>;
    using hash_type = typename collection_type::hash_type;
    using policy_type = Policy;
    using unique_ptr_t = std::unique_ptr<value_type>;

    using iterator = joker_iterator<value_type>;
    using const_iterator = joker_iterator<value_type const>;

    joker_collection() = default;
    explicit joker_collection(hash_type size) { reserve(size); }
    joker_collection(joker_collection const&) = default;
    joker_collection(joker_collection&&) = default;
    joker_collection& operator=(joker_collection const&) = default;
    joker_collection& operator=(joker_collection&&) = default;

    apair<value_type*, bool> insert(value_type const& t) 
    {
        apair<unique_ptr_t*, bool> pair = collection.emplace(
            t, 
            [&]() { return unique_ptr_t(new value_type(t)); }); 
        return { pair.first->get(), pair.second };
    }

    apair<value_type*, bool> insert(value_type&& t)
    { 
        apair<unique_ptr_t*, bool> pair = collection.emplace(
            t, 
            [&]() { return unique_ptr_t(new value_type(std::move(t))); }); 
        return { pair.first->get(), pair.second };
    }

    template<typename K>
    apair<value_type*, bool> emplace(K&& key)
    {
        apair<unique_ptr_t*, bool> pair = collection.emplace(
            key, 
            [&](){ return unique_ptr_t(new value_type()); });
        return { pair.first->get(), pair.second };
    }

    template<typename K, typename C>
    apair<value_type*, bool> emplace(K&& key, C construct)
    { 
        apair<unique_ptr_t*, bool> pair = collection.emplace(
            key, 
            [&](){ return unique_ptr_t(new value_type(construct())); });
        return { pair.first->get(), pair.second };
    }

    // Returns nullptr on failure, NOT cend()!!
    template<typename K>
    value_type const* find(K const& k) const
    { 
        unique_ptr_t* ptr = collection.find(k);
        return ptr ? ptr->get() : nullptr;
    }

    template<typename K>
    value_type* find(K const& k)
        { return const_cast<value_type*>(const_this()->find(k)); }

    // For values that might be in the container.
    template<typename K>
    bool remove(K const& k) { return collection.remove(k); }

    template<typename K>
    bool count(K const& k) const { return collection.count(k); }

    void clear() { collection.clear(); }
    void reset() { collection.reset(); }

    void swap(joker_collection& o) noexcept { collection.swap(o.collection); }
    friend void swap(joker_collection& a, joker_collection& b) noexcept 
        { a.swap(b); }

    std::size_t size() const { return collection.size(); }
    bool empty() const { return size() == 0; }
    void reserve(hash_type size) { collection.reserve(size); }

    const_iterator cbegin() const noexcept 
        { return const_iterator(collection.cbegin()); }
    const_iterator begin() const noexcept 
        { return const_iterator(collection.cbegin()); }
    iterator begin() noexcept 
        { return iterator(collection.begin()); }
    const_iterator cend() const noexcept 
        { return const_iterator(collection.cend()); }
    const_iterator end() const noexcept 
        { return const_iterator(collection.cend()); }
    iterator end() noexcept 
        { return iterator(collection.end()); }

    value_type* data() { return collection.data(); }
    value_type const* data() const { return collection.data(); }

private:
    joker_collection const* const_this() const { return this; }

    collection_type collection;
};

} // namespace

#undef LIKELY
#undef UNLIKELY
#endif
