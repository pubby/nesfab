#ifndef ROBIN_HOOD_SET_HPP
#define ROBIN_HOOD_SET_HPP

#include "collection.hpp"

// Wrappers around collections which act more set-like.
// See collection.hpp for better comments.

namespace rh
{

template<typename Key, typename Hash, typename KeyEqual>
struct set_policy
{
    using value_type = Key;
    using hasher = Hash;
    using key_equal = KeyEqual;

    static std::size_t hash(value_type const& v) 
    { 
        hasher h;
        return h(v); 
    }

    static std::size_t equal(value_type const& a, value_type const& b) 
    { 
        KeyEqual eq;
        return eq(a, b);
    }
};

template
< typename Key
, typename Hash = std::hash<Key>
, typename KeyEqual = std::equal_to<Key>
>
class robin_set
{
public:
    using key_type = Key;
    using value_type = Key;
    using collection_type = robin_collection<set_policy<Key, Hash, KeyEqual>>;
    using hash_type = typename collection_type::hash_type;

    robin_set() = default;
    explicit robin_set(hash_type size) { reserve(size); }
    robin_set(robin_set const&) = default;
    robin_set(robin_set&&) = default;
    robin_set& operator=(robin_set const&) = default;
    robin_set& operator=(robin_set&&) = default;

    apair<value_type const*, bool> insert(key_type const& k)
    { 
        apair<value_type*, bool> pair = collection.insert(k); 
        return { pair.first, pair.second };
    }

    apair<value_type const*, bool> insert(key_type&& k)
    { 
        apair<value_type*, bool> pair = collection.insert(std::move(k)); 
        return { pair.first, pair.second };
    }

    value_type const* find(key_type const& k) const
    { 
        return collection.find(k);
    }

    void erase(value_type const& v) { collection.erase(v); }
    bool remove(key_type const& k) { return collection.remove(k); }
    bool count(key_type const& k) const { return collection.count(k); }

    void clear() { collection.clear(); }
    void reset() { collection.reset(); }

    void swap(robin_set& o) noexcept { collection.swap(o.collection); }
    friend void swap(robin_set& a, robin_set& b) noexcept { a.swap(b); }

    std::size_t size() const { return collection.size(); }
    bool empty() const { return collection.empty(); }
    void reserve(hash_type size) { collection.reserve(size); }
private:
    collection_type collection;
};

template
< typename Key
, typename Hash = std::hash<Key>
, typename KeyEqual = std::equal_to<Key>
>
class batman_set
{
public:
    using key_type = Key;
    using value_type = Key;
    using collection_type = batman_collection<set_policy<Key, Hash, KeyEqual>>;
    using hash_type = typename collection_type::hash_type;
    using iterator = typename collection_type::iterator;
    using const_iterator = typename collection_type::const_iterator;

    batman_set() = default;
    explicit batman_set(hash_type size) { reserve(size); }
    batman_set(batman_set const&) = default;
    batman_set(batman_set&&) = default;
    batman_set& operator=(batman_set const&) = default;
    batman_set& operator=(batman_set&&) = default;

    apair<value_type const*, bool> insert(key_type const& k)
    { 
        apair<value_type*, bool> pair = collection.insert(k); 
        return { pair.first, pair.second };
    }

    apair<value_type const*, bool> insert(key_type&& k)
    { 
        apair<value_type*, bool> pair = collection.insert(std::move(k)); 
        return { pair.first, pair.second };
    }

    value_type const* find(key_type const& k) const
    { 
        return collection.find(k);
    }

    bool remove(key_type const& k) { return collection.remove(k); }
    bool count(key_type const& k) const { return collection.count(k); }

    void clear() { collection.clear(); }
    void reset() { collection.reset(); }

    void swap(batman_set& o) noexcept { collection.swap(o.collection); }
    friend void swap(batman_set& a, batman_set& b) noexcept { a.swap(b); }

    std::size_t size() const { return collection.size(); }
    bool empty() const { return collection.empty(); }
    void reserve(hash_type size) { collection.reserve(size); }

    const_iterator cbegin() const { return collection.cbegin(); }
    const_iterator begin() const { return collection.cbegin(); }
    iterator begin() { return collection.begin(); }
    const_iterator cend() const { return collection.cend(); }
    const_iterator end() const { return collection.cend(); }
    iterator end() { return collection.end(); }
private:
    collection_type collection;
};

template
< typename Key
, typename Hash = std::hash<Key>
, typename KeyEqual = std::equal_to<Key>
>
class joker_set
{
public:
    using key_type = Key;
    using value_type = Key;
    using collection_type = joker_collection<set_policy<Key, Hash, KeyEqual>>;
    using hash_type = typename collection_type::hash_type;
    using iterator = typename collection_type::iterator;
    using const_iterator = typename collection_type::const_iterator;

    joker_set() = default;
    explicit joker_set(hash_type size) { reserve(size); }
    joker_set(joker_set const&) = default;
    joker_set(joker_set&&) = default;
    joker_set& operator=(joker_set const&) = default;
    joker_set& operator=(joker_set&&) = default;

    apair<value_type const*, bool> insert(key_type const& k)
    { 
        apair<value_type*, bool> pair = collection.insert(k); 
        return { pair.first, pair.second };
    }

    apair<value_type const*, bool> insert(key_type&& k)
    { 
        apair<value_type*, bool> pair = collection.insert(std::move(k)); 
        return { pair.first, pair.second };
    }

    value_type const* find(key_type const& k) const
    { 
        return collection.find(k);
    }

    bool remove(key_type const& k) { return collection.remove(k); }
    bool count(key_type const& k) { return collection.count(k); }

    void clear() { collection.clear(); }
    void reset() { collection.reset(); }

    void swap(joker_set& o) noexcept { collection.swap(o.collection); }
    friend void swap(joker_set& a, joker_set& b) noexcept { a.swap(b); }

    std::size_t size() const { return collection.size(); }
    bool empty() const { return collection.empty(); }
    void reserve(hash_type size) { collection.reserve(size); }

    const_iterator cbegin() const { return collection.cbegin(); }
    const_iterator begin() const { return collection.cbegin(); }
    iterator begin() { return collection.begin(); }
    const_iterator cend() const { return collection.cend(); }
    const_iterator end() const { return collection.cend(); }
    iterator end() { return collection.end(); }
private:
    collection_type collection;
};

} // namespace

#endif
