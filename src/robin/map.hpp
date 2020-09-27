#ifndef ROBIN_HOOD_MAP_HPP
#define ROBIN_HOOD_MAP_HPP

#include "apair.hpp"
#include "collection.hpp"

// Wrappers around collections which act more map-like.
// See collection.hpp for better comments.

namespace rh
{

template <typename Key, typename Mapped>
struct insertion_t
{
    using key_type = Key;
    using mapped_type = Mapped;
    using value_type = apair<key_type, mapped_type>;

    key_type const* key;
    mapped_type* mapped;
    bool inserted;
};

template<typename ValueType, typename Hash, typename KeyEqual>
struct map_policy
{
    using key_type = typename ValueType::first_type;
    using value_type = ValueType;
    using hasher = Hash;
    using key_equal = KeyEqual;

    static std::size_t hash(key_type const& k) 
    { 
        hasher h;
        return h(k); 
    }

    static std::size_t hash(value_type const& v) 
    { 
        hasher h;
        return h(v.first); 
    }

    static std::size_t equal(key_type const& a, value_type const& b) 
    { 
        KeyEqual eq;
        return eq(a, b.first);
    }

    static std::size_t equal(value_type const& a, value_type const& b) 
    { 
        KeyEqual eq;
        return eq(a.first, b.first);
    }
};

template
< typename Key
, typename Mapped
, typename Hash = std::hash<Key>
, typename KeyEqual = std::equal_to<Key>
>
class robin_map
{
public:
    using key_type = Key;
    using mapped_type = Mapped;
    using value_type = apair<key_type, mapped_type>;
    using insertion = rh::insertion_t<Key, Mapped>;
    using collection_type = 
        robin_collection<map_policy<value_type, Hash, KeyEqual>>;
    using hash_type = typename collection_type::hash_type;

    robin_map() = default;
    explicit robin_map(hash_type size) { reserve(size); }
    robin_map(robin_map const&) = default;
    robin_map(robin_map&&) = default;
    robin_map& operator=(robin_map const&) = default;
    robin_map& operator=(robin_map&&) = default;

    mapped_type& operator[](key_type const& k)
    {
        return *emplace(k, [](){ return mapped_type(); }).mapped;
    }

    insertion insert(value_type const& v)
    { 
        apair<value_type*, bool> pair = collection.insert(v); 
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    insertion insert(value_type&& v)
    { 
        apair<value_type*, bool> pair = collection.insert(std::move(v)); 
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    template<typename K, typename MConstruct>
    insertion emplace(K&& k, MConstruct mconstruct)
    {
        apair<value_type*, bool> pair = collection.emplace(
            k,
            [&](){ return value_type{ std::forward<K>(k), mconstruct() }; });
        return { &pair.first->first, &pair.first->second, pair.second };
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

    void swap(robin_map& o) noexcept { collection.swap(o.collection); }
    friend void swap(robin_map& a, robin_map& b) noexcept { a.swap(b); }

    std::size_t size() const { return collection.size(); }
    bool empty() const { return collection.empty(); }
    void reserve(hash_type size) { collection.reserve(size); }
private:
    collection_type collection;
};

template
< typename Key
, typename Mapped
, typename Hash = std::hash<Key>
, typename KeyEqual = std::equal_to<Key>
>
class batman_map
{
public:
    using key_type = Key;
    using mapped_type = Mapped;
    using value_type = apair<key_type, mapped_type>;
    using insertion = rh::insertion_t<Key, Mapped>;
    using collection_type = 
        batman_collection<map_policy<value_type, Hash, KeyEqual>>;
    using hash_type = typename collection_type::hash_type;
    using iterator = typename collection_type::iterator;
    using const_iterator = typename collection_type::const_iterator;

    batman_map() = default;
    explicit batman_map(hash_type size) { reserve(size); }
    batman_map(batman_map const&) = default;
    batman_map(batman_map&&) = default;
    batman_map& operator=(batman_map const&) = default;
    batman_map& operator=(batman_map&&) = default;

    mapped_type& operator[](key_type const& k)
    {
        return *emplace(k, [](){ return mapped_type(); }).mapped;
    }

    insertion insert(value_type const& v)
    { 
        apair<value_type*, bool> pair = collection.insert(v); 
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    insertion insert(value_type&& v)
    { 
        apair<value_type*, bool> pair = collection.insert(std::move(v)); 
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    template<typename K, typename MConstruct>
    insertion emplace(K&& k, MConstruct mconstruct)
    {
        apair<value_type*, bool> pair = collection.emplace(
            k,
            [&](){ return value_type{ std::forward<K>(k), mconstruct() }; });
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    value_type const* find(key_type const& k) const
    { 
        return collection.find(k);
    }

    bool remove(key_type const& k) { return collection.remove(k); }
    bool count(key_type const& k) const { return collection.count(k); }

    void clear() { collection.clear(); }
    void reset() { collection.reset(); }

    void swap(batman_map& o) noexcept { collection.swap(o.collection); }
    friend void swap(batman_map& a, batman_map& b) noexcept { a.swap(b); }

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
, typename Mapped
, typename Hash = std::hash<Key>
, typename KeyEqual = std::equal_to<Key>
>
class joker_map
{
public:
    using key_type = Key;
    using mapped_type = Mapped;
    using value_type = apair<key_type, mapped_type>;
    using insertion = rh::insertion_t<Key, Mapped>;
    using collection_type = 
        joker_collection<map_policy<value_type, Hash, KeyEqual>>;
    using hash_type = typename collection_type::hash_type;
    using iterator = typename collection_type::iterator;
    using const_iterator = typename collection_type::const_iterator;

    joker_map() = default;
    explicit joker_map(hash_type size) { reserve(size); }
    joker_map(joker_map const&) = default;
    joker_map(joker_map&&) = default;
    joker_map& operator=(joker_map const&) = default;
    joker_map& operator=(joker_map&&) = default;

    mapped_type& operator[](key_type const& k)
    {
        return *emplace(k, [](){ return mapped_type(); }).mapped;
    }

    insertion insert(value_type const& v)
    { 
        apair<value_type*, bool> pair = collection.insert(v); 
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    insertion insert(value_type&& v)
    { 
        apair<value_type*, bool> pair = collection.insert(std::move(v)); 
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    template<typename K, typename MConstruct>
    insertion emplace(K&& k, MConstruct mconstruct)
    {
        apair<value_type*, bool> pair = collection.emplace(
            k,
            [&](){ return value_type{ std::forward<K>(k), mconstruct() }; });
        return { &pair.first->first, &pair.first->second, pair.second };
    }

    value_type const* find(key_type const& k) const
    { 
        return collection.find(k);
    }

    bool remove(key_type const& k) { return collection.remove(k); }
    bool count(key_type const& k) { return collection.count(k); }

    void clear() { collection.clear(); }
    void reset() { collection.reset(); }

    void swap(joker_map& o) noexcept { collection.swap(o.collection); }
    friend void swap(joker_map& a, joker_map& b) noexcept { a.swap(b); }

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
