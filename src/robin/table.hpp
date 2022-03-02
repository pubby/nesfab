#ifndef ROBIN_HOOD_TABLE_HPP
#define ROBIN_HOOD_TABLE_HPP

// Implements the storage and structure of linearly probed robin-hood 
// hash tables in a low-level manner.
// These aren't really general-use containers; see collection.hpp,
// set.hpp, and map.hpp for those.

#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <memory>
#include <ratio>
#include <type_traits>
#include <utility>

#include "apair.hpp"

#define LIKELY(b) __builtin_expect(bool(b), true)
#define UNLIKELY(b) __builtin_expect(bool(b), false)

namespace rh
{

struct c_delete { void operator()(void* ptr) { std::free(ptr); } };

constexpr std::uint64_t next_pow2(std::uint64_t v)
{
    --v;
    v |= v >> 1ull;
    v |= v >> 2ull;
    v |= v >> 4ull;
    v |= v >> 8ull;
    v |= v >> 16ull;
    v |= v >> 32ull;
    return v + 1;
}

template<typename I>
constexpr I int_ceil(I n, I d) { return ((n + d - 1) / d) * d; }

template<typename T, std::size_t Align = sizeof(std::max_align_t)>
T* table_alloc(std::size_t num)
{
    if(num == 0)
        return nullptr;
    if(Align > sizeof(std::max_align_t))
    {
        constexpr std::size_t alignment = 
            next_pow2(int_ceil(Align, sizeof(void*)));
        void* ptr = nullptr;
        if(!posix_memalign(&ptr, alignment, int_ceil(num, alignment)))
            return reinterpret_cast<T*>(ptr);
    }
    else if(void* ptr = std::malloc(num * sizeof(T)))
        return reinterpret_cast<T*>(ptr);
    throw std::bad_alloc();
}

template<typename T, std::size_t Align = sizeof(std::max_align_t)>
T* table_calloc(std::size_t num)
{
    if(num == 0)
        return nullptr;
    if(Align > sizeof(std::max_align_t))
    {
        T* ret = table_alloc<T>(num);
        std::memset(ret, 0, num * sizeof(T));
        return ret;
    }
    else if(void* ptr = std::calloc(num, sizeof(T)))
        return reinterpret_cast<T*>(ptr);
    throw std::bad_alloc();
}

// robin_table is pretty dumb. It doesn't rehash automatically, or do
// things you would conveniently expect a hash table to do.
// Use it for low-level implementation of higher-level containers.
// 
// The size of the table is a power of two, plus some amount of 
// overhang to handle collisions at the end.
// It is believed to be faster to do it this way than implementing wrap-around.
template<typename T, typename UIntType = std::uint32_t>
class robin_table
{
public:
    static_assert(std::is_unsigned<UIntType>::value);
    using value_type = T;
    using hash_type = UIntType;
    using value_storage = std::array<unsigned char, sizeof(value_type)>;

    robin_table() 
    : values()
    , hashes(&null_hash)
    , hashes_end_(&null_hash)
    , mask_(0)
    {}

    robin_table(hash_type size, hash_type mask)
    : values(table_alloc<value_storage, alignof(value_type)>(size))
    , hashes(table_calloc<hash_type>(size+1))
    , hashes_end_(hashes + size)
    , mask_(mask)
    {
        // One extra element was allocated.
        // It will be set to zero so that 
        // fast find becomes possible.
        *hashes_end_ = 0;
    }
    
    robin_table(robin_table const&) = delete;

    robin_table(robin_table&& o) { move_impl(o); }

    ~robin_table()
    {
        clear_impl<false>();
        free_memory();
    }

    robin_table& operator=(robin_table const&) = delete;

    robin_table& operator=(robin_table&& o)
    {
        clear_impl<false>();
        free_memory();
        move_impl(o);
        return *this;
    }

    void clear() { clear_impl<true>(); }

    // !!DANGEROUS!!
    // This function is a faster version of emplace 
    // that only works when the element was not already in the container. 
    // If the element DOES exist, then shit's fucked.
    // (this exists for use in rehashing, where it's a small optimization)
    template<typename C>
    [[gnu::always_inline]] inline apair<value_type*, bool> 
    emplace_unique(hash_type hash, C const& construct)
    {
        return emplace<unique_t>(hash, {}, construct);
    }

    template<typename Eq, typename Construct>
    inline apair<value_type*, bool> 
    emplace(hash_type hash, Eq const& equals, Construct const& construct)
    {
        hash_type* hash_ptr = hashes + (hash & mask_);
        hash &= ~mask_;
        for(hash_type i = 1;; ++i, ++hash_ptr)
        {
            if(!std::is_same<Eq, unique_t>::value && *hash_ptr == (hash | i))
            {
                // Matching hashes; check for further equality.
                value_type* value_ptr = get_value(hash_ptr);
                if(LIKELY(equals(*value_ptr)))
                    return { value_ptr, false };
            }
            else if((*hash_ptr & mask_) < i)
            {
                hash |= i;
                value_type* value_ptr = get_value(hash_ptr);
                if(*hash_ptr == 0)
                {
                    new((void*)value_ptr) value_type(construct());
                     // Set the hash after constructing, otherwise the 
                     // destructor is unsafe.
                    *hash_ptr = hash;
                    return { value_ptr, true };
                }
                else
                {
                    // Shift hashes right.
                    do 
                        std::swap(hash, *(hash_ptr++)), ++hash;
                    while(*hash_ptr);

                    // Shift values right.
                    value_type* copy_to = get_value(hash_ptr);
                    value_type* value_last = copy_to - 1;

                    // Construct a new value on the far right.
                    new((void*)copy_to--) value_type(std::move(*value_last--));
                     // Set the hash after constructing, otherwise the 
                     // destructor is unsafe.
                    *hash_ptr = hash;

                    while(copy_to > value_ptr)
                        *(copy_to--) = std::move(*(value_last--));

                    // Value are shifted, now we can move-assign the hole.
                    *value_ptr = value_type(construct());
                    return { value_ptr, true };
                }
            }
        }
    }

    template<typename Eq>
    apair<hash_type const*, value_type const*> 
    find(hash_type hash, Eq const& equals) const
    {
        hash_type* hash_ptr = hashes + (hash & mask_);
        hash &= ~mask_;
        for(hash_type i = 1;; ++i, ++hash_ptr)
        {
            if((*hash_ptr & mask_) < i)
                return { nullptr, nullptr };
            else if(*hash_ptr == (hash | i))
            {
                value_type const* value_ptr = get_value(hash_ptr);
                if(LIKELY(equals(*value_ptr)))
                    return { hash_ptr, value_ptr };
            }
        }
    }

    template<typename Eq>
    apair<hash_type*, value_type*> 
    find(hash_type hash, Eq const& equals)
    {
        auto pair = const_this()->find(hash, equals);
        return make_apair(
            const_cast<hash_type*>(pair.first),
            const_cast<value_type*>(pair.second));
    }

    void erase(apair<hash_type*, value_type*> pair)
    {
        while((*(pair.first+1) & mask_) > 1)
        {
            *(pair.first) = *(pair.first+1) - 1;
            ++pair.first;
            *(pair.second) = std::move(*(pair.second+1));
            ++pair.second;
        }
        pair.second->~value_type();
        *pair.first = 0;
    }

    // Enlarges table with rehashing.
    void rehash(hash_type new_size, hash_type new_mask)
    {
        hash_type const old_size = allocated_size();
        if(old_size >= new_size)
            return;

        robin_table new_table(new_size, new_mask);
        for(hash_type i = 0; i != old_size; ++i)
        {
            if(hashes[i] == 0)
                continue;
            hash_type hash = i - ((hashes[i] & mask_) - 1);
            hash |= (hashes[i] & ~mask_);
            new_table.emplace_unique(hash, 
                [&]() -> value_type&& { return std::move(value_data()[i]); });
            hashes[i] = 0;
        }
        *this = std::move(new_table);
    }

    // Enlarges the end of the table without rehashing.
    // (The table isn't circular and the buffer is larger than the hash space.
    // Elements can extend far past the upper side. This function expands 
    // that space, without expanding the hash space.)
    void realloc(hash_type new_size)
    {
        hash_type const old_size = allocated_size();
        if(old_size >= new_size)
            return;

        if(std::is_trivially_copyable<value_type>::value
           && alignof(value_type) <= sizeof(std::max_align_t))
        {
            // realloc hashes
            hash_type* const old_hashes = hashes == &null_hash ? nullptr 
                                                               : hashes;
            hashes = reinterpret_cast<hash_type*>(
                std::realloc(old_hashes, (new_size+1) * sizeof(hash_type)));
            if(UNLIKELY(!hashes))
            {
                hashes = old_hashes;
                throw std::bad_alloc();
            }
            hashes_end_ = hashes + new_size;
            *hashes_end_ = 0;
            std::fill(hashes + old_size, hashes + new_size, 0);

            // realloc values
            value_storage* const old_values = values.release();
            values.reset(reinterpret_cast<value_storage*>(
                std::realloc(old_values, new_size * sizeof(value_type))));
            if(UNLIKELY(!values))
            {
                values.reset(old_values);
                throw std::bad_alloc();
            }
        }
        else
        {
            robin_table new_table(new_size, mask_);
            for(hash_type i = 0; i != old_size; ++i)
            {
                if(hash_data()[i])
                    continue;
                new((void*)(new_table.values.get() + i))
                    value_type(std::move(value_data()[i]));
                value_data()[i].~value_type();
                new_table.hash_data()[i] = hash_data()[i];
                hash_data()[i] = 0;
            }
            *this = std::move(new_table);
        }
    }

    template<typename Ratio>
    void reserve(std::size_t size, hash_type used_size)
    {
        if(size < mask_ + 1)
            return;

        size *= Ratio::den;
        size += Ratio::num - 1;
        size /= Ratio::num;
        size = next_pow2(std::max<hash_type>(starting_size, size));

        rehash(size + std::max<hash_type>(overhang(), starting_overhang), 
               size - 1);
    }

    void grow_rehash()
    {
        hash_type new_mask = (mask_ << 1) | (starting_size - 1);
        hash_type new_size = new_mask + 1;
        new_size += std::max<hash_type>(overhang(), starting_overhang);
        rehash(new_size, new_mask);
    }

    void grow_realloc()
    {
        hash_type const new_size = 
           mask_ + 1 + std::max<hash_type>(overhang() * 4, starting_overhang);
        realloc(new_size);
    }

    hash_type const* hash_data() const { return hashes; }
    hash_type* hash_data() { return hashes; }

    hash_type const* hashes_end() const { return hashes_end_; }
    hash_type* hashes_end() { return hashes_end_; }

    value_type const* value_data() const 
        { return reinterpret_cast<value_type const*>(values.get()); }
    value_type* value_data()
        { return reinterpret_cast<value_type*>(values.get()); }

    void swap(robin_table& o) noexcept
    {
        using std::swap;
        values.swap(o.values);
        swap(hashes, o.hashes);
        swap(hashes_end_, o.hashes_end_);
        swap(mask_, o.mask_);
    }

    hash_type mask() const { return mask_; }

    hash_type const* get_hash(value_type const* ptr) const
        { return hash_data() + (ptr - (value_type const*)values.get()); }
    hash_type* get_hash(value_type* ptr) 
        { return hash_data() + (ptr - (value_type*)values.get()); }

    value_type const* get_value(hash_type const* ptr) const
        { return value_data() + (ptr - hashes); }
    value_type* get_value(hash_type* ptr) 
        { return value_data() + (ptr - hashes); }

    std::size_t allocated_size() const
        { return hashes_end_ - hashes; }

    std::size_t overhang() const
        { return allocated_size() > mask_ ? allocated_size() - (mask_+1) : 0; }

protected:
    void free_memory()
    {
        if(hashes && hashes != &null_hash)
        {
            std::free(hashes);
            hashes = hashes_end_ = &null_hash;
        }
    }

    template<bool ClearHash>
    void clear_impl()
    {
        for(hash_type* ptr = hashes; ptr != hashes_end_; ++ptr)
        {
            if(*ptr)
                get_value(ptr)->~value_type();
            if(ClearHash)
                *ptr = 0;
        }
    }

    void move_impl(robin_table& o)
    {
        values = std::move(o.values);
        assert(!o.values);
        hashes = o.hashes;
        hashes_end_ = o.hashes_end_;
        mask_ = o.mask_;

        o.hashes = o.hashes_end_ = &null_hash;
        o.mask_ = 0;
    }

    static constexpr hash_type starting_size = 64;
    static constexpr hash_type starting_overhang = 32;

    struct unique_t
    {
        template<typename A>
        bool operator()(A const&) const { return false; }
    };

    robin_table const* const_this() const { return this; }

    static hash_type null_hash;

    std::unique_ptr<value_storage> values;
    hash_type* hashes;
    hash_type* hashes_end_;
    hash_type mask_;
};

template<typename T, typename UIntType>
UIntType robin_table<T, UIntType>::null_hash = 0;


// Small wrapper around robin_table which tracks the number of elements 
// inserted and automatically enlarges the table as new elements are inserted.
template<typename T, typename UIntType = std::uint32_t>
class robin_auto_table
{
public:
    static_assert(std::is_unsigned<UIntType>::value);
    using value_type = T;
    using hash_type = UIntType;
    using ratio_type = std::ratio<1, 2>;

    robin_auto_table() = default;
    robin_auto_table(robin_auto_table const&) = default;
    robin_auto_table(robin_auto_table&& o) 
    : table(std::move(o.table))
    {
        used_size = o.used_size;
        rehash_size = o.rehash_size;
        o.used_size = 0;
        o.rehash_size = 0;
    }

    robin_auto_table& operator=(robin_auto_table const& o) = default;
    robin_auto_table& operator=(robin_auto_table&& o) 
    {
        table = std::move(o.table);
        used_size = o.used_size;
        rehash_size = o.rehash_size;
        o.used_size = 0;
        o.rehash_size = 0;
        return *this;
    }

    template<typename Eq, typename C>
    apair<value_type*, bool> 
    emplace(hash_type hash, Eq const& equals, C const& construct)
    {
        if(UNLIKELY(used_size+1 > rehash_size))
        {
            table.grow_rehash();
            rehash_size = calc_rehash_size();
        }
        auto ret = table.emplace(hash, equals, construct);
        if(ret.second)
        {
            ++used_size;
            if(UNLIKELY(table.get_hash(ret.first) + 2 > table.hashes_end()))
                table.grow_realloc();
        }
        return ret; 
    }

    template<typename Eq>
    apair<hash_type const*, value_type const*> 
    find(hash_type hash, Eq const& equals) const
    {
        return table.find(hash, equals);
    }

    template<typename Eq>
    apair<hash_type*, value_type*> 
    find(hash_type hash, Eq const& equals)
    {
        return table.find(hash, equals);
    }

    void erase(apair<hash_type*, value_type*> pair)
    {
        table.erase(pair);
        --used_size;
    }

    void clear() // Doesn't free memory.
    {
        table.clear();
        used_size = 0;
    }

    void reset() // Frees memory.
    {
        table = robin_table<value_type>();
        used_size = 0;
        rehash_size = 0;
    }

    void swap(robin_auto_table& o) noexcept
    {
        using std::swap;
        table.swap(o.table);
        swap(used_size, o.used_size);
        swap(rehash_size, o.rehash_size);
    }

    void reserve(hash_type size)
    {
        table.template reserve<ratio_type>(size, used_size);
        rehash_size = calc_rehash_size();
    }

    std::size_t size() const { return used_size; }
private:
    hash_type calc_rehash_size() const
        { return (table.mask() + 1) * ratio_type::num / ratio_type::den; }

    robin_table<T, UIntType> table;
    hash_type used_size = 0;
    hash_type rehash_size = 0;
};

} // namespace

#undef LIKELY
#undef UNLIKELY
#endif

