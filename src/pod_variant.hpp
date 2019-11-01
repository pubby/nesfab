#ifndef POD_VARIANT_HPP
#define POD_VARIANT_HPP

#include <type_traits>
#include <utility>

template <typename T>
struct pod_optional_t
{
    static_assert(std::is_pod<pod_optional_t>::value);

    using storage_t = std::aligned_storage<sizeof(T), alignof(T)>::type;
    storage_t storage;

    void raw_create() { new(&storage) T; }

    template<typename... Args>
    void create(Args&&... args)
    { 
        new(&storage) T(std::forward<Args>(args)...); 
    }

    void destroy() { operator->()->~T(); }

    T const* operator->() const { reinterpret_cast<T const*>(&storage); }
    T* operator->() { reinterpret_cast<T*>(&storage); }

    T const& operator*() const { *operator->(); }
    T& operator*() { *operator->(); }
};

template <typename... Ts>
struct pod_variant_t
{
    static_assert(std::is_pod<pod_variant_t>::value);

    using storage_t = std::aligned_union<0, Ts...>;
    storage_t storage;

    template<typename T>
    void raw_create()
    { 
        static_assert(contains<T>());
        new(&storage) T; 
    }

    template<typename T, typename... Args>
    void create(Args&&... args)
    { 
        static_assert(contains<T>());
        new(&storage) T(std::forward<Args>(args)...); 
    }

    template<typename T>
    void destroy() 
    { 
        static_assert(contains<T>());
        get().~T(); 
    }

    template<typename T>
    T const& get() const 
    { 
        static_assert(contains<T>());
        *reinterpret_cast<T const*>(&storage); 
    }

    template<typename T>
    T& get() 
    { 
        static_assert(contains<T>());
        *reinterpret_cast<T*>(&storage); 
    }

private:
    template<typename T>
    constexpr bool contains()
        { return std::disjunction_v<std::is_same<T, Ts>...>; }
};

#endif
