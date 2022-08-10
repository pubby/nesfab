#ifndef HANDLE_HPP
#define HANDLE_HPP

// handle_t is a 'strong typedef' for integer types.
// It supports the same operations that pointers support.

// EXAMPLE
//   Instead of doing:
//     using my_value_type = int;
//   Do:
//     struct my_value_type : handle_t<my_value_type, int> {};

#include <functional>
#include <ostream>
#include <ranges>
#include <mutex>
#include <type_traits>

#include "robin/hash.hpp"

#include "assert.hpp"
#include "bitset.hpp"
#include "phase.hpp"

// Handles are wrappers around an int type.
template<typename Derived, typename Int, Int Null = 0, bool GT = false>
struct handle_t
{
    using is_handle_tag = void;
    using int_type = Int;
    int_type id = Null;

    static constexpr Int null = Null;

    constexpr explicit operator bool() const { return GT ? (id > Null) : (id != Null); }
    constexpr auto operator<=>(handle_t const&) const = default;
    constexpr bool operator!() const { return !operator bool(); }

    Derived& operator++() { ++id; return as_derived(); }
    Derived operator++(int) { handle_t h = as_derived(); ++id; return h; }
    Derived& operator--() { --id; return as_derived(); }
    Derived operator--(int) { handle_t h = as_derived(); --id; return h; }

    Derived& operator+=(int_type b) { id += b; return as_derived(); }
    Derived& operator-=(int_type b) { id -= b; return as_derived(); }

    friend constexpr Derived operator+(Derived a, int_type b) 
        { return { a.id + b }; }
    friend constexpr Derived operator+(int_type a, Derived b) 
        { return { a + b.id }; }

    friend constexpr Derived operator-(Derived a, int_type b) 
        { return { a.id - b }; }
    friend constexpr Derived operator-(int_type a, Derived b)
        { return { a - b.id }; }
    friend constexpr int_type operator-(Derived a, Derived b) 
        { return { a.id - b.id }; }

    std::size_t hash() const { return id; }

    constexpr Derived const& as_derived() const { return static_cast<Derived const&>(*this); }
    constexpr Derived& as_derived() { return static_cast<Derived&>(*this); }
};

template<typename Derived, typename Int, Int Null, bool GT>
std::ostream& operator<<(std::ostream& os, handle_t<Derived, Int, Null, GT> const& handle)
{
    os << "{" << handle.id << "}";
    return os;
}

template<typename T>
struct handle_hash_t
{
    using argument_type = T;
    using result_type = std::size_t;
    result_type operator()(argument_type const& handle) const noexcept { return handle.hash(); }
};

#define DEF_HANDLE_HASH(name) template<> struct std::hash<name> : handle_hash_t<name> {};

template<typename, typename = void>
struct is_handle : std::false_type {};

template<typename t>
struct is_handle<t, std::void_t<typename t::is_handle_tag>> : std::true_type {};

// A handle type that indexes into a vector-like pool.
template<typename Derived, typename Pool, compiler_phase_t Phase>
struct pool_handle_t : public handle_t<Derived, std::uint32_t, ~0u>
{
    using pool_type = Pool;
    using value_type = typename Pool::value_type;
    static constexpr compiler_phase_t phase = Phase;

    value_type* operator->() const { return &operator*(); }
    value_type& operator*() const { return unsafe(); }

    value_type& unsafe() const
    {
        assert(compiler_phase() > Phase);
        return unsafe_impl();
    }

    value_type& safe() const
    { 
        // TODO
        //if(compiler_phase() > Phase)
            //return unsafe_impl();
        std::lock_guard<std::mutex> lock(m_pool_mutex);
        return unsafe_impl(); 
    }

    // Sets 'ptr' to the address of the new value.
    template<typename... Args>
    static Derived pool_emplace(value_type*& ptr, Args&&... args)
    {
        assert(compiler_phase() <= Phase);
        std::lock_guard<std::mutex> lock(m_pool_mutex);
        Derived const ret = { m_pool.size() };
        ptr = &m_pool.emplace_back(std::forward<Args>(args)...);
        return ret;
    }

    template<typename... Args>
    static Derived pool_make(Args&&... args)
    {
        assert(compiler_phase() <= Phase);
        value_type* ptr;
        return pool_emplace(ptr, std::forward<Args>(args)...);
    }

    static std::size_t bitset_size() { assert(compiler_phase() > Phase); return m_listener.cached_bitset_size; }

    static Derived begin() { assert(compiler_phase() > Phase); return {0}; }
    static Derived end() { assert(compiler_phase() > Phase); return {pool().size()}; }
    static auto handles() { assert(compiler_phase() > Phase); return std::ranges::iota_view(begin(), end()); }
    static auto values() { assert(compiler_phase() > Phase); return std::ranges::subrange(m_pool.begin(), m_pool.end()); }
    static Pool const& pool() { assert(compiler_phase() > Phase); return m_pool; }

    template<typename Fn>
    static auto with_pool(Fn const& fn)
    {
        assert(compiler_phase() <= Phase);
        std::lock_guard<std::mutex> lock(m_pool_mutex);
        return fn(m_pool);
    }

    template<typename Fn>
    static auto with_const_pool(Fn const& fn)
    {
        std::lock_guard<std::mutex> lock(m_pool_mutex);
        return fn(static_cast<Pool const&>(m_pool));
    }

private:
    value_type& unsafe_impl() const 
    { 
        passert(this->id < m_pool.size(), "Bad handle index", this->id);
        return m_pool[this->id]; 
    }

    struct listener_t : public on_phase_change_t
    {
        virtual void on_change(compiler_phase_t from, compiler_phase_t to)
        {
            if(from <= Phase && to > Phase)
                cached_bitset_size = ::bitset_size<>(pool().size());
        }

        unsigned cached_bitset_size = 0;
    };

    inline static std::mutex m_pool_mutex;
    inline static Pool m_pool;
    inline static listener_t m_listener;
};

#endif
