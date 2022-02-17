#ifndef DECL_HPP
#define DECL_HPP

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <deque>
#include <type_traits>

#include "phase.hpp"
#include "handle.hpp"

struct group_t;
class global_t;
class fn_t;
class gvar_t;
class const_t;
class group_vars_t;
class group_data_t;

#define GLOBAL_CLASS_XENUM \
    X(GLOBAL_UNDEFINED) \
    X(GLOBAL_FN) \
    X(GLOBAL_VAR) \
    X(GLOBAL_CONST)

enum global_class_t : std::uint8_t
{
#define X(x) x,
    GLOBAL_CLASS_XENUM
#undef X
};

enum group_class_t : std::uint8_t
{
    GROUP_UNDEFINED = 0,
    GROUP_VARS,
    GROUP_DATA,
};

// These vectors hold global implementation data, 
// which varies depending on the global's type.
// The mutex is only used during the parsing phase.
template<typename T> inline std::mutex impl_deque_mutex;
template<typename T> inline std::deque<T> impl_deque;

template<typename T, typename... Args>
static T& impl_deque_alloc(Args&&... args)
{
    std::lock_guard<std::mutex> lock(impl_deque_mutex<T>);
    return impl_deque<T>.emplace_back(std::forward<Args>(args)...);
}

template<typename T>
std::size_t impl_bitset_size()
{
    assert(compiler_phase() > PHASE_PARSE);
    return bitset_size<>(impl_deque<T>.size());
}

template<typename T>
struct impl_ht : handle_t<unsigned, T, ~0>
{
    T* operator->() const { return &operator*(); }
    T& operator*() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
#ifndef NDEBUG
        if(this->value >= impl_deque<T>.size())
            std::fprintf(stderr, "Bad handle index = %i\n", this->value);
        //assert(this->value < impl_deque<T>.size());
#endif
        return impl_deque<T>[this->value];
    }

    // This isn't thread safe without synchronization like impl_deque_mutex.
    T& unsafe() const { return impl_deque<T>[this->value]; }
};

// Handles reference globals, with their '.value' indexing into 
// the corresponding 'imple_deque'.
template<typename T, global_class_t GCLASS>
struct global_impl_ht : impl_ht<T>
{
    static constexpr global_class_t gclass = GCLASS;
    using global_handle_tag = void;
    global_t& global() const { return this->operator*().global; }
};

// Handles reference groups with their '.value' indexing into 
// the corresponding 'impl_deque'.
template<typename T, group_class_t GCLASS>
struct group_impl_ht : impl_ht<T>
{
    static constexpr group_class_t gclass = GCLASS;
    using group_handle_tag = void;
    group_t& group() const { return this->operator*().group; }
};

struct fn_ht : global_impl_ht<fn_t, GLOBAL_FN> {};
struct gvar_ht : global_impl_ht<gvar_t, GLOBAL_VAR> {};
struct const_ht : global_impl_ht<const_t, GLOBAL_CONST> {};

struct group_ht : impl_ht<group_t> {};
struct group_vars_ht : group_impl_ht<group_vars_t, GROUP_VARS> {};
struct group_data_ht : group_impl_ht<group_data_t, GROUP_DATA> {};

namespace std
{
    template<> struct hash<fn_ht> : handle_hash_t<fn_ht> {};
    template<> struct hash<gvar_ht> : handle_hash_t<gvar_ht> {};
    template<> struct hash<const_ht> : handle_hash_t<const_ht> {};
    template<> struct hash<group_ht> : handle_hash_t<group_ht> {};
    template<> struct hash<group_vars_ht> : handle_hash_t<group_vars_ht> {};
    template<> struct hash<group_data_ht> : handle_hash_t<group_data_ht> {};
}

template<typename, typename = void>
struct is_global_handle 
: std::false_type {};

template<typename t>
struct is_global_handle<t, std::void_t<typename t::global_handle_tag>>
: std::true_type {};

template<typename, typename = void>
struct is_global_impl 
: std::false_type {};

template<typename t>
struct is_global_impl<t, std::void_t<typename t::global_impl_tag>>
: std::true_type {};

template<typename, typename = void>
struct is_group_handle 
: std::false_type {};

template<typename t>
struct is_group_handle<t, std::void_t<typename t::group_handle_tag>>
: std::true_type {};

template<typename, typename = void>
struct is_group_impl 
: std::false_type {};

template<typename t>
struct is_group_impl<t, std::void_t<typename t::group_impl_tag>>
: std::true_type {};

#endif
