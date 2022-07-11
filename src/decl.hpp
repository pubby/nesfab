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

#include "robin/map.hpp"

#include "phase.hpp"
#include "handle.hpp"
#include "bitset.hpp"

constexpr unsigned MAX_FN_ARGS = 32;
constexpr unsigned MAX_MEMBERS = 256;
constexpr unsigned MAX_ATOMS = 8;

struct group_t;
class global_t;
class fn_t;
class gvar_t;
class gmember_t;
class const_t;
class struct_t;
class group_vars_t;
class group_data_t;
struct field_t;
struct lt_value_t;

#define GLOBAL_CLASS_XENUM \
    X(GLOBAL_UNDEFINED) \
    X(GLOBAL_FN) \
    X(GLOBAL_VAR) \
    X(GLOBAL_CONST) \
    X(GLOBAL_STRUCT)

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

template<typename T>
struct impl_deque_t : public std::deque<T>
{
    using std::deque<T>::deque;
    using _valid = decltype(T::impl_deque_phase);
};

template<typename T>
struct impl_vector_t : public std::vector<T>
{
    using std::vector<T>::vector;
    using _valid = decltype(T::impl_vector_phase);
};

// These vectors hold global implementation data, 
// which varies depending on the global's type.
// The mutex is only used during the parsing phase.
template<typename T> inline std::mutex impl_deque_mutex;
template<typename T> inline impl_deque_t<T> impl_deque;

// Like 'impl_deque', this is used for some data:
template<typename T> inline impl_vector_t<T> impl_vector;

template<typename T, typename... Args>
static std::size_t impl_deque_alloc(T*& ptr, Args&&... args)
{
    std::lock_guard<std::mutex> lock(impl_deque_mutex<T>);
    std::size_t const ret = impl_deque<T>.size();
    ptr = &impl_deque<T>.emplace_back(std::forward<Args>(args)...);
    return ret;
}

template<typename T>
auto impl_bitset_size() -> typename std::pair<decltype(T::impl_deque_phase), std::size_t>::second_type
{
    assert(compiler_phase() > T::impl_deque_phase);
    return bitset_size<>(impl_deque<T>.size() + 1);
}

template<typename T>
auto impl_bitset_size() -> typename std::pair<decltype(T::impl_vector_phase), std::size_t>::second_type
{
    assert(compiler_phase() > T::impl_vector_phase);
    return bitset_size<>(impl_vector<T>.size() + 1);
}

template<typename T>
struct impl_ht : handle_t<unsigned, T, ~0>
{
    T* operator->() const { return &operator*(); }
    T& operator*() const { return unsafe(); }

    T& unsafe_impl() const { return impl_deque<T>[this->value]; }

    // This isn't thread safe without synchronization like impl_deque_mutex.
    T& unsafe() const { 
        assert(compiler_phase() > T::impl_deque_phase);
#ifndef NDEBUG
        if(this->value >= impl_deque<T>.size())
            std::fprintf(stderr, "Bad handle index = %i\n", this->value);
        assert(this->value < impl_deque<T>.size());
#endif
        return unsafe_impl();
    }

    // 'safe' meaning it's safe for the impl_deque.
    T& safe() const 
    { 
        if(compiler_phase() > T::impl_deque_phase)
            return unsafe_impl();
        std::lock_guard<std::mutex> lock(impl_deque_mutex<T>);
        return unsafe_impl(); 
    }
};

struct lt_ht : impl_ht<lt_value_t> {};

// Handles reference globals, with their '.value' indexing into 
// the corresponding 'imple_deque'.
template<typename T, global_class_t GCLASS>
struct global_impl_ht : impl_ht<T>
{
    static constexpr global_class_t gclass = GCLASS;
    using impl_type = T;
    using global_handle_tag = void;
    global_t& global() const { return this->operator*().global; }
};

// Handles reference groups with their '.value' indexing into 
// the corresponding 'impl_deque'.
template<typename T, group_class_t GCLASS>
struct group_impl_ht : impl_ht<T>
{
    static constexpr group_class_t gclass = GCLASS;
    using impl_type = T;
    using group_handle_tag = void;
    group_t& group() const { return this->operator*().group; }
};

template<typename T, compiler_phase_t PastPhase>
struct vector_impl_ht : handle_t<unsigned, T, ~0>
{
    T* operator->() const { return &operator*(); }
    T& operator*() const
    { 
        assert(compiler_phase() > PastPhase);
#ifndef NDEBUG
        if(this->value >= impl_vector<T>.size())
            std::fprintf(stderr, "Bad handle index = %i\n", this->value);
        assert(this->value < impl_vector<T>.size());
#endif
        return impl_vector<T>[this->value];
    }
};

struct fn_ht : global_impl_ht<fn_t, GLOBAL_FN> {};
struct gvar_ht : global_impl_ht<gvar_t, GLOBAL_VAR> {};
struct const_ht : global_impl_ht<const_t, GLOBAL_CONST> {};
struct struct_ht : global_impl_ht<struct_t, GLOBAL_STRUCT> {};
struct gmember_ht : vector_impl_ht<gmember_t, PHASE_COUNT_MEMBERS> {};

struct group_ht : impl_ht<group_t> 
{
    group_data_t* data() const; // Defined in group.cpp
};
struct group_vars_ht : group_impl_ht<group_vars_t, GROUP_VARS> {};
struct group_data_ht : group_impl_ht<group_data_t, GROUP_DATA> {};

namespace std
{
    template<> struct hash<fn_ht> : handle_hash_t<fn_ht> {};
    template<> struct hash<gvar_ht> : handle_hash_t<gvar_ht> {};
    template<> struct hash<const_ht> : handle_hash_t<const_ht> {};
    template<> struct hash<struct_ht> : handle_hash_t<struct_ht> {};
    template<> struct hash<gmember_ht> : handle_hash_t<gmember_ht> {};
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

enum fclass_t : char
{
    FN_FN,
    FN_CT,
    FN_MODE,
};

#endif
