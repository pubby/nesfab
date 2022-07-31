#ifndef ROM_DECL_HPP
#define ROM_DECL_HPP

#include <cassert>
#include <cstdint>

#include "handle.hpp"

struct rom_alloc_t;
struct rom_static_t;
struct rom_many_t;
struct rom_once_t;
class rom_bank_t;
struct rom_alloc_t;
class rom_alloc_meta_t;
class rom_proc_t;
class rom_data_t;

struct rom_static_ht;
struct rom_many_ht;
struct rom_once_ht;

using rom_array_ht = handle_t<std::uint32_t, struct rom_array_tag, ~0>;
struct rom_proc_ht : public handle_t<std::uint32_t, struct rom_proc_tag, ~0>
{
    rom_proc_t* operator->() const { return &operator*(); }
    rom_proc_t& operator*() const;
};

template<> struct std::hash<rom_proc_ht> : handle_hash_t<rom_proc_ht> {};

enum rom_data_class_t : std::uint8_t
{
    ROMD_NONE = 0,
    ROMD_ARRAY,
    ROMD_PROC,
};

enum rom_alloc_class_t : std::uint8_t
{
    ROMA_NONE = 0,
    ROMA_STATIC,
    ROMA_MANY,
    ROMA_ONCE,
};

template<typename E>
class rom_handle_t
{
public:
    using rclass_t = E;

    rom_handle_t() = default;

    rom_handle_t(E rclass, std::uint32_t handle) { assign(rclass, handle); }

    void assign(E rclass, std::uint32_t handle)
    {
        impl = handle & 0xFFFFFF;
        assert(handle == impl);
        impl |= unsigned(rclass) << 24;
    }

    constexpr E rclass() const { return E(impl >> 24); }
    constexpr unsigned handle() const { return impl & 0xFFFFFF; }

    constexpr auto operator<=>(rom_handle_t const&) const = default;
    constexpr explicit operator bool() const { return rclass(); }

private:
    std::uint32_t impl = 0;
};

class rom_data_ht : public rom_handle_t<rom_data_class_t>
{
public:
    rom_data_ht() = default;

    rom_data_ht(rom_data_class_t rclass, std::uint32_t handle) { assign(rclass, handle); }
    rom_data_ht(rom_array_ht);
    rom_data_ht(rom_proc_ht);

    rom_data_t* get() const;

    unsigned max_size() const;
};

class rom_alloc_ht : public rom_handle_t<rom_alloc_class_t>
{
public:
    rom_alloc_ht() = default;

    rom_alloc_ht(rom_alloc_class_t rclass, std::uint32_t handle) { assign(rclass, handle); }
    rom_alloc_ht(rom_static_ht);
    rom_alloc_ht(rom_many_ht);
    rom_alloc_ht(rom_once_ht);

    rom_alloc_t* get() const;

    // Returns the bank number if it exists, -1 otherwise.
    int first_bank() const;
};


#endif
