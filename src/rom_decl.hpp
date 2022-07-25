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
struct rom_static_ht;
struct rom_many_ht;
struct rom_once_ht;

using rom_array_ht = handle_t<unsigned, struct rom_array_tag, ~0>;

enum rom_alloc_class_t : char
{
    ROM_NONE = 0,
    ROM_STATIC,
    ROM_MANY,
    ROM_ONCE,
};

class rom_alloc_ht
{
public:
    rom_alloc_ht() = default;

    rom_alloc_ht(rom_alloc_class_t rclass, std::uint32_t handle) { assign(rclass, handle); }
    rom_alloc_ht(rom_static_ht);
    rom_alloc_ht(rom_many_ht);
    rom_alloc_ht(rom_once_ht);

    void assign(rom_alloc_class_t rclass, std::uint32_t handle)
    {
        impl = handle & 0xFFFFFF;
        assert(handle == impl);
        impl |= unsigned(rclass) << 24;
    }

    void assign(rom_static_ht h);
    void assign(rom_many_ht h);
    void assign(rom_once_ht h);

    constexpr rom_alloc_class_t rclass() const { return rom_alloc_class_t(impl >> 24); }
    constexpr unsigned handle() const { return impl & 0xFFFFFF; }

    rom_alloc_t* get() const;

    // Returns the bank number if it exists, -1 otherwise.
    int first_bank() const;

    constexpr auto operator<=>(rom_alloc_ht const&) const = default;

    constexpr explicit operator bool() const { return rclass(); }

private:
    std::uint32_t impl = 0;
};


#endif
