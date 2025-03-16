#ifndef ROM_DECL_HPP
#define ROM_DECL_HPP

#include <cassert>
#include <cstdint>
#include <boost/container/deque.hpp>
#include <functional>

#include "handle.hpp"
#include "phase.hpp"
#include "bitset.hpp"

struct rom_alloc_t;
struct rom_static_t;
struct rom_many_t;
struct rom_once_t;
class rom_array_t;
class rom_proc_t;
class rom_data_t;
class rom_bank_t;

struct rom_static_ht;
struct rom_many_ht;
struct rom_once_ht;

struct rom_array_ht : public pool_handle_t<rom_array_ht, boost::container::deque<rom_array_t>, PHASE_ROM_DUMMY> {};
struct rom_proc_ht : public pool_handle_t<rom_proc_ht, boost::container::deque<rom_proc_t>, PHASE_ROM_DUMMY> {};

class locator_t;

DEF_HANDLE_HASH(rom_array_ht);
DEF_HANDLE_HASH(rom_proc_ht);

static constexpr unsigned max_banks = 256;
using bank_bitset_t = static_bitset_t<max_banks>;

// These are for different (duplicated) versions of the same data.
// i.e. one for code called from modes, another for code called from nmis
enum romv_t
{
    ROMV_MODE = 0,
    ROMV_NMI,
    ROMV_IRQ,
    NUM_ROMV,
};

using romv_flags_t = std::uint8_t;
constexpr romv_flags_t ROMVF_IN_MODE = 1 << ROMV_MODE;
constexpr romv_flags_t ROMVF_IN_NMI  = 1 << ROMV_NMI;
constexpr romv_flags_t ROMVF_IN_IRQ  = 1 << ROMV_IRQ;
constexpr romv_flags_t ROMVF_ALL = ROMVF_IN_MODE | ROMVF_IN_NMI | ROMVF_IN_IRQ;

constexpr int next_romv(romv_flags_t flags, unsigned romv)
{
    if(flags & romv)
        return romv;
    for(unsigned i = 0; i < NUM_ROMV; ++i)
        if(flags & (1 << i))
            return i;
    return -1;
}

template<typename Fn>
void romv_for_each(romv_flags_t flags, Fn const& fn)
{
    for(unsigned i = 0; i < NUM_ROMV; ++i)
        if(flags & (1 << i))
            fn(romv_t(i));
}

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

// Base of 'rom_data_ht' and 'rom_alloc_ht', used to reduce code duplication.
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
        if(handle != impl)
            impl = 0;
        else
        {
            impl |= std::uint32_t(rclass) << 24;
            assert(this->rclass() == rclass);
        }
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

    unsigned max_size(romv_t romv) const;

    void visit(std::function<void(rom_array_ht)> const& array_fn, 
               std::function<void(rom_proc_ht)> const& proc_fn) const;

    void for_each_locator(std::function<void(locator_t)> const& fn) const;
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

    int only_bank() const;

    void for_each_bank(std::function<void(unsigned)> const& fn);

    bank_bitset_t bank_bitset() const;
};

using romv_allocs_t = std::array<rom_alloc_ht, NUM_ROMV>;
#endif
