#ifndef LOCATOR_HPP
#define LOCATOR_HPP

// Handles the use of global variables inside fn IR.

// Each read_global and write_global op take a locator_ht as their [1] input.
// This locator defines a set of global variables the read/writes apply to.

#include <cassert>
#include <cstdint>
#include <functional>
#include <ostream>
#include <vector>

#include "robin/hash.hpp"
#include "robin/map.hpp"

#include "array_pool.hpp"
#include "bitset.hpp"
#include "handle.hpp"
#include "ir_decl.hpp"
#include "types.hpp"

struct global_t;
using gvar_ht = handle_t<unsigned, struct gvar_ht_tag, ~0>;

// TODO: remove
//using locator_ht = handle_t<unsigned, struct locator_ht_tag, ~0u>;

enum locator_class_t : std::uint8_t
{
    LCLASS_NONE,

    LCLASS_GLOBAL,
    LCLASS_GLOBAL_SET,
    LCLASS_RAM,

    LCLASS_THIS_ARG,
    LCLASS_CALL_ARG,
    LCLASS_RETURN,

    LCLASS_CARRY, // TODO?

    LCLASS_PHI, // TODO?

    // Labels are uses during code gen.
    LCLASS_CFG_LABEL,
    LCLASS_MINOR_LABEL,
};

class locator_t
{
friend class locator_manager_t;
public:
    locator_t(gvar_ht gvar)
    : impl{ .index = gvar.value, .lclass = LCLASS_GLOBAL }
    {}

    locator_t(locator_t const&) = default;
    locator_t& operator=(locator_t const&) = default;

    constexpr locator_class_t lclass() const { return impl.lclass; }
    constexpr std::uint32_t index() const { return impl.index; }
    gvar_ht gvar() const 
    { 
        assert(lclass() == LCLASS_GLOBAL);
        return { impl.index }; 
    }
    global_t& global() const;

    std::uint16_t byte() const { return impl.byte; }
    void set_byte(std::uint16_t byte) { impl.byte = byte; }

    constexpr std::uint64_t to_uint() const { return intv; }
    constexpr static locator_t from_uint(std::uint64_t i);

    constexpr static locator_t null() { return locator_t(); }
    constexpr static locator_t this_arg(unsigned argn, unsigned byte=0);
    constexpr static locator_t call_arg(unsigned argn);
    constexpr static locator_t this_ret();
    constexpr static locator_t ret(unsigned byte=0);
    constexpr static locator_t carry(ssa_ht h);
    constexpr static locator_t phi(unsigned id);
    constexpr static locator_t cfg_label(cfg_ht cfg_node);
    constexpr static locator_t minor_label(unsigned id);

    constexpr bool is_label() const
    {
        return lclass() == LCLASS_CFG_LABEL || lclass() == LCLASS_MINOR_LABEL;
    }

    bool operator==(locator_t const& o) const 
        { return to_uint() == o.to_uint(); }
    bool operator!=(locator_t const& o) const 
        { return to_uint() != o.to_uint(); }
    bool operator<(locator_t const& o) const 
        { return to_uint() < o.to_uint(); }

    explicit operator bool() const { return intv; }

private:
    constexpr locator_t() : intv(0) {}

    struct [[gnu::packed]] impl_t
    {
        std::uint32_t index;
        std::uint16_t byte; // TODO: Used for structs and shit
        locator_class_t lclass;
        std::uint8_t unused;
    };

    union
    {
        impl_t impl;
        std::uint64_t intv;
    };

    static_assert(sizeof(impl_t) == sizeof(std::uint64_t));
};

namespace std
{
    template<>
    struct hash<locator_t>
    {
        std::size_t operator()(locator_t const& loc) const noexcept
        {
            return rh::hash_finalize(loc.to_uint());
        }
    };
}

std::string to_string(locator_t loc);
std::ostream& operator<<(std::ostream& o, locator_t loc);

inline constexpr locator_t locator_t::from_uint(std::uint64_t i)
{
    locator_t loc;
    loc.intv = i;
    return loc;
}

inline constexpr locator_t locator_t::this_arg(unsigned argn, unsigned byte)
{
    locator_t loc;
    loc.impl = { .index = argn, .byte = byte, .lclass = LCLASS_THIS_ARG };
    return loc;
}

inline constexpr locator_t locator_t::call_arg(unsigned argn)
{
    locator_t loc;
    loc.impl = { .index = argn, .byte = 0, .lclass = LCLASS_CALL_ARG };
    return loc;
}

inline constexpr locator_t locator_t::ret(unsigned byte)
{
    locator_t loc;
    loc.impl = { .index = 0, .byte = byte, .lclass = LCLASS_RETURN };
    return loc;
}

inline constexpr locator_t locator_t::carry(ssa_ht h)
{
    locator_t loc;
    loc.impl = { .index = h.index, .byte = 0, .lclass = LCLASS_CARRY };
    return loc;
}

inline constexpr locator_t locator_t::phi(unsigned id)
{
    locator_t loc;
    loc.impl = { .index = id, .byte = 0, .lclass = LCLASS_PHI };
    return loc;
}

inline constexpr locator_t locator_t::cfg_label(cfg_ht h)
{
    locator_t loc;
    loc.impl = { .index = h.index, .byte = 0, .lclass = LCLASS_CFG_LABEL };
    return loc;
}

inline constexpr locator_t locator_t::minor_label(unsigned id)
{
    locator_t loc;
    loc.impl = { .index = id, .byte = 0, .lclass = LCLASS_MINOR_LABEL };
    return loc;
}

class locator_manager_t
{
public:
    void setup(global_t const& global);

    bitset_uint_t const* get_set(locator_t loc) const
    {
        assert(loc.lclass() == LCLASS_GLOBAL_SET);
        return static_cast<bitset_uint_t const*>(locs[loc.index()]);
    }

    std::size_t size() const { return locs.size(); }

    locator_t locator(global_t const& global) const;
    locator_t locator(unsigned i) const;
    type_t type(unsigned i) const;

    unsigned index(global_t const& global) const
    { 
        auto const* pair = map.find(&global);
        assert(pair);
        return pair->second;
    }

private:
    array_pool_t<bitset_uint_t> bitset_pool;
    std::vector<void const*> locs;
    rh::robin_map<global_t const*, unsigned> map;
    unsigned first_set = 0;
};

#endif
