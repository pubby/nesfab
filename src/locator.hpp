#ifndef LOCATOR_HPP
#define LOCATOR_HPP

// Handles the use of global variables inside fn IR.

// Each read_global and write_global op take a locator_ht as their [1] input.
// This locator defines a set of global variables the read/writes apply to.

#include <cassert>
#include <cstdint>
#include <string>
#include <ostream>
#include <functional>

#include "robin/hash.hpp"

#include "addr16.hpp"
#include "decl.hpp"
#include "ir_decl.hpp"

class type_t;
struct ssa_value_t;

enum locator_class_t : std::uint8_t
{
    LOC_NONE,

    LOC_IOTA, 

    LOC_FN, // A function.
    LOC_GVAR, // A global variable.
    //LOC_CONST,

    // When a function calls another function, 
    // the IR tracks which gvars are used in that function.
    // Rather than requiring one locator per gvar, a GVAR_SET can be used
    // which combines several gvars into a single locator.
    // (This is a size optimization)
    LOC_GVAR_SET,

    LOC_ARG,
    LOC_RETURN,

    LOC_PHI, // TODO?

    // Used to allocate local vars
    LOC_LOCAL,

    // Labels are used during code gen. They map to assembly terms.
    LOC_CFG_LABEL,
    LOC_MINOR_LABEL,

    LOC_CONST_BYTE,
    LOC_RELOCATION_ADDR,

    LOC_GLOBAL_CONST,

    LOC_SSA,
};

constexpr bool is_label(locator_class_t lclass)
{
    return lclass == LOC_CFG_LABEL || lclass == LOC_MINOR_LABEL;
}

constexpr bool is_const(locator_class_t lclass)
{
    return lclass == LOC_CONST_BYTE;
}

constexpr bool has_arg_atom(locator_class_t lclass)
{
    switch(lclass)
    {
    case LOC_GVAR:
    case LOC_ARG:
    case LOC_RETURN:
    case LOC_LOCAL:
        return true;
    default:
        return false;
    }
}

constexpr bool has_fn(locator_class_t lclass)
{
    switch(lclass)
    {
    case LOC_FN:
    case LOC_ARG:
    case LOC_RETURN:
    case LOC_PHI:
    case LOC_CFG_LABEL:
    case LOC_MINOR_LABEL:
        return true;
    default:
        return false;
    }
}

class locator_t
{
friend class gvar_locator_manager_t;
public:
    constexpr locator_t() : locator_t(LOC_NONE, 0, 0, 0) {}

    constexpr locator_t(locator_class_t lc, std::uint64_t h, std::uint16_t d, std::int16_t o)
    {
        set_lclass(lc);
        set_handle(h);
        set_data(d);
        set_offset(o);
    }

    constexpr locator_t(locator_class_t lc, std::uint64_t h, std::uint8_t a, std::uint8_t f, std::int16_t o)
    {
        set_lclass(lc);
        set_handle(h);
        set_arg(a);
        set_atom(f);
        set_offset(o);
    }

    locator_t(locator_t const&) = default;
    locator_t& operator=(locator_t const&) = default;

    constexpr locator_class_t lclass() const { return static_cast<locator_class_t>(impl >> 56ull); }
    constexpr std::uint32_t handle() const { return (impl >> 32ull) & 0xFFFFFF; }
    constexpr std::uint16_t data() const { assert(!has_arg_atom(lclass())); return impl >> 16ull; }
    // 'arg' and 'atom' overlap with data; use one or the other.
    constexpr std::uint8_t arg() const { assert(has_arg_atom(lclass())); return impl >> 24ull; }
    constexpr std::uint8_t atom() const { assert(has_arg_atom(lclass())); return impl >> 16ull; }
    constexpr std::int16_t signed_offset() const { return static_cast<std::make_signed_t<std::int16_t>>(impl); }
    constexpr std::uint16_t offset() const { return impl; }

    constexpr void set_lclass(locator_class_t lclass) 
    { 
        impl &= 0x00FFFFFFFFFFFFFFull; 
        impl |= ((std::uint64_t)lclass << 56ull); 
        assert(lclass == this->lclass());
    }

    constexpr void set_handle(std::uint32_t handle) 
    { 
        impl &= 0xFF000000FFFFFFFFull; 
        impl |= ((std::uint64_t)handle & 0xFFFFFFull) << 32ull; 
        assert(handle == this->handle());
    }

    constexpr void set_data(std::uint16_t data)
    { 
        assert(!has_arg_atom(lclass()));
        impl &= 0xFFFFFFFF0000FFFFull;
        impl |= (std::uint64_t)data << 16ull; 
        assert(data == this->data());
    }

    constexpr void set_arg(std::uint8_t arg)
    { 
        assert(has_arg_atom(lclass()));
        impl &= 0xFFFFFFFF00FFFFFFull;
        impl |= (std::uint64_t)arg << 24; 
        assert(arg == this->arg());
    }

    constexpr void set_atom(std::uint8_t atom)
    { 
        assert(has_arg_atom(lclass()));
        impl &= 0xFFFFFFFFFF00FFFFull;
        impl |= (std::uint64_t)atom << 16; 
        assert(atom == this->atom());
    }

    constexpr void set_offset(std::uint16_t offset) 
    { 
        impl &= 0xFFFFFFFFFFFF0000ull;
        impl |= offset; 
        assert(offset == this->offset()); 
    }

    gvar_ht gvar() const 
    { 
        assert(lclass() == LOC_GVAR);
        return { handle() }; 
    }

    const_ht const_() const 
    { 
        assert(lclass() == LOC_GLOBAL_CONST);
        return { handle() }; 
    }

    fn_ht fn() const
    {
        assert(has_fn(lclass()));
        return { handle() };
    }

    ssa_ht ssa_node() const
    {
        assert(lclass() == LOC_SSA);
        return { handle() };
    }

    cfg_ht cfg_node() const 
    { 
        assert(lclass() == LOC_CFG_LABEL);
        return { data() }; 
    }

    // Strips offset info from this locator.
    locator_t mem_head() const 
    {
        locator_t ret = *this;
        ret.set_offset(0);
        return ret;
    }

    type_t mem_type() const;
    bool mem_zp_only() const;

    // Number of bytes this locator represents
    std::size_t mem_size() const;

    constexpr std::uint64_t to_uint() const { return impl; }
    constexpr static locator_t from_uint(std::uint64_t i) 
    { 
        locator_t ret;
        ret.impl = i; 
        return ret;
    }

    constexpr static locator_t none() { return locator_t(); }

    constexpr static locator_t iota(std::int16_t offset = 0) 
        { return locator_t(LOC_IOTA, 0, 0, offset); }

    constexpr static locator_t fn(fn_ht fn)
        { return locator_t(LOC_FN, fn.value, 0, 0); }

    constexpr static locator_t arg(fn_ht fn, std::uint8_t arg, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_ARG, fn.value, arg, atom, offset); }

    constexpr static locator_t gvar(gvar_ht gvar, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_GVAR, gvar.value, 0, atom, offset); }

    constexpr static locator_t gvar_set(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_GVAR_SET, fn.value, id, 0); }

    constexpr static locator_t global_const(const_ht c, std::uint8_t atom=0, std::uint16_t offset=0)
        { return locator_t(LOC_GLOBAL_CONST, c.value, 0, atom, offset); }

    constexpr static locator_t local(std::uint16_t var_i, std::uint16_t offset=0)
        { return locator_t(LOC_LOCAL, var_i, 0, 0, offset); }

    constexpr static locator_t ret(fn_ht fn, std::uint16_t offset=0)
        { return locator_t(LOC_RETURN, fn.value, 0, 0, offset); }

    constexpr static locator_t phi(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_PHI, fn.value, 0, 0); }

    constexpr static locator_t cfg_label(fn_ht fn, cfg_ht cfg_node)
        { return locator_t(LOC_CFG_LABEL, fn.value, cfg_node.index, 0); }

    constexpr static locator_t minor_label(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_MINOR_LABEL, fn.value, id, 0); }

    constexpr static locator_t const_byte(std::uint8_t value)
        { return locator_t(LOC_CONST_BYTE, 0, value, 0); }

    constexpr static locator_t relocation_addr(addr16_t addr)
        { return locator_t(LOC_RELOCATION_ADDR, 0, addr, 0); }

    constexpr static locator_t ssa(ssa_ht node)
        { return locator_t(LOC_SSA, node.index, 0, 0); }

    static locator_t from_ssa_value(ssa_value_t v);

    bool operator==(locator_t const& o) const 
        { return to_uint() == o.to_uint(); }
    bool operator!=(locator_t const& o) const 
        { return to_uint() != o.to_uint(); }
    bool operator<(locator_t const& o) const 
        { return to_uint() < o.to_uint(); }

    explicit operator bool() const { return impl; }

    constexpr bool is_const_num() const { return lclass() == LOC_CONST_BYTE; }
    constexpr bool eq_const(unsigned i) const { return is_const_num() && data() == i; }
    constexpr bool eq_const_byte(std::uint8_t i) const { return is_const_num() && data() == i; }

private:

    // Starting from the high bits:
    // 8 bits: locator_class_t
    // 24 bits: handle data, used to hold handles
    // 16 bits: arbitrary user data
    // 16 bits: a signed offset
    std::uint64_t impl = 0;
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

#endif
