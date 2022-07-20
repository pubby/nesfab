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
#include "rom_array_decl.hpp"

class type_t;
struct ssa_value_t;

enum locator_class_t : std::uint8_t
{
    LOC_NONE,

    LOC_IOTA, 

    LOC_FN, // A function.
    LOC_GMEMBER, // A global member.

    // When a function calls another function, 
    // the IR tracks which gmembers are used in that function.
    // Rather than requiring one locator per gmembers, a GVAR_SET can be used
    // which combines several gmembers into a single locator.
    // (This is a size optimization)
    LOC_GMEMBER_SET,

    LOC_ARG,
    LOC_RETURN,

    LOC_PHI, // TODO?

    // Labels are used during code gen. They map to assembly terms.
    LOC_CFG_LABEL,
    LOC_MINOR_LABEL,

    LOC_CONST_BYTE,
    LOC_RELOCATION_ADDR,

    LOC_ROM_ARRAY,

    LOC_SSA,
    LOC_MINOR_VAR,

    LOC_LT_CONST_PTR,
    FIRST_LOC_LT = LOC_LT_CONST_PTR,
    LOC_LT_CONST_PTR_BANK,
    LOC_LT_EXPR, // link-time expression
    LAST_LOC_LT = LOC_LT_EXPR,

    NUM_LCLASS,
};

// We have a limited number of bits to use.
static_assert(NUM_LCLASS < 1 << 5);

constexpr bool is_label(locator_class_t lclass)
{
    return lclass == LOC_CFG_LABEL || lclass == LOC_MINOR_LABEL;
}

constexpr bool is_const(locator_class_t lclass)
{
    return lclass == LOC_CONST_BYTE;
}

constexpr bool has_arg_member_atom(locator_class_t lclass)
{
    switch(lclass)
    {
    case LOC_GMEMBER:
    case LOC_ARG:
    case LOC_RETURN:
    case LOC_LT_CONST_PTR:
    case LOC_LT_CONST_PTR_BANK:
    case LOC_LT_EXPR:
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

constexpr bool is_lt(locator_class_t lclass)
{
    return lclass >= FIRST_LOC_LT && lclass <= LAST_LOC_LT;
}

// what we need:
// - arg
// - member
// - atom
// - offset

class locator_t
{
friend class gmember_locator_manager_t;
public:
    constexpr locator_t() : locator_t(LOC_NONE, 0, 0, 0) {}

    constexpr locator_t(locator_class_t lc, std::uint32_t h, std::uint16_t d, std::int16_t o)
    {
        set_lclass(lc);
        set_handle(h);
        set_data(d);
        set_offset(o);
        assert(!byteified());
    }

    constexpr locator_t(locator_class_t lc, std::uint32_t h, std::uint8_t arg, std::uint8_t m, std::uint8_t atom, std::int16_t o)
    {
        set_lclass(lc);
        set_handle(h);
        set_arg(arg);
        set_member(m);
        set_atom(atom);
        set_offset(o);
        assert(!byteified());
    }

    locator_t(locator_t const&) = default;
    locator_t& operator=(locator_t const&) = default;

    constexpr bool byteified() const { return impl & (1ull << 56ull); }
    constexpr locator_class_t lclass() const { return static_cast<locator_class_t>(impl >> 57ull); }
    constexpr std::uint32_t handle() const { return (impl >> 32ull) & 0xFFFFFF; }
    constexpr std::uint16_t data() const { assert(!has_arg_member_atom(lclass())); return impl >> 16ull; }
    constexpr std::int16_t signed_offset() const { return static_cast<std::make_signed_t<std::int16_t>>(impl); }
    constexpr std::uint16_t offset() const { return impl; }

    // 'arg', 'member', and 'atom' overlap with 'data'; use one or the other.
    constexpr std::uint8_t member() const { assert(has_arg_member_atom(lclass())); return impl >> 24ull; }
    constexpr std::uint8_t maybe_member() const { return has_arg_member_atom(lclass()) ? member() : 0; }
    constexpr std::uint8_t arg() const { assert(has_arg_member_atom(lclass())); return (impl >> 19ull) & 0b11111; }
    constexpr std::uint8_t maybe_arg() const { return has_arg_member_atom(lclass()) ? arg() : 0; }
    constexpr std::uint8_t atom() const { assert(has_arg_member_atom(lclass())); return (impl >> 16ull) & 0b111; }
    constexpr std::uint8_t maybe_atom() const { return has_arg_member_atom(lclass()) ? atom() : 0; }

    constexpr void set_byteified(bool b)
    {
        impl &= ~(1ull << 56ull);
        impl |= std::uint64_t(b) << 56ull;
    }

    constexpr void set_lclass(locator_class_t lclass) 
    { 
        impl &= 0x00FFFFFFFFFFFFFFull; 
        impl |= ((std::uint64_t)lclass << 57ull); 
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
        assert(!has_arg_member_atom(lclass()));
        impl &= 0xFFFFFFFF0000FFFFull;
        impl |= (std::uint64_t)data << 16ull; 
        assert(data == this->data());
    }

    constexpr void set_member(std::uint8_t member)
    { 
        assert(member < MAX_MEMBERS);
        assert(has_arg_member_atom(lclass()));
        impl &= 0xFFFFFFFF00FFFFFFull;
        impl |= (std::uint64_t)member << 24; 
        assert(member == this->member());
    }

    constexpr void set_arg(std::uint8_t arg)
    { 
        assert(arg < MAX_FN_ARGS);
        assert(has_arg_member_atom(lclass()));
        impl &= 0xFFFFFFFFFF07FFFFull;
        impl |= ((std::uint64_t)arg & 0b11111) << 19; 
        assert(arg == this->arg());
    }

    constexpr void set_atom(std::uint8_t atom)
    { 
        assert(atom < MAX_ATOMS);
        assert(has_arg_member_atom(lclass()));
        impl &= 0xFFFFFFFFFFF8FFFFull;
        impl |= ((std::uint64_t)atom & 0b111) << 16; 
        assert(atom == this->atom());
    }

    constexpr void set_offset(std::uint16_t offset) 
    { 
        impl &= 0xFFFFFFFFFFFF0000ull;
        impl |= offset; 
        assert(offset == this->offset()); 
    }

    gmember_ht gmember() const 
    { 
        assert(lclass() == LOC_GMEMBER);
        return { handle() }; 
    }

    const_ht const_() const 
    { 
        assert(lclass() == LOC_LT_CONST_PTR || lclass() == LOC_LT_CONST_PTR_BANK);
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

    lt_ht lt() const
    {
        assert(lclass() == LOC_LT_EXPR);
        return { handle() };
    }

    rom_array_ht rom_array() const 
    { 
        assert(lclass() == LOC_ROM_ARRAY);
        return { handle() }; 
    }

    // Strips offset info from this locator.
    locator_t mem_head() const 
    {
        locator_t ret = *this;
        ret.set_offset(0);
        return ret.strip_byteify();
    }

    locator_t strip_byteify() const 
    {
        locator_t ret = *this;
        ret.set_byteified(false);
        return ret;
    }


    // Number of bytes this locator represents
    std::size_t mem_size() const;

    // If this must go in ZP
    bool mem_zp_only() const;

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

    constexpr static locator_t arg(fn_ht fn, std::uint8_t arg, std::uint8_t member, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_ARG, fn.value, arg, member, atom, offset); }

    constexpr static locator_t gmember(gmember_ht gmember, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_GMEMBER, gmember.value, 0, 0, atom, offset); }

    constexpr static locator_t gmember_set(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_GMEMBER_SET, fn.value, id, 0); }

    constexpr static locator_t rom_array(rom_array_ht h, std::uint16_t offset=0)
        { return locator_t(LOC_ROM_ARRAY, h.value, 0, offset); }

    constexpr static locator_t ret(fn_ht fn, std::uint8_t member, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_RETURN, fn.value, 0, member, atom, offset); }

    constexpr static locator_t phi(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_PHI, fn.value, 0, 0); }

    constexpr static locator_t cfg_label(fn_ht fn, cfg_ht cfg_node)
        { return locator_t(LOC_CFG_LABEL, fn.value, cfg_node.index, 0); }

    constexpr static locator_t minor_label(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_MINOR_LABEL, fn.value, id, 0); }

    constexpr static locator_t const_byte(std::uint8_t value)
        { locator_t loc = locator_t(LOC_CONST_BYTE, 0, value, 0); loc.set_byteified(true); return loc; }

    constexpr static locator_t relocation_addr(addr16_t addr)
        { return locator_t(LOC_RELOCATION_ADDR, 0, addr, 0); }

    constexpr static locator_t ssa(ssa_ht node)
        { return locator_t(LOC_SSA, node.index, 0, 0); }

    constexpr static locator_t minor_var(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_MINOR_VAR, fn.value, id, 0); }

    constexpr static locator_t lt_const_ptr(const_ht c, std::uint16_t offset=0)
        { return locator_t(LOC_LT_CONST_PTR, c.value, 0, 0, 0, offset); }

    constexpr static locator_t lt_const_ptr_bank(const_ht c, std::uint16_t offset=0)
        { return locator_t(LOC_LT_CONST_PTR_BANK, c.value, 0, 0, 0, offset); }

    constexpr static locator_t lt_expr(lt_ht lt, std::uint8_t member=0, std::uint8_t atom=0, std::uint16_t offset=0)
        { return locator_t(LOC_LT_EXPR, lt.value, 0, member, atom, offset); }

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

    type_t type() const;

private:

    // Starting from the high bits:
    // 8 bits: locator_class_t
    // 24 bits: handle data, used to hold handles
    // 16 bits: arbitrary user data
    // 16 bits: a signed offset
    std::uint64_t impl = 0;
};

template<>
struct std::hash<locator_t>
{
    std::size_t operator()(locator_t const& loc) const noexcept
    {
        return rh::hash_finalize(loc.to_uint());
    }
};

std::string to_string(locator_t loc);
std::ostream& operator<<(std::ostream& o, locator_t loc);

#endif
