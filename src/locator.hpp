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
#include "rom_decl.hpp"
#include "phase.hpp"

class type_t;
struct ssa_value_t;

enum locator_class_t : std::uint8_t
{
    LOC_NONE,

    LOC_IOTA, // TODO: remove?

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
    LOC_PHI, // TODO: remove?
    LOC_SSA,
    LOC_MINOR_VAR,
    LOC_LVAR,

    // Labels are used during code gen. They map to assembly terms.
    LOC_CFG_LABEL,
    LOC_MINOR_LABEL,

    LOC_CONST_BYTE,
    LOC_ADDR,

    LOC_ROM_ARRAY,

    LOC_LT_GMEMBER_PTR,
    FIRST_LOC_LT = LOC_LT_GMEMBER_PTR,
    LOC_LT_CONST_PTR,
    LOC_LT_CONST_PTR_BANK,
    LOC_LT_EXPR, // link-time expression
    LAST_LOC_LT = LOC_LT_EXPR,

    LOC_THIS_BANK, // Resolves to the bank its in
    LOC_MAIN_ENTRY,
    LOC_MAIN_ENTRY_BANK,

    NUM_LCLASS,
};

// We have a limited number of bits to use.
static_assert(NUM_LCLASS < 1 << 6);

constexpr bool is_label(locator_class_t lclass)
{
    return lclass == LOC_CFG_LABEL || lclass == LOC_MINOR_LABEL;
}

constexpr bool is_const(locator_class_t lclass)
{
    return lclass == LOC_CONST_BYTE || lclass == LOC_ADDR;
}

constexpr bool has_arg_member_atom(locator_class_t lclass)
{
    switch(lclass)
    {
    case LOC_GMEMBER:
    case LOC_ARG:
    case LOC_RETURN:
    case LOC_SSA:
    case LOC_PHI:
    case LOC_LT_GMEMBER_PTR:
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
    case LOC_MINOR_VAR:
    case LOC_LVAR:
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
    constexpr locator_t(locator_class_t lc = LOC_NONE) : locator_t(lc, 0, 0, 0) {}

    constexpr locator_t(locator_class_t lc, std::uint32_t h, std::uint16_t d, std::int16_t o)
    {
        set_lclass(lc);
        set_handle(h);
        set_data(d);
        set_offset(o);
        assert(!byteified());
        assert(!high());
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
        assert(!high());
    }

    locator_t(locator_t const&) = default;
    locator_t& operator=(locator_t const&) = default;

    constexpr locator_class_t lclass() const { return static_cast<locator_class_t>(impl >> 56ull); }
    constexpr std::uint32_t handle() const { return (impl >> 32ull) & 0x3FFFFF; }
    constexpr std::uint16_t data() const { assert(!has_arg_member_atom(lclass())); return impl >> 16ull; }
    constexpr std::int16_t signed_offset() const { return static_cast<std::make_signed_t<std::int16_t>>(impl); }
    constexpr std::uint16_t offset() const { return impl; }

    constexpr bool byteified() const { return impl & (1ull << 55); }
    constexpr bool high() const { return impl & (1ull << 54); }

    // 'arg', 'member', and 'atom' overlap with 'data'; use one or the other.
    constexpr std::uint8_t member() const { assert(has_arg_member_atom(lclass())); return impl >> 24ull; }
    constexpr std::uint8_t maybe_member() const { return has_arg_member_atom(lclass()) ? member() : 0; }
    constexpr std::uint8_t arg() const { assert(has_arg_member_atom(lclass())); return (impl >> 19ull) & 0b11111; }
    constexpr std::uint8_t maybe_arg() const { return has_arg_member_atom(lclass()) ? arg() : 0; }
    constexpr std::uint8_t atom() const { assert(has_arg_member_atom(lclass())); return (impl >> 16ull) & 0b111; }
    constexpr std::uint8_t maybe_atom() const { return has_arg_member_atom(lclass()) ? atom() : 0; }

    constexpr void set_byteified(bool b)
    {
        impl &= ~(1ull << 55ull);
        impl |= std::uint64_t(b) << 55ull;
    }

    constexpr void set_high(bool b)
    {
        impl &= ~(1ull << 54ull);
        impl |= std::uint64_t(b) << 54ull;
    }

    constexpr void set_lclass(locator_class_t lclass) 
    { 
        impl &= 0x00FFFFFFFFFFFFFFull; 
        impl |= ((std::uint64_t)lclass << 56ull); 
        assert(lclass == this->lclass());
    }

    constexpr void set_handle(std::uint32_t handle) 
    { 
        impl &= 0xFF0000003FFFFFFFull; 
        impl |= ((std::uint64_t)handle & 0x3FFFFFull) << 32ull; 
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

    constexpr void advance_offset(std::uint16_t amount) { set_offset(offset() + amount); }

    gmember_ht gmember() const 
    { 
        assert(lclass() == LOC_GMEMBER || lclass() == LOC_LT_GMEMBER_PTR);
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
        assert(lclass() == LOC_SSA || lclass() == LOC_PHI);
        assert(compiler_phase() == PHASE_COMPILE);
        return { handle() };
    }

    cfg_ht cfg_node() const 
    { 
        assert(lclass() == LOC_CFG_LABEL);
        return { handle() }; 
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
        ret.set_byteified(false);
        ret.set_high(false);
        return ret;
    }

    constexpr locator_t with_byteified(bool flag) const 
    {
        locator_t ret = *this;
        ret.set_byteified(flag);
        return ret;
    }

    constexpr locator_t with_high(bool flag) const 
    {
        locator_t ret = *this;
        ret.set_high(flag);
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

    constexpr static locator_t iota(std::int16_t offset = 0) 
        { return locator_t(LOC_IOTA, 0, 0, offset); }

    constexpr static locator_t fn(fn_ht fn)
        { return locator_t(LOC_FN, fn.id, 0, 0); }

    constexpr static locator_t arg(fn_ht fn, std::uint8_t arg, std::uint8_t member, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_ARG, fn.id, arg, member, atom, offset); }

    constexpr static locator_t gmember(gmember_ht gmember, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_GMEMBER, gmember.id, 0, 0, atom, offset); }

    constexpr static locator_t gmember_set(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_GMEMBER_SET, fn.id, id, 0); }

    constexpr static locator_t rom_array(rom_array_ht h, std::uint16_t offset=0)
        { return locator_t(LOC_ROM_ARRAY, h.id, 0, offset); }

    constexpr static locator_t ret(fn_ht fn, std::uint8_t member, std::uint8_t atom, std::uint16_t offset=0)
        { return locator_t(LOC_RETURN, fn.id, 0, member, atom, offset); }

    constexpr static locator_t cfg_label(cfg_ht cfg_node)
        { return locator_t(LOC_CFG_LABEL, cfg_node.id, 0, 0); }

    constexpr static locator_t minor_label(std::uint16_t id)
        { return locator_t(LOC_MINOR_LABEL, 0, id, 0); }

    constexpr static locator_t const_byte(std::uint8_t value)
        { return locator_t(LOC_CONST_BYTE, 0, value, 0).with_byteified(true); }

    constexpr static locator_t addr(addr16_t addr, std::uint16_t offset = 0)
        { return locator_t(LOC_ADDR, 0, addr, offset); }

    constexpr static locator_t phi(ssa_ht node)
        { return locator_t(LOC_PHI, node.id, 0, 0, 0, 0); }

    constexpr static locator_t ssa(ssa_ht node)
        { return locator_t(LOC_SSA, node.id, 0, 0, 0, 0); }

    constexpr static locator_t lvar(fn_ht fn, unsigned lvar)
        { return locator_t(LOC_LVAR, fn.id, lvar, 0); }

    constexpr static locator_t minor_var(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_MINOR_VAR, fn.id, id, 0); }

    constexpr static locator_t lt_gmember_ptr(gmember_ht m, std::uint16_t offset=0)
        { return locator_t(LOC_LT_GMEMBER_PTR, m.id, 0, 0, 0, offset); }

    constexpr static locator_t lt_const_ptr(const_ht c, std::uint16_t offset=0)
        { return locator_t(LOC_LT_CONST_PTR, c.id, 0, 0, 0, offset); }

    constexpr static locator_t lt_const_ptr_bank(const_ht c, std::uint16_t offset=0)
        { return locator_t(LOC_LT_CONST_PTR_BANK, c.id, 0, 0, 0, offset); }

    constexpr static locator_t lt_expr(lt_ht lt, std::uint8_t member=0, std::uint8_t atom=0, std::uint16_t offset=0)
        { return locator_t(LOC_LT_EXPR, lt.id, 0, member, atom, offset); }

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

    locator_t link(fn_ht fn = {}, int bank = -1) const;

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

using loc_vec_t = std::vector<locator_t>;

template<>
struct std::hash<loc_vec_t>
{
    std::size_t operator()(loc_vec_t const& vec) const noexcept
    {
        std::hash<locator_t> lh;

        // Include array size in the hash:
        std::size_t h = vec.size();

        // Only hash the first 4 locators, for speed:
        unsigned const n = std::min<unsigned>(vec.size(), 4);
        for(unsigned i = 0; i < n; ++i)
            h = rh::hash_combine(h, lh(vec[i]));

        // Also hash the last locator:
        if(!vec.empty())
            h = rh::hash_combine(h, lh(vec.back()));

        return h;
    }
};

#endif
