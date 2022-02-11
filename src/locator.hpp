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
#include "globals.hpp"
#include "handle.hpp"
#include "ir_decl.hpp"
#include "types.hpp"

// TODO: remove
//using locator_ht = handle_t<unsigned, struct locator_ht_tag, ~0u>;

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

    LOC_THIS_ARG,
    LOC_CALL_ARG,
    LOC_RETURN,

    LOC_PHI, // TODO?

    // Used to allocate local vars
    LOC_LVAR,
    LOC_LVAR_ZP,

    // Labels are used during code gen. They map to assembly terms.
    LOC_CFG_LABEL,
    LOC_MINOR_LABEL,

    LOC_CONST_BYTE,

    LOC_SSA,
};


constexpr bool is_label(locator_class_t lclass)
{
    return lclass == LOC_CFG_LABEL || lclass == LOC_MINOR_LABEL;
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

    locator_t(locator_t const&) = default;
    locator_t& operator=(locator_t const&) = default;

    constexpr locator_class_t lclass() const { return static_cast<locator_class_t>(impl >> 56ull); }
    constexpr std::uint32_t handle() const { return (impl >> 32ull) & 0xFFFFFF; }
    constexpr std::uint16_t data() const { return impl >> 16ull; }
    constexpr std::int16_t offset() const { return static_cast<std::make_signed_t<std::int16_t>>(impl); }

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
        impl &= 0xFFFFFFFF0000FFFFull;
        impl |= (std::uint64_t)data << 16ull; 
        assert(data == this->data());
    }

    constexpr void set_offset(std::int16_t offset) 
    { 
        impl &= 0xFFFFFFFFFFFF0000ull;
        impl |= static_cast<std::uint16_t>(offset); 
        assert(offset == this->offset()); 
    }

    gvar_ht gvar() const 
    { 
        assert(lclass() == LOC_GVAR);
        return { handle() }; 
    }

    fn_ht fn() const
    {
        assert(lclass() == LOC_FN
               || lclass() == LOC_CALL_ARG 
               || lclass() == LOC_THIS_ARG
               || lclass() == LOC_LVAR
               || lclass() == LOC_RETURN
               || lclass() == LOC_PHI
               || lclass() == LOC_CFG_LABEL
               || lclass() == LOC_MINOR_LABEL);
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

    // Number of bytes this locator represents
    std::size_t mem_size() const;

    constexpr std::uint64_t to_uint() const { return impl; }
    constexpr static locator_t from_uint(std::uint64_t i) 
    { 
        locator_t ret;
        ret.impl = i; 
        return ret;
    }

    constexpr static locator_t null() { return locator_t(); }

    constexpr static locator_t iota(std::int16_t offset = 0) 
        { return locator_t(LOC_IOTA, 0, 0, offset); }

    constexpr static locator_t fn(fn_ht fn)
        { return locator_t(LOC_FN, fn.value, 0, 0); }

    constexpr static locator_t this_arg(fn_ht fn, std::uint16_t arg, std::int16_t offset=0)
        { return locator_t(LOC_THIS_ARG, fn.value, arg, offset); }

    constexpr static locator_t call_arg(fn_ht fn, std::uint16_t arg, std::int16_t offset=0)
        { return locator_t(LOC_CALL_ARG, fn.value, arg, offset); }

    constexpr static locator_t gvar(gvar_ht gvar, std::int16_t offset=0)
        { return locator_t(LOC_GVAR, gvar.value, 0, offset); }

    constexpr static locator_t lvar(fn_ht fn, std::uint16_t var, std::int16_t offset=0)
        { return locator_t(LOC_LVAR, fn.value, var, offset); }

    constexpr static locator_t ret(fn_ht fn, std::int16_t offset=0)
        { return locator_t(LOC_RETURN, fn.value, 0, offset); }

    constexpr static locator_t phi(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_PHI, fn.value, 0, 0); }

    constexpr static locator_t cfg_label(fn_ht fn, cfg_ht cfg_node)
        { return locator_t(LOC_CFG_LABEL, fn.value, cfg_node.index, 0); }

    constexpr static locator_t minor_label(fn_ht fn, std::uint16_t id)
        { return locator_t(LOC_MINOR_LABEL, fn.value, id, 0); }

    constexpr static locator_t const_byte(std::uint8_t value)
        { return locator_t(LOC_CONST_BYTE, 0, value, 0); }

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


// This class exists to manage LOC_GVAR_SETs.
// It puts gvars into these sets, then allows queries on what each set contains.
class gvar_locator_manager_t
{
public:
    // Call this to initialize the manager.
    void setup(fn_t const& fn);

    bitset_uint_t const* get_set(locator_t loc) const
    {
        assert(loc.lclass() == LOC_GVAR_SET);
        assert(loc.handle() >= first_set);
        return static_cast<bitset_uint_t const*>(locs[loc.handle()]);
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

    template<typename SingleFn, typename SetFn>
    void for_each(SingleFn single_fn, SetFn set_fn)
    {
        assert(first_set <= locs.size());

        for(unsigned i = 0; i < first_set; ++i)
            single_fn(gvar_ht{ static_cast<global_t const*>(locs[i])->index() }, i);

        for(unsigned i = first_set; i < locs.size(); ++i)
            set_fn(static_cast<bitset_uint_t const*>(locs[i]), i);
    }

private:
    array_pool_t<bitset_uint_t> bitset_pool;
    std::vector<void const*> locs;

    rh::robin_map<global_t const*, unsigned> map;
    unsigned first_set = 0;
};

#endif
