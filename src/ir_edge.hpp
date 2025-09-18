#ifndef IR_EDGE_HPP
#define IR_EDGE_HPP

#include <cstdint>
#include <ostream>

#include <boost/container/small_vector.hpp>

#include "ir_decl.hpp"
#include "locator.hpp"
#include "ssa_op.hpp"
#include "fixed.hpp"
#include "type_name.hpp"
#include "type_mask.hpp"

namespace bc = boost::container;

class ssa_value_t;

////////////////////////////////////////
// edge types                         //
////////////////////////////////////////

class ssa_fwd_edge_t
{
protected:
    using uint_t = std::uint64_t;

    static_assert(sizeof(std::uint64_t) <= sizeof(uint_t));
    static_assert(sizeof(std::uintptr_t) <= sizeof(uint_t));
    static_assert(sizeof(fixed_uint_t) <= sizeof(uint_t));

public:
    uint_t value;

    // 'value' has 'flag' set when holding a constant:
    static constexpr uint_t const_flag = 0b11ull << 62;
    static constexpr uint_t ptr_flag = 0b10ull << 62;
    static constexpr uint_t locator_flag = 0b01ull << 62;
public:
    constexpr ssa_fwd_edge_t() = default;
    constexpr ssa_fwd_edge_t(unsigned v, type_name_t tn) { set(v, tn); }
    constexpr ssa_fwd_edge_t(fixed_t fixed, type_name_t tn) { set(fixed, tn); }
    constexpr ssa_fwd_edge_t(locator_t loc) { set(loc); }
    constexpr ssa_fwd_edge_t(ssa_ht ht, std::uint32_t index) { set(ht, index); }
    explicit ssa_fwd_edge_t(ssa_value_t const* ptr) { set(ptr); }

    constexpr ssa_fwd_edge_t(ssa_fwd_edge_t const&) = default;
    constexpr ssa_fwd_edge_t(ssa_fwd_edge_t&&) = default;
    constexpr ssa_fwd_edge_t& operator=(ssa_fwd_edge_t const&) = default;
    constexpr ssa_fwd_edge_t& operator=(ssa_fwd_edge_t&&) = default;

    static ssa_fwd_edge_t from_uint(std::uint64_t v)
    {
        ssa_fwd_edge_t ret;
        ret.value = v;
        return ret;
    }

    constexpr bool is_const() const { return value & const_flag; }
    constexpr bool is_num() const 
        { return (value & const_flag) == const_flag; }
    constexpr bool is_ptr()   const 
        { return (value & const_flag) == ptr_flag; }
    constexpr bool is_locator() const 
        { return (value & const_flag) == locator_flag; }
    constexpr bool is_handle() const { return !(value & const_flag); }
    constexpr bool holds_ref() const { return is_handle() && handle(); }

    fixed_t fixed() const 
        { assert(is_num()); return { value & MAX_FIXED_MASK }; }
    fixed_sint_t signed_fixed() const 
        { assert(is_num()); return to_signed(fixed().value, num_type_name()); }
    std::uint32_t whole() const 
        { assert(is_num()); return fixed().whole(); }
    fixed_sint_t signed_whole() const 
        { assert(is_num()); return signed_fixed() >> fixed_t::shift ; }
    std::uint32_t carry() const 
        { assert(is_num()); assert(whole() < 2); return fixed().whole(); }
    locator_t locator() const
    { 
        assert(is_locator()); 
        return locator_t::from_uint(value & ~const_flag); 
    }

    ssa_ht handle() const { assert(is_handle()); return { (std::uint32_t)value }; }
    ssa_ht maybe_handle() const { return is_handle() ? handle() : ssa_ht{}; }
    std::uint32_t index() const 
        { assert(is_handle()); return value >> 32ull; };

    template<typename T>
    T const* ptr() const 
        { assert(is_ptr()); return reinterpret_cast<T const*>(value << 2ull); }

    bool eq_whole(unsigned w) const 
        { return is_num() && fixed() == fixed_t::whole(w); }
    bool eq_signed_whole(unsigned w) const 
        { return is_num() && fixed_uint_t(signed_fixed()) == fixed_t::whole(w).value; }
    bool eq_fixed(fixed_t f) const 
        { return is_num() && fixed() == f; }
    bool eq_signed_fixed(fixed_t f) const 
        { return is_num() && fixed_uint_t(signed_fixed()) == f.value; }
    bool eq_low_bit() const
        { return eq_fixed({ low_bit_only(numeric_bitmask(type().name())) }); }

    constexpr void set(fixed_t fixed, type_name_t type_name) 
    { 
        fixed_uint_t const mask = numeric_bitmask(type_name);
        assert((mask & MAX_FIXED_MASK) == mask);
        value = (fixed.value & mask) | (uint_t(type_name) << 56) | const_flag; 
    }
    constexpr void set(unsigned u, type_name_t type_name) 
        { set(fixed_t::whole(u), type_name); }
    constexpr void set(locator_t loc) 
    { 
        uint_t uint = loc.to_uint();
        passert((uint & ~const_flag) == uint, loc);
        value = (uint & ~const_flag) | locator_flag; 
    }

    void set(void const* ptr) 
    { 
        uint_t uint = reinterpret_cast<std::uintptr_t>(ptr);
        value = (uint >> 2ull) | ptr_flag;
        assert((uint & 0b11) == 0);
        assert(is_const());
        assert(is_ptr());
        assert(!is_handle());
    }

    constexpr void set(ssa_ht ht, std::uint32_t index) 
    {
        value = ht.id;
        value |= (uint_t)index << 32ull;
    }

    constexpr void set_num_type_name(type_name_t type_name) 
    { 
        value &= ~(0xFFull << 56);
        value |= (std::uint64_t(type_name) << 56) | const_flag;
        assert(is_num());
        assert(num_type_name() == type_name);
    }

    void set_handle(ssa_ht ht) 
    { 
        assert(is_handle());
        value &= ~0xFFFFFFFFull;
        value |= ht.id;
        assert(this->handle() == ht);
    }

    void set_index(std::uint32_t index) 
    { 
        assert(is_handle());
        value &= 0xFFFFFFFFull;
        value |= (uint_t)index << 32ull;
        assert(this->index() == index);
    }

    // Matches locator_t functions. Used in code-gen.
    ssa_value_t mem_head() const;
    std::size_t mem_size() const;

    // Used when comparing two edges.
    std::uint64_t target() const 
    { 
        if(is_handle())
            return handle().id;
        return value;
    }

    type_name_t num_type_name() const { assert(is_num()); return type_name_t((value & ~const_flag) >> 56); }

    class ssa_bck_edge_t* output() const;

    // This version ignores edges and only checks the pointed-to value.
    bool targets_eq(ssa_fwd_edge_t o) const { return target() == o.target(); }

    // This version checks edges too.
    bool edges_eq(ssa_fwd_edge_t const& o) const { return value == o.value; }

    type_t type() const;

    explicit operator bool() const { return this->value; }
    bool operator!() const { return !this->value; }
};

std::ostream& operator<<(std::ostream& o, ssa_fwd_edge_t s);

class ssa_bck_edge_t
{
public:
    ssa_ht handle;
    std::uint32_t index;

    ssa_fwd_edge_t& input() const;
    input_class_t input_class() const;

    bool edges_eq(ssa_bck_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
};

class cfg_fwd_edge_t
{
public:
    cfg_ht handle;
    std::uint32_t index;
    
    class cfg_bck_edge_t& output() const;

    bool edges_eq(cfg_fwd_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
};

class cfg_bck_edge_t
{
public:
    cfg_ht handle;
    std::uint32_t index;

    cfg_fwd_edge_t& input() const;

    bool edges_eq(cfg_bck_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
};

////////////////////////////////////////
// ssa_value_t                        //
////////////////////////////////////////

class ssa_value_t : public ssa_fwd_edge_t
{
public:
    constexpr ssa_value_t() = default;
    constexpr ssa_value_t(unsigned v, type_name_t tn) : ssa_fwd_edge_t(v, tn) {}
    constexpr ssa_value_t(fixed_t fixed, type_name_t tn) : ssa_fwd_edge_t(fixed, tn) {}
    constexpr ssa_value_t(ssa_ht ht) : ssa_fwd_edge_t(ht, 0) {}
    constexpr ssa_value_t(locator_t loc) : ssa_fwd_edge_t(loc) {}
    constexpr ssa_value_t(ssa_fwd_edge_t const& edge) : ssa_fwd_edge_t(edge) {}
    //constexpr explicit ssa_value_t(void const* ptr) : ssa_fwd_edge_t(ptr) {}

    ssa_node_t& operator*() const;
    ssa_node_t* operator->() const;

    bool operator==(ssa_value_t const& o) const { return targets_eq(o); }
    bool operator!=(ssa_value_t const& o) const { return !targets_eq(o); }
    bool operator<(ssa_value_t const& o) const 
        { return target() < o.target(); }
};

namespace std
{
    template<>
    struct hash<ssa_value_t>
    {
        std::size_t operator()(ssa_value_t const& v) const noexcept
        {
            return rh::hash_finalize(v.target());
        }
    };
}

#endif
