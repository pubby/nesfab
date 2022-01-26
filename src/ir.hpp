#ifndef IR_HPP
#define IR_HPP

#include <functional>
#include "robin/hash.hpp"

#include "fixed.hpp"
#include "ir_decl.hpp"
#include "locator.hpp"
#include "sizeof_bits.hpp"
#include "ssa_op.hpp"
#include "static_pool.hpp"

////////////////////////////////////////
// edge types                         //
////////////////////////////////////////

class ssa_fwd_edge_t
{
protected:
    using uint_t = std::uint64_t;

    static_assert(sizeof(std::uint64_t) <= sizeof(uint_t));
    static_assert(sizeof(std::uintptr_t) <= sizeof(uint_t));
    static_assert(sizeof(fixed_int_t) <= sizeof(uint_t));

public:
    uint_t value;

    // 'value' has 'flag' set when holding a constant:
    static constexpr uint_t const_flag = 0b11ull << 62;
    static constexpr uint_t ptr_flag = 0b10ull << 62;
    static constexpr uint_t locator_flag = 0b01ull << 62;
    static_assert((fixed_t::mask & (const_flag | ptr_flag)) == 0);
public:
    constexpr ssa_fwd_edge_t() = default;
    constexpr ssa_fwd_edge_t(unsigned v) { set(v); }
    constexpr ssa_fwd_edge_t(fixed_t fixed) { set(fixed); }
    constexpr ssa_fwd_edge_t(locator_t loc) { set(loc); }
    constexpr ssa_fwd_edge_t(ssa_ht ht, std::uint32_t index) { set(ht, index); }
    constexpr explicit ssa_fwd_edge_t(void const* ptr) { set(ptr); }

    constexpr ssa_fwd_edge_t(ssa_fwd_edge_t const&) = default;
    constexpr ssa_fwd_edge_t(ssa_fwd_edge_t&&) = default;
    constexpr ssa_fwd_edge_t& operator=(ssa_fwd_edge_t const&) = default;
    constexpr ssa_fwd_edge_t& operator=(ssa_fwd_edge_t&&) = default;

    constexpr bool is_const() const { return value & const_flag; }
    constexpr bool is_num() const 
        { return (value & const_flag) == const_flag; }
    constexpr bool is_ptr()   const 
        { return (value & const_flag) == ptr_flag; }
    constexpr bool is_locator()   const 
        { return (value & const_flag) == locator_flag; }
    constexpr bool is_handle() const { return !(value & const_flag); }
    constexpr bool holds_ref() const { return is_handle() && handle(); }

    fixed_t fixed() const 
        { assert(is_num()); return { value & fixed_t::mask }; }
    std::uint32_t whole() const 
        { assert(is_num()); return fixed().whole(); }
    std::uint32_t carry() const 
        { assert(is_num()); assert(whole() < 2); return fixed().whole(); }
    locator_t locator() const
    { 
        assert(is_locator()); 
        return locator_t::from_uint(value & ~const_flag); 
    }

    ssa_ht handle() const 
        { assert(is_handle()); return { (std::uint32_t)value }; }
    std::uint32_t index() const 
        { assert(is_handle()); return value >> 32ull; };

    template<typename T>
    T const* ptr() const 
        { assert(is_ptr()); return reinterpret_cast<T const*>(value << 2ull); }

    bool eq_whole(unsigned w) const 
        { return is_const() && fixed() == fixed_t::whole(w); }
    bool eq_fixed(fixed_t f) const 
        { return is_const() && fixed() == f; }

    constexpr void set(fixed_t fixed) 
        { value = (fixed.value & fixed_t::mask) | const_flag; }
    constexpr void set(unsigned u) 
        { set(fixed_t::whole(u)); }
    constexpr void set(locator_t loc) 
        { value = (loc.to_uint() & ~const_flag) | locator_flag; }

    constexpr void set(void const* ptr) 
    { 
        uint_t uint = reinterpret_cast<std::uintptr_t>(ptr);
        assert((uint & 0b11) == 0);
        value = (uint >> 2ull) | ptr_flag;
        assert(is_const());
        assert(is_ptr());
        assert(!is_handle());
    }

    void set(ssa_ht ht, std::uint32_t index) 
    {
        value = ht.index;
        value |= (uint_t)index << 32ull;
    }

    void set_handle(ssa_ht ht) 
    { 
        assert(is_handle());
        value &= ~0xFFFFFFFFull;
        value |= ht.index;
        assert(this->handle() == ht);
    }

    void set_index(std::uint32_t index) 
    { 
        assert(is_handle());
        value &= 0xFFFFFFFFull;
        value |= (uint_t)index << 32ull;
        assert(this->index() == index);
    }

    // Sets locator bytes to 0 and returns.
    // Used in code-gen.
    ssa_value_t cg_mem() const;

    // Used when comparing two edges.
    std::uint64_t target() const 
    { 
        if(is_handle())
            return handle().index;
        return value;
    }

    struct ssa_bck_edge_t* output() const;

    // This version ignores edges and only checks the pointed-to value.
    bool targets_eq(ssa_fwd_edge_t o) const { return target() == o.target(); }

    // This version checks edges too.
    bool edges_eq(ssa_fwd_edge_t const& o) const { return value == o.value; }

    type_t type() const;

    explicit operator bool() const { return this->value; }
    bool operator!() const { return !this->value; }
};

std::ostream& operator<<(std::ostream& o, ssa_fwd_edge_t s);

struct ssa_bck_edge_t
{
    ssa_ht handle;
    std::uint32_t index;

    ssa_fwd_edge_t& input() const;
    input_class_t input_class() const;

    bool edges_eq(ssa_bck_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
};

struct cfg_fwd_edge_t
{
    cfg_ht handle;
    std::uint32_t index;
    
    struct cfg_bck_edge_t& output() const;

    bool edges_eq(cfg_fwd_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
};

struct cfg_bck_edge_t
{
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
    constexpr ssa_value_t(unsigned v) : ssa_fwd_edge_t(v) {}
    constexpr ssa_value_t(fixed_t fixed) : ssa_fwd_edge_t(fixed) {}
    constexpr ssa_value_t(ssa_ht ht) : ssa_fwd_edge_t(ht, 0) {}
    constexpr ssa_value_t(locator_t loc) : ssa_fwd_edge_t(loc) {}
    constexpr ssa_value_t(ssa_fwd_edge_t const& edge) : ssa_fwd_edge_t(edge) {}
    constexpr explicit ssa_value_t(void const* ptr) : ssa_fwd_edge_t(ptr) {}

    ssa_node_t& operator*() const 
        { assert(handle()); return handle().operator*(); }
    ssa_node_t* operator->() const 
        { assert(handle()); return handle().operator->(); }

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

////////////////////////////////////////
// node_io_buffers_t                  //
////////////////////////////////////////

// A size-optimized vector-like class that holds both inputs and outputs.
// It makes use of SBO - small buffer optimization.
template<typename I, typename O, std::size_t ISize, std::size_t OSize>
class node_io_buffers_t 
{
static_assert(std::is_trivially_constructible<I>::value);
static_assert(std::is_trivially_constructible<O>::value);
static_assert(std::is_trivially_copyable<I>::value);
static_assert(std::is_trivially_copyable<O>::value);
static_assert(std::is_trivially_destructible<I>::value);
static_assert(std::is_trivially_destructible<O>::value);
private:
    I* m_input = nullptr;
    O* m_output = nullptr;

    std::uint16_t m_input_size = 0;
    std::uint16_t m_output_size = 0;

    std::uint16_t m_input_capacity = 0;
    std::uint16_t m_output_capacity = 0;

    std::array<I, ISize> m_input_sbo = {};
    std::array<O, OSize> m_output_sbo = {};
public:
    node_io_buffers_t() = default;
    node_io_buffers_t(node_io_buffers_t const&) = delete;
    node_io_buffers_t(node_io_buffers_t&& o) { operator=(std::move(o)); }
    node_io_buffers_t& operator=(node_io_buffers_t const&) = delete;
    node_io_buffers_t& operator=(node_io_buffers_t&&);
    ~node_io_buffers_t();

    void alloc_input(unsigned size);
    void alloc_output(unsigned size);

    void resize_input(unsigned size);
    void resize_output(unsigned size);

    void shrink_input(unsigned size);
    void shrink_output(unsigned size);

    void clear_input();
    void clear_output();
    void clear() { clear_input(); clear_output(); }

    void reset();

    std::size_t input_size() const { return m_input_size; }
    std::size_t output_size() const { return m_output_size; }

    std::size_t input_capacity() const { return m_input_capacity; }
    std::size_t output_capacity() const { return m_output_capacity; }

    bool input_empty() const { return m_input_size == 0; }
    bool output_empty() const { return m_output_size == 0; }
    bool empty() const { return input_empty() && output_empty(); }

    I const& input(unsigned i) const 
        { assert(i < m_input_size); return m_input[i]; } 
    I& input(unsigned i)
        { assert(i < m_input_size); return m_input[i]; } 
    O const& output(unsigned i) const 
        { assert(i < m_output_size); return m_output[i]; } 
    O& output(unsigned i)
        { assert(i < m_output_size); return m_output[i]; }

    I const& last_input() const { return input(input_size()-1); }
    I& last_input() { return input(input_size()-1); }
    O const& last_output() const { return output(output_size()-1); }
    O& last_output() { return output(output_size()-1); }
};

using ssa_buffer_t = node_io_buffers_t<ssa_fwd_edge_t, ssa_bck_edge_t, 3, 1>;
using cfg_buffer_t = node_io_buffers_t<cfg_fwd_edge_t, cfg_bck_edge_t, 2, 2>;

template class node_io_buffers_t<ssa_fwd_edge_t, ssa_bck_edge_t, 3, 1>;
template class node_io_buffers_t<cfg_fwd_edge_t, cfg_bck_edge_t, 2, 2>;

////////////////////////////////////////
// ssa_node_t                         //
////////////////////////////////////////

class alignas(32) ssa_node_t : public intrusive_t<ssa_ht>
{
    friend class ssa_fwd_edge_t;
    friend class ssa_bck_edge_t;
    friend class cfg_node_t;
    friend class ir_t;

    // The following data members have been carefully aligned based on 
    // 64-byte cache lines. Don't mess with it unless you understand it!
private:
    type_t m_type = TYPE_VOID;
    cfg_ht m_cfg_h = {};
    ssa_op_t m_op = SSA_null;
    std::uint16_t m_flags = 0;
    ssa_buffer_t m_io;
public:
    ssa_node_t() = default;
    ssa_node_t(ssa_node_t&&) = default;
    ssa_node_t& operator=(ssa_node_t&&) = default;

    ssa_ht handle() const { return { this - ssa_pool::data() }; }

    void set_flags(std::uint16_t f) { m_flags |= f; }
    void clear_flags(std::uint16_t f) { m_flags &= ~f; }
    bool test_flags(std::uint16_t f) const { return (m_flags & f) == f; }

    void set_mark(mark_t mark) { m_flags &= ~MARK_MASK; m_flags |= mark; }
    mark_t get_mark() const { return (mark_t)(m_flags & MARK_MASK); }

    cfg_ht cfg_node() const { return m_cfg_h; }
    cfg_ht input_cfg(std::size_t i) const;
    ssa_op_t op() const { return m_op; }
    type_t type() const { return m_type; }

    ssa_value_t input(unsigned i) const { return m_io.input(i); }
    ssa_fwd_edge_t input_edge(unsigned i) const { return m_io.input(i); }
    std::uint32_t input_size() const { return m_io.input_size(); }

    ssa_ht output(unsigned i) const { return m_io.output(i).handle; }
    ssa_bck_edge_t output_edge(unsigned i) const { return m_io.output(i); }
    std::uint32_t output_size() const { return m_io.output_size(); }

    // Be careful with this; don't change from/to phi nodes or other
    // nodes that have some extra behavior tied to their op.
    void unsafe_set_op(ssa_op_t new_op) { m_op = new_op; }

    // Allocates memory for input/output, but doesn't link anything up.
    void alloc_input(unsigned size);
    void alloc_output(unsigned size);
    // Used after 'alloc_input' to assign to the input array.
    void build_set_input(unsigned i, ssa_value_t value);
    // Allocates and assigns the input, linking it up.
    template<typename It>
    void assign_input(It begin, It end)
    {
        std::size_t size = end - begin;
        alloc_input(size);
        for(std::size_t i = 0; i < size; ++i)
            build_set_input(i, begin[i]);
    }

    void link_append_input(ssa_value_t input);
    void link_append_input(ssa_value_t* begin, ssa_value_t* end);
    void link_remove_input(unsigned i);
    bool link_change_input(unsigned i, ssa_value_t new_value);
    void link_clear_inputs();
    void link_shrink_inputs(unsigned new_size);

    ssa_ht split_output_edge(bool this_cfg, unsigned output_i, ssa_op_t op);

    bool in_daisy() const { return test_flags(FLAG_DAISY); }
    void insert_daisy(ssa_ht it);
    void append_daisy();
    void erase_daisy();
    
    ssa_ht prev_daisy() const
        { return (in_daisy() && prev) ? prev : ssa_ht{}; }
    ssa_ht next_daisy() const
        { return (next && next->in_daisy()) ? next : ssa_ht{}; }
    
    // Every node that takes this node as input now takes this value instead.
    // 'output_size() == 0' after calling.
    void replace_with(ssa_value_t value);    
    // Returns number of nodes changed:
    unsigned replace_with(input_class_t input_class, ssa_value_t value);

    ssa_ht prune(); // Returns the next handle

private:
    ssa_node_t(ssa_node_t const&) = default;
    ssa_node_t& operator=(ssa_node_t const&) = default;

    void create(cfg_ht cfg_h, ssa_op_t op, type_t type);
    void destroy();

    unsigned append_output(ssa_bck_edge_t edge);
    void remove_inputs_output(unsigned i);
};

////////////////////////////////////////
// cfg_node_t                         //
////////////////////////////////////////

class alignas(32) cfg_node_t : public intrusive_t<cfg_ht>
{
    friend class cfg_fwd_edge_t;
    friend class cfg_bck_edge_t;
    friend class ssa_node_t;
    friend class ir_t;

    // The following data members have been carefully aligned based on 
    // 64-byte cache lines. Don't mess with it unless you understand it!
private:
    ssa_ht m_first_ssa = {};
    ssa_ht m_last_ssa = {};

    ssa_ht m_first_phi = {};
    ssa_ht m_last_daisy = {};

    std::uint16_t m_flags = 0;
    std::uint16_t m_ssa_size = 0;
private:
    cfg_buffer_t m_io;
public:
    cfg_node_t() = default;
    cfg_node_t(cfg_node_t&&) = default;
    cfg_node_t& operator=(cfg_node_t&&) = default;

    cfg_ht handle() const { return { this - cfg_pool::data() }; }

    void set_flags(std::uint16_t f) { m_flags |= f; }
    void clear_flags(std::uint16_t f) { m_flags &= ~f; }
    bool test_flags(std::uint16_t f) const { return (m_flags & f) == f; }

    void set_mark(mark_t mark) { m_flags &= ~MARK_MASK; m_flags |= mark; }
    mark_t get_mark() const { return (mark_t)(m_flags & MARK_MASK); }

    cfg_ht input(unsigned i) const { return m_io.input(i).handle; }
    cfg_fwd_edge_t input_edge(unsigned i) const { return m_io.input(i); }
    std::uint32_t input_size() const { return m_io.input_size(); }

    cfg_ht output(unsigned i) const { return m_io.output(i).handle; }
    cfg_bck_edge_t output_edge(unsigned i) const { return m_io.output(i); }
    std::uint32_t output_size() const { return m_io.output_size(); }

    // Allocates memory for input/output, but doesn't link anything up.
    void alloc_input(unsigned size);
    void alloc_output(unsigned size);
    // Used after 'alloc_output' to assign to the output array.
    void build_set_output(unsigned i, cfg_ht new_node_h);

    void link_clear_inputs();

    void link_remove_output(unsigned i);
    template<typename PhiFn>
    void link_append_output(cfg_ht new_h, PhiFn phi_fn);
    template<typename PhiFn>
    void link_change_output(unsigned i, cfg_ht new_h, PhiFn phi_fn);
    void link_clear_outputs();

    ssa_ht ssa_begin() const { return m_first_ssa; }
    ssa_ht phi_begin() const { return m_first_phi; }
    ssa_ht first_daisy() const { return last_daisy() ? m_first_ssa : ssa_ht{};}
    ssa_ht last_daisy() const { return m_last_daisy; }
    ssa_ht begin() const { return m_first_ssa; }
    ssa_ht end() const { return {}; }

    std::size_t ssa_size() const { return m_ssa_size; }

    ssa_ht emplace_ssa(ssa_op_t op, type_t type);
    template<typename... Args>
    ssa_ht emplace_ssa(ssa_op_t op, type_t type, Args&&... args)
    {
        ssa_ht h = emplace_ssa(op, type);
        ssa_node_t& node = *h;

        // Assign the inputs.
        if(sizeof...(Args) > 0)
        {
            node.alloc_input(sizeof...(Args));
            unsigned i = 0;
            (node.build_set_input(i++, args), ...);
        }

        return h;
    }

    void prune_ssa();
    ssa_ht prune_ssa(ssa_ht ssa_h);

    // Moves all the SSA nodes in 'cfg' into this node.
    void steal_ssa_nodes(cfg_ht cfg);

private:
    cfg_node_t(cfg_node_t const& o) = default;
    cfg_node_t& operator=(cfg_node_t const& o) = default;

    void create();
    void destroy();

    void resize_input(std::uint32_t size);
    void resize_output(std::uint32_t size);
    unsigned append_input(cfg_fwd_edge_t edge);
    void remove_inputs_output(unsigned i);
    void remove_outputs_input(unsigned i);

    void list_insert(ssa_ht it, ssa_node_t& node);
    void list_insert(ssa_node_t& node);
    ssa_ht list_erase(ssa_node_t& node);

    void list_insert_daisy(ssa_ht it, ssa_node_t& node);
    void list_append_daisy(ssa_node_t& node);
    void list_erase_daisy(ssa_node_t& node);
};

////////////////////////////////////////
// ir_t                               //
////////////////////////////////////////

class ir_t
{
    friend class ssa_node_t;
    friend class cfg_node_t;
private:
    cfg_ht m_cfg_begin = {};
    unsigned m_size = 0;
public:
    ir_t() = default;
    ir_t(ir_t const&) = delete;
    ir_t(ir_t&&) = delete;

    ir_t& operator=(ir_t const&) = delete;
    ir_t& operator=(ir_t&&) = delete;

    cfg_ht root = {};
    cfg_ht exit = {};

    gvar_locator_manager_t gvar_locators;

    cfg_ht cfg_begin() const { return m_cfg_begin; }
    cfg_ht begin() const { return m_cfg_begin; }
    cfg_ht end() const { return {}; }

    cfg_ht emplace_cfg();
    cfg_ht prune_cfg(cfg_ht cfg_h);
    
    std::size_t cfg_size() const { return m_size; }

    // Creates a new node along an edge.
    cfg_ht split_edge(cfg_bck_edge_t edge);

    // Removes a node that has exactly 1 input and 1 output.
    // (Clear the node's SSA first!)
    cfg_ht merge_edge(cfg_ht cfg_h);

    // Used for debug asserts.
#ifdef NDEBUG
    [[gnu::always_inline]]
    void assert_valid() const {}
#else
    void assert_valid() const;
#endif
};

////////////////////////////////////////
// edge functions                     //
////////////////////////////////////////

inline ssa_bck_edge_t* ssa_fwd_edge_t::output() const
{
    if(!holds_ref())
        return nullptr;
    assert(index() < handle()->output_size());
    return &handle()->m_io.output(index());
}

inline ssa_fwd_edge_t& ssa_bck_edge_t::input() const
{
    assert(index < handle->input_size());
    return handle->m_io.input(index);
}

inline type_t ssa_fwd_edge_t::type() const
{ 
    assert(operator bool());
    if(is_num())
        return smallest_representable(fixed());
    if(holds_ref())
        return handle()->type();
    return { TYPE_VOID };
}

inline input_class_t ssa_bck_edge_t::input_class() const
{
    assert(index < handle->input_size());
    if(index == 0)
        return ssa_input0_class(handle->op());
    if(handle->op() == SSA_trace)
        return INPUT_LINK;
    return INPUT_VALUE;
}

inline cfg_bck_edge_t& cfg_fwd_edge_t::output() const
{
    assert(index < handle->output_size());
    return handle->m_io.output(index);
}

inline cfg_fwd_edge_t& cfg_bck_edge_t::input() const
{
    assert(index < handle->input_size());
    return handle->m_io.input(index);
}

inline ssa_value_t ssa_fwd_edge_t::cg_mem() const
{
    if(is_locator())
    {
        locator_t loc = locator();
        loc.set_byte(0);
        return loc;
    }
    return *this;
}

////////////////////////////////////////
// ssa_node_t functions               //
////////////////////////////////////////

inline cfg_ht ssa_node_t::input_cfg(std::size_t i) const
{
    switch(op())
    {
    case SSA_phi: return cfg_node()->input(i);
    case SSA_trace: return cfg_node()->input(0);
    default: return cfg_node();
    }
}

inline void ssa_node_t::insert_daisy(ssa_ht it)
    { return cfg_node()->list_insert_daisy(it, *this); }

inline void ssa_node_t::append_daisy()
    { return cfg_node()->list_append_daisy(*this); }

inline void ssa_node_t::erase_daisy()
    { return cfg_node()->list_erase_daisy(*this); }

////////////////////////////////////////
// cfg_node_t functions               //
////////////////////////////////////////

template<typename PhiFn>
void cfg_node_t::link_append_output(cfg_ht new_h, PhiFn phi_fn)
{
    cfg_node_t& node = *new_h;

    // Append to all phi nodes first.
    for(ssa_ht phi_it = node.phi_begin(); phi_it; ++phi_it)
    {
        ssa_node_t& phi = *phi_it;
        assert(phi.op() == SSA_phi);
        phi.link_append_input(phi_fn(phi_it));
    }

    unsigned const i = output_size();
    m_io.resize_output(i + 1);
    m_io.output(i) = { new_h, node.append_input({ handle(), i }) };
}

template<typename PhiFn>
void cfg_node_t::link_change_output(unsigned i, cfg_ht new_h, PhiFn phi_fn)
{
    assert(i < output_size());
    if(new_h == output(i))
        return;

    // First deal with the node we're receiving output along 'i' from.
    remove_outputs_input(i);

    cfg_node_t& node = *new_h;

    // Append to all phi nodes first.
    for(ssa_ht phi_it = node.phi_begin(); phi_it; ++phi_it)
    {
        ssa_node_t& phi = *phi_it;
        assert(phi.op() == SSA_phi);
        phi.link_append_input(phi_fn(phi_it));
    }

    // Now change our input.
    m_io.output(i) = { new_h, node.input_size() };
    node.append_input({ handle(), i });
}

////////////////////////////////////////
// Utility functions                  //
////////////////////////////////////////

inline ssa_value_t orig_def(ssa_value_t v)
{
    if(v.holds_ref() && ssa_flags(v->op()) & SSAF_COPY)
        return orig_def(v->input(0));
    return v;
}

inline ssa_value_t is_orig_def(ssa_value_t v)
{
    return v == orig_def(v);
}

// Searches for this node's carry output node.
// Returns -1 if it doesn't exist.
inline int carry_output_i(ssa_node_t const& node)
{
    for(unsigned i = 0; i < node.output_size(); ++i)
        if(node.output(i)->op() == SSA_carry)
            return i;
    return -1;
}

inline ssa_ht carry_output(ssa_node_t const& node)
{
    int i = carry_output_i(node);
    return i >= 0 ? node.output(i) : ssa_ht{};
}

inline bool carry_used(ssa_node_t const& node)
{
    int i = carry_output_i(node);
    return i >= 0 && node.output(i)->output_size();
}

inline fn_t const& get_fn(ssa_node_t const& node)
{
    assert(node.op() == SSA_fn_call);
    return node.input(0).ptr<global_t>()->impl<fn_t>();
}

inline unsigned get_condition_i(ssa_op_t op)
{
    assert(op == SSA_if);
    return 0;
}

inline ssa_value_t get_condition(ssa_node_t& node)
{
    assert(node.op() == SSA_if);
    assert(node.input_size() > get_condition_i(node.op()));
    return node.input(get_condition_i(node.op()));
}

// The first input index of a write globals array.
inline unsigned write_globals_begin(ssa_op_t op)
{
    SSA_VERSION(1);
    assert(ssa_flags(op) & SSAF_WRITE_GLOBALS);
    if(op == SSA_fn_call)
        return 1;
    if(op == SSA_return)
        return 0;
    assert(false);
    return 0;
}

inline bool is_locator_write(ssa_bck_edge_t e)
{
    ssa_op_t const op = e.handle->op();
    return ((ssa_flags(op) & SSAF_WRITE_GLOBALS)
            && e.index >= write_globals_begin(op));
}

/* TODO
inline locator_ht get_locator(ssa_node_t const& node, unsigned i = 0)
{
    assert(node.op() == SSA_read_global || node.op() == SSA_write_globals);
    assert(node.input(i*2 + 1).is_const());
    return node.input(i*2 + 1).locator();;
}
*/

inline int locator_input(ssa_ht h, locator_t loc)
{
    assert(ssa_flags(h->op()) & SSAF_WRITE_GLOBALS);
    unsigned const begin = write_globals_begin(h->op());
    unsigned const input_size = h->input_size();
    assert((input_size - begin) % 2 == 0);
    for(unsigned i = write_globals_begin(h->op()); i < input_size; i += 2)
        if(h->input(i+1).locator() == loc)
            return i;
    return -1;
}

inline int locator_output(ssa_ht h, locator_t loc)
{
    unsigned const output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht output = h->output(i);
        if(output->op() != SSA_read_global)
            continue;
        if(output->input(1).locator() == loc)
            return i;
    }
    return -1;
}

template<typename Fn>
void for_each_written_global(ssa_ht h, Fn fn)
{
    assert(ssa_flags(h->op()) & SSAF_WRITE_GLOBALS);
    unsigned const begin = write_globals_begin(h->op());
    unsigned const input_size = h->input_size();
    assert((input_size - begin) % 2 == 0);
    for(unsigned i = begin; i < input_size; i += 2)
        fn(h->input(i), h->input(i+1).locator());
}

template<typename Fn>
void for_each_node_input(ssa_ht h, Fn fn)
{
    unsigned const input_size = h->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t input = h->input(i);
        if(input.holds_ref())
            fn(input.handle());
    }
}

template<typename Fn>
void for_each_input_matching(ssa_ht h, ssa_op_t match, Fn fn)
{
    unsigned const input_size = h->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t input = h->input(i);
        if(input.holds_ref() && input->op() == match)
            fn(input.handle());
    }
}

template<typename Fn>
void for_each_output_matching(ssa_ht h, input_class_t match, Fn fn)
{
    unsigned const output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto oe = h->output_edge(i);
        if(oe.input_class() == match)
            fn(oe.handle);
    }
}

inline bool has_output_matching(ssa_ht h, input_class_t match)
{
    unsigned const output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
        if(h->output_edge(i).input_class() == match)
            return true;
    return false;
}

template<typename Fn>
void for_each_read_global(ssa_ht h, Fn fn)
{
    unsigned const output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht output = h->output(i);
        if(output->op() != SSA_read_global)
            continue;
        fn(output, output->input(1).locator());
    }
}

#endif
