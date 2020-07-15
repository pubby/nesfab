#ifndef IR_HPP
#define IR_HPP

#include "fixed.hpp"
#include "ir_decl.hpp"
#include "pool.hpp"
#include "ssa_op.hpp"

////////////////////////////////////////
// FLAGS
////////////////////////////////////////

// Flags that are useful among different passes.
// These apply to both CFG and SSA nodes (though not all make sense for both)

constexpr std::uint16_t FLAG_IN_WORKLIST = 1ull << 0;
constexpr std::uint16_t FLAG_PROCESSED   = 1ull << 1;
constexpr std::uint16_t FLAG_CARRY_KNOWN = 1ull << 2;
constexpr std::uint16_t FLAG_CARRY_SET   = 1ull << 3;

////////////////////////////////////////
// edge types                         //
////////////////////////////////////////

class ssa_fwd_edge_t
{
protected:
    std::uint64_t value;
    static constexpr std::uint64_t flag = 1ull << 63ull;
public:
    constexpr ssa_fwd_edge_t() = default;
    constexpr ssa_fwd_edge_t(std::nullptr_t) : value(0) {}
    constexpr ssa_fwd_edge_t(unsigned v) { set(v); }
    constexpr ssa_fwd_edge_t(fixed_t fixed) { set(fixed); }
    constexpr ssa_fwd_edge_t(ssa_ht ht, std::uint32_t index) { set(ht, index); }

    constexpr ssa_fwd_edge_t(ssa_fwd_edge_t const&) = default;
    constexpr ssa_fwd_edge_t(ssa_fwd_edge_t&&) = default;
    constexpr ssa_fwd_edge_t& operator=(ssa_fwd_edge_t const&) = default;
    constexpr ssa_fwd_edge_t& operator=(ssa_fwd_edge_t&&) = default;

    bool is_const() const { return value & flag; }
    bool is_handle() const { return !(value & flag); }
    bool holds_ref() const { return value && is_handle(); }

    fixed_t fixed() const { return { value & ~flag}; }
    std::uint32_t whole() const { return fixed().whole(); }

    ssa_ht handle() const { return { (std::uint32_t)value }; }
    std::uint32_t index() const { return value >> 32ull; };

    constexpr void set(fixed_t fixed) { value = fixed.value | flag; }
    constexpr void set(unsigned u) { set(fixed_t::whole(u)); }

    void set(ssa_ht ht, std::uint32_t index) 
    {
        value = ht.index;
        value |= (std::uint64_t)index << 32ull;
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
        value |= (std::uint64_t)index << 32ull;
        assert(this->index() == index);
    }

    // Used when comparing two edges.
    std::uint64_t target() const 
    { 
        if(is_handle())
            return handle().index;
        return value;
    }

    struct ssa_bck_edge_t* output() const;

    bool operator==(ssa_fwd_edge_t const& o) const { return value == o.value; }
    bool operator!=(ssa_fwd_edge_t const& o) const { return value != o.value; }
    bool operator< (ssa_fwd_edge_t const& o) const { return value <  o.value; }
    bool operator<=(ssa_fwd_edge_t const& o) const { return value <= o.value; }
    bool operator> (ssa_fwd_edge_t const& o) const { return value >  o.value; }
    bool operator>=(ssa_fwd_edge_t const& o) const { return value >= o.value; }
};

struct ssa_bck_edge_t
{
    ssa_ht handle;
    std::uint32_t index;

    ssa_fwd_edge_t& input() const;

    bool operator==(ssa_bck_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
    bool operator!=(ssa_bck_edge_t const& o) const 
        { return !operator==(o); }
};

struct cfg_fwd_edge_t
{
    cfg_ht handle;
    std::uint32_t index;
    
    struct cfg_bck_edge_t& output() const;

    bool operator==(cfg_fwd_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
    bool operator!=(cfg_fwd_edge_t const& o) const 
        { return !operator==(o); }
};

struct cfg_bck_edge_t
{
    cfg_ht handle;
    std::uint32_t index;

    cfg_fwd_edge_t& input() const;

    bool operator==(cfg_bck_edge_t const& o) const 
        { return handle == o.handle && index == o.index; }
    bool operator!=(cfg_bck_edge_t const& o) const 
        { return !operator==(o); }
};

////////////////////////////////////////
// ssa_value_t                        //
////////////////////////////////////////

class ssa_value_t : public ssa_fwd_edge_t
{
public:
    constexpr ssa_value_t() = default;
    constexpr ssa_value_t(std::nullptr_t n) : ssa_fwd_edge_t(n) {}
    constexpr ssa_value_t(unsigned v) : ssa_fwd_edge_t(v) {}
    constexpr ssa_value_t(fixed_t fixed) : ssa_fwd_edge_t(fixed) {}
    constexpr ssa_value_t(ssa_ht ht) : ssa_fwd_edge_t(ht, 0) {}
    constexpr ssa_value_t(ssa_fwd_edge_t const& edge) : ssa_fwd_edge_t(edge) {}

    explicit operator bool() const { return this->value; }
    bool operator!() const { return !this->value; }

    ssa_node_t& operator*() const 
        { assert(handle()); return handle().operator*(); }
    ssa_node_t* operator->() const 
        { assert(handle()); return handle().operator->(); }

    type_t type() const;
};

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
    type_t m_type = {};
    cfg_ht m_cfg_h = {};
    ssa_op_t m_op = SSA_pruned;
    std::uint16_t m_flags = 0;
public:
    std::uint32_t temp = 0; // Usable by any pass for any purpose.
private:
    ssa_buffer_t m_io;
public:
    ssa_node_t() = default;
    ssa_node_t(ssa_node_t&&) = default;

    ssa_ht handle() const { return { this - ssa_pool::data() }; }

    void set_flags(std::uint16_t f) { m_flags |= f; }
    void clear_flags(std::uint16_t f) { m_flags &= ~f; }
    void test_flags(std::uint16_t f) { return (m_flags & f) == f; }

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
    
    // Every node that takes this node as input now takes this value instead.
    // 'output_size() == 0' after calling.
    void replace_with_const(fixed_t const_val);
    void replace_with(ssa_value_t value);    

    ssa_ht unsafe_prune(); // Returns the next handle

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
    ssa_ht m_ssa_begin = {};
    ssa_ht m_last_non_phi = {};
    ssa_ht m_phi_begin = {};
    std::uint16_t m_flags = 0;
public:
    std::uint32_t temp = 0; // Usable by any pass for any purpose.
    ssa_ht exit = {};
private:
    cfg_buffer_t m_io;
public:
    cfg_node_t() = default;
    cfg_node_t(cfg_node_t&&) = default;

    cfg_ht handle() const { return { this - cfg_pool::data() }; }

    void set_flags(std::uint16_t f) { m_flags |= f; }
    void clear_flags(std::uint16_t f) { m_flags &= ~f; }
    void test_flags(std::uint16_t f) { return (m_flags & f) == f; }

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

    ssa_ht ssa_begin() { return m_ssa_begin; }
    ssa_ht phi_begin() { return m_phi_begin; }
    ssa_ht begin() const { return m_ssa_begin; }
    ssa_ht end() const { return {}; }

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

    void unsafe_prune_ssa();
    ssa_ht unsafe_prune_ssa(ssa_ht ssa_h);

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
public:
    cfg_ht root = {};
    cfg_ht exit = {};

    cfg_ht cfg_begin() const { return m_cfg_begin; }
    cfg_ht begin() const { return m_cfg_begin; }
    cfg_ht end() const { return {}; }

    cfg_ht emplace_cfg();
    cfg_ht unsafe_prune_cfg(cfg_ht cfg_h);

    // Creates a new node along an edge.
    cfg_ht split_edge(cfg_bck_edge_t edge);

    // Removes a node that has exactly 1 input and 1 output.
    // (Clear the node's SSA first!)
    cfg_ht merge_edge(cfg_ht cfg_h);

    // Used for debug asserts.
    bool valid() const;
};

////////////////////////////////////////
// edge functions                     //
////////////////////////////////////////

inline ssa_bck_edge_t* ssa_fwd_edge_t::output() const
{
    if(is_const())
        return nullptr;
    assert(index() < handle()->output_size());
    return &handle()->m_io.output(index());
}

inline ssa_fwd_edge_t& ssa_bck_edge_t::input() const
{
    assert(index < handle->input_size());
    return handle->m_io.input(index);
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

inline type_t ssa_value_t::type() const
{ 
    assert(operator bool());
    return is_const() ? type_t{TYPE_LARGEST_FIXED} : handle()->type();
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

#endif
