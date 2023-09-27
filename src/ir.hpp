#ifndef IR_HPP
#define IR_HPP

#include <functional>

#include "robin/hash.hpp"

#include "fixed.hpp"
#include "type.hpp"
#include "ir_decl.hpp"
#include "ir_edge.hpp"
#include "gmanager.hpp"
#include "locator.hpp"
#include "sizeof_bits.hpp"
#include "ssa_op.hpp"
#include "static_pool.hpp"
#include "flags.hpp"
#include "loop_test.hpp" // TODO: move with iter functions?

//////////////////////////
// edge implementations //
//////////////////////////

inline ssa_node_t& ssa_value_t::operator*() const
    { assert(handle()); return handle().operator*(); }
inline ssa_node_t* ssa_value_t::operator->() const
    { assert(handle()); return handle().operator->(); }

////////////////////////////////////////
// node_io_buffers_t                  //
////////////////////////////////////////

// A size-optimized vector-like class that holds both inputs and outputs.
// It makes use of SBO - small buffer optimization.
template<typename I, typename O, std::size_t ISize, std::size_t OSize>
class node_io_buffers_t 
{
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
        { passert(i < m_input_size, i, m_input_size); return m_input[i]; } 
    I& input(unsigned i)
        { passert(i < m_input_size, i, m_input_size); return m_input[i]; } 
    O const& output(unsigned i) const 
        { passert(i < m_output_size, i, m_input_size); return m_output[i]; } 
    O& output(unsigned i)
        { passert(i < m_output_size, i, m_input_size); return m_output[i]; }

    I const& last_input() const { return input(input_size()-1); }
    I& last_input() { return input(input_size()-1); }
    O const& last_output() const { return output(output_size()-1); }
    O& last_output() { return output(output_size()-1); }
};

using ssa_buffer_t = node_io_buffers_t<ssa_fwd_edge_t, ssa_bck_edge_t, 3, 1>;
using cfg_buffer_t = node_io_buffers_t<cfg_fwd_edge_t, cfg_bck_edge_t, 3, 2>;

////////////////////////////////////////
// ssa_node_t                         //
////////////////////////////////////////

class cfg_node_t;

class alignas(32) ssa_node_t : public intrusive_t<ssa_ht>, public flag_owner_t
{
    friend class ssa_fwd_edge_t;
    friend class ssa_bck_edge_t;
    friend class cfg_node_t;
    friend class ir_t;
    friend ssa_ht split_output_edge(ssa_ht ssa_node, bool this_cfg, unsigned output_i, ssa_op_t op);

    // The following data members have been carefully aligned based on 
    // 64-byte cache lines. Don't mess with it unless you understand it!
private:
    type_t m_type = TYPE_VOID;
    cfg_ht m_cfg_h = {};
    ssa_op_t m_op = SSA_null;
    ssa_buffer_t m_io;
public:
    ssa_node_t() = default;
    ssa_node_t(ssa_node_t&&) = default;
    ssa_node_t& operator=(ssa_node_t&&) = default;

    ssa_ht handle() const { return { this - ssa_pool::data() }; }

    cfg_ht cfg_node() const { return m_cfg_h; }
    cfg_ht input_cfg(std::size_t i) const;
    ssa_op_t op() const { return m_op; }
    type_t type() const { return m_type; }

    void set_type(type_t type) { m_type = type; }

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
        assert(input_size() == 0);
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
    void link_swap_inputs(unsigned ai, unsigned bi);

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

class alignas(32) cfg_node_t : public intrusive_t<cfg_ht>, public flag_owner_t
{
    friend struct cfg_fwd_edge_t;
    friend struct cfg_bck_edge_t;
    friend class ssa_node_t;
    friend class ir_t;

    // The following data members have been carefully aligned based on 
    // 64-byte cache lines. Don't mess with it unless you understand it!
private:
    ssa_ht m_first_ssa = {};
    ssa_ht m_last_ssa = {};

    ssa_ht m_first_phi = {};
    ssa_ht m_last_daisy = {};

    unsigned m_ssa_size = 0;
private:
    cfg_buffer_t m_io;
public:
    cfg_node_t() = default;
    cfg_node_t(cfg_node_t&&) = default;
    cfg_node_t& operator=(cfg_node_t&&) = default;

    cfg_ht handle() const { return { this - cfg_pool::data() }; }

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
    unsigned build_set_output(unsigned i, cfg_ht new_node_h);
    unsigned build_append_output(cfg_ht new_h);

    void link_clear_inputs();

    void link_remove_output(unsigned i);
    template<typename PhiFn>
    unsigned link_append_output(cfg_ht new_h, PhiFn phi_fn);
    template<typename PhiFn>
    void link_change_output(unsigned i, cfg_ht new_h, PhiFn phi_fn);
    void link_shrink_outputs(unsigned new_size);
    void link_clear_outputs();
    void link_swap_inputs(unsigned ai, unsigned bi);
    void link_swap_outputs(unsigned ai, unsigned bi);

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

    // Steals all the outputs of 'cfg'.
    // Only valid if this node has no outputs.
    void steal_outputs(cfg_node_t& cfg);

    // Moves all the SSA nodes in 'cfg' into this node.
    void steal_ssa_nodes(cfg_ht cfg);

    // Same, but only 1 node.
    // Returns the node after 'ssa', previously.
    ssa_ht steal_ssa(ssa_ht ssa, bool steal_linked);
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
    ir_t();
    ir_t(ir_t const&) = delete;
    ir_t(ir_t&&) = delete;

    ~ir_t();

    ir_t& operator=(ir_t const&) = delete;
    ir_t& operator=(ir_t&&) = delete;

    cfg_ht root = {};

    gmanager_t gmanager;

    cfg_ht cfg_begin() const { return m_cfg_begin; }
    cfg_ht begin() const { return m_cfg_begin; }
    cfg_ht end() const { return {}; }

    cfg_ht emplace_cfg(std::uint16_t flags);
    cfg_ht prune_cfg(cfg_ht cfg_h);
    
    std::size_t cfg_size() const { return m_size; }
    std::size_t ssa_size() const;

    // Creates a new node along an edge.
    cfg_ht split_edge(cfg_bck_edge_t edge);

    // Removes a node that has exactly 1 input and 1 output.
    // (Clear the node's SSA first!)
    cfg_ht merge_edge(cfg_ht cfg_h);

    // Used for debug asserts.
#ifdef NDEBUG
    [[gnu::always_inline]]
    void assert_valid(bool cg = false) const {}
#else
    void assert_valid(bool cg = false) const;
#endif
};

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

// Returns the input index.
template<typename PhiFn>
unsigned cfg_node_t::link_append_output(cfg_ht new_h, PhiFn phi_fn)
{
    cfg_node_t& node = *new_h;

    // Append to all phi nodes first.
    for(ssa_ht phi_it = node.phi_begin(); phi_it; ++phi_it)
    {
        ssa_node_t& phi = *phi_it;
        assert(phi.op() == SSA_phi);
        phi.link_append_input(phi_fn(phi_it));
    }

    unsigned const output_i = output_size();
    m_io.resize_output(output_i + 1);
    unsigned const input_i = node.append_input({ handle(), output_i });
    m_io.output(output_i) = { new_h, input_i };
    return input_i;
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

ssa_ht split_output_edge(ssa_ht ssa_node, bool this_cfg, unsigned output_i, ssa_op_t op);

// Recurses through copies until finding the original definition.
inline ssa_value_t orig_def(ssa_value_t v)
{
    if(v.holds_ref() && ssa_flags(v->op()) & SSAF_COPY)
        return orig_def(v->input(ssa_copy_input(v->op())));
    return v;
}

inline bool is_orig_def(ssa_value_t v)
{
    return v == orig_def(v);
}

// Recurses through copies until finding the original use.
inline ssa_ht orig_use(ssa_ht h)
{
    while((ssa_flags(h->op()) & SSAF_COPY) && h->output_size() == 1)
        h = h->output(0);
    return h;
}

// Searches for this node's output node matching 'Op'.
// Returns -1 if it doesn't exist.
template<ssa_op_t Op>
int find_output_i(ssa_node_t const& node)
{
    for(unsigned i = 0; i < node.output_size(); ++i)
        if(node.output(i)->op() == Op)
            return i;
    return -1;
}

// Searches for this node's output node matching 'Op'.
// Returns a null handle if it doesn't exist.
template<ssa_op_t Op>
ssa_ht find_output(ssa_node_t const& node)
{
    for(unsigned i = 0; i < node.output_size(); ++i)
        if(node.output(i)->op() == Op)
            return node.output(i);
    return {};
}

inline ssa_ht carry_output(ssa_node_t const& node) { return find_output<SSA_carry>(node); }
inline ssa_ht mul8_output(ssa_node_t const& node) { return find_output<SSA_mul8_hi>(node); }

inline int carry_output_i(ssa_node_t const& node) { return find_output_i<SSA_carry>(node); }
inline int mul8_output_i(ssa_node_t const& node) { return find_output_i<SSA_mul8_hi>(node); }

inline bool carry_used(ssa_node_t const& node)
{
    int i = carry_output_i(node);
    return i >= 0 && node.output(i)->output_size();
}

inline fn_ht get_fn(ssa_node_t const& node)
{
    if(node.op() == SSA_fn_ptr_call)
        return {};
    if(direct_fn(node.op()) && has_fn(node.input(0).locator().lclass()))
        return node.input(0).locator().fn();
    return {};
}

inline fn_set_ht get_fn_set(ssa_node_t const& node)
{
    if(node.op() == SSA_fn_ptr_call && has_fn_set(node.input(0).locator().lclass()))
        return node.input(0).locator().fn_set();
    return {};
}

class callable_t const* get_callable(ssa_node_t const& node, bool allow_goto = false);

inline unsigned get_condition_i(ssa_op_t op)
{
    assert(ssa_flags(op) & SSAF_CONDITIONAL);
    return 0;
}

inline ssa_value_t get_condition(ssa_node_t& node)
{
    assert(ssa_flags(node.op()) & SSAF_CONDITIONAL);
    assert(node.input_size() > get_condition_i(node.op()));
    return node.input(get_condition_i(node.op()));
}

// The first input index of a write globals array.
inline bool is_locator_write(ssa_bck_edge_t e)
{
    ssa_op_t const op = e.handle->op();
    return ((ssa_flags(op) & SSAF_WRITE_GLOBALS)
            && e.index >= write_globals_begin(op));
}

inline int locator_input(ssa_ht h, locator_t loc)
{
    assert(ssa_flags(h->op()) & SSAF_WRITE_GLOBALS);
    unsigned const input_size = h->input_size();
    assert((input_size - write_globals_begin(h->op())) % 2 == 0);
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
        ssa_ht const output = h->output(i);
        if(output->op() != SSA_read_global)
            continue;
        if(output->input(1).locator() == loc)
            return i;
    }
    return -1;
}

template<typename Fn>
void for_each_written_global(ssa_ht h, Fn const& fn)
{
    assert(ssa_flags(h->op()) & SSAF_WRITE_GLOBALS);
    unsigned const begin = write_globals_begin(h->op());
    unsigned const input_size = h->input_size();
    passert((input_size - begin) % 2 == 0, h->op(), input_size, begin);
    for(unsigned i = begin; i < input_size; i += 2)
        fn(h->input(i), h->input(i+1).locator());
}

template<typename Fn>
void for_each_node_input(ssa_ht h, Fn const& fn)
{
    unsigned const input_size = h->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t const input = h->input(i);
        if(input.holds_ref())
            fn(input.handle());
    }
}

template<typename Fn>
void for_each_input_matching(ssa_ht h, ssa_op_t match, Fn const& fn)
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
void for_each_output(ssa_ht h, Fn const& fn)
{
    unsigned const output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht output = h->output(i);
        fn(output);
    }
}

template<typename Fn>
bool for_each_output_with_links(ssa_ht const h, Fn const& fn)
{
    unsigned const output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto const oe = h->output_edge(i);
        if(oe.input_class() == INPUT_LINK)
        {
            if(!for_each_output_with_links(oe.handle, fn))
                return false;
        }
        else
            if(!LOOP_TEST(fn, h, oe.handle))
               return false;
    }

    return true;
}

template<typename Fn>
bool for_each_output_matching(ssa_ht h, input_class_t match, Fn const& fn)
{
    unsigned const output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto const oe = h->output_edge(i);
        if(oe.input_class() == match)
            if(!LOOP_TEST(fn, oe.handle))
                return false;
    }
    return true;
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
void for_each_read_global(ssa_ht h, Fn const& fn)
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
