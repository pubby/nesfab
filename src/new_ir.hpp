#ifndef IR_HPP
#define IR_HPP

#include "pool.hpp"

using ssa_data_pool = any_pool_t<class ssa_node_t>;
using cfg_data_pool = any_pool_t<class cfg_node_t>;

using ssa_pool = static_pool_t<class ssa_node_t>;
using cfg_pool = static_pool_t<class cfg_node_t>;

using ssa_ht = ssa_pool::handle_t;
using cfg_ht = cfg_pool::handle_t;

////////////////////////////////////////
// edge types                         //
////////////////////////////////////////

class ssa_fwd_edge_t
{
private:
    std::uint64_t value;
    constexpr std::uint64_t flag = 1ull << 63ull;
public:
    ssa_swd_edge_t(unsigned v) { set(fixed_t::whole(v)); }
    ssa_swd_edge_t(fixed_t fixed) { set(fixed); }
    ssa_swd_edge_t(ssa_ht ht, std::uint32_t index) { set(ht, index); }

    bool is_ht() const { return !(value & flag); }
    bool is_const() const { return value & flag; }

    fixed_t fixed() const { return { value & ~flag}; }
    std::uint32_t whole() const { return fixed().whole(); }

    ssa_ht ht() const { return { (std::uint32_t)value } };
    std::uint32_t index() const { return value >> 32ull; };

    void set(fixed_t fixed) { value = fixed.value | flag; }

    void set(ssa_ht ht, std::uint32_t index) 
    {
        value = ht.index;
        value |= (std::uint64_t)index << 32ull;
    }

    void set_ht(ssa_ht ht) 
    { 
        assert(is_ht());
        value &= ~0xFFFFFFFFull;
        value |= ht.value;
        assert(ht() == ht);
    }

    void set_index(std::uint32_t index) 
    { 
        assert(is_ht());
        value &= 0xFFFFFFFFull;
        value |= (std::uint64_t)index << 32ull;
        assert(index() == index);
    }

    explicit operator bool() { return value; }

    // Used when comparing two edges.
    std::uint64_t target() const { return is_ht() ? ht().value : value; }

    ssa_bck_edge_t* output() const;
};

struct ssa_bck_edge_t
{
    ssa_ht ht;
    std::uint32_t index;

    ssa_fwd_edge_t& input() const;
};

struct cfg_fwd_edge_t
{
    cfg_ht ht;
    std::uint32_t index;
    
    cfg_bck_edge_t& output() const;
};

struct cfg_bck_edge_t
{
    cfg_ht ht;
    std::uint32_t index;

    cfg_fwd_edge_t& input() const;
};

////////////////////////////////////////
// ssa_node_t                         //
////////////////////////////////////////

class alignas(32) ssa_node_t : public intrusive_t<ssa_ht>
{
    // The following data members have been carefully aligned based on 
    // 64-byte cache lines. Don't mess with it unless you understand it!
private:
    std::uint32_t m_type; // TODO
    cfg_ht m_cfg_h;
    ssa_op_t m_op = SSA_prune;
public:
    std::uint32_t flags;
private:
    ssa_fwd_edge_t* m_input;
    ssa_bck_edge_t* m_output;

    std::uint16_t m_input_size;
    std::uint16_t m_output_size;

    std::uint16_t m_input_capacity;
    std::uint16_t m_output_capacity;

    std::array<ssa_fwd_edge_t, 4> m_input_sbo;
    std::array<ssa_bck_edge_t, 2> m_output_sbo;
public:
    
    ssa_node_t(ssa_node_t&& o);
    ~ssa_node_t();

    ssa_ht ht() const { return { this - ssa_pool::data() }; }

    cfg_ht cfg_node() const { return m_cfg_h; }
    ssa_op_t op() const { return m_op; }
    type_t type() const { return m_type; }

    ssa_fwd_edge_t input(unsigned i) const { return m_input[i]; }
    std::uint32_t input_size() const { return m_input_size; }

    ssa_bck_edge_t output(unsigned i) const { return m_output[i]; }
    std::uint32_t output_size() const { return m_output_size; }

    // Allocates memory for input/output, but doesn't link anything up.
    void alloc_input(unsigned size);
    void alloc_output(unsigned size);
    // Used after 'alloc_input' to assign to the input array.
    void build_set_input(unsigned i, cfg_ht new_node_h);

    void link_append_input(ssa_fwd_edge_t input);
    void link_remove_input(unsigned i);
    bool link_change_input(unsigned i, ssa_fwd_edge_t new_value);
    void link_clear_inputs();
    
    // Every node that takes this node as input now takes this value instead.
    // 'output_size() == 0' after calling.
    void replace_with_const(fixed_t const_val);
    void replace_with(ssa_value_t value);    

private:
    ssa_node_t(ssa_node_t& const) = default;
    ssa_node_t& operator=(ssa_node_t& const) = default;

    void create(ssa_op_t op, type_t type);
    void destroy();

    ssa_fwd_edge_t& last_input() { return m_input[m_input_size-1]; }
    ssa_bck_edge_t& last_output() { return m_output[m_output_size-1]; }
    void resize_input(unsigned size);
    void resize_output(unsigned size);
};

////////////////////////////////////////
// cfg_node_t                         //
////////////////////////////////////////

class alignas(32) cfg_node_t : public intrusive_t<cfg_ht>
{
    // The following data members have been carefully aligned based on 
    // 64-byte cache lines. Don't mess with it unless you understand it!
private:
    ssa_ht m_ssa_begin;
    ssa_ht m_last_non_phi;
    ssa_ht m_phi_begin;
public:
    std::uint32_t flags;
private:
    cfg_fwd_edge_t* m_input;
    cfg_bck_edge_t* m_output;

    std::uint16_t m_input_size;
    std::uint16_t m_output_size;

    std::uint16_t m_input_capacity;
    std::uint16_t m_output_capacity;

    std::array<cfg_fwd_edge_t, 4> m_input_sbo;
    std::array<cfg_bck_edge_t, 2> m_output_sbo;
public:
    
    cfg_node_t(cfg_node_t&& o);
    cfg_node_t& operator=(cfg_node_t&& o);
    ~cfg_node_t();

    cfg_ht ht() const { return { this - cfg_pool::data() }; }

    cfg_ht cfg_node() const { return m_cfg_h; }
    cfg_op_t op() const { return m_op; }
    type_t type() const { return m_type; }

    cfg_fwd_edge_t input(unsigned i) const { return m_input[i]; }
    std::uint32_t input_size() const { return m_input_size; }

    cfg_bck_edge_t output(unsigned i) const { return m_output[i]; }
    std::uint32_t output_size() const { return m_output_size; }

    // Allocates memory for input/output, but doesn't link anything up.
    void alloc_input(unsigned size);
    void alloc_output(unsigned size);
    // Used after 'alloc_output' to assign to the output array.
    void build_set_output(unsigned i, cfg_ht new_node_h);

    void link_remove_input(unsigned i);
    bool link_change_input(unsigned i, cfg_fwd_edge_t new_value);
    void link_clear_inputs();

    ssa_ht emplace_ssa(ssa_op_t op, type_t type);
    template<typename... Args>
    ssa_node_t& emplace_ssa(ssa_op_t op, type_t type, Args&&... args)
    {
        ssa_ht h = emplace_ssa(op, type);
        ssa_node_t& node = *h;

        // Assign the inputs.
        node.alloc_input(sizeof...(Args));
        unsigned i = 0;
        (node.build_set_input(i++, args), ...);

        return h;
    }

    ssa_ht unsafe_prune_ssa(ssa_ht ssa_h);

private:
    cfg_node_t(cfg_node_t const& o) = default;
    cfg_node_t& operator=(cfg_node_t const& o) = default;

    void create();
    void destroy();

    void resize_input(std::uint32_t size);
    void resize_output(std::uint32_t size);

    void remove_inputs_output(unsigned i);
};

////////////////////////////////////////
// ir_t                               //
////////////////////////////////////////

class ir_t
{
    friend class ssa_node_t;
    friend class cfg_node_t;
private:
    cfg_ht m_cfg_begin;
    cfg_ht m_root;
    cfg_ht m_exit;
public:
    cfg_ht emplace_cfg();
    cfg_ht unsafe_prune_cfg(cfg_ht cfg_h);

    // Creates a new node along an edge.
    cfg_ht split_edge(cfg_bck_edge_t edge);

    // Removes a node that has exactly 1 input and 1 output.
    // (Clear the node's SSA first!)
    cfg_ht merge_edge(cfg_ht cfg_h);
};

////////////////////////////////////////
// edge functions                     //
    ssa_ht next;
    ssa_ht prev;
    
////////////////////////////////////////

inline ssa_bck_edge_t* ssa_fwd_edge_t::output() const
{
    if(is_const())
        return nullptr;
    assert(index() < ht()->m_output_size);
    return ht()->m_output + index();
}

inline ssa_fwd_edge_t& ssa_bck_edge_t::input() const
{
    assert(index < ht->m_input_size);
    return ht->m_input[index];
}

inline cfg_bck_edge_t& cfg_fwd_edge_t::output() const
{
    assert(index < ht->m_output_size);
    return ht->m_output[index];
}

inline cfg_fwd_edge_t& cfg_bck_edge_t::input() const
{
    assert(index < ht->m_input_size);
    return ht->m_input[index];
}

#endif
