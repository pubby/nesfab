#ifndef IR_HPP
#define IR_HPP

#include <cstdint>
#include <memory>
#include <ostream>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_map.hpp"

#include "array_pool.hpp"
#include "fixed.hpp"
#include "intrusive.hpp"
#include "o.hpp"
#include "pod_variant.hpp"
#include "link.hpp"
#include "ssa_op.hpp"
#include "types.hpp"

namespace bc = ::boost::container;

struct ssa_value_t
{
    using uint_type = std::common_type<std::uint64_t, std::uintptr_t>::type;
    uint_type value;

    ssa_value_t() = default;

    // Implict pointer construction.
    ssa_value_t(struct ssa_node_t* ptr) 
    : value(reinterpret_cast<uint_type>(ptr)) { assert(is_ptr()); }

    // Implicit int construction.
    ssa_value_t(std::uint32_t value)
    : value(((uint_type)value << 40) | 1) { assert(is_const()); }

    // Implict fixed construction.
    ssa_value_t(fixed_t fixed)
    : value((fixed.value << 16) | 1) { assert(is_const()); }

    ssa_value_t(ssa_value_t const&) = default;
    ssa_value_t(ssa_value_t&& ) = default;

    ssa_value_t& operator=(ssa_value_t const&) = default;
    ssa_value_t& operator=(ssa_value_t&& ) = default;

    constexpr bool is_ptr() const { return (value & 1) == 0; }
    constexpr bool is_const() const { return (value & 1) == 1; }
    constexpr std::uint32_t whole() const { return value >> 40; }
    constexpr fixed_t fixed() const { return { value >> 16 }; }

    ssa_node_t* ptr() const { return reinterpret_cast<ssa_node_t*>(value); }
    ssa_node_t* operator->() const { return ptr(); }
    ssa_node_t& operator*() const { return *operator->(); }
    constexpr explicit operator bool() { return value; }
    constexpr bool operator==(ssa_value_t o) const { return value == o.value; }
    constexpr bool operator!=(ssa_value_t o) const { return value != o.value; }
    constexpr bool operator<=(ssa_value_t o) const { return value <= o.value; }
    constexpr bool operator>=(ssa_value_t o) const { return value >= o.value; }
    constexpr bool operator<(ssa_value_t o) const { return value < o.value; }
    constexpr bool operator>(ssa_value_t o) const { return value > o.value; }
};

static_assert(std::is_trivially_copyable<ssa_value_t>::value);

class ir_t;
class cfg_node_t;
class ssa_node_t;

struct cfg_forward_edge_t : public link_t<cfg_node_t*> 
{
    struct cfg_reverse_edge_t& output() const;
};
struct cfg_reverse_edge_t : public link_t<cfg_node_t*> 
{
    struct cfg_forward_edge_t& input() const;
};
struct ssa_forward_edge_t : public link_t<ssa_value_t> 
{
    struct ssa_reverse_edge_t* output() const;
};
struct ssa_reverse_edge_t : public link_t<ssa_node_t*> 
{
    struct ssa_forward_edge_t& input() const;
};

class alignas(2) ssa_node_t : public intrusive_t<ssa_node_t>
{
    friend class ir_t;
    friend class cfg_node_t;
    friend struct ssa_forward_edge_t;
    friend struct ssa_reverse_edge_t;

    using input_vec_t = bc::small_vector<ssa_forward_edge_t, 3>;
    using output_vec_t = bc::small_vector<ssa_reverse_edge_t, 3>;

    cfg_node_t* cfg_node_;
    ssa_op_t op_;
    type_t type_;
    input_vec_t input_vec;
    output_vec_t output_vec;

public:
    std::uint64_t flags;
    union 
    {
        ai_ssa_t ai_data;
        phi_ssa_t phi_data;
    };

    ssa_node_t() = default;
    ssa_node_t(ssa_node_t const&) = delete;
    ssa_node_t& operator=(ssa_node_t const&) = delete;

    cfg_node_t& cfg_node() const { return *cfg_node_; }
    inline cfg_node_t& input_cfg(std::size_t i) const;
    ssa_op_t op() const { return op_; }
    type_t type() const { return type_; }
    bool pruned() const { return op_ == SSA_pruned; }
    bool has_side_effects();

    //void set_op(ssa_op_t op_) { this->op_ = op_; } // TODO: remove?
    void set_type(type_t type_) { this->type_ = type_; }

    ssa_value_t input(std::size_t i) const { return input_vec[i].node; }
    ssa_forward_edge_t input_edge(std::size_t i) const 
        { return input_vec[i]; }
    std::size_t input_size() const { return input_vec.size(); }

    ssa_node_t& output(std::size_t i) const { return *output_vec[i].node; }
    ssa_reverse_edge_t output_edge(std::size_t i) const 
        { return output_vec[i]; }
    std::size_t output_size() const { return output_vec.size(); }

    void link_append_input(ssa_value_t value);
    void link_remove_input(unsigned i);
    bool link_change_input(unsigned i, ssa_value_t new_value);
    void link_clear_inputs();

    // Every node that takes this node as input now takes this value instead.
    // Clears 'output_vec'.
    void replace_with_const(fixed_t const_val);
    void replace_with(ssa_value_t value);

    template<typename It>
    void link_assign_input(It begin, It end)
    {
        link_clear_inputs();
        for(It it = begin; it != end; ++it)
            link_append_input(*it);
    }

    std::string gv_id() const 
        { return "ssa" + std::to_string((std::uintptr_t)(this)); }
private:
    template<typename... Args>
    void create(cfg_node_t& cfg_node, ssa_op_t op, type_t type, Args&&... args)
    {
        cfg_node_ = &cfg_node;
        op_ = op;
        type_ = type;
        flags = 0;
        input_vec.clear();
        output_vec.clear();
        (link_append_input(std::forward<Args>(args)), ...);
    }
    
    void unsafe_prune();

    void remove_inputs_output(unsigned i);
};

using ssa_iterator_t = intrusive_list_t<ssa_node_t>::iterator;
using cfg_iterator_t = intrusive_list_t<cfg_node_t>::iterator;

class cfg_node_t : public intrusive_t<cfg_node_t>
{
    friend class ir_t;
    friend class ssa_node_t;
    friend struct cfg_forward_edge_t;
    friend struct cfg_reverse_edge_t;

    using input_vec_t = bc::small_vector<cfg_forward_edge_t, 2>;
    using output_vec_t = bc::small_vector<cfg_reverse_edge_t, 2>;

    partitioned_intrusive_list_t<ssa_node_t> ssa_list;

    input_vec_t input_vec;
    output_vec_t output_vec;
    bool alive;

public:
    ssa_node_t* exit; // TODO: private? rename to 'condition'?
    unsigned preorder_i;
    unsigned postorder_i;

    cfg_node_t* idom;
    cfg_node_t* iloop_header; // TODO: move
    //std::vector<cfg_node_t*> loop_entrances; // TODO

    std::uint64_t flags;
    union
    {
        //pod_optional_t<ai_cfg_t> ai_data;
        ai_cfg_t ai_data;
        class block_data_t* block_data;
    };

    bool pruned() const { return !alive; }

    cfg_node_t& input(std::size_t i) const { return *input_vec[i].node; }
    cfg_forward_edge_t input_edge(std::size_t i) const 
        { return input_vec[i]; }
    std::size_t input_size() const { return input_vec.size(); }

    cfg_node_t& output(std::size_t i) const { return *output_vec[i].node; }
    cfg_reverse_edge_t output_edge(std::size_t i) const 
        { return output_vec[i]; }
    std::size_t output_size() const { return output_vec.size(); }

    template<typename... Args>
    ssa_node_t& emplace_ssa(ir_t& ir, ssa_op_t op, type_t type, Args&&...);
    ssa_node_t* unsafe_prune_ssa(ir_t& ir, ssa_node_t& ssa_node);
    void unsafe_prune_ssa(ir_t& ir);

    // These exist for ir_builder_t, specifically as a fast-ish and
    // convenient way to set output_vec.
    // Allocates 'i' NULLs in output_vec.
    void build_resize_output(unsigned i);
    // Changes a NULL in output_vec to 'new_node'.
    void build_set_output(unsigned i, cfg_node_t& new_node);

    void link_remove_output(unsigned i);
    template<typename PhiFn>
    void link_append_output(cfg_node_t& new_node, PhiFn phi_fn);
    template<typename PhiFn>
    void link_change_output(unsigned i, cfg_node_t& new_node, PhiFn phi_fn);
    void link_clear_outputs();

    bool dominates(cfg_node_t const& node) const;

    std::string gv_id() const 
        { return "cfg" + std::to_string((std::uintptr_t)(this)); }

    ssa_iterator_t ssa_begin() const { return ssa_list.begin(); }
    ssa_iterator_t phi_begin() const { return ssa_list.partition_begin(); }

    template<typename Fn>
    void ssa_foreach(Fn const& fn) { ssa_list.foreach(fn); }

private:
    void remove_outputs_input(unsigned i);

    void create();
    void unsafe_prune(ir_t& ir);
};

class ir_t
{
    friend class ssa_node_t;
    friend class cfg_node_t;

    intrusive_pool_t<cfg_node_t> cfg_pool;
    intrusive_pool_t<ssa_node_t> ssa_pool;
    intrusive_list_t<cfg_node_t> cfg_list;
public:
    cfg_node_t* root = nullptr;
    cfg_node_t* exit = nullptr;
    std::vector<cfg_node_t*> preorder;
    std::vector<cfg_node_t*> postorder;
    
    void clear();
    void finish_construction();
    void build_order();
    void build_dominators();
    void build_loops();

    std::ostream& gv_ssa(std::ostream& o);
    std::ostream& gv_cfg(std::ostream& o);

    cfg_node_t& emplace_cfg();
    cfg_node_t& split_edge(cfg_reverse_edge_t edge);
    cfg_node_t* merge_edge(cfg_node_t& node);
    cfg_node_t* unsafe_prune_cfg(cfg_node_t& cfg_node);

    cfg_iterator_t cfg_begin() const { return cfg_list.begin(); }

    template<typename Fn>
    void cfg_foreach(Fn const& fn) { cfg_list.foreach(fn); }

    template<typename Fn>
    void ssa_foreach(Fn const& fn) 
    { 
        cfg_foreach([&](cfg_node_t& cfg_node)
        { 
            cfg_node.ssa_foreach(fn);
        }); 
    }

    void reclaim_pools();

    bool valid(); // Used for debugging asserts.

private:
    void visit_order(cfg_node_t& node);
    cfg_node_t* visit_loops(cfg_node_t& node);
    void tag_loop_header(cfg_node_t* node, cfg_node_t* header);
};


inline cfg_reverse_edge_t& cfg_forward_edge_t::output() const
{
    assert(node);
    return node->output_vec[index];
}

inline cfg_forward_edge_t& cfg_reverse_edge_t::input() const
{
    assert(node);
    return node->input_vec[index];
}

inline ssa_reverse_edge_t* ssa_forward_edge_t::output() const
{
    if(!node.is_ptr())
        return nullptr;
    assert(node.ptr());
    return &node->output_vec[index];
}

inline ssa_forward_edge_t& ssa_reverse_edge_t::input() const
{
    assert(node);
    return node->input_vec[index];
}

////////////////////////////////////////
// ssa_node_t                         //
////////////////////////////////////////

inline cfg_node_t& ssa_node_t::input_cfg(std::size_t i) const
{
    switch(op())
    {
    case SSA_phi: return cfg_node().input(i);
    case SSA_trace: return cfg_node().input(0);
    default: return cfg_node();
    }
}

////////////////////////////////////////
// cfg_node_t                         //
////////////////////////////////////////

template<typename... Args>
ssa_node_t& cfg_node_t::emplace_ssa(ir_t& ir, ssa_op_t op, type_t type, 
                                    Args&&... args)
{ 
    ssa_node_t& node = ir.ssa_pool.alloc();
    node.create(*this, op, type, std::forward<Args>(args)...);
    if(op == SSA_phi)
        ssa_list.partition_insert(node);
    else
        ssa_list.insert(node);
    return node;
}

template<typename PhiFn>
void cfg_node_t::link_append_output(cfg_node_t& new_node, PhiFn phi_fn)
{
    // Append to all phi nodes first.
    // TODO: this could be sped up using phi iteration
    for(ssa_iterator_t it = new_node.ssa_begin(); it; ++it)
        if(it->op() == SSA_phi)
            it->link_append_input(phi_fn(*it));

    output_vec.push_back({ &new_node, new_node.input_vec.size() });
    new_node.input_vec.push_back({ this, output_vec.size() - 1 });
}


template<typename PhiFn>
void cfg_node_t::link_change_output(unsigned i, cfg_node_t& new_node, 
                                    PhiFn phi_fn)
{
    assert(i < output_vec.size());
    if(&new_node == &output(i))
        return;

    // First deal with the node we're receiving output along 'i' from.
    remove_outputs_input(i);

    // Append to all phi nodes.
    // TODO: this could be sped up using phi iteration
    for(ssa_iterator_t it = new_node.ssa_begin(); it; ++it)
        if(it->op() == SSA_phi)
            it->link_append_input(phi_fn(*it));

    // Now change our input.
    output_vec[i] = { &new_node, new_node.input_vec.size() };
    new_node.input_vec.push_back({ this, i });

}

#endif
