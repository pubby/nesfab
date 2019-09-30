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

    ssa_node_t const* ptr() const { return (ssa_node_t const*)(value); }
    ssa_node_t* ptr() { return (ssa_node_t*)(value); }
    ssa_node_t const* operator->() const { return ptr(); }
    ssa_node_t* operator->() { return ptr(); }
    ssa_node_t const& operator*() const { return *operator->(); }
    ssa_node_t& operator*() { return *operator->(); }
    constexpr explicit operator bool() { return value; }
    constexpr bool operator==(ssa_value_t o) const { return value == o.value; }
    constexpr bool operator!=(ssa_value_t o) const { return value != o.value; }
    constexpr bool operator<=(ssa_value_t o) const { return value <= o.value; }
    constexpr bool operator>=(ssa_value_t o) const { return value >= o.value; }
    constexpr bool operator<(ssa_value_t o) const { return value < o.value; }
    constexpr bool operator>(ssa_value_t o) const { return value > o.value; }
};

static_assert(std::is_trivially_copyable<ssa_value_t>::value);

struct ir_t;
struct cfg_node_t;
struct ssa_node_t;

struct cfg_forward_edge_t : public link_t<struct cfg_node_t>
{
    cfg_node_t*& out() const;
};

struct ssa_reverse_edge_t : public link_t<struct ssa_node_t>
{
    ssa_value_t& in() const;
};

// TODO: remove?
enum edge_type_t : short
{
    BACK_EDGE,
    CROSS_EDGE,
    FORWARD_EDGE,
};

struct alignas(2) ssa_node_t
{
    explicit ssa_node_t(cfg_node_t* cfg_node) 
    : cfg_node(cfg_node), op(), type() {}

    template<typename... Args>
    ssa_node_t(cfg_node_t* cfg_node, ssa_op_t op, Args&&... args)
    : cfg_node(cfg_node), op(op), type()
    , in({ std::forward<Args>(args)... }) {}

    template<typename... Args>
    ssa_node_t(cfg_node_t* cfg_node, ssa_op_t op, type_t type, Args&&... args)
    : cfg_node(cfg_node), op(op), type(type)
    , in({ std::forward<Args>(args)... }) {}

    cfg_node_t* cfg_node;
    ssa_op_t op;
    type_t type;
    bc::small_vector<ssa_value_t, 2> in;
    bc::small_vector<ssa_reverse_edge_t, 2> out;

    // TODO:
    bool in_worklist;
    unsigned visited;
    union
    {
        struct constraints_t* constraints;
        //unsigned worklist_flags = 0;
    };

    unsigned flags() const { return ssa_flags(op); }

    bool has_side_effects() const;

    void remove_out(ssa_reverse_edge_t output);
    void link_change_input(unsigned i, ssa_value_t new_value);
    void link_append_input(ssa_value_t value);
    void link_clear_in();

    std::string gv_id() const 
        { return "ssa" + std::to_string((std::uintptr_t)(this)); }
};

struct cfg_node_t
{
    bc::small_vector<cfg_forward_edge_t, 2> in;
    bc::small_vector<cfg_node_t*, 2> out = {};
    ssa_node_t* exit = nullptr;
    std::vector<ssa_node_t*> ssa_nodes;

    std::array<edge_type_t, 2> out_edge_types;
    unsigned preorder_i;
    unsigned postorder_i;

    cfg_node_t* idom;
    cfg_node_t* iloop_header;
    std::vector<cfg_node_t*> loop_entrances;

    using reachable_int_t = std::uint64_t;

    union
    {
        struct
        {
            bool in_worklist;
            bool executed;
            std::array<bool, 2> out_executable;
        };
        reachable_int_t reachable;
        class block_data_t* block_data;
    };

    template<typename... Args>
    ssa_node_t& emplace_ssa(ir_t&, Args&&... args);
    template<typename... Args>
    ssa_node_t& linked_emplace_ssa(ir_t&, Args&&... args);

    void remove_in(cfg_forward_edge_t input);
    void link_remove_in(cfg_forward_edge_t input);
    void link_remove_out(unsigned out_i);
    void link_insert_out(unsigned i, cfg_node_t& node);

    bool dominates(cfg_node_t const& node) const;

    std::string gv_id() const 
        { return "cfg" + std::to_string((std::uintptr_t)(this)); }
};

struct ir_t
{
    array_pool_t<cfg_node_t> cfg_pool;
    array_pool_t<ssa_node_t> ssa_pool;
    cfg_node_t* root = nullptr;
    cfg_node_t* exit = nullptr;
    std::vector<cfg_node_t*> preorder;
    std::vector<cfg_node_t*> postorder;
    
    void clear();
    void finish_construction();
    void build_users();
    void build_order();
    void build_dominators();
    void build_loops();

    std::ostream& gv_ssa(std::ostream& o);
    std::ostream& gv_cfg(std::ostream& o);

private:
    void visit_order(cfg_node_t& node);
    cfg_node_t* visit_loops(cfg_node_t& node);
    void tag_loop_header(cfg_node_t* node, cfg_node_t* header);
};

cfg_node_t* best_idom(cfg_node_t const& node, cfg_node_t* root = nullptr);

inline cfg_node_t*& cfg_forward_edge_t::out() const 
    { return node->out[index]; }
inline ssa_value_t& ssa_reverse_edge_t::in() const { return node->in[index]; }

template<typename... Args>
ssa_node_t& cfg_node_t::emplace_ssa(ir_t& ir, Args&&... args)
{ 
    ssa_node_t& node = ir.ssa_pool.emplace(this, std::forward<Args>(args)...); 
    ssa_nodes.push_back(&node);
    return node;
}

#endif
