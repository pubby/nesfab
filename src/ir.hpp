#ifndef IR_HPP
#define IR_HPP

#include <cstdint>
#include <memory>
#include <ostream>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "array_pool.hpp"
#include "fixed.hpp"
#include "link.hpp"
#include "types.hpp"

namespace bc = ::boost::container;

enum ssa_op_t : short
{
#define SSA_DEF(x, ...) SSA_##x __VA_ARGS__,
#include "ssa.inc"
#undef SSA_DEF
};

std::string_view to_string(ssa_op_t node_type);
std::ostream& operator<<(std::ostream& o, ssa_op_t node_type);

// TODO: remove this?
constexpr ssa_op_t SSA_branch(bool b) 
    { return b ? SSA_true_branch : SSA_false_branch; }

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

struct cfg_input_t : public link_t<struct cfg_node_t>
{
    cfg_node_t*& out() const;
};

struct ssa_output_t : public link_t<struct ssa_node_t>
{
    ssa_value_t& in() const;
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
    std::vector<ssa_value_t> in;
    std::vector<ssa_output_t> out;
    unsigned worklist_flags = 0;

    bool has_side_effects() const;

    void link_remove_out(ssa_output_t output);
    void link_change_input(unsigned i, ssa_value_t new_value);
    void link_clear_in();

    std::string gv_id() const 
        { return "ssa" + std::to_string((std::uintptr_t)(this)); }
};

struct cfg_node_t
{
    bc::small_vector<cfg_input_t, 2> in;
    std::array<cfg_node_t*, 2> out = {};
    ssa_node_t* exit = nullptr;

    std::vector<ssa_node_t*> ssa_nodes;

    union
    {
        class block_data_t* block_data;
    };

    template<typename... Args>
    ssa_node_t& emplace_ssa(ir_t&, Args&&... args);

    void link_insert_out(unsigned i, cfg_node_t& node);

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

    std::ostream& gv(std::ostream& o);

private:
    unsigned visit_order(cfg_node_t& node);
};

inline cfg_node_t*& cfg_input_t::out() const { return node->out[index]; }
inline ssa_value_t& ssa_output_t::in() const { return node->in[index]; }

template<typename... Args>
ssa_node_t& cfg_node_t::emplace_ssa(ir_t& ir, Args&&... args)
{ 
    ssa_node_t& node = ir.ssa_pool.emplace(this, std::forward<Args>(args)...); 
    ssa_nodes.push_back(&node);
    return node;
}

#endif
