#ifndef IR_HPP
#define IR_HPP

#include <cstdint>
#include <memory>
#include <ostream>
#include <vector>

#include "array_pool.hpp"
#include "types.hpp"

namespace bc = boost::container;

enum ssa_op_t : short
{
#define SSA_DEF(x, ...) SSA_##x __VA_ARGS__,
#include "ssa.inc"
#undef SSA_DEF
};

std::string_view to_string(ssa_op_t node_type);
std::ostream& operator<<(std::ostream& o, ssa_op_t node_type);

// TODO: move this
struct fixed_t
{
    std::uint64_t value;
};

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
    : value(((uint_type)value << 32) | 1) { assert(is_const()); }

    // Implict fixed construction.
    ssa_value_t(fixed_t fixed)
    : value((fixed.value << 1) | 1) { assert(is_const()); }

    constexpr bool is_ptr() const { return (value & 1) == 0; }
    constexpr bool is_const() const { return (value & 1) == 1; }
    constexpr std::uint32_t whole() const { return value >> 32; }
    constexpr fixed_t fixed() const { return { value >> 1 }; }

    ssa_node_t const* ptr() const { return (ssa_node_t const*)(value); }
    ssa_node_t* ptr() { return (ssa_node_t*)(value); }
    ssa_node_t const* operator->() const { return ptr(); }
    ssa_node_t* operator->() { return ptr(); }
    ssa_node_t const& operator*() const { return *operator->(); }
    ssa_node_t& operator*() { return *operator->(); }
    constexpr explicit operator bool() { return value; }
};

static_assert(std::is_trivially_copyable<ssa_value_t>::value);

struct ir_t;
struct ssa_node_t;

struct ssa_usage_t
{
    ssa_node_t* node;
    unsigned index;
};

struct alignas(2) ssa_node_t
{
    ssa_op_t op;
    unsigned short input_size;
    ssa_value_t* input;
    ssa_node_t* control;
    type_t type;

    unsigned order_i;

    unsigned short users_size;
    unsigned short users_capacity;
    ssa_usage_t* users;

    union
    {
        class block_data_t* block_data;
        class cfg_node_t* cfg_node;
    };

    // Returns the owning SSA_block.
    ssa_node_t* block();

    void alloc_input(ir_t& ir, std::size_t size);
    void set_input(ir_t& ir, ssa_value_t const* begin, ssa_value_t const* end);
    void set_input(ir_t& ir, ssa_node_t* const* begin, ssa_node_t* const* end);
    template<typename... Args>
    void set_input_v(ir_t& ir, Args&&... args);

    std::uintptr_t id() const { return (std::uintptr_t)(this); }
};

struct ir_t
{
    array_pool_t<ssa_node_t> ssa_pool;
    array_pool_t<ssa_value_t> value_pool;
    array_pool_t<ssa_usage_t> usage_pool;
    ssa_node_t* root = nullptr;
    ssa_node_t* exit = nullptr;
    std::vector<ssa_node_t*> order;
    
    void clear();
    
    void finish_construction();
    void build_order();
    void build_users();

    std::ostream& gv(std::ostream& o);

private:
    void visit_order(ssa_node_t& node);
};

template<typename... Args>
void ssa_node_t::set_input_v(ir_t& ir, Args&&... args)
{
    input = ir.value_pool.insert_v(std::forward<Args>(args)...);
    input_size = sizeof...(Args);
}

#endif
