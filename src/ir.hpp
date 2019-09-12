#ifndef IR_HPP
#define IR_HPP

#include <cstdint>
#include <memory>
#include <ostream>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_map.hpp"
#include "flat/small_multiset.hpp"
#include "flat/small_set.hpp"
#include "robin/map.hpp"

#include "handle.hpp"
#include "symbol_table.hpp"
#include "types.hpp"

namespace bc = boost::container;

using ssa_handle_t = handle_t<unsigned, struct ssa_handle_tag_t>;

constexpr bool ssa_is_const(ssa_handle_t handle)
{
    return handle.value & (1 << 31);
}

constexpr std::uint16_t ssa_extract_const(ssa_handle_t handle)
{
    return handle.value & 0xFFFFu;
}

constexpr ssa_handle_t ssa_make_const(int value)
{
    return { static_cast<std::uint32_t>(value) | 0xFFFF0000u };
}

constexpr type_t ssa_const_type = { TYPE_SHORT };

enum ssa_op_t : short
{
#define SSA_DEF(x, ...) SSA_##x __VA_ARGS__,
#include "ssa.inc"
#undef SSA_DEF
};

std::string_view op_name(ssa_op_t node_type);

class ir_t;

struct ssa_node_t
{
    ssa_op_t op;

    unsigned short input_size;

    // Index into ir_t's index vector.
    unsigned input_i;

    // Handle pointing to the node determining this node's control flow.
    ssa_handle_t control_h;

    // Type information.
    type_t type;

    // Arbitrary data that can be overloaded.
    union
    {
        class region_data_t* region_data;
        class cfg_node_t* cfg_node;
    };

    /////

    inline unsigned alloc_input(ir_t& ir, unsigned input_size);
    inline unsigned set_input(ir_t& ir, ssa_handle_t const* input_begin, 
                                        ssa_handle_t const* input_end);
    template<typename... Ts>
    unsigned set_input_v(ir_t& ir, Ts... ts);

    inline ssa_handle_t const* input(ir_t const& ir) const;
    inline ssa_handle_t* input(ir_t& ir);

};

class ir_t
{
public:
    std::vector<ssa_node_t> ssa;
    std::vector<ssa_handle_t> input_vec;
    ssa_handle_t return_h;
    
    void clear();

    unsigned alloc_input(unsigned size)
    {
        if(size == 0)
            return 0;
        unsigned ret = input_vec.size();
        input_vec.resize(ret + size);
        return ret;
    }

    ssa_handle_t next_handle() const { return { ssa.size() }; }

    ssa_handle_t insert(ssa_node_t node)
    {
        ssa_handle_t ret = next_handle();
        ssa.push_back(node);
        return ret;
    }

    ssa_node_t const& operator[](ssa_handle_t handle) const
    { 
        assert(!ssa_is_const(handle));
        return ssa[handle.value]; 
    }
    ssa_node_t& operator[](ssa_handle_t handle)
    { 
        assert(!ssa_is_const(handle));
        return ssa[handle.value]; 
    }

    ssa_handle_t const* input(ssa_handle_t handle) const
        { return &input_vec[operator[](handle).input_i]; }
    ssa_handle_t* input(ssa_handle_t handle)
        { return &input_vec[operator[](handle).input_i]; }

    ssa_handle_t region_h(ssa_handle_t handle) const
    {
        while(true)
        {
            ssa_node_t const& node = operator[](handle);
            if(node.op == SSA_cfg_region)
                return handle;
            handle = node.control_h;
        }
    }

    std::string handle_name(ssa_handle_t) const;
    void debug_print();

    std::ostream& gv(std::ostream& o);
};


inline unsigned ssa_node_t::alloc_input(ir_t& ir, unsigned input_size)
{
    this->input_size = input_size;
    return this->input_i = ir.alloc_input(input_size);
}

inline unsigned ssa_node_t::set_input(
    ir_t& ir, ssa_handle_t const* input_begin, ssa_handle_t const* input_end)
{
    alloc_input(ir, input_end - input_begin);
    unsigned i = input_i;
    for(auto it = input_begin; it != input_end; ++it)
        ir.input_vec[i++] = *it;
    return input_i;
}

template<typename... Ts>
unsigned ssa_node_t::set_input_v(ir_t& ir, Ts... ts)
{
    alloc_input(ir, sizeof...(Ts));
    unsigned i = input_i;
        ((ir.input_vec[i++] = ts), ...);
    return input_i;
}

inline ssa_handle_t const* ssa_node_t::input(ir_t const& ir) const 
{ 
    return &ir.input_vec[input_i]; 
}

inline ssa_handle_t* ssa_node_t::input(ir_t& ir) 
{ 
    return &ir.input_vec[input_i]; 
}

#endif
