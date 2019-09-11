#ifndef IR_BUILDER_HPP
#define IR_BUILDER_HPP

#include "ir.hpp"
#include "globals.hpp"
#include "pstring.hpp"
#include "ram.hpp"
#include "reusable_stack.hpp"

#include <boost/container/deque.hpp>
#include <boost/pool/object_pool.hpp>

namespace bc = boost::container;

enum value_category_t
{
    RVAL, 
    LVAL_LOCAL,
    LVAL_GLOBAL,
};

struct rpn_value_t
{
    ssa_handle_t handle;
    value_category_t category;
    type_t type;
    pstring_t pstring;
    unsigned local_var_i;
};

// Data associated with each region node, to be used when making IRs.
class region_data_t
{
public:
    region_data_t(ssa_handle_t* local_vars, bool sealed) 
    : local_vars(local_vars)
    , sealed(sealed) 
    {}

    // Keeps track of which ssa node a local var refers to.
    // A handle of {0} means the local var isn't in the region.
    ssa_handle_t* local_vars;

    // A CFG node is sealed when all its predecessors are set.
    bool sealed;

    void seal(class ir_builder_t& ir_builder);
};

class ir_builder_t
{
public:
    ir_builder_t(global_manager_t& global_manager, global_t& global);

    global_manager_t& globals() { return *global_manager_ptr; }
    global_t& global() { return *global_ptr; }
    fn_t& fn() { return *global().fn; }

    rpn_value_t& rpn_peek(int i) 
    { 
        assert(i < (int)rpn_stack.size());
        return rpn_stack.rbegin()[i]; 
    }

    void rpn_pop() 
    {
        assert(!rpn_stack.empty());
        rpn_stack.pop_back();
    }

    void rpn_pop(unsigned i) 
    { 
        assert(i <= rpn_stack.size());
        rpn_stack.resize(rpn_stack.size() - i); 
    }

    void rpn_push(rpn_value_t rpn_value) 
    { 
        rpn_stack.push_back(std::move(rpn_value)); 
    }

    void rpn_tuck(rpn_value_t rpn_value, unsigned place) 
    { 
        assert(place <= rpn_stack.size());
        rpn_stack.insert(rpn_stack.end() - place, std::move(rpn_value));
    }

    struct branch_t
    {
        ssa_handle_t if_h;
        ssa_handle_t true_h;
        ssa_handle_t false_h;
    };

    void compile();
    void compile_block();
    ssa_handle_t insert_partial_fence(ssa_node_t node, 
                                      ds_bitset_t const& bitset);
    ssa_handle_t insert_fence();
    void new_pasture(ds_bitset_t const& bitset, ssa_handle_t handle);
    branch_t compile_branch(ssa_handle_t condition);
    ssa_handle_t compile_jump();

    void fill_phi_args(ssa_handle_t phi_h, unsigned local_var_i);
    ssa_handle_t local_lookup(ssa_handle_t region_h, unsigned local_var_i);

    rpn_value_t compile_expression(token_t const* expr);
    void compile_assign();
    void compile_assign_arith(ssa_op_t op);
    void compile_binary_operator(ssa_op_t op, type_t result_type);
    void compile_arith(ssa_op_t op);
    void append_cast_op(rpn_value_t& rpn_value, type_t to_type);
    bool cast(rpn_value_t& rpn_value, type_t to_type);
    std::uint64_t cast_args(
        pstring_t pstring, 
        rpn_value_t* begin, rpn_value_t* end, 
        type_t const* type_begin);

    ssa_handle_t new_active_region(bool seal, 
                                   ssa_handle_t const* begin,
                                   ssa_handle_t const* end)
    {
        assert(begin <= end);

        ssa_node_t node =
        {
            .op = SSA_cfg_region,
            .control_h = ir.next_handle(),
            .region_data = &region_data_pool.emplace(
                handle_pool.alloc(fn().local_vars.size()), seal),
        };

        node.set_input(ir, begin, end);

        active_region_h = ir.insert(node);

        // Create a new pasture.
        assert(pastures.empty());
        new_pasture(ds_bitset_t::make_all_true(), active_region_h);

        return active_region_h;
    }

    template<typename... Ts>
    ssa_handle_t new_active_region_v(bool seal, Ts... ts)
    {
        ssa_handle_t array[] = { ts... };
        return new_active_region(seal, array, array + sizeof...(Ts));
    }

    // Pairs which handle last modified 'bitset'.
    // (A pasture is surrounded by fences, har har har)
    struct pasture_t
    {
        ds_bitset_t bitset;
        ssa_handle_t handle;
    };

public:
    ir_t ir;
    global_manager_t* global_manager_ptr;
    global_t* global_ptr;
    stmt_t* stmt;
    std::vector<rpn_value_t> rpn_stack;
    reusable_stack<std::vector<ssa_handle_t>> break_stack;
    reusable_stack<std::vector<ssa_handle_t>> continue_stack;
    std::vector<ssa_handle_t> return_values;
    std::vector<ssa_handle_t> return_jumps;

    // Every 'bitset' in this is disjoint with each other.
    std::vector<pasture_t*> pastures;
    boost::object_pool<pasture_t> pasture_pool;

    array_pool_t<ssa_handle_t> handle_pool; // TODO: comment
    array_pool_t<region_data_t> region_data_pool;
    ssa_handle_t active_region_h = {};

};

#endif
