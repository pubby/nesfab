#ifndef IR_BUILDER_HPP
#define IR_BUILDER_HPP

#include <exception>

#include "ir.hpp"
#include "globals.hpp"
#include "pstring.hpp"
#include "ram.hpp"
#include "reusable_stack.hpp"

namespace bc = boost::container;

class local_lookup_error_t : public std::exception
{
public:
    virtual const char* what() const noexcept
        { return "Failed local lookup."; }
};

enum value_category_t
{
    RVAL, 
    LVAL_LOCAL,
    LVAL_GLOBAL,
};

struct rpn_value_t
{
    ssa_value_t ssa_value;
    value_category_t category;
    type_t type;
    pstring_t pstring;
    unsigned local_var_i;
};

// Data associated with each block node, to be used when making IRs.
struct block_data_t
{
    // Keeps track of which ssa node a local var refers to.
    // A handle of {0} means the local var isn't in the block.
    ssa_value_t* local_vars = nullptr;

    // Phi nodes in the block which have yet to be sealed.
    ssa_value_t* unsealed_phis = nullptr;

    // Only used for labels.
    pstring_t label_name = {};

    // A CFG node is sealed when all its predecessors are set.
    constexpr bool sealed() const { return unsealed_phis == nullptr; }
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
        ssa_node_t* if_node;
        ssa_node_t* true_node;
        ssa_node_t* false_node;
    };

    void compile();
    void compile_block();
    ssa_node_t* insert_partial_fence(ssa_node_t node, 
                                     ds_bitset_t const& bitset);
    ssa_node_t* insert_fence();
    void new_pasture(ds_bitset_t const& bitset, ssa_node_t* handle);
    branch_t compile_branch(ssa_value_t condition);
    ssa_node_t* compile_jump();
    ssa_value_t default_construct(type_t type);

    // Block and local variable functions
    block_data_t* new_block_data(bool seal, pstring_t label_name = {});
    ssa_node_t* new_active_block(bool seal, pstring_t label_name = {});
    void seal_block(block_data_t& block_data);
    void fill_phi_args(ssa_node_t& phi, unsigned local_var_i);
    ssa_value_t local_lookup(ssa_node_t* block, unsigned local_var_i);

    // IR generation functions
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

    // Pairs which handle last modified 'bitset'.
    // (A pasture is surrounded by fences, har har har)
    struct pasture_t
    {
        ds_bitset_t bitset;
        ssa_node_t* node;
    };

public:
    ir_t ir;
    global_manager_t* global_manager_ptr;
    global_t* global_ptr;
    stmt_t* stmt;
    std::vector<rpn_value_t> rpn_stack;
    reusable_stack<std::vector<ssa_node_t*>> break_stack;
    reusable_stack<std::vector<ssa_node_t*>> continue_stack;
    std::vector<ssa_value_t> return_values;
    std::vector<ssa_node_t*> return_jumps;

    // Every 'bitset' in this is disjoint with each other.
    std::vector<pasture_t*> pastures;
    array_pool_t<pasture_t> pasture_pool;

    array_pool_t<ssa_value_t> input_pool;
    array_pool_t<block_data_t> block_pool;
    ssa_node_t* active_block = nullptr;
};

#endif
