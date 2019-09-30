#ifndef IR_BUILDER_HPP
#define IR_BUILDER_HPP

#include <exception>

#include "ir.hpp"
#include "globals.hpp"
#include "pstring.hpp"
#include "ram.hpp"
#include "reusable_stack.hpp"

struct local_lookup_error_t : public std::exception
{
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

    // TODO: comment
    //rh::robin_map<ssa_handle_t, ssa_node_t*> trace_map;

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

    rpn_value_t& rpn_peek(int i);
    void rpn_pop();
    void rpn_pop(unsigned i);
    void rpn_push(rpn_value_t rpn_value);
    void rpn_tuck(rpn_value_t rpn_value, unsigned place);

    void compile();
    cfg_node_t& compile_block(cfg_node_t&);
    ssa_node_t& insert_fenced(cfg_node_t& cfg_node, ssa_op_t op, type_t type, 
                              ds_bitset_t const& bitset);
    ssa_node_t& insert_fence(cfg_node_t& cfg_node);
    void new_pasture(ds_bitset_t const& bitset, ssa_node_t* handle);
    void exits_with_jump(cfg_node_t& node);
    void exits_with_branch(cfg_node_t& node, ssa_value_t condition);
    cfg_node_t& compile_goto(cfg_node_t& branch_node);

    // Block and local variable functions
    block_data_t* new_block_data(bool seal, pstring_t label_name = {});
    cfg_node_t& insert_cfg(bool seal, pstring_t label_name = {});
    void seal_block(block_data_t& block_data);
    void fill_phi_args(ssa_node_t& phi, unsigned local_var_i);
    ssa_value_t local_lookup(cfg_node_t& node, unsigned local_var_i);
    ssa_value_t local_lookup(cfg_node_t& node, ssa_node_t* ssa_node);

    // IR generation functions
    cfg_node_t& compile_expr(cfg_node_t&, token_t const* expr);
    cfg_node_t& compile_logical_begin(cfg_node_t& cfg_node, bool short_cut_i);
    cfg_node_t& compile_logical_end(cfg_node_t& cfg_node, bool short_cut_i);
    void compile_assign(cfg_node_t& cfg_node);
    void compile_assign_arith(cfg_node_t&, ssa_op_t op);
    void compile_binary_operator(cfg_node_t&, ssa_op_t op, type_t result_type);
    void compile_arith(cfg_node_t& cfg_node, ssa_op_t op);
    void compile_compare(cfg_node_t& cfg_node, ssa_op_t op);
    void force_cast(cfg_node_t&, rpn_value_t& rpn_value, type_t to_type);
    bool cast(cfg_node_t&, rpn_value_t& rpn_value, type_t to_type);
    void throwing_cast(cfg_node_t&, rpn_value_t& rpn_value, type_t to_type);
    std::uint64_t cast_args(
        cfg_node_t& cfg_node,
        pstring_t pstring, 
        rpn_value_t* begin, rpn_value_t* end, 
        type_t const* type_begin);
    void trace(cfg_node_t& cfg_node, ssa_node_t& ssa_node);

    // Pairs which handle last modified 'bitset'.
    // (A pasture is surrounded by fences, har har har)
    struct pasture_t
    {
        ds_bitset_t bitset;
        ssa_node_t* node;
    };

    struct logical_data_t
    {
        cfg_node_t* branch_node;
        pstring_t lhs_pstring;
    };

public:
    ir_t ir;
    global_manager_t* global_manager_ptr;
    global_t* global_ptr;
    stmt_t const* stmt;
    std::vector<rpn_value_t> rpn_stack;
    std::vector<logical_data_t> logical_stack;
    reusable_stack<std::vector<cfg_node_t*>> break_stack;
    reusable_stack<std::vector<cfg_node_t*>> continue_stack;
    std::vector<ssa_value_t> return_values;
    std::vector<cfg_node_t*> return_jumps;

    // Every 'bitset' in this is disjoint with each other.
    std::vector<pasture_t*> pastures;
    array_pool_t<pasture_t> pasture_pool;

    array_pool_t<ssa_value_t> input_pool;
    array_pool_t<block_data_t> block_pool;
};

#endif
