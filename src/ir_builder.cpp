#include "ir_builder.hpp"

#include <boost/container/small_vector.hpp>

#include "alloca.hpp"
#include "bitset.hpp"
#include "compiler_error.hpp"
#include "globals.hpp"
#include "pstring.hpp"
#include "ir.hpp"
#include "gvar_loc_manager.hpp"
#include "rpn.hpp"

namespace bc = boost::container;

namespace // Anonymous namespace
{ 

// Data associated with each block node, to be used when making IRs.
struct block_d
{
    // Variables are mapped to a range in this order:
    // 1) local variables
    // 2) global variables
    // 3) global variable sets
    // The following sets use this order.

    // An array of size 'num_fn_vars()'
    // Keeps track of which ssa node a var refers to.
    // A handle of {0} means the local var isn't in the block.
    ssa_value_t* fn_vars = nullptr;

    // An array of size 'num_fn_vars()'
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
    ir_builder_t(ir_t& ir, fn_t const& fn);
private:
    // Helpers
    std::size_t num_locals() const { return fn.def().local_vars.size(); }
    std::size_t num_fn_vars() const { return num_locals() + ir.gvar_loc_manager.num_unique_locators(); }
    type_t var_i_type(unsigned var_i) const
    {
        if(var_i < num_locals())
            return fn.def().local_vars[var_i].type;
        return ir.gvar_loc_manager.type({ var_i - num_locals() });
    }

    locator_t var_i_locator(unsigned var_i) const
    {
        if(var_i < num_locals())
            return locator_t::local(var_i);
        return ir.gvar_loc_manager.locator(gvar_loc_manager_t::index_t{ var_i - num_locals() });
    }

    unsigned to_var_i(gvar_loc_manager_t::index_t index) const { return index.value + num_locals(); }

    [[gnu::noreturn]] 
    void compiler_error(pstring_t pstring, std::string const& what) const
    { 
        // The parsed file was already deleted,
        // so reload it just for this error:
        file_contents_t file(pstring.file_i);
        ::compiler_error(file, pstring, what); 
    }

    void compile();
    cfg_ht compile_block(cfg_ht cfg_h);

    ssa_ht insert_fn_call(cfg_ht cfg_node, fn_ht fn, rpn_value_t const* args);
    ssa_ht insert_array_index(cfg_ht cfg_node, rpn_value_t const& array, rpn_value_t const& arg);
    void exits_with_jump(cfg_node_t& node);
    void exits_with_branch(cfg_node_t& node, ssa_value_t condition);
    cfg_ht compile_goto(cfg_ht branch_node);

    // Block and local variable functions
    cfg_ht insert_cfg(bool seal, pstring_t label_name = {});
    void seal_block(block_d& block_data);
    void fill_phi_args(ssa_ht phi, unsigned var_i);
    ssa_value_t var_lookup(cfg_ht node, unsigned var_i);

    // IR generation functions
    cfg_ht compile_expr(cfg_ht cfg_node, token_t const* expr);
    cfg_ht compile_logical_begin(cfg_ht cfg_node, bool short_cut_i);
    cfg_ht compile_logical_end(cfg_ht cfg_node, bool short_cut_i);
    void compile_assign(cfg_node_t& cfg_node);
    void compile_assign_arith(cfg_node_t&, ssa_op_t op, bool carry = false);
    void compile_binary_operator(cfg_node_t&, ssa_op_t op, type_t result_type, 
                                 bool carry = false);
    void compile_arith(cfg_node_t& cfg_node, ssa_op_t op, bool carry = false);
    void compile_shift(cfg_node_t& cfg_node, ssa_op_t op);
    void compile_compare(cfg_node_t& cfg_node, ssa_op_t op);
    void force_cast(cfg_node_t&, rpn_value_t& rpn_value, type_t to_type);
    void force_boolify(cfg_node_t& cfg_node, rpn_value_t& rpn_value);
    bool cast(cfg_node_t&, rpn_value_t& rpn_value, type_t to_type);
    void throwing_cast(cfg_node_t&, rpn_value_t& rpn_value, type_t to_type);
    std::uint64_t cast_args(
        cfg_node_t& cfg_node,
        pstring_t pstring, 
        rpn_value_t* begin, rpn_value_t* end, 
        type_t const* type_begin);

    struct logical_data_t
    {
        cfg_ht branch_node;
        pstring_t lhs_pstring;
    };

    struct label_t
    {
        cfg_ht node = {};
        bc::small_vector<cfg_ht, 2> inputs;
    };

public:
    ir_t& ir;
private:
    fn_t const& fn;
    stmt_t const* stmt;
    rpn_stack_t rpn_stack;
    bc::small_vector<logical_data_t, 8> logical_stack;

    bc::small_vector<bc::small_vector<cfg_ht, 4>, 4> break_stack;
    bc::small_vector<bc::small_vector<cfg_ht, 4>, 4> continue_stack;

    bc::small_vector<ssa_value_t, 8> return_values;
    bc::small_vector<cfg_ht, 8> return_jumps;

    rh::robin_map<stmt_t const*, label_t> label_map;

    inline static thread_local array_pool_t<ssa_value_t> input_pool;
};

ir_builder_t::ir_builder_t(ir_t& ir, fn_t const& fn)
: ir(ir)
, fn(fn)
{
    ir.gvar_loc_manager.init(fn.handle());
    stmt = fn.def().stmts.data();
    input_pool.clear();
    compile();
}

void ir_builder_t::compile()
{
    ir.root = insert_cfg(true);

    ssa_ht entry = ir.root->emplace_ssa(SSA_entry, TYPE_VOID);
    entry->append_daisy();

    // Insert nodes for the arguments
    for(unsigned i = 0; i < fn.def().num_params; ++i)
    {
        ir.root.data<block_d>().fn_vars[i] = ir.root->emplace_ssa(
            SSA_read_global, fn.def().local_vars[i].type, entry, 
            locator_t::arg(fn.handle(), i, 0));
    }

    // Insert nodes for gvar reads
    ir.gvar_loc_manager.for_each_locator([&](locator_t loc, gvar_loc_manager_t::index_t i)
    {
        ir.root.data<block_d>().fn_vars[to_var_i(i)] = 
            ir.root->emplace_ssa(
                SSA_read_global, ir.gvar_loc_manager.type(i), 
                entry, loc);
    });

    // Create all of the SSA graph, minus the exit node:
    cfg_ht end = compile_block(ir.root);
    exits_with_jump(*end);

    // Now create the exit block.
    // All return statements create a jump, which will jump to the exit node.
    type_t return_type = fn.type().return_type();
    if(return_type != TYPE_VOID)
        return_values.push_back(
            end->emplace_ssa(SSA_uninitialized, return_type));

    ir.exit = insert_cfg(true);

    for(cfg_ht node : return_jumps)
        node->build_set_output(0, ir.exit);
    end->build_set_output(0, ir.exit);

    // Write all globals at the exit:
    std::vector<ssa_value_t> return_inputs;
    return_inputs.reserve(ir.gvar_loc_manager.num_unique_locators() * 2);

    ir.gvar_loc_manager.for_each_locator([&](locator_t loc, gvar_loc_manager_t::index_t i)
    {
        return_inputs.push_back(var_lookup(ir.exit, to_var_i(i)));
        return_inputs.push_back(loc);
    });

    ssa_ht ret = ir.exit->emplace_ssa(SSA_return, TYPE_VOID);

    // Append the return value, if it exists:
    if(return_type != TYPE_VOID)
    {
        ssa_ht phi = ir.exit->emplace_ssa(SSA_phi, return_type);
        phi->assign_input(&*return_values.begin(), &*return_values.end());
        return_inputs.push_back(phi);
        return_inputs.push_back(locator_t::ret(fn.handle()));
    }

    assert(return_inputs.size() % 2 == 0);
    ret->assign_input(&*return_inputs.begin(), &*return_inputs.end());
    ret->append_daisy();

#ifndef NDEBUG
    for(cfg_ht h = ir.cfg_begin(); h; ++h)
        assert(h.data<block_d>().sealed());
#endif
}

cfg_ht ir_builder_t::compile_block(cfg_ht cfg_node)
{
    while(true)
    switch(stmt->name)
    {
    default: // Handles var inits
        if(is_var_init(stmt->name))
        {
            unsigned const var_i = get_local_var_i(stmt->name);

            ssa_value_t value;
            if(stmt->expr)
            {
                cfg_node = compile_expr(cfg_node, stmt->expr);
                throwing_cast(*cfg_node, rpn_stack.peek(0), 
                              fn.def().local_vars[var_i].type);
                value = rpn_stack.only1().value;
            }
            else
            {
                value = cfg_node->emplace_ssa(
                    SSA_uninitialized, fn.def().local_vars[var_i].type);
            }

            cfg_node.data<block_d>().fn_vars[var_i] = value;
            ++stmt;
        }
        else
            throw std::runtime_error("Unimplemented stmt.");
        break;

    case STMT_END_BLOCK:
        ++stmt;
        return cfg_node;

    case STMT_EXPR:
        cfg_node = compile_expr(cfg_node, stmt->expr);
        ++stmt;
        break;

    case STMT_IF:
        {
            // Branch the active node.
            cfg_ht branch = compile_expr(cfg_node, stmt->expr);
            ++stmt;
            throwing_cast(*branch, rpn_stack.only1(), {TYPE_BOOL});
            exits_with_branch(*branch, rpn_stack.only1().value);

            // Create new cfg_node for the 'true' branch.
            cfg_ht begin_true = insert_cfg(true);
            branch->build_set_output(1, begin_true);
            cfg_ht end_true = compile_block(begin_true);
            exits_with_jump(*end_true);

            if(stmt->name == STMT_ELSE)
            {
                // Create new block for the 'false' branch.
                ++stmt;
                cfg_ht begin_false = insert_cfg(true);
                branch->build_set_output(0, begin_false);
                // repurpose 'branch' to hold end of the 'false' branch.
                // Simplifies the assignment that follows.
                branch = compile_block(begin_false);
                exits_with_jump(*branch);
            }

            // Merge the two nodes.
            cfg_node = insert_cfg(true);
            end_true->build_set_output(0, cfg_node);
            branch->build_set_output(0, cfg_node);
            break;
        }

    case STMT_WHILE:
        {
            // The loop condition will go in its own block.
            exits_with_jump(*cfg_node);
            cfg_ht begin_branch = insert_cfg(false);
            cfg_node->build_set_output(0, begin_branch);

            cfg_ht end_branch = compile_expr(begin_branch, stmt->expr);
            ++stmt;
            throwing_cast(*end_branch, rpn_stack.only1(), {TYPE_BOOL});
            exits_with_branch(*end_branch, rpn_stack.only1().value);

            continue_stack.emplace_back();
            break_stack.emplace_back();

            // Compile the body.
            cfg_ht begin_body = insert_cfg(true);
            end_branch->build_set_output(1, begin_body);
            cfg_ht end_body = compile_block(begin_body);
            exits_with_jump(*end_body);
            end_body->build_set_output(0, begin_branch);

            // All continue statements jump to branch_node.
            // TODO: properly implement continue in 'for'
            assert(false);
            for(cfg_ht node : continue_stack.back())
                node->build_set_output(0, begin_branch);
            seal_block(begin_branch.data<block_d>());

            // Create the exit node.
            cfg_node = insert_cfg(true);
            end_branch->build_set_output(0, cfg_node);
            for(cfg_ht node : break_stack.back())
                node->build_set_output(0, cfg_node);

            continue_stack.pop_back();
            break_stack.pop_back();
            break;
        }

    case STMT_DO:
        {
            ++stmt;
            continue_stack.emplace_back();
            break_stack.emplace_back();

            // Compile the loop body
            exits_with_jump(*cfg_node);
            cfg_ht begin_body = insert_cfg(false);
            cfg_node->build_set_output(0, begin_body);
            cfg_ht end_body = compile_block(begin_body);

            assert(stmt->name == STMT_WHILE);

            // The loop condition can go in its own block, which is
            // necessary to implement 'continue'.
            exits_with_jump(*end_body);
            cfg_ht begin_branch = insert_cfg(true);
            end_body->build_set_output(0, begin_branch);

            cfg_ht end_branch = compile_expr(begin_branch, stmt->expr);
            ++stmt;
            throwing_cast(*end_branch, rpn_stack.only1(), {TYPE_BOOL});
            exits_with_branch(*end_branch, rpn_stack.only1().value);

            end_branch->build_set_output(1, begin_body);
            seal_block(begin_body.data<block_d>());

            // All continue statements jump to the branch node.
            for(cfg_ht node : continue_stack.back())
                node->build_set_output(0, begin_branch);

            // Create the exit cfg_node.
            cfg_node = insert_cfg(true);
            end_branch->build_set_output(0, cfg_node);
            for(cfg_ht node : break_stack.back())
                node->build_set_output(0, cfg_node);

            continue_stack.pop_back();
            break_stack.pop_back();
            break;
        }

    case STMT_RETURN:
        {
            type_t return_type = fn.type().return_type();
            if(stmt->expr)
            {
                cfg_node = compile_expr(cfg_node, stmt->expr);
                throwing_cast(*cfg_node, rpn_stack.only1(), return_type);
                return_values.push_back(rpn_stack.only1().value);
            }
            else if(return_type.name() != TYPE_VOID)
                compiler_error(stmt->pstring, fmt(
                    "Expecting return expression of type %.", return_type));
            return_jumps.push_back(cfg_node);
            cfg_node = compile_goto(cfg_node);
            ++stmt;
            break;
        }

    case STMT_BREAK:
        if(break_stack.empty())
            compiler_error(stmt->pstring, "break statement outside of loop.");
        break_stack.back().push_back(cfg_node);
        cfg_node = compile_goto(cfg_node);
        ++stmt;
        break;

    case STMT_CONTINUE:
        if(continue_stack.empty())
            compiler_error(stmt->pstring, 
                           "continue statement outside of loop.");
        continue_stack.back().push_back(cfg_node);
        cfg_node = compile_goto(cfg_node);
        ++stmt;
        break;

    case STMT_LABEL:
        {
            label_t& label = label_map[stmt];
            unsigned const use_count = stmt->use_count;
            pstring_t label_name = stmt->pstring;
            ++stmt;

            // If there's no goto to this label, just ignore it.
            if(use_count == 0)
                break;

            exits_with_jump(*cfg_node);
            label.inputs.push_back(cfg_node);

            if(use_count + 1 == label.inputs.size())
            {
                // All the gotos to this label have been compiled,
                // that means this block can be sealed immediately!
                label.node = cfg_node = insert_cfg(true, label_name);
                for(cfg_ht node : label.inputs)
                    node->build_set_output(0, label.node);
            }
            else // Otherwise, seal the node at a later time.
                label.node = cfg_node = insert_cfg(false, label_name);

            break;
        }

    case STMT_GOTO:
        {
            label_t& label = label_map[&fn.def()[stmt->link]];
            unsigned const use_count = fn.def()[stmt->link].use_count;
            assert(use_count > 0);
            ++stmt;

            // Add the jump to the label.
            label.inputs.push_back(cfg_node);
            cfg_node = compile_goto(cfg_node);

            // If this is the last goto, finish and seal the node.
            if(use_count + 1 == label.inputs.size())
            {
                assert(label.node);
                for(cfg_ht node : label.inputs)
                    node->build_set_output(0, label.node);
                // Seal the block.
                seal_block(label.node.data<block_d>());
            }
            break;
        }

    case STMT_GOTO_MODE:
        {
            assert(stmt->expr);

            cfg_ht branch = cfg_node;

            cfg_ht mode = insert_cfg(true);

            cfg_node = compile_goto(cfg_node);
            branch->build_set_output(0, mode);

            compile_expr(mode, stmt->expr);

            ++stmt;
            break;
        }
    }
    assert(false);
}

ssa_ht ir_builder_t::insert_fn_call(cfg_ht cfg_node, fn_ht call, rpn_value_t const* args)
{
    bc::small_vector<ssa_value_t, 32> fn_inputs;

    // The [0] argument holds the fn_t ptr.
    fn_inputs.push_back(ssa_value_t(locator_t::fn(call)));
    
    // Prepare the input globals

    bool const is_idep = fn.global.ideps().count(&call->global) > 0;

    bitset_t const* reads_set;
    if(!call->mode || is_idep)
        reads_set = &call->ir_reads();
    else
        reads_set = &call->lang_gvars();

    ir.gvar_loc_manager.for_each_singleton([&](gvar_ht gvar, gvar_loc_manager_t::index_t i)
    {
        if(reads_set->test(gvar.value))
        {
            fn_inputs.push_back(var_lookup(cfg_node, to_var_i(i)));
            fn_inputs.push_back(ir.gvar_loc_manager.locator(i));
        }
    });

    unsigned const bs_size = impl_bitset_size<gvar_t>();
    assert(bs_size == reads_set->size());
    assert(bs_size == gvar_loc_manager_t::bitset_size());
    auto* const temp_set = ALLOCA_T(bitset_uint_t, bs_size);

    ir.gvar_loc_manager.for_each_set([&](bitset_uint_t const* gvar_set, gvar_loc_manager_t::index_t i)
    {
        bitset_copy(bs_size, temp_set, gvar_set);
        bitset_and(bs_size, temp_set, reads_set->data());
        if(!bitset_all_clear(bs_size, temp_set))
        {
            fn_inputs.push_back(var_lookup(cfg_node, to_var_i(i)));
            fn_inputs.push_back(ir.gvar_loc_manager.locator(i));
        }

    });

    // Prepare the arguments
    unsigned const num_args = call->type().num_params();
    for(unsigned i = 0; i < num_args; ++i)
    {
        locator_t const loc = locator_t::arg(call, i, 0);

        // If the arg isn't used in the function, ignore it.
        if(is_idep && call->lvars().index(loc) < 0)
            continue;

        fn_inputs.push_back(args[i].value);
        fn_inputs.push_back(loc);
    }

    // Create the dependent node.
    ssa_op_t const op = call->mode ? SSA_goto_mode : SSA_fn_call;
    ssa_ht ret = cfg_node->emplace_ssa(op, call->type().return_type());
    assert(fn_inputs.size() % 2 == 1);
    ret->link_append_input(&*fn_inputs.begin(), &*fn_inputs.end());
    ret->append_daisy();

    if(!call->mode)
    {
        // After the fn is called, read all the globals it has written to:
        bitset_t const& writes_set = call->ir_writes();

        ir.gvar_loc_manager.for_each_singleton([&](gvar_ht gvar, gvar_loc_manager_t::index_t i)
        {
            if(writes_set.test(gvar.value))
            {
                ssa_ht read = cfg_node->emplace_ssa(
                    SSA_read_global, gvar->type(), ret, ir.gvar_loc_manager.locator(i));
                block_d& block_data = cfg_node.data<block_d>();
                block_data.fn_vars[to_var_i(i)] = read;
            }
        });

        ir.gvar_loc_manager.for_each_set([&](bitset_uint_t const* gvar_set, gvar_loc_manager_t::index_t i)
        {
            bitset_copy(bs_size, temp_set, gvar_set);
            bitset_and(bs_size, temp_set, writes_set.data());
            if(!bitset_all_clear(bs_size, temp_set))
            {
                ssa_ht read = cfg_node->emplace_ssa(
                    SSA_read_global, TYPE_VOID, ret, ir.gvar_loc_manager.locator(i));
                block_d& block_data = cfg_node.data<block_d>();
                block_data.fn_vars[to_var_i(i)] = read;
            }
        });
    }

    return ret;
}

ssa_ht ir_builder_t::insert_array_index(cfg_ht cfg_node, rpn_value_t const& array, rpn_value_t const& arg)
{
    locator_t loc = var_i_locator(array.var_i);
    ssa_ht ret = cfg_node->emplace_ssa(SSA_read_array, array.type.elem_type(),
                                       array.value, loc, arg.value);
    return ret;
}

void ir_builder_t::exits_with_jump(cfg_node_t& node)
{
    node.alloc_output(1);
}

void ir_builder_t::exits_with_branch(cfg_node_t& node, ssa_value_t condition)
{
    ssa_ht if_h = node.emplace_ssa(SSA_if, TYPE_VOID, condition);
    if_h->append_daisy();
    node.alloc_output(2);
    assert(node.output_size() == 2);
    assert(node.last_daisy() == if_h);
}

// Jumps are like 'break', 'continue', 'goto', etc.
cfg_ht ir_builder_t::compile_goto(cfg_ht branch_node)
{
    // The syntax allows code to exist following a jump statement.
    // Said code is unreachable, but gets compiled anyway.
    // Implement using a conditional that always takes the false branch.
    // (This will be optimized out later)

    exits_with_branch(*branch_node, 0u);
    cfg_ht dead_branch = insert_cfg(true);
    branch_node->build_set_output(1, dead_branch);
    return dead_branch;
}

cfg_ht ir_builder_t::insert_cfg(bool seal, pstring_t label_name)
{
    cfg_ht new_node = ir.emplace_cfg();
    cfg_data_pool::resize<block_d>(cfg_pool::array_size());
    block_d& block_data = new_node.data<block_d>();
    block_data.fn_vars = input_pool.alloc(num_fn_vars());
    if(seal == false)
        block_data.unsealed_phis = input_pool.alloc(num_fn_vars());
    else
        block_data.unsealed_phis = nullptr;
    block_data.label_name = label_name;
    return new_node;
}

void ir_builder_t::seal_block(block_d& block_data)
{
    assert(block_data.sealed() == false);
    for(unsigned i = 0; i < num_fn_vars(); ++i)
        if(block_data.unsealed_phis[i].holds_ref())
            fill_phi_args(block_data.unsealed_phis[i].handle(), i);
    block_data.unsealed_phis = nullptr;
}

// Relevant paper:
//   Simple and Efficient Construction of Static Single Assignment Form
ssa_value_t ir_builder_t::var_lookup(cfg_ht cfg_node, unsigned var_i)
{
    block_d& block_data = cfg_node.data<block_d>();
    assert(block_data.fn_vars);

    if(ssa_value_t lookup = block_data.fn_vars[var_i])
    {
        assert(lookup);
        return lookup;
    }
    else if(block_data.sealed())
    {
        // If the block doesn't contain a definition for 'var_i',
        // recursively look up its definition in predecessor nodes.
        // If there are multiple predecessors, a phi node will be created.
        try
        {
            switch(cfg_node->input_size())
            {
            case 0:
                throw var_lookup_error_t();
            case 1:
                return var_lookup(cfg_node->input(0), var_i);
            default:
                ssa_ht phi = cfg_node->emplace_ssa(
                    SSA_phi, var_i_type(var_i));
                block_data.fn_vars[var_i] = phi;
                fill_phi_args(phi, var_i);
            #ifndef NDEBUG
                for(unsigned i = 0; i < phi->input_size(); ++i)
                    assert(phi->input(i));
            #endif
                assert(phi);
                return phi;
            }
        }
        catch(var_lookup_error_t&)
        {
            if(block_data.label_name.size && var_i < num_locals())
            {
                pstring_t var_name = fn.def().local_vars[var_i].name;
                file_contents_t file(var_name.file_i);
                throw compiler_error_t(
                    fmt_error(file, block_data.label_name, fmt(
                        "Jump to label crosses initialization "
                        "of variable %.", var_name.view(file.source())))
                    + fmt_error(file, var_name, fmt(
                        "Variable is defined here:")));
            }
            throw;
        }
    }
    else 
    {
        // If the node is unsealed, the predecessors are not fully known,
        // and thus it's impossible to determine the var's definition.
        // To work around this, an incomplete phi node can be created, which
        // will then be filled when the node is sealed.
        assert(block_data.unsealed_phis);
        ssa_ht phi = cfg_node->emplace_ssa(SSA_phi, var_i_type(var_i));
        block_data.fn_vars[var_i] = phi;
        block_data.unsealed_phis[var_i] = phi;
        assert(phi);
        return phi;
    }
}

void ir_builder_t::fill_phi_args(ssa_ht phi, unsigned var_i)
{
    // Input must be an empty phi node.
    assert(phi->op() == SSA_phi);
    assert(phi->input_size() == 0);

    // Fill the input array using local lookups.
    cfg_ht cfg_node = phi->cfg_node();
    unsigned const input_size = cfg_node->input_size();
    phi->alloc_input(input_size);
    for(unsigned i = 0; i < input_size; ++i)
    {
        // This has to be on two lines, otherwise reference invalidation lurks.
        ssa_value_t v = var_lookup(cfg_node->input(i), var_i);
        phi->build_set_input(i, v);
    }
}

cfg_ht ir_builder_t::compile_expr(cfg_ht cfg_node, token_t const* expr)
{
    rpn_stack.clear();
    logical_stack.clear();

    for(token_t const* token = expr; token->type; ++token)
    {
        switch(token->type)
        {
        default:
            throw std::runtime_error(fmt("Invalid token '%' in expression.", token_name(token->type)));

        case TOK_ident:
            rpn_stack.push({ 
                .value = var_lookup(cfg_node, token->value), 
                .category = LVAL, 
                .type = fn.def().local_vars[token->value].type, 
                .pstring = token->pstring,
                .var_i = token->value });
            break;

        case TOK_global_ident:
            {
                global_t* global = token->ptr<global_t>();
                switch(global->gclass())
                {
                default:
                    throw std::runtime_error(
                        "Unimplemented global in expression.");
                case GLOBAL_VAR:
                    {
                        unsigned const var_i = to_var_i(ir.gvar_loc_manager.index(global->handle<gvar_ht>()));
                        rpn_stack.push({ 
                            .value = var_lookup(cfg_node, var_i), 
                            .category = LVAL, 
                            .type = global->impl<gvar_t>().type(), 
                            .pstring = token->pstring,
                            .var_i = var_i });
                    }
                    break;

                case GLOBAL_FN:
                    rpn_stack.push({ 
                        .value = ssa_value_t(locator_t::fn(global->handle<fn_ht>())),
                        .category = RVAL, 
                        .type = global->impl<fn_t>().type(), 
                        .pstring = token->pstring });
                    break;
                }
            }
            break;

        case TOK_assign:
            compile_assign(*cfg_node);
            break;

        case TOK_number:
            rpn_stack.push({
                .value = token->value, 
                .category = RVAL, 
                .type = { TYPE_NUM }, 
                .pstring = token->pstring });
            break;

        case TOK_apply:
            {
                // TOK_apply is a psuedo token used to represent application. 
                // The token's 'value' stores the application's arity:
                std::size_t const num_args = token->value;

                // The eval stack contains the arguments to be applied.
                // Right beneath those it contains the fn value to be called.
                rpn_value_t& fn_val = rpn_stack.peek(num_args);

                if(fn_val.type.name() != TYPE_FN)
                {
                    compiler_error(fn_val.pstring, fmt(
                        "Expecting function type. Got %.", fn_val.type));
                }

                std::size_t const num_params = fn_val.type.num_params();
                type_t const* const params = fn_val.type.types();

                if(num_args != num_params)
                {
                    compiler_error(
                        fn_val.pstring, fmt(
                        "Passed % arguments to a function of type %. "
                        "Expecting % arguments.",
                        num_args, fn_val.type, num_params));
                }

                // Now for the arguments.
                // Cast all arguments to match the fn signature.
                rpn_value_t* const args = &rpn_stack.peek(num_args - 1);

                for(std::uint64_t arg = 0, fail_bits = cast_args(*cfg_node, 
                    fn_val.pstring, args, rpn_stack.past_top(), params)
                   ; fail_bits; ++arg)
                {
                    if(fail_bits & 1)
                    {
                        compiler_error(
                            args[arg].pstring, fmt(
                            "Unable to convert type % "
                            "to type % in function application.\n"
                            "Expected signature: % ",
                            args[arg].type, params[arg], fn_val.type));
                    }
                    fail_bits >>= 1;
                }

                // For now, only const fns are allowed.
                // In the future, fn pointers may be supported.
                assert(fn_val.value.is_locator());
                fn_ht fn = fn_val.value.locator().fn();

                // Type checks are done. Now convert the call to SSA.
                ssa_ht fn_node = insert_fn_call(cfg_node, fn, args);

                // Update the eval stack.
                rpn_value_t new_top =
                {
                    .value = fn_node,
                    .category = RVAL, 
                    .type = fn_val.type.return_type(), 
                    .pstring = concat(fn_val.pstring, token->pstring)
                };

                rpn_stack.pop(num_args + 1);
                rpn_stack.push(std::move(new_top));

                break;
            }

        case TOK_index:
            {
                // TOK_index is a psuedo token used to array indexing. 

                // The eval stack contains the index on top.
                // Right beneath it contains the array.
                rpn_value_t& array_val = rpn_stack.peek(1);

                if(array_val.type.name() != TYPE_ARRAY)
                {
                    compiler_error(array_val.pstring, fmt(
                        "Expecting array type. Got %.", array_val.type));
                }

                rpn_value_t& array_index = rpn_stack.peek(0);

                // Array indexes are always bytes.
                throwing_cast(*cfg_node, array_index, TYPE_U);

                // Type checks are done. Now convert the index to SSA.
                ssa_ht index_node = insert_array_index(cfg_node, array_val, array_index);

                // Update the eval stack.
                array_val.value = index_node;
                array_val.category = to_indexed(array_val.category);
                array_val.type = array_val.type.elem_type();
                array_val.pstring = token->pstring;
                rpn_stack.pop(1);
            break;

                // TODO
                break;
            }

        case TOK_logical_and:
            cfg_node = compile_logical_begin(cfg_node, false);
            break;
        case TOK_logical_or:
            cfg_node = compile_logical_begin(cfg_node, true);
            break;
        case TOK_end_logical_and:
            cfg_node = compile_logical_end(cfg_node, false);
            break;
        case TOK_end_logical_or:
            cfg_node = compile_logical_end(cfg_node, true);
            break;

        case TOK_eq:
            compile_compare(*cfg_node, SSA_eq);
            break;
        case TOK_not_eq:
            compile_compare(*cfg_node, SSA_not_eq);
            break;
        case TOK_gt:
            std::swap(rpn_stack.peek(0), rpn_stack.peek(1));
            // fall-through
        case TOK_lt:
            compile_compare(*cfg_node, SSA_lt);
            break;
        case TOK_gte:
            std::swap(rpn_stack.peek(0), rpn_stack.peek(1));
            // fall-through
        case TOK_lte:
            compile_compare(*cfg_node, SSA_lte);
            break;

        case TOK_asterisk: 
            compile_arith(*cfg_node, SSA_mul);
            break;
        case TOK_fslash: 
            compile_arith(*cfg_node, SSA_div);
            break;
        case TOK_plus:
            compile_arith(*cfg_node, SSA_add, true);  
            break;
        case TOK_minus: 
            compile_arith(*cfg_node, SSA_sub, true);
            break;
        case TOK_lshift:
            compile_shift(*cfg_node, SSA_shl);  
            break;
        case TOK_rshift: 
            compile_shift(*cfg_node, SSA_shr);
            break;
        case TOK_bitwise_and: 
            compile_arith(*cfg_node, SSA_and);
            break;
        case TOK_bitwise_or:  
            compile_arith(*cfg_node, SSA_or);
            break;
        case TOK_bitwise_xor:
            compile_arith(*cfg_node, SSA_xor);
            break;

        case TOK_plus_assign:
            compile_assign_arith(*cfg_node, SSA_add, true);
            break;
        case TOK_minus_assign:
            compile_assign_arith(*cfg_node, SSA_sub, true);
            break;
        case TOK_bitwise_and_assign:
            compile_assign_arith(*cfg_node, SSA_and);
            break;
        case TOK_bitwise_or_assign:
            compile_assign_arith(*cfg_node, SSA_or);
            break;
        case TOK_bitwise_xor_assign:
            compile_assign_arith(*cfg_node, SSA_xor);
            break;
        }
    }

    assert(rpn_stack.size() == 1);
    assert(logical_stack.empty());
    return cfg_node;
}

cfg_ht ir_builder_t::compile_logical_begin(cfg_ht cfg_node, bool short_cut_i)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast(*cfg_node, top, { TYPE_BOOL });
    cfg_ht branch_node = cfg_node;
    exits_with_branch(*branch_node, top.value);
    logical_stack.push_back({ branch_node, top.pstring });
    rpn_stack.pop();

    cfg_ht long_cut = insert_cfg(true);
    branch_node->build_set_output(!short_cut_i, long_cut);
    return long_cut;
}

cfg_ht ir_builder_t::compile_logical_end(cfg_ht cfg_node, bool short_cut_i)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast(*cfg_node, top, {TYPE_BOOL});

    logical_data_t logical = logical_stack.back();
    logical_stack.pop_back();

    cfg_ht merge_node = insert_cfg(true);
    exits_with_jump(*cfg_node);
    cfg_node->build_set_output(0, merge_node);
    logical.branch_node->build_set_output(short_cut_i, merge_node);

    top.value = merge_node->emplace_ssa(
        SSA_phi, type_t{TYPE_BOOL}, top.value, short_cut_i);
    top.pstring = concat(logical.lhs_pstring, top.pstring);
    top.category = RVAL;
    assert(top.type == type_t{TYPE_BOOL});
    return merge_node;
}

// TODO: handle?
// TODO: copy arrays
void ir_builder_t::compile_assign(cfg_node_t& cfg_node)
{
    rpn_value_t& assignee = rpn_stack.peek(1);
    rpn_value_t& assignment = rpn_stack.peek(0);

    assignee.pstring = concat(assignee.pstring, assignee.pstring);

    if(assignee.category == RVAL)
    {
        compiler_error(assignee.pstring, 
            "Expecting lvalue on left side of assignment.");
    }

    throwing_cast(cfg_node, assignment, assignee.type);

    assert(assignee.var_i < num_fn_vars());
    assert(assignment.value);

    // Remap the identifier to point to the new value:
    block_d& block_data = cfg_node.handle().data<block_d>();
    if(assignee.category == LVAL_INDEX)
    {
        // For arrays, we'll have to create a SSA_write_array node.

        assert(assignee.value.holds_ref());
        ssa_ht read = assignee.value.handle();
        assert(read->op() == SSA_read_array);

        locator_t loc = var_i_locator(assignee.var_i);
        ssa_ht write = cfg_node.emplace_ssa(
            SSA_write_array, var_i_type(assignee.var_i),
            read->input(0), loc, read->input(2), assignment.value);

        block_data.fn_vars[assignee.var_i] = write;
    }
    else
    {
        assert(assignee.category == LVAL);
        block_data.fn_vars[assignee.var_i] = assignment.value;
    }


    // Leave the assignee on the stack, slightly modified.
    assignee.category = RVAL;
    rpn_stack.pop();
}

void ir_builder_t::compile_assign_arith(cfg_node_t& cfg_node, ssa_op_t op, 
                                        bool carry)
{
    rpn_stack.tuck(rpn_stack.peek(1), 1);
    throwing_cast(cfg_node, rpn_stack.peek(0), rpn_stack.peek(1).type);
    compile_arith(cfg_node, op, carry);
    compile_assign(cfg_node);
}

// Applies an operator to the top two values on the eval stack,
// turning them into a single value.
void ir_builder_t::compile_binary_operator(cfg_node_t& cfg_node, 
                                           ssa_op_t op, type_t result_type,
                                           bool carry)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);


    // Result will remain in 'lhs'.
    ssa_value_t result;
    if(carry)
        result = cfg_node.emplace_ssa(op, result_type, lhs.value, rhs.value, 0);
    else
        result = cfg_node.emplace_ssa(op, result_type, lhs.value, rhs.value);

    rpn_value_t new_top =
    {
        .value = result,
        .category = RVAL, 
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };
    
    rpn_stack.pop(2);
    rpn_stack.push(new_top);
}

void ir_builder_t::compile_shift(cfg_node_t& cfg_node, ssa_op_t op)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    if(!is_arithmetic(lhs.type.name()) || !is_arithmetic(rhs.type.name()))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting arithmetic types.");
    }

    if(rhs.type.name() != TYPE_U)
        compiler_error(rhs.pstring, "Ride-hand side of shift must be type U.");

    compile_binary_operator(cfg_node, op, lhs.type);
}

void ir_builder_t::compile_arith(cfg_node_t& cfg_node, ssa_op_t op, bool carry)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    if(!is_arithmetic(lhs.type.name()) || !is_arithmetic(rhs.type.name()))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting arithmetic types.");
    }

    type_t result_type = lhs.type;
    if(result_type.name() == TYPE_NUM)
        result_type = rhs.type;

    if(result_type.name() == TYPE_NUM)
    {
        // TODO
        assert(false);
        return;
    }

    if(result_type != rhs.type)
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, fmt("Operator is not defined for this type combination. (% and %)",
                                    lhs.type, rhs.type));
    }

    assert(is_arithmetic(result_type));

    compile_binary_operator(cfg_node, op, result_type, carry);
}

void ir_builder_t::compile_compare(cfg_node_t& cfg_node, ssa_op_t op)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    if(!is_arithmetic(lhs.type.name()) || !is_arithmetic(rhs.type.name()))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting arithmetic types.");
    }

    compile_binary_operator(cfg_node, op, { TYPE_BOOL });
}

// This is used to implement the other cast functions.
void ir_builder_t::force_cast(cfg_node_t& cfg_node, 
                              rpn_value_t& rpn_value, type_t to_type)
{
    rpn_value.value = cfg_node.emplace_ssa(
        SSA_cast, to_type, rpn_value.value);
    rpn_value.type = to_type;
    rpn_value.category = RVAL;
}

// This is used to implement the other cast functions.
void ir_builder_t::force_boolify(cfg_node_t& cfg_node, rpn_value_t& rpn_value)
{
    rpn_value.value = cfg_node.emplace_ssa(
        SSA_not_eq, {TYPE_BOOL}, rpn_value.value, 0u);
    rpn_value.type = {TYPE_BOOL};
    rpn_value.category = RVAL;
}


bool ir_builder_t::cast(cfg_node_t& cfg_node,
                        rpn_value_t& rpn_value, type_t to_type)
{
    switch(can_cast(rpn_value.type, to_type))
    {
    default: assert(false);
    case CAST_FAIL: return false;
    case CAST_NOP:
        rpn_value.type = to_type;
        rpn_value.category = RVAL;
        return true;
    case CAST_OP:
        force_cast(cfg_node, rpn_value, to_type);
        return true;
    case CAST_BOOLIFY:
        force_boolify(cfg_node, rpn_value);
        return true;
    }
}

void ir_builder_t::throwing_cast(cfg_node_t& cfg_node,
                                 rpn_value_t& rpn_value, type_t to_type)
{
    if(!cast(cfg_node, rpn_value, to_type))
    {
        compiler_error(rpn_value.pstring, fmt(
            "Unable to convert type % to type %.", 
            rpn_value.type, to_type));
    }
}
// Converts multiple values at once, but only if all casts are valid.
// On success, 0 is returned and 'val_begin' to 'val_end' may be modified
// to their casted type.
// On failure, a bitset is returned where a 1 bit signifies each failed cast.
std::uint64_t ir_builder_t::cast_args(
    cfg_node_t& cfg_node,
    pstring_t pstring, 
    rpn_value_t* begin, rpn_value_t* end, 
    type_t const* type_begin)
{
    assert(begin <= end);
    std::size_t const size = end - begin;
    if(size > 64)
        compiler_error(pstring, "Functions are limited to 64 arguments.");

    std::uint64_t failure = 0;
    cast_result_t* results = ALLOCA_T(cast_result_t, size);
    for(std::size_t i = 0; i != size; ++i)
        if(!(results[i] = can_cast(begin[i].type, type_begin[i])))
            failure |= 1 << i;

    if(failure)
        return failure;

    for(std::size_t i = 0; i != size; ++i)
    {
        if(results[i] == CAST_OP)
            force_cast(cfg_node, begin[i], type_begin[i]);
        else if(results[i] == CAST_BOOLIFY)
            force_boolify(cfg_node, begin[i]);
    }

    return 0; // 0 means no errors!
}

} // end anonymous namespace

void build_ir(ir_t& ir, fn_t const& fn)
{
    cfg_data_pool::scope_guard_t<block_d> cfg_data_guard;
    ir_builder_t b(ir, fn);
}
