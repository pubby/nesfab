#include "ir_builder.hpp"

#include "alloca.hpp"
#include "compiler_error.hpp"

ir_builder_t::ir_builder_t(global_manager_t& global_manager, global_t& global)
: global_manager_ptr(&global_manager)
, global_ptr(&global)
{
    assert(global.gclass = GLOBAL_FN);
    stmt = global.fn->stmts.data();
    new_active_block(true); // TODO?
}

void ir_builder_t::compile()
{
    // Create all of the SSA graph, minus the exit node:
    compile_block();

    // Now create the exit block.
    // All return statements create a jump, which will jump to the exit node.
    type_t return_type = global().type.return_type();
    if(return_type.name != TYPE_VOID)
        return_values.push_back(default_construct(return_type));
    return_jumps.push_back(insert_fence());

    new_active_block(true)->set_input(ir, &*return_jumps.begin(), 
                                          &*return_jumps.end());

    // The actual exit point is a SSA_return node.
    ir.exit = &ir.ssa_pool.insert({
        .op = SSA_return,
        .control = active_block });

    if(return_type.name != TYPE_VOID)
    {
        ssa_node_t* phi = &ir.ssa_pool.insert({
            .op = SSA_phi,
            .control = active_block,
            .type = return_type });
        phi->set_input(ir, &*return_values.begin(), &*return_values.end());
        ir.exit->set_input_v(ir, phi);
    }

    // Finish the IR
    ir.finish_construction();
}

void ir_builder_t::compile_block()
{
    while(true) 
    switch(stmt->name)
    {
    default:
        if(is_var_init(stmt->name))
        {
            unsigned const local_var_i = get_local_var_i(stmt->name);

            ssa_value_t value;
            if(stmt->expr)
                value = compile_expression(stmt->expr).ssa_value;
            else
                value = default_construct(fn().local_vars[local_var_i].type);

            active_block->block_data->local_vars[local_var_i] = value;
            ++stmt;
        }
        else
            throw std::runtime_error("Unimplemented stmt.");
        break;

    case STMT_END_BLOCK:
        ++stmt;
        return;

    case STMT_EXPR:
        compile_expression(stmt->expr);
        ++stmt;
        break;

    case STMT_IF:
        {
            // Create the if node and branches.
            rpn_value_t cond = compile_expression(stmt->expr);
            branch_t branch = compile_branch(cond.ssa_value);

            // Create new block for the 'true' branch.
            new_active_block(true)->set_input_v(ir, branch.true_node);
            ++stmt;
            compile_block();
            branch.true_node = insert_fence(); // End of 'true' block.

            if(stmt->name == STMT_ELSE)
            {
                // Create new block for the 'false' branch.
                new_active_block(true)->set_input_v(ir, branch.false_node);
                ++stmt;
                compile_block();
                branch.false_node = insert_fence(); // End of 'false' block.
            }

            // Merge both branches together into a new block.
            new_active_block(true)->set_input_v(ir, branch.true_node,
                                                    branch.false_node);
            break;
        }

    case STMT_WHILE:
        {
            // The loop condition will go in its own block.
            ssa_node_t* entry_fence = insert_fence();
            ssa_node_t* condition_block = new_active_block(false);

            // Create the if node and branches.
            rpn_value_t cond = compile_expression(stmt->expr);
            branch_t branch = compile_branch(cond.ssa_value);

            // Create new block for the 'true' branch.
            new_active_block(true)->set_input_v(ir, branch.true_node);
            ++stmt;
            continue_stack.push();
            break_stack.push();
            compile_block();

            // All continue statements jump to the condition block,
            // along with the entry fence and the end of the true branch fence.
            {
                auto& vec = continue_stack.top();
                vec.push_back(insert_fence()); // End of the 'true' block.
                vec.push_back(entry_fence);
                condition_block->set_input(ir, &*vec.begin(), &*vec.end());
                seal_block(*condition_block->block_data);
            }

            // Create the exit block.
            {
                auto& vec = break_stack.top();
                vec.push_back(branch.false_node);
                new_active_block(true)->set_input(ir, &*vec.begin(), 
                                                      &*vec.end());
            }

            continue_stack.pop();
            break_stack.pop();
            break;
        }

    case STMT_DO:
        {
            // Compile the loop body
            ssa_node_t* body_block = new_active_block(false);
            body_block->set_input_v(ir, insert_fence(), nullptr);
            ++stmt;
            continue_stack.push();
            break_stack.push();
            compile_block();

            // The loop condition can go in its own block, which is
            // necessary to implement 'continue'.
            {
                auto& vec = continue_stack.top();
                // Only do this if needed; otherwise use the previous block.
                if(!vec.empty())
                {
                    vec.push_back(insert_fence());
                    new_active_block(true)->set_input(ir, &*vec.begin(),
                                                          &*vec.end());
                }
            }

            // Create the if node and branches.
            rpn_value_t cond = compile_expression(stmt->expr);
            branch_t branch = compile_branch(cond.ssa_value);

            // Can finally set the entry block node's input and seal it.
            {
                assert(body_block->input_size == 2);
                assert(body_block->input[1].ptr() == nullptr);

                body_block->input[1] = branch.true_node;
                seal_block(*body_block->block_data);
            }

            // Create the exit block.
            {
                auto& vec = break_stack.top();
                vec.push_back(branch.false_node);
                new_active_block(true)->set_input(ir, &*vec.begin(), 
                                                      &*vec.end());
            }

            continue_stack.pop();
            break_stack.pop();
            break;
        }

    case STMT_RETURN:
        {
            type_t return_type = global().type.return_type();

            if(stmt->expr)
            {
                rpn_value_t value = compile_expression(stmt->expr);

                // Cast value to the return type.
                if(!cast(value, return_type))
                {
                    compiler_error(expr_pstring(stmt->expr), fmt(
                        "Unable to convert type % to type % in return.", 
                        value.type, return_type));
                }

                return_values.push_back(value.ssa_value);
                return_jumps.push_back(compile_jump());
            }
            else if(return_type.name == TYPE_VOID)
                return_jumps.push_back(compile_jump());
            else
            {
                compiler_error(stmt->pstring, fmt(
                    "Expecting return expression of type %.", return_type));
            }

            ++stmt;
            break;
        }

    case STMT_BREAK:
        if(break_stack.empty())
            compiler_error(stmt->pstring, "break statement outside of loop.");
        break_stack.top().push_back(compile_jump());
        ++stmt;
        break;

    case STMT_CONTINUE:
        if(continue_stack.empty())
            compiler_error(stmt->pstring, 
                           "continue statement outside of loop.");
        continue_stack.top().push_back(compile_jump());
        ++stmt;
        break;

    case STMT_LABEL:
        {
            label_t* label = stmt->label; assert(label);
            pstring_t label_name = stmt->pstring;
            ++stmt;

            // If there's no goto to this label, just ignore it.
            if(label->goto_count == 0)
                break;

            if(label->goto_count == label->inputs.size())
            {
                label->inputs.push_back(insert_fence());

                // All the gotos to this label have been compiled,
                // that means this block can be sealed immediately!
                label->node = new_active_block(true, label_name);
                label->node->set_input(ir, &*label->inputs.begin(),
                                           &*label->inputs.end());
            }
            else
            {
                // Otherwise, seal the node at a later time.
                label->inputs.push_back(insert_fence());
                label->node = new_active_block(false, label_name);
            }

            break;
        }

    case STMT_GOTO:
        {
            label_t* label = stmt->label; assert(label);
            ++stmt;
            assert(label->goto_count > 0);

            // Add the jump to the label.
            label->inputs.push_back(compile_jump());

            // +1 because creating the label adds an input (the predecessor).
            // (This condition will only be taken if the label node exists)
            if(label->goto_count+1 == label->inputs.size())
            {
                assert(label->node);
                assert(label->node->input_size == 0);

                // Add the inputs to the ssa block node.
                label->node->set_input(ir, &*label->inputs.begin(),
                                           &*label->inputs.end());

                // Seal the block.
                seal_block(*label->node->block_data);
            }
            break;
        }
    }
    assert(false);
}

// Inserts a fence that covers all memory values in 'bitset',
// then creates 'node' as its dependent.
ssa_node_t* ir_builder_t::insert_partial_fence(ssa_node_t node, 
                                               ds_bitset_t const& bitset)
{
    assert(!node.control);

    ssa_node_t** input_begin = ALLOCA_T(ssa_node_t*, pastures.size());
    ssa_node_t** input_end = input_begin;

    for(auto it = pastures.begin(); it != pastures.end();)
    {
        std::uint64_t is_fence_input = 0;
        std::uint64_t keep_pasture = 0;
        for(unsigned i = 0; i < bitset.array.size(); ++i)
        {
            // Subtract the bits from the array.
            std::uint64_t a = bitset.array[i] & (*it)->bitset.array[i];
            is_fence_input |= a;
            keep_pasture |= (*it)->bitset.array[i] ^= a;
        }

        if(is_fence_input)
            *(input_end++) = (*it)->node;

        // Remove the pasture if its bitset is all zeroes.
        if(keep_pasture == 0)
        {
            *it = pastures.back();
            pastures.pop_back();
        }
        else
            ++it;
    }

    // Clear out memory if possible.
    if(pastures.empty())
        pasture_pool.clear();

    // If there's no intersection, that means the bitset was all zeroes,
    // and thus we don't need a fence; just return the active block.
    if(input_begin == input_end)
    {
        node.control = active_block;
        return &ir.ssa_pool.insert(node);
    }
    else
    {
        // Create the fence node.
        node.control = &ir.ssa_pool.insert({
            .op = SSA_fence,
            .control = active_block });
        node.control->set_input(ir, input_begin, input_end);

        // Create the dependent node.
        ssa_node_t* ret = &ir.ssa_pool.insert(node);

        // Add the bits back.
        new_pasture(bitset, ret);

        return ret;
    }
}

// Creates a fence with ALL pastures as input.
// Also clears 'pastures', so you'll have to insert into it again.
ssa_node_t* ir_builder_t::insert_fence()
{
    if(pastures.size() <= 1)
    {
        pastures.clear();
        return active_block;
    }

    ssa_node_t* ret = &ir.ssa_pool.insert({
        .op = SSA_fence,
        .control = active_block });
    ret->alloc_input(ir, pastures.size());
    for(unsigned i = 0; i < pastures.size(); ++i)
        ret->input[i] = pastures[i]->node;

    pastures.clear();
    pasture_pool.clear();

    return ret;
}

void ir_builder_t::new_pasture(ds_bitset_t const& bitset, ssa_node_t* node)
{
    pastures.push_back(&pasture_pool.insert({ bitset, node }));
}

auto ir_builder_t::compile_branch(ssa_value_t condition) -> branch_t
{
    branch_t branch;

    branch.if_node = &ir.ssa_pool.insert({
        .op = SSA_if,
        .control = insert_fence() });
    branch.if_node->set_input_v(ir, condition);

    branch.true_node = &ir.ssa_pool.insert({
        .op = SSA_true_branch,
        .control = branch.if_node });

    branch.false_node = &ir.ssa_pool.insert({
        .op = SSA_false_branch,
        .control = branch.if_node });

    return branch;
}

// Jumps are like 'break', 'continue', 'goto', etc.
ssa_node_t* ir_builder_t::compile_jump()
{
    // The syntax allows code to exist following a jump statement.
    // Said code is unreachable, but gets compiled anyway.
    // Implement using a conditional that always takes the false branch.
    // (This will be optimized out later)

    branch_t branch = compile_branch(0u);

    // Create new block for the dead code 'true' branch.
    new_active_block(true)->set_input_v(ir, branch.true_node);

    // Return the node to jump to.
    return branch.false_node;
}

ssa_value_t ir_builder_t::default_construct(type_t type)
{
    if(is_arithmetic(type.name))
        return 0u;
    throw std::runtime_error("Unimplemented default construct.");
}


block_data_t* ir_builder_t::new_block_data(bool seal, pstring_t label_name)
{
    block_data_t& block_data = block_pool.emplace();
    std::size_t const num_local_vars = fn().local_vars.size();

    block_data.local_vars = input_pool.alloc(num_local_vars);
    if(seal == false)
        block_data.unsealed_phis = input_pool.alloc(num_local_vars);
    
    return &block_data;
}

ssa_node_t* ir_builder_t::new_active_block(bool seal, pstring_t label_name)
{
    active_block = &ir.ssa_pool.insert({
        .op = SSA_block,
        .block_data = new_block_data(seal, label_name) });
    active_block->control = active_block;

    // Create a new pasture.
    assert(pastures.empty());
    new_pasture(ds_bitset_t::make_all_true(), active_block);

    return active_block;
}

void ir_builder_t::seal_block(block_data_t& block_data)
{
    assert(block_data.sealed() == false);
    std::size_t const num_local_vars = fn().local_vars.size();
    for(unsigned i = 0; i < num_local_vars; ++i)
        if(block_data.local_vars[i])
            fill_phi_args(*block_data.unsealed_phis[i], i);
    block_data.unsealed_phis = nullptr;
}

// Relevant paper:
//   Simple and Efficient Construction of Static Single Assignment Form
ssa_value_t ir_builder_t::local_lookup(ssa_node_t* block, unsigned local_var_i)
{
    assert(block);
    assert(block->op == SSA_block);
    assert(block->block_data);

    ssa_value_t lookup = block->block_data->local_vars[local_var_i];
    if(lookup)
        return lookup;
    else if(block->block_data->sealed())
    {
        // If the block doesn't contain a definition for local_var_i,
        // recursively look up its definition in predecessor nodes.
        // If there are multiple predecessors, a phi node will be created.
        try
        {
            switch(block->input_size)
            {
            case 0:
                throw local_lookup_error_t();
            case 1:
                return local_lookup(block->input[0]->block(), local_var_i);
            default:
                ssa_node_t* phi = &ir.ssa_pool.insert({
                    .op = SSA_phi,
                    .control = block });
                block->block_data->local_vars[local_var_i] = phi;
                fill_phi_args(*phi, local_var_i);
                return phi;
            }
        }
        catch(local_lookup_error_t&)
        {
            if(block->block_data->label_name.size)
            {
                pstring_t var_name = fn().local_vars[local_var_i].name;
                throw compiler_error_t(
                    fmt_error(block->block_data->label_name, fmt(
                        "Jump to label crosses initialization "
                        "of variable %.", var_name.view()))
                    + fmt_error(var_name, fmt(
                        "Variable is defined here.")));
            }
            throw;
        }
    }
    else 
    {
        // If the node is unsealed, the predecessors are not fully known,
        // and thus it's impossible to determine the local var's definition.
        // To work around this, an incomplete phi node can be created, which
        // will then be filled when the node is sealed.
        ssa_node_t* phi = &ir.ssa_pool.insert({
            .op = SSA_phi,
            .control = block });
        block->block_data->local_vars[local_var_i] = phi;
        block->block_data->unsealed_phis[local_var_i] = phi;
        return phi;
    }
}

void ir_builder_t::fill_phi_args(ssa_node_t& phi, unsigned local_var_i)
{
    // Input must be an empty phi node.
    assert(phi.op == SSA_phi);
    assert(phi.input_size == 0);

    // Fill the input array using local lookups.
    ssa_node_t* block = phi.control; assert(block);
    phi.alloc_input(ir, block->input_size);
    for(unsigned i = 0; i < block->input_size; ++i)
        phi.input[i] = local_lookup(block->input[i]->block(), local_var_i);
}

rpn_value_t ir_builder_t::compile_expression(token_t const* expr)
{
    rpn_stack.clear();

    for(token_t const* token = expr; token->type; ++token)
    {
        switch(token->type)
        {
        default:
            throw std::runtime_error("Invalid token in expression.");

        case TOK_ident:
            rpn_push({ 
                .ssa_value = local_lookup(active_block, token->value), 
                .category = LVAL_LOCAL, 
                .type = fn().local_vars[token->value].type, 
                .pstring = token->pstring,
                .local_var_i = token->value });
            break;

        case TOK_global_ident:
            {
                global_t& global = globals()[token->value];
                switch(global.gclass)
                {
                default:
                    throw std::runtime_error(
                        "Unimplemented global in expression.");
                case GLOBAL_FN:
                    rpn_push({ 
                        .ssa_value = token->value,
                        .category = RVAL, 
                        .type = global.type, 
                        .pstring = token->pstring });
                    break;
                }
                break;
            }

        case TOK_assign:
            compile_assign();
            break;

        case TOK_number:
            rpn_push({
                .ssa_value = token->value, 
                .category = RVAL, 
                .type = { TYPE_INT }, 
                .pstring = token->pstring });
            break;

        case TOK_apply:
            {
                // TOK_apply is a psuedo token used to represent application. 
                // The token's 'value' stores the application's arity:
                std::size_t const num_args = token->value;

                // The eval stack contains the arguments to be applied.
                // Right beneath those it contains the fn value to be called.
                rpn_value_t& fn_val = rpn_peek(num_args);

                if(fn_val.type.name != TYPE_FN)
                {
                    compiler_error(fn_val.pstring, fmt(
                        "Expecting function type. Got %.", fn_val.type));
                }

                std::size_t const num_params = fn_val.type.num_params();
                type_t const return_type = fn_val.type.return_type();
                type_t const* const params = fn_val.type.tail();

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
                rpn_value_t* const args = &rpn_peek(num_args - 1);
                if(std::uint64_t fail_bits = 
                   cast_args(fn_val.pstring, args, &*rpn_stack.end(), params))
                {
                    auto arg = 0;
                    do
                    {
                        if(fail_bits & 1)
                        {
                            compiler_error(
                                args[arg].pstring, fmt(
                                "Unable to convert type % "
                                "to type % in function application.\n"
                                "Expected signature: % ",
                                args[arg].type,
                                params[arg],
                                fn_val.type));
                        }
                        ++arg;
                        fail_bits >>= 1;
                    }
                    while(fail_bits);
                }

                // For now, only const fns are allowed.
                // In the future, fn pointers may be supported.
                assert(fn_val.ssa_value.is_const());
                global_t& fn_global = globals()[fn_val.ssa_value.whole()];

                // Type checks are done. Now convert the call to SSA.
                ssa_node_t fn_node = {
                    .op = SSA_fn_call,
                    .type = return_type };
                fn_node.alloc_input(ir, num_args + 1);
                fn_node.input[0] = fn_val.ssa_value;
                for(unsigned i = 0; i != num_args; ++i)
                    fn_node.input[i + 1] = args[i].ssa_value;

                // Update the eval stack.
                rpn_pop(num_args + 1);
                rpn_push({ 
                    .ssa_value = insert_partial_fence(
                        fn_node, fn_global.modifies), 
                    .category = RVAL, 
                    .type = return_type, 
                    .pstring = token->pstring });

                break;
            }

        case TOK_plus:
            compile_arith(SSA_add);  
            break;
        case TOK_minus: 
            compile_arith(SSA_sub);
            break;
        case TOK_bitwise_and: 
            compile_arith(SSA_and);
            break;
        case TOK_bitwise_or:  
            compile_arith(SSA_or);
            break;
        case TOK_bitwise_xor:
            compile_arith(SSA_xor);
            break;

        case TOK_plus_assign:
            compile_assign_arith(SSA_add);
            break;
        case TOK_minus_assign:
            compile_assign_arith(SSA_sub);
            break;
        case TOK_bitwise_and_assign:
            compile_assign_arith(SSA_and);
            break;
        case TOK_bitwise_or_assign:
            compile_assign_arith(SSA_or);
            break;
        case TOK_bitwise_xor_assign:
            compile_assign_arith(SSA_xor);
            break;
        }
    }

    assert(rpn_stack.size() == 1);
    return rpn_stack[0];
}

void ir_builder_t::compile_assign()
{
    rpn_value_t& assignee = rpn_peek(1);
    rpn_value_t& assignment = rpn_peek(0);

    assignee.pstring = concat(assignee.pstring, assignee.pstring);

    // TODO: global assigns

    if(assignee.category != LVAL_LOCAL)
    {
        compiler_error(assignee.pstring, 
            "Expecting lvalue on left side of assignment.");
    }

    if(!cast(assignment, assignee.type))
    {
        compiler_error(assignee.pstring, fmt(
            "Unable to convert type % to type % in assignment.", 
            assignment.type, assignee.type));
    }

    // Remap the identifier to point to the new value.
    active_block->block_data->local_vars[assignee.local_var_i] = 
        assignment.ssa_value;
    assignee.ssa_value = assignment.ssa_value;

    // Leave the assignee on the stack, but extend its pstring.
    rpn_pop();
}

void ir_builder_t::compile_assign_arith(ssa_op_t op)
{
    rpn_tuck(rpn_peek(1), 1);
    compile_arith(op);
    compile_assign();
}

// Applies an operator to the top two values on the eval stack,
// turning them into a single value.
void ir_builder_t::compile_binary_operator(ssa_op_t op, type_t result_type)
{
    assert(rpn_stack.size() >= 2);

    ssa_node_t new_node = { 
        .op = op, 
        .control = active_block, 
        .type = result_type };

    new_node.set_input_v(ir, rpn_peek(1).ssa_value, rpn_peek(0).ssa_value);

    rpn_peek(1).ssa_value = &ir.ssa_pool.insert(new_node);
    rpn_peek(1).type = result_type;
    rpn_peek(1).category = RVAL;
    rpn_peek(1).pstring = concat(rpn_peek(1).pstring, rpn_peek(0).pstring);
    rpn_pop();
}

void ir_builder_t::compile_arith(ssa_op_t op)
{
    rpn_value_t& lhs = rpn_peek(1);
    rpn_value_t& rhs = rpn_peek(0);

    if(!is_arithmetic(lhs.type.name) || !is_arithmetic(rhs.type.name))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting arithmetic types.");
    }

    // Promote result type to the largest argument type.
    type_t new_type = { promote_arithmetic(lhs.type.name, rhs.type.name) };
    assert(is_arithmetic(new_type.name));

    compile_binary_operator(op, new_type);
}

void ir_builder_t::append_cast_op(rpn_value_t& rpn_value, type_t to_type)
{
    ssa_node_t new_node = { 
        .op = SSA_cast, 
        .control = active_block,
        .type = to_type };
    new_node.set_input_v(ir, rpn_value.ssa_value);
    rpn_value.ssa_value = &ir.ssa_pool.insert(new_node);
}

bool ir_builder_t::cast(rpn_value_t& rpn_value, type_t to_type)
{
    switch(can_cast(rpn_value.type, to_type))
    {
    default:
    case CAST_FAIL: return false;
    case CAST_NOP:  return true;
    case CAST_OP:
        append_cast_op(rpn_value, to_type);
        return true;
    }
}

// Converts multiple values at once, but only if all casts are valid.
// On success, 0 is returned and 'val_begin' to 'val_end' may be modified
// to their casted type.
// On failure, a bitset is returned where a 1 bit signifies each failed cast.
std::uint64_t ir_builder_t::cast_args(
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
        if(results[i] == CAST_OP)
            append_cast_op(begin[i], type_begin[i]);

    return 0; // 0 means no errors!
}

