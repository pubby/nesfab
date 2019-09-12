#include "ir_builder.hpp"

#include "alloca.hpp"
#include "compiler_error.hpp"

region_data_t::region_data_t(ir_builder_t& ir_builder, bool sealed, 
                             pstring_t label_name)
: sealed(sealed) 
, label_name(label_name)
{
    std::size_t size = ir_builder.fn().local_vars.size();
    local_vars = ir_builder.handle_pool.alloc(size);
    if(!sealed)
        unsealed_phis = ir_builder.handle_pool.alloc(size);
}

void region_data_t::seal(class ir_builder_t& ir_builder)
{
    assert(!sealed);
    sealed = true;
    for(unsigned i = 0; i < ir_builder.fn().local_vars.size(); ++i)
        if(local_vars[i])
            ir_builder.fill_phi_args(unsealed_phis[i], i);
}

ir_builder_t::ir_builder_t(global_manager_t& global_manager, global_t& global)
: global_manager_ptr(&global_manager)
, global_ptr(&global)
{
    assert(global.gclass = GLOBAL_FN);
    stmt = global.fn->stmts.data();

    new_active_region(true, nullptr, nullptr);
}

void ir_builder_t::compile()
{
    compile_block();

    // Always end with one final return (probably a dummy).
    type_t return_type = global().type.return_type();
    if(return_type.name != TYPE_VOID)
        return_values.push_back(ssa_make_const(0));
    return_jumps.push_back(insert_fence());

    new_active_region(true, &*return_jumps.begin(), &*return_jumps.end());

    ssa_node_t ret =
    {
        .op = SSA_return,
        .control_h = active_region_h,
    };

    if(return_type.name != TYPE_VOID)
    {
        ssa_node_t phi =
        {
            .op = SSA_phi,
            .control_h = active_region_h,
            .type = return_type,
        };
        phi.set_input(ir, &*return_values.begin(), &*return_values.end());
        ssa_handle_t phi_h = ir.insert(phi);
        ret.set_input_v(ir, phi_h);
    }

    ir.return_h = ir.insert(ret);
}

void ir_builder_t::compile_block()
{
    while(true) 
    switch(stmt->name)
    {
    default:
        if(is_var_init(stmt->name))
        {
            ssa_handle_t handle = ssa_make_const(0);
            if(stmt->expr)
                handle = compile_expression(stmt->expr).handle;

            unsigned local_var_i = get_local_var_i(stmt->name);
            region_data_t& region_data = *ir[active_region_h].region_data;
            region_data.local_vars[local_var_i] = handle;
            ++stmt;
        }
        else
            assert(false);
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
            branch_t branch = compile_branch(cond.handle);

            // Create new region for the 'true' branch.
            new_active_region_v(true, branch.true_h);
            ++stmt;
            compile_block();
            branch.true_h = insert_fence(); // End of the 'true' block.

            if(stmt->name == STMT_ELSE)
            {
                // Create new region for the 'false' branch.
                new_active_region_v(true, branch.false_h);
                ++stmt;
                compile_block();
                branch.false_h = insert_fence(); // End of the 'false' block.
            }

            // Merge both branches together into a new region.
            new_active_region_v(true, branch.true_h, branch.false_h);
            break;
        }

    case STMT_WHILE:
        {
            // The loop condition will go in its own region.
            ssa_handle_t entry_fence_h = insert_fence();
            ssa_handle_t condition_region_h = new_active_region_v(false);

            // Create the if node and branches.
            rpn_value_t cond = compile_expression(stmt->expr);
            branch_t branch = compile_branch(cond.handle);

            // Create new region for the 'true' branch.
            new_active_region_v(true, branch.true_h);
            ++stmt;
            continue_stack.push();
            break_stack.push();
            compile_block();

            // All continue statements jump to the condition region,
            // along with the entry fence and the end of the true branch fence.
            {
                auto& input = continue_stack.top();
                input.push_back(insert_fence()); // End of the 'true' block.
                input.push_back(entry_fence_h);
                ir[condition_region_h].set_input(
                    ir, &*input.begin(), &*input.end());
                ir[condition_region_h].region_data->seal(*this);
            }

            // Create the exit region.
            {
                auto& input = break_stack.top();
                input.push_back(branch.false_h);
                new_active_region(true, &*input.begin(), &*input.end());
            }

            continue_stack.pop();
            break_stack.pop();
            break;
        }

    case STMT_DO:
        {
            // Compile the loop body
            ssa_handle_t body_region_h = 
                new_active_region_v(false, insert_fence(), ssa_handle_t{});
            ++stmt;
            continue_stack.push();
            break_stack.push();
            compile_block();

            // The loop condition can go in its own region, which is
            // necessary to implement continue.
            {
                auto& input = continue_stack.top();
                // Only do this if needed; otherwise use the previous region.
                if(!input.empty())
                {
                    input.push_back(insert_fence());
                    new_active_region(true, &*input.begin(), &*input.end());
                }
            }

            // Create the if node and branches.
            rpn_value_t cond = compile_expression(stmt->expr);
            branch_t branch = compile_branch(cond.handle);

            // Can finally set the entry region node's input and seal it.
            {
                ssa_node_t& body_region = ir[body_region_h];
                assert(body_region.input_size == 2);
                assert(body_region.input(ir)[1].value == 0);
                body_region.input(ir)[1] = branch.true_h;
                body_region.region_data->seal(*this);
            }

            // Create the exit region.
            {
                auto& input = break_stack.top();
                input.push_back(branch.false_h);
                new_active_region(true, &*input.begin(), &*input.end());
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

                return_values.push_back(value.handle);
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
            label_t* label = stmt->label;
            pstring_t label_name = stmt->pstring;
            ++stmt;
            assert(label);

            std::printf("LABEL! %i %i\n", label->goto_count, label->inputs.size() + 1);

            // If there's no goto to this label, just ignore it.
            if(label->goto_count == 0)
                break;

            if(label->goto_count == label->inputs.size())
            {
                label->inputs.push_back(insert_fence());

                // All the gotos to this label have been compiled,
                // that means this region can be sealed immediately!
                label->ssa_h = new_active_region(true, 
                    &*label->inputs.begin(), &*label->inputs.end(),
                    label_name);
            }
            else
            {
                // Otherwise, seal the node at a later time.
                label->inputs.push_back(insert_fence());
                label->ssa_h = new_active_region(false, nullptr, nullptr);
            }

            break;
        }

    case STMT_GOTO:
        {
            label_t* label = stmt->label;
            pstring_t label_name = stmt->pstring;
            ++stmt;
            assert(label);
            assert(label->goto_count > 0);

            // Add the jump to the label.
            label->inputs.push_back(compile_jump());

            std::printf("GOTO! %i %i\n", label->goto_count, label->inputs.size());

            // +1 because creating the label adds an input.
            // Note that this will only be true if the label already exists!
            if(label->goto_count+1 == label->inputs.size())
            {
                std::puts("OK!\n");
                assert(label->ssa_h);
                assert(ir[label->ssa_h].input_size == 0);

                // Add the inputs to the ssa region node.
                ir[label->ssa_h].set_input(ir, 
                    &*label->inputs.begin(), &*label->inputs.end());

                // Seal the node.
                ir[label->ssa_h].region_data->seal(*this);
            }
            break;
        }
    }
    assert(false);
}

// Inserts a fence that covers all memory values in 'bitset',
// then creates 'node' as its dependent.
ssa_handle_t ir_builder_t::insert_partial_fence(ssa_node_t node, 
                                                ds_bitset_t const& bitset)
{
    assert(!node.control_h);

    ssa_handle_t* input_begin = ALLOCA_T(ssa_handle_t, pastures.size());
    ssa_handle_t* input_end = input_begin;

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
            *(input_end++) = (*it)->handle;

        // Remove the pasture if its bitset is all zeroes.
        if(keep_pasture == 0)
        {
            pasture_pool.free(*it);
            *it = pastures.back();
            pastures.pop_back();
        }
        else
            ++it;
    }

    // If there's no intersection, that means the bitset was all zeroes,
    // and thus we don't need a fence; just return the active region.
    if(input_begin == input_end)
    {
        node.control_h = active_region_h;
        return ir.insert(node);
    }
    else
    {
        // Create the fence node.
        ssa_node_t fence = { .op = SSA_fence, .control_h = active_region_h };
        fence.set_input(ir, input_begin, input_end);

        // Create the dependent node.
        node.control_h = ir.insert(fence);
        ssa_handle_t handle = ir.insert(node);

        // Add the bits back.
        new_pasture(bitset, handle);

        return handle;
    }
}

// Creates a fence with ALL pastures as input.
// Also clears 'pastures', so you'll have to insert into it again.
ssa_handle_t ir_builder_t::insert_fence()
{
    ssa_handle_t ret = active_region_h;

    if(pastures.size() > 1)
    {
        ssa_node_t node = { .op = SSA_fence, .control_h = active_region_h };

        unsigned input_i = node.alloc_input(ir, pastures.size());
        for(unsigned i = 0; i < pastures.size(); ++i)
            ir.input_vec[input_i + i] = pastures[i]->handle;

        ret = ir.insert(node);
    }

    for(pasture_t* pasture : pastures)
        pasture_pool.free(pasture);
    pastures.clear();

    return ret;
}

void ir_builder_t::new_pasture(ds_bitset_t const& bitset, ssa_handle_t handle)
{
    pasture_t* pasture = pasture_pool.malloc();
    pasture->bitset = bitset;
    pasture->handle = handle;
    pastures.push_back(pasture);
}

auto ir_builder_t::compile_branch(ssa_handle_t condition_h) -> branch_t
{
    branch_t result;

    ssa_node_t node = 
    {
        .op = SSA_if,
        .control_h = insert_fence()
    };
    node.set_input_v(ir, condition_h);
    result.if_h = ir.insert(node);

    result.true_h = ir.insert(
        { .op = SSA_true_branch, .control_h = result.if_h });

    result.false_h = ir.insert(
        { .op = SSA_false_branch, .control_h = result.if_h });

    return result;
}

// Jumps are like 'break', 'continue', 'goto', etc.
ssa_handle_t ir_builder_t::compile_jump()
{
    // The syntax allows code to exist following a jump statement.
    // Said code is unreachable, but gets compiled anyway.
    // Implement using a conditional that always takes the false branch.
    // (This will be optimized out later)

    branch_t branch = compile_branch(ssa_make_const(0));

    // Create new region for the dead code 'true' branch.
    new_active_region_v(true, branch.true_h);

    // Return the node to jump to.
    return branch.false_h;
}

// Relevant paper:
//   Simple and Efficient Construction of Static Single Assignment Form
ssa_handle_t ir_builder_t::local_lookup(ssa_handle_t region_h, 
                                        unsigned local_var_i)
{
    // Careful about reference invalidation! 
    ssa_node_t& region = ir[region_h];

    // Input must be a region node.
    assert(region.op == SSA_cfg_region);
    assert(region.region_data);

    // Careful about reference invalidation! 
    region_data_t& region_data = *region.region_data;

    ssa_handle_t handle = region_data.local_vars[local_var_i];
    if(handle)
        return handle;
    else if(region_data.sealed)
    {
        // If the region doesn't contain a definition for local_var_i,
        // recursively look up its definition in predecessor nodes.
        // If there are multiple predecessors, a phi node will be created.

        try
        {
            switch(region.input_size)
            {
            case 0:
                throw local_lookup_error_t();
            case 1:
                return local_lookup(ir.region_h(*region.input(ir)), 
                                    local_var_i);
            default:
                ssa_handle_t phi_h = ir.insert(
                    { .op = SSA_phi, .control_h = region_h });
                // 'region' reference is now invalidated.
                // but 'region_data' is still valid!
                region_data.local_vars[local_var_i] = phi_h;
                fill_phi_args(phi_h, local_var_i);
                return phi_h;
            }
        }
        catch(local_lookup_error_t)
        {
            if(region_data.label_name.size)
            {
                pstring_t var_name = fn().local_vars[local_var_i].name;
                throw compiler_error_t(
                    fmt_error(region_data.label_name, fmt(
                        "Jump to label crosses initialization "
                        "of variable %.", var_name.view()))
                    + fmt_error(var_name, fmt(
                        "Variable is defined here.")));
            }
            else
                throw;
        }
    }
    else 
    {
        // If the node is unsealed, the predecessors are not fully known,
        // and thus it's impossible to determine the local var's definition.
        // To work around this, an incomplete phi node can be created, which
        // will then be filled when the node is sealed.

        ssa_handle_t phi_h = ir.insert(
            { .op = SSA_phi, .control_h = region_h });
        // 'region' reference is now invalidated, 
        // but 'region_data' is still valid!
        region_data.local_vars[local_var_i] = phi_h;
        region_data.unsealed_phis[local_var_i] = phi_h;
        return phi_h;
    }
}

void ir_builder_t::fill_phi_args(ssa_handle_t phi_h, unsigned local_var_i)
{
    // Input must be an empty phi node.
    assert(ir[phi_h].op == SSA_phi);
    assert(ir[phi_h].input_size == 0);

    // Allocate a spot for the input to go.
    unsigned input_size;
    unsigned input_i;
    ssa_handle_t region_h;
    {
        // This reference can be invalidated; keep inside {} block.
        ssa_node_t& phi = ir[phi_h];
        region_h = phi.control_h;
        assert(ir[region_h].op == SSA_cfg_region);
        assert(region_h == ir.region_h(phi_h));
        input_size = phi.input_size = ir[region_h].input_size;
        input_i = phi.input_i = ir.alloc_input(input_size);
    }

    // Fill the input array using local lookups.
    for(unsigned i = 0; i < input_size; ++i)
        ir.input_vec[input_i + i] = local_lookup(
            ir.region_h(ir[region_h].input(ir)[i]), local_var_i);
}

rpn_value_t ir_builder_t::compile_expression(token_t const* expr)
{
    rpn_stack.clear();

    for(token_t const* token = expr; token->type; ++token)
    {
        switch(token->type)
        {
        default:
            assert(false);
            break;

        case TOK_ident:
            rpn_push({ 
                .handle = local_lookup(active_region_h, token->value), 
                .category = LVAL_LOCAL, 
                .type = fn().local_vars[token->value].type, 
                .pstring = token->pstring,
                .local_var_i = token->value,
            });
            break;

        case TOK_global_ident:
            {
                global_t& global = globals()[token->value];
                switch(global.gclass)
                {
                default:
                    assert(false);
                case GLOBAL_FN:
                    rpn_push({ 
                        .handle = ssa_make_const(token->value),
                        .category = RVAL, 
                        .type = global.type, 
                        .pstring = token->pstring 
                    });
                    break;
                }
                break;
            }

        case TOK_assign:
            compile_assign();
            break;

        case TOK_number:
            rpn_push({ 
                .handle = ssa_make_const(token->value), 
                .category = RVAL, 
                .type = ssa_const_type, 
                .pstring = token->pstring 
            });
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
                assert(ssa_is_const(fn_val.handle));
                global_t& fn_global = 
                    globals()[ssa_extract_const(fn_val.handle)];

                // Type checks are done. Now convert the call to SSA.
                ssa_node_t fn_node =
                {
                    .op = SSA_fn_call,
                    .type = return_type,
                };

                unsigned input_i = fn_node.alloc_input(ir, num_args + 1);
                ir.input_vec[input_i] = fn_val.handle;
                for(unsigned i = 0; i != num_args; ++i)
                    ir.input_vec[input_i + i + 1] = args[i].handle;

                ssa_handle_t handle = 
                    //insert_partial_fence(fn_node, fn_global.modifies);
                    insert_partial_fence(fn_node, fn_global.modifies);

                // Update the eval stack.
                rpn_pop(num_args + 1);
                rpn_push({ handle, RVAL, return_type, token->pstring });

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
    region_data_t& region_data = *ir[active_region_h].region_data;
    region_data.local_vars[assignee.local_var_i] = assignment.handle;
    assignee.handle = assignment.handle;

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

    ssa_node_t new_node = 
    { 
        .op = op, 
        .control_h = active_region_h, 
        .type = result_type
    };

    new_node.set_input_v(ir, rpn_peek(1).handle, rpn_peek(0).handle);

    rpn_peek(1).handle = ir.insert(new_node);
    rpn_peek(1).type = result_type;
    rpn_peek(1).category = RVAL;
    rpn_peek(1).pstring = concat(rpn_peek(1).pstring, rpn_peek(0).pstring);
    rpn_pop();
}

void ir_builder_t::compile_arith(ssa_op_t op)
{
    rpn_value_t& lhs = rpn_peek(1);
    rpn_value_t& rhs = rpn_peek(0);

    if(!is_integer(lhs.type.name) || !is_integer(rhs.type.name))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting integral types.");
    }

    // Promote result type to the largest argument type.
    type_t new_type = { std::max(lhs.type.name, rhs.type.name) };
    assert(is_integer(new_type.name));

    compile_binary_operator(op, new_type);
}

void ir_builder_t::append_cast_op(rpn_value_t& rpn_value, type_t to_type)
{
    ssa_node_t new_node = 
    { 
        .op = SSA_cast, 
        .control_h = active_region_h,
        .type = to_type
    };

    new_node.set_input_v(ir, rpn_value.handle);

    rpn_value.handle = ir.insert(new_node);
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

