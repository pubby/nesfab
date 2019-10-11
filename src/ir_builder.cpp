#include "ir_builder.hpp"

#include "alloca.hpp"
#include "compiler_error.hpp"

ir_builder_t::ir_builder_t(global_manager_t& global_manager, global_t& global)
: global_manager_ptr(&global_manager)
, global_ptr(&global)
{
    assert(global.gclass = GLOBAL_FN);
    stmt = global.fn->stmts.data();
}

rpn_value_t& ir_builder_t::rpn_peek(int i) 
{ 
    assert(i < (int)rpn_stack.size());
    return rpn_stack.rbegin()[i]; 
}

void ir_builder_t::rpn_pop() 
{
    assert(!rpn_stack.empty());
    rpn_stack.pop_back();
}

void ir_builder_t::rpn_pop(unsigned i) 
{ 
    assert(i <= rpn_stack.size());
    rpn_stack.resize(rpn_stack.size() - i); 
}

void ir_builder_t::rpn_push(rpn_value_t rpn_value) 
{ 
    rpn_stack.push_back(std::move(rpn_value)); 
}

void ir_builder_t::rpn_tuck(rpn_value_t rpn_value, unsigned place) 
{ 
    assert(place <= rpn_stack.size());
    rpn_stack.insert(rpn_stack.end() - place, std::move(rpn_value));
}

void ir_builder_t::compile()
{
    ir.root = &insert_cfg(true);

    // Create all of the SSA graph, minus the exit node:
    cfg_node_t& end = compile_block(*ir.root);
    exits_with_jump(end);

    // Now create the exit block.
    // All return statements create a jump, which will jump to the exit node.
    type_t return_type = global().type.return_type();
    if(return_type.name != TYPE_VOID)
        return_values.push_back(
            &end.emplace_ssa(ir, SSA_uninitialized, return_type));

    ir.exit = &insert_cfg(true);

    for(cfg_node_t* node : return_jumps)
        node->build_set_output(0, *ir.exit);
    end.build_set_output(0, *ir.exit);

    ir.exit->exit = &ir.exit->emplace_ssa(ir, SSA_return, {});

    if(return_type.name != TYPE_VOID)
    {
        ssa_node_t& phi = ir.exit->emplace_ssa(ir, SSA_phi, return_type);
        phi.link_assign_input(return_values.begin(), return_values.end());
        ir.exit->exit->link_append_input(&phi);
    }

    // Finish the IR
    ir.finish_construction();
}

cfg_node_t& ir_builder_t::compile_block(cfg_node_t& node_)
{
    cfg_node_t* cfg_node = &node_;
    while(true)
    switch(stmt->name)
    {
    default:
        if(is_var_init(stmt->name))
        {
            unsigned const local_var_i = get_local_var_i(stmt->name);

            ssa_value_t value;
            if(stmt->expr)
            {
                cfg_node = &compile_expr(*cfg_node, stmt->expr);
                value = rpn_stack[0].ssa_value;
            }
            else
                value = &cfg_node->emplace_ssa(
                    ir, SSA_uninitialized, fn().local_vars[local_var_i].type);

            cfg_node->block_data->local_vars[local_var_i] = value;
            ++stmt;
        }
        else
            throw std::runtime_error("Unimplemented stmt.");
        break;

    case STMT_END_BLOCK:
        ++stmt;
        return *cfg_node;

    case STMT_EXPR:
        cfg_node = &compile_expr(*cfg_node, stmt->expr);
        ++stmt;
        break;

    case STMT_IF:
        {
            // Branch the active node.
            cfg_node_t* branch_node = &compile_expr(*cfg_node, stmt->expr);
            ++stmt;
            throwing_cast(*branch_node, rpn_stack[0], {TYPE_BOOL});
            exits_with_branch(*branch_node, rpn_stack[0].ssa_value);

            // Create new cfg_node for the 'true' branch.
            cfg_node_t& begin_true = insert_cfg(true);
            branch_node->build_set_output(1, begin_true);
            cfg_node_t& end_true = compile_block(begin_true);
            exits_with_jump(end_true);

            if(stmt->name == STMT_ELSE)
            {
                // Create new block for the 'false' branch.
                ++stmt;
                cfg_node_t& begin_false = insert_cfg(true);
                branch_node->build_set_output(0, begin_false);
                // repurpose 'branch_node' to hold end of the 'false' branch.
                // Simplifies the assignment that follows.
                branch_node = &compile_block(begin_false);
                exits_with_jump(*branch_node);
            }

            // Merge the two nodes.
            cfg_node = &insert_cfg(true);
            end_true.build_set_output(0, *cfg_node);
            branch_node->build_set_output(0, *cfg_node);
            break;
        }

    case STMT_WHILE:
        {
            // The loop condition will go in its own block.
            exits_with_jump(*cfg_node);
            cfg_node_t& begin_branch = insert_cfg(false);
            cfg_node->build_set_output(0, begin_branch);

            cfg_node_t& end_branch = compile_expr(begin_branch, stmt->expr);
            ++stmt;
            throwing_cast(end_branch, rpn_stack[0], {TYPE_BOOL});
            exits_with_branch(end_branch, rpn_stack[0].ssa_value);

            continue_stack.push();
            break_stack.push();

            // Compile the body.
            cfg_node_t& begin_body = insert_cfg(true);
            end_branch.build_set_output(1, begin_body);
            cfg_node_t& end_body = compile_block(begin_body);
            exits_with_jump(end_body);
            end_body.build_set_output(0, begin_branch);

            // All continue statements jump to branch_node.
            for(cfg_node_t* node : continue_stack.top())
                node->build_set_output(0, begin_branch);
            seal_block(*begin_branch.block_data);

            // Create the exit node.
            cfg_node = &insert_cfg(true);
            end_branch.build_set_output(0, *cfg_node);
            for(cfg_node_t* node : break_stack.top())
                node->build_set_output(0, *cfg_node);

            continue_stack.pop();
            break_stack.pop();
            break;
        }

    case STMT_DO:
        {
            ++stmt;
            continue_stack.push();
            break_stack.push();

            // Compile the loop body
            exits_with_jump(*cfg_node);
            cfg_node_t& begin_body = insert_cfg(false);
            cfg_node->build_set_output(0, begin_body);
            cfg_node_t& end_body = compile_block(begin_body);

            assert(stmt->name == STMT_WHILE);

            // The loop condition can go in its own block, which is
            // necessary to implement 'continue'.
            exits_with_jump(end_body);
            cfg_node_t& begin_branch = insert_cfg(true);
            end_body.build_set_output(0, begin_branch);

            cfg_node_t& end_branch = compile_expr(begin_branch, stmt->expr);
            ++stmt;
            throwing_cast(end_branch, rpn_stack[0], {TYPE_BOOL});
            exits_with_branch(end_branch, rpn_stack[0].ssa_value);

            end_branch.build_set_output(1, begin_body);
            seal_block(*begin_body.block_data);

            // All continue statements jump to the branch node.
            for(cfg_node_t* node : continue_stack.top())
                node->build_set_output(0, begin_branch);

            // Create the exit cfg_node.
            cfg_node = &insert_cfg(true);
            end_branch.build_set_output(0, *cfg_node);
            for(cfg_node_t* node : break_stack.top())
                node->build_set_output(0, *cfg_node);

            continue_stack.pop();
            break_stack.pop();
            break;
        }

    case STMT_RETURN:
        {
            type_t return_type = global().type.return_type();
            if(stmt->expr)
            {
                cfg_node = &compile_expr(*cfg_node, stmt->expr);
                throwing_cast(*cfg_node, rpn_stack[0], return_type);
                return_values.push_back(rpn_stack[0].ssa_value);
            }
            else
                compiler_error(stmt->pstring, fmt(
                    "Expecting return expression of type %.", return_type));
            return_jumps.push_back(cfg_node);
            cfg_node = &compile_goto(*cfg_node);
            ++stmt;
            break;
        }

    case STMT_BREAK:
        if(break_stack.empty())
            compiler_error(stmt->pstring, "break statement outside of loop.");
        break_stack.top().push_back(cfg_node);
        cfg_node = &compile_goto(*cfg_node);
        ++stmt;
        break;

    case STMT_CONTINUE:
        if(continue_stack.empty())
            compiler_error(stmt->pstring, 
                           "continue statement outside of loop.");
        continue_stack.top().push_back(cfg_node);
        cfg_node = &compile_goto(*cfg_node);
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

            exits_with_jump(*cfg_node);
            label->inputs.push_back(cfg_node);

            if(label->goto_count + 1 == label->inputs.size())
            {
                // All the gotos to this label have been compiled,
                // that means this block can be sealed immediately!
                label->node = cfg_node = &insert_cfg(true, label_name);
                for(cfg_node_t* node : label->inputs)
                    node->build_set_output(0, *label->node);
            }
            else // Otherwise, seal the node at a later time.
                label->node = cfg_node = &insert_cfg(false, label_name);

            break;
        }

    case STMT_GOTO:
        {
            label_t* label = stmt->label; assert(label);
            ++stmt;
            assert(label->goto_count > 0);

            // Add the jump to the label.
            label->inputs.push_back(cfg_node);
            cfg_node = &compile_goto(*cfg_node);

            // If this is the last goto, finish and seal the node.
            if(label->goto_count + 1 == label->inputs.size())
            {
                assert(label->node);
                for(cfg_node_t* node : label->inputs)
                    node->build_set_output(0, *label->node);
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
ssa_node_t& ir_builder_t::insert_fenced(
    cfg_node_t& cfg_node, ssa_op_t op, type_t type, ds_bitset_t const& bitset)
{
    std::uint64_t non_zero = 0;
    for(unsigned i = 0; i < bitset.array.size(); ++i)
        non_zero |= bitset.array[i];

    // Function is pure! We don't need a fence.
    if(!non_zero)
        return cfg_node.emplace_ssa(ir, op, type, 0u);

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
        {
            *input_end = (*it)->node;
            ++input_end;
        }

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

    // Create the fence node.
    ssa_node_t* fence = &cfg_node.emplace_ssa(ir, SSA_fence, {});
    fence->link_assign_input(input_begin, input_end);

    // Create the dependent node.
    ssa_node_t& ret = cfg_node.emplace_ssa(ir, op, type, fence);

    // Add the bits back.
    new_pasture(bitset, &ret);

    return ret;
}

// Creates a fence with ALL pastures as input.
// Also clears 'pastures', so you'll have to insert into it again.
ssa_node_t& ir_builder_t::insert_fence(cfg_node_t& cfg_node)
{
    ssa_node_t& node = cfg_node.emplace_ssa(ir, SSA_fence, {});
    for(unsigned i = 0; i < pastures.size(); ++i)
        node.link_append_input(pastures[i]->node);
    pastures.clear();
    pasture_pool.clear();
    return node;
}

void ir_builder_t::new_pasture(ds_bitset_t const& bitset, ssa_node_t* node)
{
    pastures.push_back(&pasture_pool.insert({ bitset, node }));
}

void ir_builder_t::exits_with_jump(cfg_node_t& node)
{
    assert(node.exit == nullptr);
    node.exit = &insert_fence(node);
    node.build_resize_output(1);
}

void ir_builder_t::exits_with_branch(cfg_node_t& node, ssa_value_t condition)
{
    assert(node.exit == nullptr);
    node.exit = &node.emplace_ssa(ir, SSA_if, {},
                                  &insert_fence(node), condition);
    node.build_resize_output(2);
}

// Jumps are like 'break', 'continue', 'goto', etc.
cfg_node_t& ir_builder_t::compile_goto(cfg_node_t& branch_node)
{
    // The syntax allows code to exist following a jump statement.
    // Said code is unreachable, but gets compiled anyway.
    // Implement using a conditional that always takes the false branch.
    // (This will be optimized out later)

    exits_with_branch(branch_node, 0u);
    cfg_node_t& dead_branch = insert_cfg(true);
    branch_node.build_set_output(1, dead_branch);
    return dead_branch;
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

cfg_node_t& ir_builder_t::insert_cfg(bool seal, pstring_t label_name)
{
    cfg_node_t& new_node = ir.emplace_cfg();
    new_node.block_data = new_block_data(seal, label_name);
    return new_node;
}

void ir_builder_t::seal_block(block_data_t& block_data)
{
    assert(block_data.sealed() == false);
    std::size_t const num_local_vars = fn().local_vars.size();
    for(unsigned i = 0; i < num_local_vars; ++i)
        if(block_data.unsealed_phis[i])
            fill_phi_args(*block_data.unsealed_phis[i], i);
    block_data.unsealed_phis = nullptr;
}

// Relevant paper:
//   Simple and Efficient Construction of Static Single Assignment Form
ssa_value_t ir_builder_t::local_lookup(cfg_node_t& node, unsigned local_var_i)
{
    assert(node.block_data);
    assert(node.block_data->local_vars);

    ssa_value_t lookup = node.block_data->local_vars[local_var_i];
    if(lookup)
        return lookup;
    else if(node.block_data->sealed())
    {
        // If the block doesn't contain a definition for local_var_i,
        // recursively look up its definition in predecessor nodes.
        // If there are multiple predecessors, a phi node will be created.
        try
        {
            switch(node.input_size())
            {
            case 0:
                throw local_lookup_error_t();
            case 1:
                return local_lookup(node.input(0), local_var_i);
            default:
                ssa_node_t& phi = node.emplace_ssa(
                    ir, SSA_phi, fn().local_vars[local_var_i].type);
                node.block_data->local_vars[local_var_i] = &phi;
                fill_phi_args(phi, local_var_i);
                return &phi;
            }
        }
        catch(local_lookup_error_t&)
        {
            if(node.block_data->label_name.size)
            {
                pstring_t var_name = fn().local_vars[local_var_i].name;
                throw compiler_error_t(
                    fmt_error(node.block_data->label_name, fmt(
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
        assert(node.block_data->unsealed_phis);
        ssa_node_t& phi = node.emplace_ssa(
            ir, SSA_phi, fn().local_vars[local_var_i].type);
        node.block_data->local_vars[local_var_i] = &phi;
        node.block_data->unsealed_phis[local_var_i] = &phi;
        return &phi;
    }
}

void ir_builder_t::fill_phi_args(ssa_node_t& phi, unsigned local_var_i)
{
    // Input must be an empty phi node.
    assert(phi.op() == SSA_phi);
    assert(phi.input_size() == 0);

    // Fill the input array using local lookups.
    for(unsigned i = 0; i < phi.cfg_node().input_size(); ++i)
        phi.link_append_input(local_lookup(phi.cfg_node().input(i), 
                                           local_var_i));
}

cfg_node_t& ir_builder_t::compile_expr(cfg_node_t& node_, token_t const* expr)
{
    cfg_node_t* cfg_node = &node_;
    rpn_stack.clear();
    logical_stack.clear();

    for(token_t const* token = expr; token->type; ++token)
    {
        switch(token->type)
        {
        default:
            throw std::runtime_error("Invalid token in expression.");

        case TOK_ident:
            rpn_push({ 
                .ssa_value = local_lookup(*cfg_node, token->value), 
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
            compile_assign(*cfg_node);
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
                if(std::uint64_t fail_bits = cast_args(
                *cfg_node, fn_val.pstring, args, &*rpn_stack.end(), params))
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
                ssa_node_t& fn_node = insert_fenced(
                    *cfg_node, SSA_fn_call, return_type, fn_global.modifies);
                fn_node.link_append_input(fn_val.ssa_value);
                for(unsigned i = 0; i != num_args; ++i)
                    fn_node.link_append_input(args[i].ssa_value);

                // Update the eval stack.
                rpn_pop(num_args + 1);
                rpn_push({ 
                    .ssa_value = &fn_node,
                    .category = RVAL, 
                    .type = return_type, 
                    .pstring = token->pstring });

                break;
            }

        case TOK_logical_and:
            cfg_node = &compile_logical_begin(*cfg_node, false);
            break;
        case TOK_logical_or:
            cfg_node = &compile_logical_begin(*cfg_node, true);
            break;
        case TOK_end_logical_and:
            cfg_node = &compile_logical_end(*cfg_node, false);
            break;
        case TOK_end_logical_or:
            cfg_node = &compile_logical_end(*cfg_node, true);
            break;

        case TOK_eq:
            compile_compare(*cfg_node, SSA_eq);
            break;
        case TOK_not_eq:
            compile_compare(*cfg_node, SSA_not_eq);
            break;
        case TOK_gt:
            std::swap(rpn_peek(0), rpn_peek(1));
            // fall-through
        case TOK_lt:
            compile_compare(*cfg_node, SSA_lt);
            break;
        case TOK_gte:
            std::swap(rpn_peek(0), rpn_peek(1));
            // fall-through
        case TOK_lte:
            compile_compare(*cfg_node, SSA_lte);
            break;

        case TOK_plus:
            compile_arith(*cfg_node, SSA_add);  
            break;
        case TOK_minus: 
            compile_arith(*cfg_node, SSA_sub);
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
            compile_assign_arith(*cfg_node, SSA_add);
            break;
        case TOK_minus_assign:
            compile_assign_arith(*cfg_node, SSA_sub);
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
    return *cfg_node;
}

cfg_node_t& ir_builder_t::compile_logical_begin(cfg_node_t& cfg_node, 
                                                bool short_cut_i)
{
    rpn_value_t& top = rpn_peek(0);
    throwing_cast(cfg_node, top, { TYPE_BOOL });
    cfg_node_t& branch_node = cfg_node;
    exits_with_branch(branch_node, top.ssa_value);
    logical_stack.push_back({ &branch_node, top.pstring });
    rpn_pop();

    cfg_node_t& long_cut = insert_cfg(true);
    branch_node.build_set_output(!short_cut_i, long_cut);
    return long_cut;
}

cfg_node_t& ir_builder_t::compile_logical_end(cfg_node_t& cfg_node, 
                                              bool short_cut_i)
{
    rpn_value_t& top = rpn_peek(0);
    throwing_cast(cfg_node, top, {TYPE_BOOL});

    logical_data_t logical = logical_stack.back();
    logical_stack.pop_back();

    cfg_node_t& merge_node = insert_cfg(true);
    exits_with_jump(cfg_node);
    cfg_node.build_set_output(0, merge_node);
    logical.branch_node->build_set_output(short_cut_i, merge_node);

    top.ssa_value = &merge_node.emplace_ssa(
        ir, SSA_phi, type_t{TYPE_BOOL}, short_cut_i, top.ssa_value);
    top.pstring = concat(logical.lhs_pstring, top.pstring);
    top.category = RVAL;
    assert(top.type == type_t{TYPE_BOOL});
    return merge_node;
}

void ir_builder_t::compile_assign(cfg_node_t& cfg_node)
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

    throwing_cast(cfg_node, assignment, assignee.type);

    // Remap the identifier to point to the new value.
    cfg_node.block_data->local_vars[assignee.local_var_i] = 
        assignment.ssa_value;
    assignee.ssa_value = assignment.ssa_value;

    // Leave the assignee on the stack, but extend its pstring.
    rpn_pop();
}

void ir_builder_t::compile_assign_arith(cfg_node_t& cfg_node, ssa_op_t op)
{
    rpn_tuck(rpn_peek(1), 1);
    compile_arith(cfg_node, op);
    compile_assign(cfg_node);
}

// Applies an operator to the top two values on the eval stack,
// turning them into a single value.
void ir_builder_t::compile_binary_operator(cfg_node_t& cfg_node, 
                                           ssa_op_t op, type_t result_type)
{
    assert(rpn_stack.size() >= 2);
    rpn_peek(1).ssa_value = &cfg_node.emplace_ssa(
        ir, op, result_type, rpn_peek(1).ssa_value, rpn_peek(0).ssa_value);
    rpn_peek(1).type = result_type;
    rpn_peek(1).category = RVAL;
    rpn_peek(1).pstring = concat(rpn_peek(1).pstring, rpn_peek(0).pstring);
    rpn_pop();
}

void ir_builder_t::compile_arith(cfg_node_t& cfg_node, ssa_op_t op)
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

    compile_binary_operator(cfg_node, op, new_type);
}

void ir_builder_t::compile_compare(cfg_node_t& cfg_node, ssa_op_t op)
{
    rpn_value_t& lhs = rpn_peek(1);
    rpn_value_t& rhs = rpn_peek(0);

    if(!is_arithmetic(lhs.type.name) || !is_arithmetic(rhs.type.name))
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
    rpn_value.ssa_value = &cfg_node.emplace_ssa(
        ir, SSA_cast, to_type, rpn_value.ssa_value);
    rpn_value.type = to_type;
    rpn_value.category = RVAL;
}

bool ir_builder_t::cast(cfg_node_t& cfg_node,
                        rpn_value_t& rpn_value, type_t to_type)
{
    switch(can_cast(rpn_value.type, to_type))
    {
    default:
    case CAST_FAIL: return false;
    case CAST_NOP:  return true;
    case CAST_OP:
        force_cast(cfg_node, rpn_value, to_type);
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
        if(results[i] == CAST_OP)
            force_cast(cfg_node, begin[i], type_begin[i]);

    return 0; // 0 means no errors!
}

