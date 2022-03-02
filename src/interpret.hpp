#ifndef INTERPRETER_HPP
#define INTERPRETER_HPP

#include <vector>

#include <boost/container/small_vector.hpp>

#include "decl.hpp"
#include "ir_decl.hpp"
#include "rpn.hpp"
#include "stmt.hpp"

namespace bc = boost::container;

class interpreter_t
{
private:
    fn_t const* fn = nullptr;
    stmt_t const* stmt = nullptr;
    rpn_stack_t rpn_stack;
    std::vector<cval_t> local_vars;
    std::vector<type_t> local_var_types;
public:
    cpair_t final_result;

    explicit interpreter_t(token_t const* expr, type_t expected_type = TYPE_VOID);
    interpreter_t(fn_t const& fn, cval_t const* args);

private:
    struct access_t
    {
        type_t type = {};
        unsigned member = 0;
        int index = -1;
    };

    cval_t to_cval(rpn_value_t const& rpn_value) const;

    void interpret_stmts();
    void interpret_expr(token_t const* expr);
    void interpret_assign();
    template<typename Fn>
    void interpret_compare(Fn fn);
    template<typename Fn>
    void interpret_arith(Fn fn);

    // Cast-related
    void force_cast(rpn_value_t& rpn_value, type_t to_type);
    void force_boolify(rpn_value_t& rpn_value);
    bool cast(rpn_value_t& rpn_value, type_t to_type);
    void throwing_cast(rpn_value_t& rpn_value, type_t to_type);
    std::uint64_t cast_args(pstring_t pstring, rpn_value_t* begin, 
                            rpn_value_t* end, type_t const* type_begin);

    std::size_t num_locals() const;
    type_t var_i_type(unsigned var_i) const;
    void init_locals(access_t a, cval_t& cval);
    access_t access(rpn_value_t const& rpn_value) const;
};

cpair_t interpret_expr(token_t const* expr, type_t expected_type = TYPE_VOID);


/*
void interpreter_t::compile_logical_begin(cfg_ht cfg_node, bool short_cut_i)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast(top, { TYPE_BOOL });

    if(!!rpn_stack.peek(0).whole() == short_cut_i)
    {
        // TODO
    }

    rpn_stack.pop();

    cfg_ht long_cut = insert_cfg(true);
    branch_node->build_set_output(!short_cut_i, long_cut);
    return long_cut;
}

void interpreter_t::compile_logical_end(cfg_ht cfg_node, bool short_cut_i)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast(*cfg_node, top, {TYPE_BOOL});

    logical_data_t logical = logical_stack.back();
    logical_stack.pop_back();

    cfg_ht merge_node = insert_cfg(true);
    exits_with_jump(*cfg_node);
    cfg_node->build_set_output(0, merge_node);
    logical.branch_node->build_set_output(short_cut_i, merge_node);

    top.ssa_value = merge_node->emplace_ssa(
        SSA_phi, type_t{TYPE_BOOL}, top.ssa_value, short_cut_i);
    top.pstring = concat(logical.lhs_pstring, top.pstring);
    top.category = RVAL;
    assert(top.type == type_t{TYPE_BOOL});
    return merge_node;
}









struct interpreter_t
{
    ssa_value_t var_lookup(cfg_ht, unsigned var_i);
    ssa_value_t var_lookup(cfg_ht, gvar_ht gvar);

    throwing_cast(cfg_node_t&, rpn_value_t& rpn_value, type_t to_type);

    std::pair<ssa_value_t, ssa_value_t> array_index(rpn_value_t const& array, rpn_value_t const& index);
    void assign(rpn_value_t const& assignee, rpn_value_t const& assignment);

    ssa_value_t op_add(type_name_t result_type, rpn_value_t const& lhs, rpn_value_t const& rhs)
    {
        op2(result_type, lhs, rhs, [](fixed_int_t lhs, fixed_int_t rhs)
            { return lhs.value + rhs.value; });
    }

    ssa_value_t op_sub(type_name_t result_type, rpn_value_t const& lhs, rpn_value_t const& rhs)
    {
        op2(result_type, lhs, rhs, [](fixed_int_t lhs, fixed_int_t rhs)
            { return lhs.value - rhs.value; });
    }

private:
    fixed_t get_fixed(ssa_value_t value, pstring_t pstring) const
    {
        if(value.is_num())
            return value.fixed();
        compiler_error(pstring, "Expecting numeric type.");
    }

    unsigned get_whole(ssa_value_t value, pstring_t pstring) const
    {
        if(value.is_num())
            return value.whole();
        compiler_error(pstring, "Expecting integral type.");
    }

    ssa_value_t& index_value(unsigned var_i, std::uint8_t index, pstring_t pstring)
    {
        if(var_i > m_runtime_values.size())
            compiler_error(pstring, "Name not in scope.");
        runtime_value_t& rtv = m_runtime_values[var_i];
        if(index > rtv.values.size())
            compiler_error(pstring, "Array index out of bounds.");
        return rtv.values[index];
    }

    template<typename Fn>
    ssa_value_t op2(type_name_t result_type, rpn_value_t const& lhs, rpn_value_t const& rhs, Fn fn)
    {
        if(!lhs.value.is_num())
            compiler_error(lhs.pstring, "Expecting constant numeric expression.");
        if(!rhs.value.is_num())
            compiler_error(rhs.pstring, "Expecting constant numeric expression.");

        fixed_int_t result = Fn(lhs.value.fixed().value, rhs.value.fixed().value);
        result &= numeric_bitmask(result_type);

        return ssa_value_t(fixed_t{ result });
    }

    std::vector<runtime_value_t> m_runtime_values;
};

std::pair<ssa_value_t, ssa_value_t> interpreter_t::array_index(rpn_value_t const& array, rpn_value_t const& index)
{
    return std::make_pair(array.value, index.value);
}

void interpreter_t::assign(rpn_value_t const& assignee, rpn_value_t const& assignment)
{
    if(assignee.category == LVAL_INDEX)
    {
        std::uint8_t const index = get_whole(assignee.index, assignee.pstring);
        index_value(assignee.var_i, index, assignee.pstring) = assignment.value;
    }
    else
        m_runtime_values[assignee.var_i].values = assignment.ssa_value;
}

do_array_index()
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
    auto pair = policy.array_index(array_val, array_index);

    // Update the eval stack.
    array_val.value = pair.first;
    array_val.index = pair.second;
    array_val.category = to_indexed(array_val.category);
    array_val.type = array_val.type.elem_type();
    array_val.pstring = token->pstring;
    rpn_stack.pop(1);
}

void ir_builder_t::do_assign()
{
    rpn_value_t& assignee   = rpn_stack.peek(1);
    rpn_value_t& assignment = rpn_stack.peek(0);

    assignee.pstring = concat(assignee.pstring, assignee.pstring);

    if(assignee.category == RVAL)
        compiler_error(assignee.pstring, "Expecting lvalue on left side of assignment.");

    policy.throwing_cast(assignment, assignee.type);

    assert(assignee.var_i < num_fn_vars());
    assert(assignment.ssa_value);

    // Remap the identifier to point to the new value:
    block_d& block_data = cfg_node.handle().data<block_d>();
    if(assignee.category == LVAL_INDEX)
    {
        // For arrays, we'll have to create a SSA_write_array node.

        assert(assignee.ssa_value.holds_ref());
        ssa_ht read = assignee.ssa_value.handle();
        assert(read->op() == SSA_read_array);

        locator_t loc = var_i_locator(assignee.var_i);
        ssa_ht write = cfg_node.emplace_ssa(
            SSA_write_array, var_i_type(assignee.var_i),
            read->input(0), loc, read->input(2), assignment.ssa_value);

        block_data.fn_vars[assignee.var_i] = write;
    }
    else
    {
        assert(assignee.category == LVAL);
        block_data.fn_vars[assignee.var_i] = assignment.ssa_value;
    }


    // Leave the assignee on the stack, slightly modified.
    assignee.category = RVAL;
    rpn_stack.pop();
}

void ir_builder_t::compile_arith(cfg_node_t& cfg_node, ssa_op_t op, bool carry)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    if(!is_arithmetic(lhs.type) || !is_arithmetic(rhs.type))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting arithmetic types.");
    }

    type_t result_type = lhs.type;
    if(result_type.name() == TYPE_NUM)
        result_type = rhs.type;

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

template<typename P>
class eval_t
{
private:
    using flow_t = typename P::flow_t;

    P& policy;
    fn_t const* fn;
    stmt_t const* stmt;
    rpn_stack_t rpn_stack;
    flow_t current;
public:
    eval_t(P& policy, fn_t const& fn)
    : policy(policy)
    , fn(&fn)
    {
        stmt = fn.defs.stmts.data();
        eval();
    }

    void eval()
    {
        eval_block();
    }

    auto eval_block()
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
                                  fn.def.local_vars[var_i].type);
                    value = rpn_stack.only1().ssa_value;
                }
                else
                {
                    value = cfg_node->emplace_ssa(
                        SSA_uninitialized, fn.def.local_vars[var_i].type);
                }

                cfg_node.data<block_d>().fn_vars[var_i] = value;
                ++stmt;
            }
            else
                throw std::runtime_error("Unimplemented stmt.");
            break;

        case STMT_END_BLOCK:
            ++stmt;
            return current;

        case STMT_EXPR:
            current = compile_expr(current, stmt->expr);
            ++stmt;
            break;

        case STMT_IF:
            auto branch = compile_expr(stmt->expr);
            ++stmt;
            throwing_cast(rpn_stack.only1(), TYPE_BOOL);

            policy.if_(branch);

            // 1. compile expr
            // if




            if(stmt->name == STMT_ELSE)
            {
                // Create new block for the 'false' branch.
                ++stmt;

                policy.else_();

                cfg_ht begin_false = insert_cfg(true);
                branch->build_set_output(0, begin_false);
                // repurpose 'branch' to hold end of the 'false' branch.
                // Simplifies the assignment that follows.
                branch = compile_block(begin_false);
                exits_with_jump(*branch);
            }

            break;

            policy.if_(

            {
                // Branch the active node.
                cfg_ht branch = compile_expr(cfg_node, stmt->expr);
                ++stmt;
                throwing_cast(*branch, rpn_stack.only1(), {TYPE_BOOL});
                exits_with_branch(*branch, rpn_stack.only1().ssa_value);

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
                exits_with_branch(*end_branch, rpn_stack.only1().ssa_value);

                continue_stack.emplace_back();
                break_stack.emplace_back();

                // Compile the body.
                cfg_ht begin_body = insert_cfg(true);
                end_branch->build_set_output(1, begin_body);
                cfg_ht end_body = compile_block(begin_body);
                exits_with_jump(*end_body);
                end_body->build_set_output(0, begin_branch);

                // All continue statements jump to branch_node.
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
                exits_with_branch(*end_branch, rpn_stack.only1().ssa_value);

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
                type_t return_type = fn.type.return_type();
                if(stmt->expr)
                {
                    cfg_node = compile_expr(cfg_node, stmt->expr);
                    throwing_cast(*cfg_node, rpn_stack.only1(), return_type);
                    return_values.push_back(rpn_stack.only1().ssa_value);
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
                    label->node = cfg_node = insert_cfg(true, label_name);
                    for(cfg_ht node : label->inputs)
                        node->build_set_output(0, label->node);
                }
                else // Otherwise, seal the node at a later time.
                    label->node = cfg_node = insert_cfg(false, label_name);

                break;
            }

        case STMT_GOTO:
            {
                label_t* label = stmt->label; assert(label);
                ++stmt;
                assert(label->goto_count > 0);

                // Add the jump to the label.
                label->inputs.push_back(cfg_node);
                cfg_node = compile_goto(cfg_node);

                // If this is the last goto, finish and seal the node.
                if(label->goto_count + 1 == label->inputs.size())
                {
                    assert(label->node);
                    for(cfg_ht node : label->inputs)
                        node->build_set_output(0, label->node);
                    // Seal the block.
                    seal_block(label->node.data<block_d>());
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


    do_expr(token_t const* expr)
    {
        for(token_t const* token = expr; token->type; ++token)
        {
            switch(token->type)
            {
            default:
                throw std::runtime_error(fmt("Invalid token '%' in expression.", token_name(token->type)));

            case TOK_ident:
                rpn_stack.push({ 
                    .ssa_value = var_lookup(cfg_node, token->value), 
                    .category = LVAL, 
                    .type = fn.def.local_vars[token->value].type, 
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
                                .ssa_value = var_lookup(cfg_node, var_i), 
                                .category = LVAL, 
                                .type = global->impl<gvar_t>().type, 
                                .pstring = token->pstring,
                                .var_i = var_i });
                        }
                        break;

                    case GLOBAL_FN:
                        rpn_stack.push({ 
                            .ssa_value = ssa_value_t(locator_t::fn(global->handle<fn_ht>())),
                            .category = RVAL, 
                            .type = global->impl<fn_t>().type, 
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
                    .ssa_value = token->value, 
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
                    assert(fn_val.ssa_value.is_locator());
                    fn_ht fn = fn_val.ssa_value.locator().fn();

                    // Type checks are done. Now convert the call to SSA.
                    ssa_ht fn_node = insert_fn_call(cfg_node, fn, args);

                    // Update the eval stack.
                    rpn_value_t new_top =
                    {
                        .ssa_value = fn_node,
                        .category = RVAL, 
                        .type = fn_val.type.return_type(), 
                        .pstring = token->pstring
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
                    array_val.ssa_value = index_node;
                    array_val.category = to_indexed(array_val.category);
                    array_val.type = array_val.type.elem_type();
                    array_val.pstring = token->pstring;
                    rpn_stack.pop(1);
                break;
1G
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
    }
};
*/

#endif
