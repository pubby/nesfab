#include "interpret.hpp"

#include "alloca.hpp"
#include "globals.hpp"

#include <iostream> // TODO

cpair_t interpret_expr(token_t const* expr, type_t expected_type)
{
    interpreter_t i(expr, expected_type);
    return i.final_result;
}

interpreter_t::interpreter_t(token_t const* expr, type_t expected_type)
{
    interpret_expr(expr);
    if(expected_type.name() != TYPE_VOID)
        throwing_cast(rpn_stack.only1(), expected_type);
    final_result.value = to_cval(rpn_stack.only1());
    final_result.type = rpn_stack.only1().type;
}

interpreter_t::interpreter_t(fn_t const& fn_ref, cval_t const* args)
: fn(&fn_ref)
, stmt(fn_ref.def().stmts.data())
{
    local_vars.resize(fn->def().local_vars.size());
    local_var_types.resize(fn->def().local_vars.size());

    for(unsigned i = 0; i < local_var_types.size(); ++i)
        local_var_types[i] = fn->def().local_vars[i].type;

    unsigned const argn = fn->type().num_params();
    for(unsigned i = 0; i < argn; ++i)
        local_vars[i] = args[i];

    for(unsigned i = argn; i < local_vars.size(); ++i)
    {
        type_t const t = var_i_type(i);
        switch(t.name())
        {
        case TYPE_ARRAY:
            local_vars[i].resize(num_members(t));
            for(auto& v : local_vars[i])
                v.resize(t.size());
            break;
        case TYPE_STRUCT:
            local_vars[i].resize(num_members(t));
            init_locals({ t }, local_vars[i]);
            break;
        default:
            local_vars[i].resize(1);
            local_vars[i][0].resize(1);
            break;
        }
    }

    interpret_stmts();
}

std::size_t interpreter_t::num_locals() const 
{ 
    return fn->def().local_vars.size(); 
}

type_t interpreter_t::var_i_type(unsigned var_i) const
{
    assert(var_i < local_var_types.size());
    return local_var_types[var_i];
}

void interpreter_t::init_locals(access_t a, cval_t& cval)
{
    if(a.type.name() == TYPE_STRUCT)
    {
        struct_t const& s = a.type.struct_();
        for(unsigned i = 0; i < s.fields().size(); ++i)
            init_locals({ s.field(i).type, a.member + s.member(i) , a.index }, cval);
    }
    else if(a.type.name() == TYPE_ARRAY)
        init_locals({ a.type.elem_type(), a.member, a.type.size() }, cval);
    else
    {
        assert(a.member < cval.size());
        a.index = std::max(a.index, 1);
        cval[a.member].resize(a.index);
    }
}

auto interpreter_t::access(rpn_value_t const& rpn_value) const -> access_t
{
    access_t a = { var_i_type(rpn_value.var_i) };
    assert(a.index < 0);

    for(unsigned m = 0;;)
    {
        if(a.type.name() == TYPE_ARRAY)
        {
            if(!rpn_value.index)
            {
                assert(m == rpn_value.members.size());
                return a;
            }

            assert(rpn_value.index.is_num());
            assert(a.index < 0);

            a.index = rpn_value.index.whole();
            assert((unsigned)a.index < a.type.size());

            a.type = a.type.elem_type();
        }
        else if(m == rpn_value.members.size())
            return a;
        else
        {
            assert(a.type.name() == TYPE_STRUCT);

            struct_t const& s = a.type.struct_();
            a.member += s.member(rpn_value.members[m]);
            a.type = s.field(rpn_value.members[m]).type;

            ++m;
        }
    }
}

cval_t interpreter_t::to_cval(rpn_value_t const& rpn_value) const
{
    switch(rpn_value.type.name())
    {
    default:
        return cval_t(1, { rpn_value.value });

    case TYPE_ARRAY:
    case TYPE_STRUCT:
        auto a = access(rpn_value);

        cval_t cval;
        cval.resize(num_members(a.type));
        std::printf("cval size = %i\n", cval.size());
        for(unsigned i = 0; i < cval.size(); ++i)
        {
            if(a.index < 0)
                cval[i] = local_vars[rpn_value.var_i][a.member + i];
            else
                cval[i][a.index] = local_vars[rpn_value.var_i][a.member + i][a.index];
        }

        return cval;
    }
}

void interpreter_t::interpret_stmts()
{
    while(true)
    switch(stmt->name)
    {
    default: // Handles var inits
        if(is_var_init(stmt->name))
        {
            if(stmt->expr)
            {
                unsigned const var_i = get_local_var_i(stmt->name);
                interpret_expr(stmt->expr);
                throwing_cast(rpn_stack.peek(0), fn->def().local_vars[var_i].type);
                local_vars[var_i] = to_cval(rpn_stack.only1());
            }
            ++stmt;
        }
        else
            compiler_error(stmt->pstring, "Statement cannot appear in constant evaluation.");
        break;

    case STMT_EXPR:
    case STMT_FOR_EFFECT:
        interpret_expr(stmt->expr);
        ++stmt;
        break;

    case STMT_DO:
    case STMT_END_BLOCK:
    case STMT_LABEL:
        ++stmt;
        break;

    case STMT_ELSE:
    case STMT_END_WHILE:
    case STMT_END_FOR:
    case STMT_BREAK:
    case STMT_CONTINUE:
    case STMT_GOTO:
        stmt = &fn->def()[stmt->link];
        break;

    case STMT_IF:
        interpret_expr(stmt->expr);
        throwing_cast(rpn_stack.only1(), TYPE_BOOL);
        if(rpn_stack.only1().fixed())
            ++stmt;
        else
        {
            stmt = &fn->def()[stmt->link];
            if(stmt->name == STMT_ELSE)
                ++stmt;
        }
        break;

    case STMT_WHILE:
    case STMT_FOR:
        interpret_expr(stmt->expr);
        throwing_cast(rpn_stack.only1(), TYPE_BOOL);
        if(rpn_stack.only1().fixed())
            ++stmt;
        else
            stmt = &fn->def()[stmt->link];
        break;

    case STMT_END_DO:
        interpret_expr(stmt->expr);
        throwing_cast(rpn_stack.only1(), TYPE_BOOL);
        if(rpn_stack.only1().fixed())
            stmt = &fn->def()[stmt->link];
        else
            ++stmt;
        break;

    case STMT_RETURN:
        type_t return_type = fn->type().return_type();
        if(stmt->expr)
        {
            interpret_expr(stmt->expr);
            throwing_cast(rpn_stack.only1(), return_type);
            std::cout << "return " << rpn_stack.only1().type << std::endl;
            final_result.value = to_cval(rpn_stack.only1());
            final_result.type = rpn_stack.only1().type;
        }
        else if(return_type.name() != TYPE_VOID)
        {
            compiler_error(stmt->pstring, fmt(
                "Expecting return expression of type %.", return_type));
        }
        return;
    }
    assert(false);
}

void interpreter_t::interpret_expr(token_t const* expr)
{
    using I = fixed_int_t;

    rpn_stack.clear();
    local_vars.resize(num_locals());
    local_var_types.resize(num_locals());

    for(token_t const* token = expr; token->type; ++token)
    {
        switch(token->type)
        {
        default:
            throw std::runtime_error(fmt("Invalid token '%' in expression.", token_name(token->type)));

        case TOK_ident:
            assert(token->value < num_locals());
            rpn_stack.push(rpn_value_t{ 
                .value = local_vars[token->value][0][0], 
                .category = LVAL, 
                .type = fn->def().local_vars[token->value].type, 
                .pstring = token->pstring,
                .var_i = token->value });
            break;

        case TOK_global_ident:
            {
                global_t* global = token->ptr<global_t>();
                switch(global->gclass())
                {
                default:
                    throw std::runtime_error("Unimplemented global in expression.");
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

                for(std::uint64_t arg = 0, 
                    fail_bits = cast_args(fn_val.pstring, args, rpn_stack.past_top(), params)
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

                // Do the call:
                bc::small_vector<cval_t, 8> cval_args(num_args);
                for(unsigned i = 0; i < num_args; ++i)
                    cval_args[i] = to_cval(args[i]);
                interpreter_t call(*fn, cval_args.data());

                // Allocate a local var for the return:
                unsigned return_i = local_vars.size();
                local_vars.push_back(call.final_result.value);
                local_var_types.push_back(call.final_result.type);

                // Update the eval stack.
                rpn_value_t new_top =
                {
                    .value = call.final_result.value[0][0],
                    .category = RVAL, 
                    .type = fn_val.type.return_type(), 
                    .pstring = concat(fn_val.pstring, token->pstring),
                    .var_i = return_i
                };

                rpn_stack.pop(num_args + 1);
                rpn_stack.push(std::move(new_top));

                break;
            }

        case TOK_index:
            {
                // TOK_index is a psuedo token used to implement array indexing. 

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
                throwing_cast(array_index, TYPE_U);

                unsigned const index = array_index.whole();

                if(array_val.type.name() != TYPE_STRUCT)
                {
                    access_t const a = access(array_val);
                    assert(a.index < 0);
                    array_val.value = local_vars[array_val.var_i][a.member][index];
                }
                else
                    array_val.value = {};

                // Update the eval stack.
                array_val.index = index;
                array_val.type = array_val.type.elem_type();
                array_val.pstring = concat(array_val.pstring, token->pstring);

                rpn_stack.pop(1);

                break;
            }

        case TOK_assign:
            interpret_assign();
            break;

            /* TODO
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
            */

        case TOK_eq:
            interpret_compare([](I lhs, I rhs) { return lhs == rhs; });
            break;
        case TOK_not_eq:
            interpret_compare([](I lhs, I rhs) { return lhs != rhs; });
            break;
        case TOK_gt:
            interpret_compare([](I lhs, I rhs) { return lhs > rhs; });
            break;
        case TOK_lt:
            interpret_compare([](I lhs, I rhs) { return lhs < rhs; });
            break;
        case TOK_gte:
            interpret_compare([](I lhs, I rhs) { return lhs >= rhs; });
            break;
        case TOK_lte:
            interpret_compare([](I lhs, I rhs) { return lhs <= rhs; });
            break;

        case TOK_plus:
            interpret_arith([](I lhs, I rhs) { return lhs + rhs; });
            break;
        case TOK_minus: 
            interpret_arith([](I lhs, I rhs) { return lhs - rhs; });
            break;
        case TOK_bitwise_and: 
            interpret_arith([](I lhs, I rhs) { return lhs & rhs; });
            break;
        case TOK_bitwise_or:  
            interpret_arith([](I lhs, I rhs) { return lhs | rhs; });
            break;
        case TOK_bitwise_xor:
            interpret_arith([](I lhs, I rhs) { return lhs ^ rhs; });
            break;

            /* TODO
        case TOK_lshift:
            compile_shift(*cfg_node, SSA_shl);  
            break;
        case TOK_rshift: 
            compile_shift(*cfg_node, SSA_shr);
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
            */
        }
    }
}

void interpreter_t::interpret_assign()
{
    rpn_value_t& assignee = rpn_stack.peek(1);
    rpn_value_t& assignment = rpn_stack.peek(0);

    pstring_t const pstring = concat(assignee.pstring, assignee.pstring);

    if(assignee.category == RVAL)
        compiler_error(pstring, "Expecting lvalue on left side of assignment.");

    throwing_cast(assignment, assignee.type);

    assert(assignee.var_i < local_vars.size());
    assert(assignment.value);

    // Remap the identifier to point to the new value.
    {
        cval_t cval = to_cval(assignment);

        auto a = access(assignee);
        assert(cval.size() == num_members(a.type));

        for(unsigned i = 0; i < cval.size(); ++i)
        {
            if(a.index < 0)
                local_vars[assignee.var_i][a.member + i] = std::move(cval[i]);
            else
            {
                assert(cval[i].size() == 1);
                local_vars[assignee.var_i][a.member + i][a.index] = cval[i][0];
            }
        }
    }

    // Leave the assignee on the stack, slightly modified.
    assignee.category = RVAL;
    rpn_stack.pop();
}

template<typename Fn>
void interpreter_t::interpret_compare(Fn fn)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    if(!is_arithmetic(lhs.type) || !is_arithmetic(rhs.type))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting arithmetic types.");
    }

    bool const result = fn(lhs.fixed().value, rhs.fixed().value);

    rpn_value_t new_top =
    {
        .value = ssa_value_t((unsigned)result),
        .category = RVAL, 
        .type = { TYPE_BOOL }, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

    rpn_stack.pop(2);
    rpn_stack.push(new_top);
}

template<typename Fn>
void interpreter_t::interpret_arith(Fn fn)
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

    if(lhs.type.name() == TYPE_NUM)
        throwing_cast(lhs, result_type);
    else if(rhs.type.name() == TYPE_NUM)
        throwing_cast(rhs, result_type);

    if(lhs.type != rhs.type)
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, fmt("Operator is not defined for this type combination. (% and %)",
                                    lhs.type, rhs.type));
    }

    assert(is_arithmetic(result_type));
    assert(lhs.type == result_type);
    assert(rhs.type == result_type);
    assert(is_masked(lhs.fixed(), lhs.type.name()));
    assert(is_masked(rhs.fixed(), rhs.type.name()));

    fixed_t result = { fn(lhs.fixed().value, rhs.fixed().value) };
    result.value &= numeric_bitmask(result_type.name());

    rpn_value_t new_top =
    {
        .value = ssa_value_t(result),
        .category = RVAL, 
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

    rpn_stack.pop(2);
    rpn_stack.push(new_top);
}

// This is used to implement the other cast functions.
void interpreter_t::force_cast(rpn_value_t& rpn_value, type_t to_type)
{
    if(is_arithmetic(rpn_value.type.name()))
        rpn_value.value = mask_numeric(rpn_value.fixed(), rpn_value.type.name());
    rpn_value.type = to_type;
    rpn_value.category = RVAL;
}

// This is used to implement the other cast functions.
void interpreter_t::force_boolify(rpn_value_t& rpn_value)
{
    if(is_arithmetic(rpn_value.type.name()))
        rpn_value.value = boolify(rpn_value.fixed());
    rpn_value.type = {TYPE_BOOL};
    rpn_value.category = RVAL;
}

bool interpreter_t::cast(rpn_value_t& rpn_value, type_t to_type)
{
    switch(can_cast(rpn_value.type, to_type))
    {
    default: assert(false);
    case CAST_FAIL: 
        return false;
    case CAST_NOP:
        rpn_value.type = to_type;
        rpn_value.category = RVAL;
        return true;
    case CAST_OP:
        force_cast(rpn_value, to_type);
        return true;
    case CAST_BOOLIFY:
        force_boolify(rpn_value);
        return true;
    }
}

void interpreter_t::throwing_cast(rpn_value_t& rpn_value, type_t to_type)
{
    if(!cast(rpn_value, to_type))
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
std::uint64_t interpreter_t::cast_args(
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
            force_cast(begin[i], type_begin[i]);
        else if(results[i] == CAST_BOOLIFY)
            force_boolify(begin[i]);
    }

    return 0; // 0 means no errors!
}

