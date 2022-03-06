#include "interpret.hpp"

#include "alloca.hpp"
#include "globals.hpp"
#include "file.hpp"
#include "options.hpp"

#include <iostream> // TODO

cpair_t interpret_expr(pstring_t pstring, token_t const* expr, type_t expected_type)
{
    interpreter_t i(pstring, expr, expected_type);
    return i.final_result;
}

interpreter_t::interpreter_t(pstring_t pstring, token_t const* expr, type_t expected_type)
: pstring(pstring)
, start_time(clock::now())
{
    interpret_expr(expr);
    if(expected_type.name() != TYPE_VOID)
        throwing_cast(rpn_stack.only1(), expected_type);
    final_result.value = to_cval(rpn_stack.only1());
    final_result.type = rpn_stack.only1().type;
}

interpreter_t::interpreter_t(pstring_t pstring, fn_t const& fn_ref, cval_t const* args)
: pstring(pstring)
, fn(&fn_ref)
, stmt(fn_ref.def().stmts.data())
, start_time(clock::now())
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
    return fn ? fn->def().local_vars.size() : 0; 
}

type_t interpreter_t::var_i_type(unsigned var_i) const
{
    assert(var_i < local_var_types.size());
    return local_var_types[var_i];
}

ssa_value_t const& interpreter_t::get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index) const
{
    assert(var_i < local_var_types.size());
    assert(member < local_vars[var_i].size());

    auto& array = local_vars[var_i][member];
    if(index >= array.size())
    {
        compiler_error(pstring, fmt("Array index is out of bounds. (index of % >= size of %)", 
                       index, array.size()));
    }

    return array[index];
}

ssa_value_t& interpreter_t::get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index)
{
    return const_cast<ssa_value_t&>(static_cast<interpreter_t const*>(this)->get_local(pstring, var_i, member, index));
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
        for(unsigned i = 0; i < cval.size(); ++i)
        {
            if(a.index < 0)
                cval[i] = local_vars[rpn_value.var_i][a.member + i];
            else
                cval[i][a.index] = get_local(rpn_value.pstring, rpn_value.var_i,
                                             a.member + i, a.index);
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
        {
            type_t return_type = fn->type().return_type();
            if(stmt->expr)
            {
                interpret_expr(stmt->expr);
                throwing_cast(rpn_stack.only1(), return_type);
                final_result.value = to_cval(rpn_stack.only1());
                final_result.type = rpn_stack.only1().type;
            }
            else if(return_type.name() != TYPE_VOID)
            {
                compiler_error(stmt->pstring, fmt(
                    "Expecting return expression of type %.", return_type));
            }
        }
        return;

    case STMT_END_FN:
        type_t return_type = fn->type().return_type();
        if(return_type.name() != TYPE_VOID)
        {
            compiler_error(stmt->pstring, fmt(
                "Interpreter reached end of function without returning %.", return_type));
        }
        return;
    }
    assert(false);
}

void interpreter_t::interpret_expr(token_t const* expr)
{
    using namespace std::literals::chrono_literals;
    using U = fixed_int_t;
    using S = sfixed_int_t;

    auto elapsed = clock::now() - start_time;
    if(compiler_options().time_limit > 0)
    {
        if(elapsed > sc::milliseconds(compiler_options().time_limit))
        {
            file_contents_t file(this->pstring.file_i);
            throw interpreter_out_of_time_t(
                fmt_error(file, this->pstring, "Interpreter ran out of time executing expression.")
                + fmt_note("Computation is likely divergent.\n")
                + fmt_note(fmt("Use compiler flag --timelimit 0 to ignore this error.\n", compiler_options().time_limit))
                );
        }
    }

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
                case GLOBAL_CONST:
                    {
                        const_t const& c = global->impl<const_t>();
                        if(c.type().name() == TYPE_ARRAY || c.type().name() == TYPE_STRUCT)
                        {
                            rpn_stack.push({ 
                                .category = RVAL, 
                                .type = c.type(), 
                                .pstring = token->pstring,
                                .var_i = local_vars.size() });
                            local_vars.push_back(c.cval());
                            local_var_types.push_back(c.type());

                        }
                        else
                        {
                            rpn_stack.push({ 
                                .value = c.cval()[0][0],
                                .category = RVAL, 
                                .type = c.type(), 
                                .pstring = token->pstring });
                        }
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

        case TOK_number:
            rpn_stack.push({
                .value = fixed_t{ token->value }, 
                .category = RVAL, 
                .type = { TYPE_NUM }, 
                .pstring = token->pstring });
            break;

        case TOK_period:
            {
                // Periods represent struct member access.

                rpn_value_t& struct_val = rpn_stack.peek(0);

                if(struct_val.type.name() != TYPE_STRUCT)
                {
                    compiler_error(struct_val.pstring, fmt(
                        "Expecting struct type. Got %.", struct_val.type));
                }

                struct_t const& s = struct_val.type.struct_();

                std::uint64_t const hash = token->value;
                auto const it = s.fields().find(hash);

                if(!it)
                {
                    file_contents_t file(token->pstring.file_i);
                    compiler_error(file, token->pstring, fmt(
                        "% is not a member of %.", 
                        token->pstring.view(file.source()), s.global.name));
                }

                unsigned const field_i = it - s.fields().begin();
                pstring_t const pstring = concat(struct_val.pstring, token->pstring);

                struct_val.type = it->second.type;
                struct_val.members.push_back(field_i);
                struct_val.pstring = pstring;

                if(struct_val.type.name() == TYPE_ARRAY || struct_val.type.name() == TYPE_STRUCT)
                    struct_val.value =  {};
                else
                {
                    access_t a = access(struct_val);
                    a.index = std::max(a.index, 0);
                    struct_val.value = get_local(pstring, struct_val.var_i, a.member, a.index);
                }

                break;

            }

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

                pstring_t const call_pstring = concat(fn_val.pstring, token->pstring);

                // Do the call:
                bc::small_vector<cval_t, 8> cval_args(num_args);
                for(unsigned i = 0; i < num_args; ++i)
                    cval_args[i] = to_cval(args[i]);

                try
                {
                    interpreter_t call(call_pstring, *fn, cval_args.data());

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
                        .pstring = call_pstring,
                        .var_i = return_i
                    };

                    rpn_stack.pop(num_args + 1);
                    rpn_stack.push(std::move(new_top));
                }
                catch(interpreter_out_of_time_t& e)
                {
                    file_contents_t file(this->pstring.file_i);
                    e.msg += fmt_note(file, this->pstring, "Backtrace:");
                    throw;
                }

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
                pstring_t const pstring = concat(array_val.pstring, token->pstring);

                if(array_val.type.name() != TYPE_STRUCT)
                {
                    access_t const a = access(array_val);
                    assert(a.index < 0);
                    array_val.value = get_local(pstring, array_val.var_i, a.member, index);
                }
                else
                    array_val.value = {};

                // Update the eval stack.
                array_val.index = index;
                array_val.type = array_val.type.elem_type();
                array_val.pstring = pstring;

                rpn_stack.pop(1);

                break;
            }

        case TOK_assign:
            interpret_assign();
            break;

        case TOK_logical_and:
            interpret_logical_begin(token, TOK_logical_and, TOK_end_logical_and);
            break;
        case TOK_logical_or:
            interpret_logical_begin(token, TOK_logical_or, TOK_end_logical_or);
            break;
        case TOK_end_logical_and:
            interpret_logical_end();
            break;
        case TOK_end_logical_or:
            interpret_logical_end();
            break;

            // TODO: handle signed properly
        case TOK_eq:
            interpret_compare([](S lhs, S rhs) { return lhs == rhs; });
            break;
        case TOK_not_eq:
            interpret_compare([](S lhs, S rhs) { return lhs != rhs; });
            break;
        case TOK_gt:
            interpret_compare([](S lhs, S rhs) { return lhs > rhs; });
            break;
        case TOK_lt:
            interpret_compare([](S lhs, S rhs) { return lhs < rhs; });
            break;
        case TOK_gte:
            interpret_compare([](S lhs, S rhs) { return lhs >= rhs; });
            break;
        case TOK_lte:
            interpret_compare([](S lhs, S rhs) { return lhs <= rhs; });
            break;

        case TOK_plus:
            interpret_arith([](S lhs, S rhs) { return lhs + rhs; });
            break;
        case TOK_unary_minus:
        case TOK_minus: 
            interpret_arith([](S lhs, S rhs) { return lhs - rhs; });
            break;
        case TOK_bitwise_and: 
            interpret_arith([](S lhs, S rhs) { return lhs & rhs; });
            break;
        case TOK_bitwise_or:  
            interpret_arith([](S lhs, S rhs) { return lhs | rhs; });
            break;
        case TOK_unary_xor:
        case TOK_bitwise_xor:
            interpret_arith([](S lhs, S rhs) { return lhs ^ rhs; });
            break;

        case TOK_lshift:
            interpret_shift([](S lhs, std::uint8_t shift){ return lhs << shift; });  
            break;
        case TOK_rshift: 
            interpret_shift([](S lhs, std::uint8_t shift){ return lhs >> shift; });  
            break;

        case TOK_plus_assign:
            interpret_assign_arith([](S lhs, S rhs) { return lhs + rhs; });
            break;
        case TOK_minus_assign:
            interpret_assign_arith([](S lhs, S rhs) { return lhs - rhs; });
            break;
        case TOK_bitwise_and_assign:
            interpret_assign_arith([](S lhs, S rhs) { return lhs & rhs; });
            break;
        case TOK_bitwise_or_assign:
            interpret_assign_arith([](S lhs, S rhs) { return lhs | rhs; });
            break;
        case TOK_bitwise_xor_assign:
            interpret_assign_arith([](S lhs, S rhs) { return lhs ^ rhs; });
            break;
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

    if(!is_arithmetic(lhs.type.name()) || !is_arithmetic(rhs.type.name()))
    {
        // TODO: improve error string
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, "Expecting arithmetic types.");
    }

    bool const result = fn(lhs.sfixed(), rhs.sfixed());

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

    if(!is_arithmetic(lhs.type.name()) || !is_arithmetic(rhs.type.name()))
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

    assert(is_arithmetic(result_type).name());
    assert(lhs.type == result_type);
    assert(rhs.type == result_type);
    assert(is_masked(lhs.fixed(), lhs.type.name()));
    assert(is_masked(rhs.fixed(), rhs.type.name()));

    fixed_t result = { fn(lhs.sfixed(), rhs.sfixed()) };
    std::printf("result = %li\n", fn(lhs.sfixed(), rhs.sfixed()));
    result.value &= numeric_bitmask(result_type.name());
    std::printf("result = %li\n", result.value);
    std::printf("result = %li\n", to_signed(result.value, TYPE_NUM));

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

template<typename Fn>
void interpreter_t::interpret_shift(Fn fn)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    if(!is_arithmetic(lhs.type.name()))
        compiler_error(lhs.pstring, "Expecting arithmetic type.");

    if(rhs.type.name() == TYPE_NUM)
        throwing_cast(rhs, { TYPE_U });
    else if(rhs.type.name() != TYPE_U)
        compiler_error(rhs.pstring, "Ride-hand side of shift must be type U or Num.");

    type_t const result_type = lhs.type;

    assert(is_arithmetic(result_type.name()));
    assert(is_masked(lhs.fixed(), lhs.type.name()));
    assert(is_masked(rhs.fixed(), rhs.type.name()));

    fixed_t result = { fn(lhs.sfixed(), rhs.whole()) };
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

template<typename Fn>
void interpreter_t::interpret_assign_arith(Fn fn)
{
    rpn_stack.tuck(rpn_stack.peek(1), 1);
    throwing_cast(rpn_stack.peek(0), rpn_stack.peek(1).type);
    interpret_arith(fn);
    interpret_assign();
}

void interpreter_t::interpret_logical_begin(token_t const*& token, token_type_t logical, token_type_t logical_end)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast(top, { TYPE_BOOL });

    if(bool(top.fixed()) == (logical == TOK_logical_or))
    {
        for(int left = 1; left; --left)
        while(token->type != logical_end)
        {
            assert(token->type);
            ++token;
            if(token->type == logical)
                ++left;
        }
    }
    else
        rpn_stack.pop(1);
}

void interpreter_t::interpret_logical_end()
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast(top, { TYPE_BOOL });
}

// This is used to implement the other cast functions.
void interpreter_t::force_cast(rpn_value_t& rpn_value, type_t to_type)
{
    assert(is_arithmetic(to_type.name()) && is_arithmetic(rpn_value.type.name()));
    assert(rpn_value.type.name() != TYPE_NAME);

    fixed_t const value = mask_numeric(rpn_value.fixed(), to_type.name());

    rpn_value.value = fixed_t{ value };
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

void interpreter_t::force_round_num(rpn_value_t& rpn_value, type_t to_type)
{
    assert(rpn_value.type.name() == TYPE_NUM);
    assert(is_arithmetic(to_type.name()));

    fixed_int_t value = rpn_value.fixed().value;
    fixed_int_t const mask = numeric_bitmask(to_type.name());
    if(fixed_int_t z = builtin::ctz(mask))
        value += (1ull << (z - 1)) & value;
    value &= mask;

    fixed_int_t const supermask = numeric_supermask(to_type.name());

    if((to_signed(value, mask) & supermask) != (rpn_value.sfixed() & supermask))
        compiler_error(rpn_value.pstring, fmt("Num value doesn't fit in type %.", to_type));

    rpn_value.value = fixed_t{ value };
    rpn_value.type = to_type;
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
    case CAST_ROUND_NUM:
        force_round_num(rpn_value, to_type);
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
        else if(results[i] == CAST_ROUND_NUM)
            force_round_num(begin[i], type_begin[i]);
    }

    return 0; // 0 means no errors!
}

