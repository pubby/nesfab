#include "parser.hpp"

#include "alloca.hpp"
#include "compiler_limits.hpp"
#include "format.hpp"
#include "globals.hpp"

static constexpr bool is_operator(token_type_t type)
    { return type > TOK_lparen && type < TOK_rparen; }

static constexpr bool operator_left_assoc(token_type_t type)
    { return type != TOK_lparen; }

static constexpr int operator_precedence(token_type_t type)
    { return token_precedence_table[type]; }

static constexpr bool is_type_prefix(token_type_t type)
{
    return (type == TOK_void || type == TOK_bool || type == TOK_byte
            || type == TOK_short || type == TOK_int || type == TOK_fixed
            || type == TOK_ram || type == TOK_rom || type == TOK_fn);
}

template<typename P>
parser_t<P>::parser_t(P& policy, file_contents_t const& file)
: policy_ptr(&policy)
, file(file)
{
    line_source = token_source = next_char = source();
    token.pstring.file_i = file_i();
    parse_indented_token();
}

// Template definitions have to come before anything that uses them!

template<typename P>
template<typename Func> 
unsigned parser_t<P>::parse_args(token_type_t l, token_type_t r, 
                                 Func parse_func)
{
    unsigned count = 0;
    parse_token(l);
    while(token.type != r)
    {
        while(true)
        {
            ++count;
            parse_func();
            if(token.type == r)
                break;
            else if(token.type != TOK_comma)
                compiler_error(fmt("Expecting , or %.", token_string(r)));
            parse_token();
        }
    }
    parse_token();
    return count;
}

template<typename P>
template<typename Func>
void parser_t<P>::maybe_parse_block(int parent_indent, Func func)
{
    int const block_indent = indent;
    while(indent > parent_indent)
    {
        if(indent != block_indent)
        {
            if(indent > block_indent)
                compiler_error("Unexpected indentation.");
            compiler_error("Unexpected deindentation.");
        }
        func();
    }
}

template<typename P>
template<typename Func>
void parser_t<P>::parse_block(int const parent_indent, Func func)
{
    if(indent <= parent_indent)
        compiler_error("Expecting indented block.");
    maybe_parse_block(parent_indent, func);
}

template<typename P>
void parser_t<P>::expect_token(token_type_t expecting) const
{
    if(token.type != expecting)
        compiler_error(fmt("Unexpected token. Expecting %.", 
                           token_string(expecting)));
}

template<typename P>
bool parser_t<P>::parse_token(token_type_t expecting)
{
    expect_token(expecting);
    return parse_token();
}

// Returns true on EOF.
template<typename P>
bool parser_t<P>::parse_token()
{
    token_t::int_type value;
restart:
    // Lex 1 token
    token_source = next_char;
    token_type_t lexed = TOK_START;
    while(lexed > TOK_LAST_STATE)
    {
        unsigned char const c = *next_char;
        lexed = lexer_transition_table[lexed + lexer_ec_table[c]];
        ++next_char;
    }
    --next_char;

    // Assign the lexed token to 'token'
    token.type = lexed;
    token.pstring.offset = token_source - source();
    token.pstring.size = next_char - token_source;
    token.value = 0;
    assert(token.pstring.file_i == file_i());

    switch(token.type)
    {
    case TOK_ERROR:
        compiler_error("Invalid token.");

    case TOK_eof:
        next_char = token_source;
        token.type = TOK_eol;
        return true;

    case TOK_comment:
        token.type = TOK_eol;
        return false;

    case TOK_whitespace:
        goto restart;

    case TOK_decimal:
        value = 0;
        for(char const* it = token_source; it != next_char; ++it)
        {
            value *= 10;
            value += *it - '0';
            if(value > std::numeric_limits<token_t::int_type>::max())
                compiler_error("Integer literal is too large.");
        }
        token.type = TOK_number;
        token.value = value;
        // fall-through
    default: 
        return false;
    }
}

template<typename P>
bool parser_t<P>::parse_line_ending()
{
    if(token.type != TOK_eol)
        compiler_error("Unexpected token. Expecting line ending.");
    return parse_indented_token();
}

template<typename P>
bool parser_t<P>::parse_indented_token()
{
    do
    {
        ++line_number;
        line_source = next_char;
        while(*next_char == ' ')
            ++next_char;
        indent = next_char - line_source;
        if(parse_token())
        {
            indent = -1;
            return true;
        }
    }
    while(token.type == TOK_eol); // Ignore blank lines.
    return false;
}

template<typename P>
pstring_t parser_t<P>::parse_ident()
{
    pstring_t ident = token.pstring;
    parse_token(TOK_ident);
    return ident;
}

template<typename P>
expr_temp_t parser_t<P>::parse_expr()
{
    expr_temp_t expr_temp;
    parse_expr(expr_temp, indent, 0);
    return expr_temp;
}

// Unlike parse_expr, this function ends with parse_line_ending();
template<typename P>
expr_temp_t parser_t<P>::parse_expr_then()
{
    int const pre_indent = indent;
    unsigned const pre_line_number = line_number;
    expr_temp_t expr = parse_expr();
    if(pre_line_number != line_number)
    {
        parse_line_ending();
        parse_token(TOK_then);
    }
    if(pre_indent != indent)
        compiler_error("Unexpected indentation.");
    parse_line_ending();
    return expr;
}

template<typename P>
void parser_t<P>::parse_expr(expr_temp_t& expr_temp, 
                             int starting_indent, int open_parens)
{
    // Expression parsing is based on the shunting yard algorithm,
    // with small modifications to support more varied expressions.

    using shunting_yard_t = bc::small_vector<token_t, 16>;
    shunting_yard_t shunting_yard;

    // The algorithm toggles between two states: applicable and inapplicable.
    // An applicable string is one that can be on the left side of an operator.
    // An inapplicable string is one that can't (e.g. "foo +").
inapplicable:
    switch(token.type)
    {
    case TOK_ident:
    case TOK_number:
        expr_temp.push_back(token);
        goto applicable_advance;

    case TOK_lparen:
        shunting_yard.push_back(token);
        parse_token();
        if(token.type == TOK_rparen)
            compiler_error("() is not a valid expr.");
        ++open_parens;
        goto inapplicable; // already advanced token

    case TOK_eol:
        if(open_parens)
        {
            parse_indented_token();
            if(indent <= starting_indent)
                compiler_error("Multi-line expressions must be indented. "
                            "(Did you forget to close a parenthesis?)");
            goto inapplicable;
        }
        else
            compiler_error("Line ended with an incomplete expression.");
    default:
        compiler_error("Unexpected token while parsing expression.");
    }

applicable_advance:
    parse_token();
applicable:
    switch(token.type)
    {
    case TOK_lparen:
        {
            char const* begin = token_source;
            int const fn_indent = indent;
            unsigned argument_count = parse_args(TOK_lparen, TOK_rparen,
                [&]() { parse_expr(expr_temp, fn_indent, open_parens+1); });
            char const* end = token_source;
            pstring_t pstring = { begin - source(), end - begin, file_i() };
            expr_temp.push_back({ TOK_apply, pstring, argument_count });
            goto applicable;
        }

    case TOK_rparen:
        while(true)
        {
            if(shunting_yard.empty())
                goto finish_expr;
            else if(shunting_yard.back().type == TOK_lparen)
            {
                shunting_yard.pop_back();
                --open_parens;
                goto applicable_advance;
            }
            expr_temp.push_back(shunting_yard.back());
            shunting_yard.pop_back();
        }
    
    case TOK_eol:
        if(open_parens)
        {
            parse_indented_token();
            if(indent <= starting_indent)
                compiler_error("Multi-line expressions must be indented. "
                            "(Did you forget to close a parenthesis?)");
            goto applicable;
        }
        goto finish_expr;

    default:
        if(is_operator(token.type))
        {
            auto const token_precedence = operator_precedence(token.type);
            while(shunting_yard.size()
                  && operator_left_assoc(shunting_yard.back().type)
                  && (operator_precedence(shunting_yard.back().type) 
                      <= token_precedence))
            {
                expr_temp.push_back(shunting_yard.back());
                shunting_yard.pop_back();
            }

            if(token.type == TOK_logical_and)
            {
                expr_temp.push_back(token);
                token.type = TOK_end_logical_and;
            }

            if(token.type == TOK_logical_or)
            {
                expr_temp.push_back(token);
                token.type = TOK_end_logical_or;
            }

            shunting_yard.push_back(token);
            parse_token();
            goto inapplicable;
        }
        goto finish_expr;
    }

finish_expr:
    while(shunting_yard.size())
    {
        if(shunting_yard.back().type == TOK_lparen)
            compiler_error("Incomplete expression. Expecting ).");
        expr_temp.push_back(shunting_yard.back());
        shunting_yard.pop_back();
    }
    assert(!expr_temp.empty());
}

template<typename P>
type_t parser_t<P>::parse_type(bool allow_void)
{
    type_t type = TYPE_VOID;

    switch(token.type)
    {
    case TOK_bool:  parse_token(); type = TYPE_BOOL; break;
    case TOK_byte:  parse_token(); type = TYPE_BYTE; break;
    case TOK_short: parse_token(); type = TYPE_SHORT; break;
    case TOK_int:   parse_token(); type = TYPE_INT; break;
    case TOK_fixed: 
        {
            unsigned w = token_source[5] - '0';
            unsigned f = token_source[6] - '0';

            if(w > 3 || f == 0 || f > 3)
            {
                compiler_error(
                    "Fixed-point type has invalid size. Valid types are "
                    "between fixed01 and fixed33.");
            }

            type = TYPE_arithmetic(w, f);
            parse_token(); 
            break;
        }
    case TOK_fn: // fn pointer
        {
            parse_token();

            bc::small_vector<type_t, 8> params_and_return;

            // Parse the function parameters.
            parse_args(TOK_lbrace, TOK_rbrace,
                [&]{ params_and_return.push_back(parse_type(false)); });

            // Parse the return type.
            params_and_return.push_back(parse_type(true));

            // Set the tail.
            type = type_t::fn(&*params_and_return.begin(), 
                              &*params_and_return.end());

        }
        break;

    case TOK_ram: // ram pointer
        {
            parse_token();

            group_bitset_t group_bitset = 0;

            // Parse the group names:
            parse_args(TOK_lbrace, TOK_rbrace, [&]
            { 
                group_bitset |= 1ull << global_t::lookup_group(file, parse_ident()).value;
            });

            type = type_t::ram_ptr(group_bitset);
        }
        break;

    case TOK_rom: // rom pointer
        {
            parse_token();

            parse_token(TOK_lbrace);

            vbank_ht bank = {};
            if(token.type == TOK_ident)
                bank = global_t::lookup_vbank(file, parse_ident());

            parse_token(TOK_rbrace);

            type = type_t::rom_ptr(bank);
        }
        break;

        /* TODO: buffers
    case TOK_lbracket:
        parse_token();
        if(token.type != TOK_number || token.value <= 0)
            compiler_error("Invalid buffer size.");
        type = type_t::buffer(token.value);
        parse_token(TOK_number); // TODO: allow expressions
        parse_token(TOK_rbracket);
        break;
        */

    default: 
        if(!allow_void)
            compiler_error("Expecting type.");
        type = TYPE_VOID;
        break;
    }

    // Arrays
    if(token.type == TOK_lbracket)
    {
        parse_token();

        // TODO: allow expressions
        if(token.type != TOK_number || token.value <= 0 || token.value > 256)
            compiler_error("Invalid array size.");
        type = type_t::array(type, token.value);
        parse_token();

        parse_token(TOK_rbracket);
    }

    return type;
}

template<typename P>
var_decl_t parser_t<P>::parse_var_decl()
{
    return { parse_type(false), parse_ident() };
}

// Returns true if the var init contains an expression.
template<typename P>
bool parser_t<P>::parse_var_init(var_decl_t& var_decl, expr_temp_t& expr)
{
    var_decl = parse_var_decl();
    if(token.type == TOK_assign)
    {
        parse_token();
        expr = parse_expr();
        return true;
    }
    return false;
}

template<typename P>
void parser_t<P>::parse_top_level()
{
    parse_block(-1, [this]{ parse_top_level_def(); });
}

template<typename P>
void parser_t<P>::parse_top_level_def()
{
    switch(token.type)
    {
    case TOK_fn: 
        return parse_fn();
    case TOK_mode: 
        return parse_mode();
    case TOK_ram: 
        return parse_group();
    default: 
        compiler_error("Unexpected token at top level.");
    }
}

template<typename P>
void parser_t<P>::parse_group()
{
    int const vars_indent = indent;

    // Parse the declaration
    parse_token(TOK_ram);
    pstring_t group_name = token.pstring;
    if(token.type == TOK_ident)
        parse_token();
    else
        group_name.size = 0;
    parse_line_ending();

    group_ht group = policy().begin_group(group_name);

    maybe_parse_block(vars_indent, 
    [&]{ 
        var_decl_t var_decl;
        expr_temp_t expr;
        if(parse_var_init(var_decl, expr))
            compiler_error("Variables in ram block cannot have an initial value.");
        else
            policy().global_var(group, var_decl, nullptr);
        parse_line_ending();
    });

    policy().end_group(group);
}

template<typename P>
void parser_t<P>::parse_fn()
{
    int const fn_indent = indent;

    // Parse the declaration
    parse_token(TOK_fn);
    pstring_t fn_name = parse_ident();

    // Parse the arguments
    bc::small_vector<var_decl_t, 8> params;
    parse_args(TOK_lparen, TOK_rparen, [&]() 
    { 
        if(params.size() >= MAX_FN_PARAMS)
            compiler_error(fmt("Compiler limit reached: too many fn parameters (max is %).", MAX_FN_PARAMS));
        params.push_back(parse_var_decl()); 
    });

    // Parse the return type
    type_t return_type = parse_type(true);

    // Parse the body of the function
    auto state = policy().begin_fn(fn_name, &*params.begin(), &*params.end(), 
                                   return_type);
    parse_line_ending();
    parse_block_statement(fn_indent);
    policy().end_fn(std::move(state));
}

template<typename P>
void parser_t<P>::parse_mode()
{
    int const mode_indent = indent;

    // Parse the declaration
    parse_token(TOK_mode);
    pstring_t mode_name = parse_ident();

    // Parse the arguments
    bc::small_vector<var_decl_t, 8> params;
    parse_args(TOK_lparen, TOK_rparen, [&]() 
    { 
        if(params.size() >= MAX_FN_PARAMS)
            compiler_error(fmt("Compiler limit reached: too many mode parameters (max is %).", MAX_FN_PARAMS));
        params.push_back(parse_var_decl()); 
    });

    // Parse the body of the function
    auto state = policy().begin_mode(mode_name, &*params.begin(), &*params.end());
    parse_line_ending();
    parse_block_statement(mode_indent);
    policy().end_mode(std::move(state));
}

template<typename P>
void parser_t<P>::parse_statement()
{
    switch(token.type)
    {
    case TOK_if:       return parse_if();
    case TOK_do:       return parse_do_while();
    case TOK_while:    return parse_while();
    case TOK_for:      return parse_for();
    case TOK_return:   return parse_return();
    case TOK_break:    return parse_break();
    case TOK_continue: return parse_continue();
    case TOK_goto:     return parse_goto();
    case TOK_label:    return parse_label();
    default: 
        if(is_type_prefix(token.type))
            return parse_var_init_statement();
        else
            return parse_expr_statement();
    }
}

template<typename P>
void parser_t<P>::parse_flow_statement()
{
    switch(token.type)
    {
    case TOK_if:    return parse_if();
    case TOK_do:    return parse_do_while();
    case TOK_while: return parse_while();
    case TOK_for:   return parse_for();
    default:
        compiler_error("Unexpected token. Expecting if, do, while, or for.");
    }
}

template<typename P>
void parser_t<P>::parse_block_statement(int const parent_indent)
{
    maybe_parse_block(parent_indent, [&]{ parse_statement(); });
}

template<typename P>
void parser_t<P>::parse_expr_statement()
{
    expr_temp_t expr = parse_expr();
    policy().expr_statement(expr);
    parse_line_ending();
}

template<typename P>
void parser_t<P>::parse_var_init_statement()
{
    var_decl_t var_decl;
    expr_temp_t expr;
    if(parse_var_init(var_decl, expr))
        policy().local_var(var_decl, &expr);
    else
        policy().local_var(var_decl, nullptr);
    parse_line_ending();
}

template<typename P>
void parser_t<P>::parse_if()
{
    int const if_indent = indent;
    pstring_t pstring = token.pstring;
    parse_token(TOK_if);
    expr_temp_t expr = parse_expr_then();
    auto if_state = policy().begin_if(pstring, expr);
    parse_block_statement(if_indent);
    if(indent == if_indent && token.type == TOK_else)
    {
        pstring = token.pstring;
        parse_token();
        auto else_state = 
            policy().end_if_begin_else(std::move(if_state), pstring);
        if(token.type != TOK_eol)
            parse_flow_statement();
        else
        {
            parse_line_ending();
            parse_block_statement(if_indent);
        }
        policy().end_else(std::move(else_state));
    }
    else
        policy().end_else(std::move(if_state));
}

template<typename P>
void parser_t<P>::parse_do_while()
{
    int const do_indent = indent;
    pstring_t pstring = token.pstring;
    parse_token(TOK_do);
    parse_line_ending();
    auto do_state = policy().begin_do_while(pstring);
    parse_block_statement(do_indent);
    pstring = token.pstring;
    parse_token(TOK_while);
    expr_temp_t expr = parse_expr();
    policy().end_do_while(std::move(do_state), pstring, expr);
    parse_line_ending();
}

template<typename P>
void parser_t<P>::parse_while()
{
    int const while_indent = indent;
    pstring_t pstring = token.pstring;
    parse_token(TOK_while);
    expr_temp_t expr = parse_expr_then();
    auto while_state = policy().begin_while(pstring, expr);
    parse_block_statement(while_indent);
    policy().end_while(std::move(while_state));
}

template<typename P>
void parser_t<P>::parse_for()
{
    unsigned const pre_line_number = line_number;
    int const for_indent = indent;
    pstring_t pstring = token.pstring;

    auto parse_statement_separator = [&]()
    {
        if(token.type == TOK_eol)
            parse_token();
        parse_token(TOK_semicolon);
        if(indent != for_indent)
            compiler_error("Multi-line for loop statements must "
                           "use same indentation.");
    };

    parse_token(TOK_for);
    expr_temp_t init_expr, *maybe_init_expr = nullptr;
    var_decl_t var_init, *maybe_var_init = nullptr;
    if(token.type != TOK_semicolon && token.type != TOK_eol)
    {
        if(is_type_prefix(token.type))
        {
            if(parse_var_init(var_init, init_expr))
                maybe_init_expr = &init_expr;
            maybe_var_init = &var_init;
        }
        else
            maybe_init_expr = &(init_expr = parse_expr());
    }
    parse_statement_separator();
    expr_temp_t condition, *maybe_condition = nullptr;
    if(token.type != TOK_semicolon && token.type != TOK_eol)
        maybe_condition = &(condition = parse_expr());
    parse_statement_separator();
    expr_temp_t effect, *maybe_effect = nullptr;
    if(token.type != TOK_semicolon && token.type != TOK_eol)
        maybe_effect = &(effect = parse_expr());
    if(line_number != pre_line_number) 
    {
        parse_line_ending();
        parse_token(TOK_then);
    }
    parse_line_ending();

    auto for_state = policy().begin_for(pstring, 
                                        maybe_var_init, maybe_init_expr, 
                                        maybe_condition, maybe_effect);
    parse_block_statement(for_indent);
    policy().end_for(std::move(for_state));
}

template<typename P>
void parser_t<P>::parse_return()
{
    pstring_t pstring = token.pstring;
    parse_token(TOK_return);
    if(token.type == TOK_eol)
        policy().return_statement(pstring, nullptr);
    else
    {
        expr_temp_t expr = parse_expr();
        policy().return_statement(pstring, &expr);
    }
    parse_line_ending();
}

template<typename P>
void parser_t<P>::parse_break()
{
    pstring_t pstring = token.pstring;
    parse_token(TOK_break);
    parse_line_ending();
    policy().break_statement(pstring);
}

template<typename P>
void parser_t<P>::parse_continue()
{
    pstring_t pstring = token.pstring;
    parse_token(TOK_continue);
    parse_line_ending();
    policy().continue_statement(pstring);
}

template<typename P>
void parser_t<P>::parse_goto()
{
    parse_token(TOK_goto);
    if(token.type == TOK_mode)
    {
        parse_token();

        // Parse like a fn call:
        expr_temp_t expr_temp;
        expr_temp.push_back({ TOK_weak_ident, token.pstring });
        pstring_t const mode = parse_ident();
        char const* begin = token_source;
        int const mode_indent = indent;
        unsigned argument_count = parse_args(TOK_lparen, TOK_rparen,
            [&]() { parse_expr(expr_temp, mode_indent, 1); });
        char const* end = token_source;
        pstring_t pstring = { begin - source(), end - begin, file_i() };
        expr_temp.push_back({ TOK_apply, pstring, argument_count });

        policy().goto_mode_statement(mode, expr_temp);
    }
    else
        policy().goto_statement(parse_ident());
    parse_line_ending();
}

template<typename P>
void parser_t<P>::parse_label()
{
    parse_token(TOK_label);
    policy().label_statement(parse_ident());
    parse_line_ending();
}

// The policies for the parser:
#include "pass1.hpp"
template class parser_t<pass1_t>;

