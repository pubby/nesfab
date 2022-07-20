#include "parser.hpp"

#include "alloca.hpp"
#include "fixed.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "fnv1a.hpp"
#include "convert_file.hpp"
#include "eternal_new.hpp"

static constexpr bool is_operator(token_type_t type)
    { return type > TOK_lparen && type < TOK_rparen; }

static constexpr bool operator_left_assoc(token_type_t type)
    { return type != TOK_lparen; }

static constexpr int operator_precedence(token_type_t type)
    { return token_precedence_table[type]; }

static constexpr bool is_type_prefix(token_type_t type)
{
    return (type >= TOK_Void && type <= TOK_Bool) || type == TOK_type_ident || type == TOK_lbracket;
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
template<bool TrailingComma, typename Func> 
unsigned parser_t<P>::parse_args(token_type_t l, token_type_t r, Func parse_func)
{
    unsigned count = 0;
    parse_token(l);
    while(token.type != r)
    {
        while(true)
        {
            ++count;

            while(token.type == TOK_eol)
                parse_line_ending();

            parse_func();

            while(token.type == TOK_eol)
                parse_line_ending();

            if(token.type == r)
                break;

            if(token.type != TOK_comma)
                compiler_error(fmt("Expecting , or %.", token_string(r)));

            parse_token();

            if(TrailingComma)
            {
                while(token.type == TOK_eol)
                    parse_line_ending();

                if(token.type == r)
                    break;
            }
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
    token_t::int_type value, frac;
    unsigned frac_scale = 0;
restart:
    // Lex 1 token
    token_source = next_char;
    assert(next_char);
    token_type_t lexed = TOK_START;
    while(lexed > TOK_LAST_STATE)
    {
#if 0 // Enable to debug
        assert(next_char < source() + file.size());
        assert(next_char >= source());
#endif
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
        value = frac = 0;
        frac_scale = 1;

        for(char const* it = token_source; it != next_char; ++it)
        {
            if(*it == '.')
            {
                for(++it; it != next_char; ++it)
                {
                    if(frac_scale <= (1ull << fixed_t::shift))
                    {
                        frac_scale *= 10;
                        frac *= 10;
                        frac += *it - '0';
                    }
                }

                frac *= 1ull << fixed_t::shift;
                frac /= frac_scale;
                assert(frac < 1ull << fixed_t::shift);

                token.type = TOK_real;
                goto not_int;
            }
            else
            {
                value *= 10;
                value += *it - '0';
                if(value >= (1ull << 31))
                    compiler_error("Integer literal is too large.");
            }
        }
        token.type = TOK_int;
    not_int:

        value <<= fixed_t::shift;
        value |= frac;

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

static constexpr char _escape_char(char ch) 
{
    switch(ch)
    {
    case 'n': 
    case 'N': 
        return '\n';
    case '\'': 
        return '\'';
    case '"': 
        return '"';
    case '\\': 
        return '\\';
    case '0':
        return '\0';
    default:
        return ch;
    }
}

template<typename P>
string_literal_t parser_t<P>::parse_string_literal()
{
    char const* begin = token_source;
    expect_token(TOK_dquote);

    string_literal_t ret;
    while(true)
    {
        char ch = *next_char++;

        if(ch == '\"')
            break;
        else if(std::iscntrl(ch))
            compiler_error("Unexpected character in string literal.");
        else if(ch == '\\')
        {
            char const e = *next_char++;
            if(std::iscntrl(e))
                compiler_error("Unexpected character in string literal.");
            ch = _escape_char(e);
        }

        ret.string.push_back(ch);
    }

    ret.pstring = { begin - source(), next_char - begin, file_i() };
    parse_token();

    return ret;
}

template<typename P>
pstring_t parser_t<P>::parse_group_ident()
{
    parse_token(TOK_fslash);
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

    auto const type_info_impl = [&](token_type_t expr_token, std::size_t(type_t::*fn)() const)
    {
        token_t t = token;
        parse_token();

        if(is_type_prefix(token.type))
        {
            type_t const type = parse_type(false, false, {}).type;
            t.pstring.size = token.pstring.offset - t.pstring.offset;

            std::uint64_t const size = (type.*fn)();

            if(size == 0)
            {
                t.set_ptr(type_t::new_type(type));
                expr_temp.push_back(t);
            }
            else
                expr_temp.push_back({ TOK_int, t.pstring, size << fixed_t::shift });
        }
        else
        {
            int const impl_indent = indent;
            expr_temp_t impl_temp;

            parse_token(TOK_lparen);
            parse_expr(impl_temp, impl_indent, open_parens+1);
            parse_token(TOK_rparen);

            t.pstring.size = token.pstring.offset - t.pstring.offset;
            t.type = expr_token;
            t.set_ptr(policy().convert_expr(impl_temp));
            expr_temp.push_back(t);
        }
    };

    // The algorithm toggles between two states: applicable and inapplicable.
    // An applicable string is one that can be on the left side of an operator.
    // An inapplicable string is one that can't (e.g. "foo +").
inapplicable:
    switch(token.type)
    {
    case TOK_ident:
    case TOK_int:
    case TOK_real:
    number:
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

    case TOK_minus:
        {
            token_t t = token;
            parse_token();

            // Handling numbers separately may grant a tiny speedup
            // (note the code would still work without this)
            if(token.type == TOK_int || token.type == TOK_real)
            {
                token.value = -token.value;
                goto number;
            }

            t.type = TOK_unary_minus;
            shunting_yard.push_back(t);
            goto inapplicable;
        }

    case TOK_unary_xor:
    case TOK_unary_negate:
        shunting_yard.push_back(token);
        parse_token();
        goto inapplicable;

    case TOK_sizeof:
        type_info_impl(TOK_sizeof_expr, &type_t::size_of);
        goto applicable;

    case TOK_len:
        type_info_impl(TOK_len_expr, &type_t::array_length);
        goto applicable;

    case TOK_at:
        {
            parse_token();
            token_t t = token;
            t.type = TOK_at;
            parse_token(TOK_ident);
            expr_temp.push_back(std::move(t));
        }
        goto applicable;

    default:
        if(is_type_prefix(token.type))
        {
            parse_cast(expr_temp, open_parens+1);
            goto applicable;
        }
        else
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

    case TOK_lbracket:
        {
            char const* begin = token_source;
            int const bracket_indent = indent;
            parse_token();
            parse_expr(expr_temp, bracket_indent, open_parens+1);
            parse_token(TOK_rbracket);
            char const* end = token_source;
            pstring_t pstring = { begin - source(), end - begin, file_i() };
            expr_temp.push_back({ TOK_index, pstring });
            goto applicable;
        }

    case TOK_period:
        {
            parse_token();
            pstring_t pstring = token.pstring;
            parse_token(TOK_ident);

            std::uint64_t const hash = fnv1a<std::uint64_t>::hash(pstring.view(source()));
            expr_temp.push_back({ TOK_period, pstring, hash });

            goto applicable;
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
src_type_t parser_t<P>::parse_cast(expr_temp_t& expr_temp, int open_parens)
{
    int const cast_indent = indent;
    char const* begin = token_source;

    src_type_t src_type = parse_type(false, true, {});

    unsigned argument_count = parse_args<true>(TOK_lparen, TOK_rparen,
        [&]() { parse_expr(expr_temp, cast_indent, open_parens+1); });

    char const* end = token_source;
    pstring_t pstring = { begin - source(), end - begin, file_i() };

    if(src_type.type.is_unsized_array())
        src_type.type.set_array_length(argument_count, src_type.pstring);

    // Casts are implemented as a pair of two tokens:
    // - a TOK_cast_argn first, counting how many arguments the cast parsed
    // - a TOK_cast_type second, containing a pointer to the desired type
    expr_temp.push_back({ TOK_cast_argn, pstring, argument_count });
    expr_temp.push_back(token_t::make_ptr(TOK_cast_type, src_type.pstring, type_t::new_type(src_type.type)));
    
    return src_type;
}

template<typename P>
src_type_t parser_t<P>::parse_type(bool allow_void, bool allow_blank_size, group_ht group)
{
    src_type_t result = { TYPE_VOID, token.pstring };
    switch(token.type)
    {
    case TOK_Void:   parse_token(); result.type = TYPE_VOID; break;
    case TOK_Int:    parse_token(); result.type = TYPE_INT; break;
    case TOK_Real:   parse_token(); result.type = TYPE_REAL; break;
    case TOK_Bool:   parse_token(); result.type = TYPE_BOOL; break;
    case TOK_F:      parse_token(); result.type = TYPE_F1; break;
    case TOK_FF:     parse_token(); result.type = TYPE_F2; break;
    case TOK_FFF:    parse_token(); result.type = TYPE_F3; break;
    case TOK_U:      parse_token(); result.type = TYPE_U10; break;
    case TOK_UU:     parse_token(); result.type = TYPE_U20; break;
    case TOK_UUU:    parse_token(); result.type = TYPE_U30; break;
    case TOK_UF:     parse_token(); result.type = TYPE_U11; break;
    case TOK_UUF:    parse_token(); result.type = TYPE_U21; break;
    case TOK_UUUF:   parse_token(); result.type = TYPE_U31; break;
    case TOK_UFF:    parse_token(); result.type = TYPE_U12; break;
    case TOK_UUFF:   parse_token(); result.type = TYPE_U22; break;
    case TOK_UUUFF:  parse_token(); result.type = TYPE_U32; break;
    case TOK_UFFF:   parse_token(); result.type = TYPE_U13; break;
    case TOK_UUFFF:  parse_token(); result.type = TYPE_U23; break;
    case TOK_UUUFFF: parse_token(); result.type = TYPE_U33; break;
    case TOK_S:      parse_token(); result.type = TYPE_S10; break;
    case TOK_SS:     parse_token(); result.type = TYPE_S20; break;
    case TOK_SSS:    parse_token(); result.type = TYPE_S30; break;
    case TOK_SF:     parse_token(); result.type = TYPE_S11; break;
    case TOK_SSF:    parse_token(); result.type = TYPE_S21; break;
    case TOK_SSSF:   parse_token(); result.type = TYPE_S31; break;
    case TOK_SFF:    parse_token(); result.type = TYPE_S12; break;
    case TOK_SSFF:   parse_token(); result.type = TYPE_S22; break;
    case TOK_SSSFF:  parse_token(); result.type = TYPE_S32; break;
    case TOK_SFFF:   parse_token(); result.type = TYPE_S13; break;
    case TOK_SSFFF:  parse_token(); result.type = TYPE_S23; break;
    case TOK_SSSFFF: parse_token(); result.type = TYPE_S33; break;

    case TOK_PPP:
    case TOK_PP:
        {
            bool const banked = token.type == TOK_PPP;
            parse_token();
            
            bc::small_vector<group_ht, 8> groups;

            while(token.type == TOK_fslash)
                groups.push_back(group_t::lookup(source(), parse_group_ident()).handle());

            result.type = type_t::ptr(&*groups.begin(), &*groups.end(), banked);
            break;
        }

    // PAA
    case TOK_lbracket:
        {
            if(!group)
                compiler_error("Pointer-addressable array types can only appear in group contexts.");

            pstring_t const start_pstring = token.pstring;
            parse_token();

            if(token.type == TOK_rbracket)
            {
                if(allow_blank_size)
                    result.type = type_t::paa(0, group);
                else
                    compiler_error("Array length must be specified in this context.");
            }
            else
            {
                int const array_indent = indent;
                expr_temp_t expr_temp;
                parse_expr(expr_temp, array_indent, 1);

                if(expr_temp.size() == 1 && expr_temp[0].type == TOK_int)
                    result.type = type_t::paa(expr_temp[0].signed_() >> fixed_t::shift, group, expr_temp[0].pstring);
                else
                {
                    expr_temp.push_back({});
                    result.type = type_t::paa_thunk(fast_concat(start_pstring, token.pstring), 
                        policy().convert_expr(expr_temp), group);
                }
            }

            parse_token(TOK_rbracket);
        }
        break;

    case TOK_type_ident:
        {
            global_t const& global = global_t::lookup(source(), token.pstring);
            parse_token();
            result.type = type_t::struct_thunk(global);
            break;
        }

    default: 
        if(!allow_void)
            compiler_error("Expecting type.");
        result.type = TYPE_VOID;
        break;
    }

    // TEA
    if(token.type == TOK_lbracket)
    {
        pstring_t const start_pstring = token.pstring;
        parse_token();

        if(token.type == TOK_rbracket)
        {
            if(allow_blank_size)
                result.type = type_t::tea(result.type, 0);
            else
                compiler_error("Array length must be specified in this context.");
        }
        else
        {
            int const array_indent = indent;
            expr_temp_t expr_temp;
            parse_expr(expr_temp, array_indent, 1);

            if(expr_temp.size() == 1 && expr_temp[0].type == TOK_int)
                result.type = type_t::tea(result.type, expr_temp[0].signed_() >> fixed_t::shift, expr_temp[0].pstring);
            else
            {
                expr_temp.push_back({});
                result.type = type_t::tea_thunk(fast_concat(start_pstring, token.pstring), result.type, 
                    policy().convert_expr(expr_temp));
            }
        }

        parse_token(TOK_rbracket);
    }

    result.pstring.size = token.pstring.offset - result.pstring.offset;
    return result;
}

template<typename P>
var_decl_t parser_t<P>::parse_var_decl(bool block_init, group_ht group)
{
    return { parse_type(false, block_init, group), parse_ident() };
}

// Returns true if the var init contains an expression.
template<typename P>
bool parser_t<P>::parse_var_init(var_decl_t& var_decl, expr_temp_t& expr, bool block_init, group_ht group)
{
    int const var_indent = indent;
    var_decl = parse_var_decl(block_init, group);
    if(token.type == TOK_assign)
    {
        parse_token();
        expr = parse_expr();
        if(block_init)
            parse_line_ending();
        return true;
    }
    else if(block_init && is_paa(var_decl.src_type.type.name()))
    {
        parse_line_ending();

        maybe_parse_block(var_indent, [&]
        { 
            if(token.type == TOK_file)
            {
                parse_token(TOK_file);
                pstring_t const script = parse_ident();
                string_literal_t filename = parse_string_literal();
                std::vector<locator_t> data = convert_file(source(), script, filename);
                expr.push_back(token_t::make_ptr(TOK_push_paa_byte_array, filename.pstring, 
                                                 eternal_new<locator_t>(&*data.begin(), &*data.end())));
                expr.push_back({ TOK_push_paa, filename.pstring, data.size() });
            }
            else
            {
                src_type_t const cast_type = parse_cast(expr); // Appends to 'expr'
                unsigned const cast_size = cast_type.type.size_of();
                if(cast_size == 0)
                    compiler_error(cast_type.pstring, fmt("Type % cannot appear in pointer-addressable array.", cast_type.type));
                expr.push_back({ TOK_push_paa });
            }

            parse_line_ending();
        });

        return !expr.empty();
    }
    else if(block_init)
        parse_line_ending();
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
        return parse_fn(false);
    case TOK_ct: 
        return parse_fn(true);
    case TOK_mode: 
        return parse_mode();
    case TOK_vars: 
        return parse_group_vars();
    case TOK_once: 
        return parse_group_data(true);
    case TOK_many: 
        return parse_group_data(false);
    case TOK_struct: 
        return parse_struct();
    default: 
        if(is_type_prefix(token.type))
            return parse_const();
        else
            compiler_error("Unexpected token at top level.");
    }
}

template<typename P>
void parser_t<P>::parse_struct()
{
    policy().prepare_global();
    int const struct_indent = indent;

    // Parse the declaration
    parse_token(TOK_struct);
    pstring_t const struct_name = token.pstring;
    parse_token(TOK_type_ident);
    parse_line_ending();

    auto struct_ = policy().begin_struct(struct_name);

    maybe_parse_block(struct_indent, [&]
    { 
        var_decl_t var_decl;
        expr_temp_t expr;
        if(parse_var_init(var_decl, expr, true, {})) // TODO: Allow default values in structs.
            compiler_error(var_decl.name, "Variables in struct block cannot have an initial value.");
        else
            policy().struct_field(struct_, var_decl, nullptr);
    });

    policy().end_struct(struct_);
}

template<typename P>
void parser_t<P>::parse_group_vars()
{
    policy().prepare_global();
    int const vars_indent = indent;

    // Parse the declaration
    parse_token(TOK_vars);
    pstring_t const group_name = parse_group_ident();
    parse_line_ending();

    auto group = policy().begin_group_vars(group_name);

    maybe_parse_block(vars_indent, 
    [&]{ 
        policy().prepare_global();
        var_decl_t var_decl;
        expr_temp_t expr;
        if(parse_var_init(var_decl, expr, true, group.first->group.handle()))
            compiler_error(var_decl.name, "Variables in vars block cannot have an initial value.");
        else
            policy().global_var(group, var_decl, nullptr);
    });

    policy().end_group();
}

template<typename P>
void parser_t<P>::parse_group_data(bool once)
{
    policy().prepare_global();
    int const group_indent = indent;

    // Parse the declaration
    parse_token(once ? TOK_once : TOK_many);
    pstring_t const group_name = parse_group_ident();
    parse_line_ending();

    auto group = policy().begin_group_data(group_name, once);

    maybe_parse_block(group_indent, [&]{ 
        policy().prepare_global();
        var_decl_t var_decl;
        expr_temp_t expr;
        if(!parse_var_init(var_decl, expr, true, group.first->group.handle()))
            compiler_error(var_decl.name, "Constants must be assigned a value.");
        policy().global_const(group, var_decl, expr);
    });

    policy().end_group();
}

template<typename P>
void parser_t<P>::parse_const()
{
    policy().prepare_global();
    var_decl_t var_decl;
    expr_temp_t expr;
    if(!parse_var_init(var_decl, expr, true, {}))
        compiler_error(var_decl.name, "Constants must be assigned a value.");

    if(var_decl.src_type.type.name() == TYPE_PAA)
        compiler_error(var_decl.name, "Pointer-addressable arrays cannot be defined at top-level.");

    policy().global_const({}, var_decl, expr);
}

template<typename P>
void parser_t<P>::parse_fn(bool ct)
{
    int const fn_indent = indent;

    // Parse the declaration
    parse_token(ct ? TOK_ct : TOK_fn);
    pstring_t fn_name = parse_ident();

    policy().prepare_fn(fn_name);

    // Parse the arguments
    bc::small_vector<var_decl_t, 8> params;
    parse_args(TOK_lparen, TOK_rparen, [&](){ params.push_back(parse_var_decl(false, {})); });

    // Parse the return type
    src_type_t return_type = parse_type(true, false, {});

    auto state = policy().fn_decl(fn_name, &*params.begin(), &*params.end(), return_type);

    // Parse the body of the function
    parse_line_ending();
    parse_block_statement(fn_indent);
    policy().end_fn(std::move(state), ct ? FN_CT : FN_FN);
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
    parse_args(TOK_lparen, TOK_rparen, [&](){ params.push_back(parse_var_decl(false, {})); });

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
    if(parse_var_init(var_decl, expr, true, {}))
        policy().local_var(var_decl, &expr);
    else
        policy().local_var(var_decl, nullptr);
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
        auto else_state = policy().end_if_begin_else(std::move(if_state), pstring);
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
            if(parse_var_init(var_init, init_expr, false, {}))
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

