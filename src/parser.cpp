#include "parser.hpp"

#include <filesystem>

#include <boost/container/static_vector.hpp>
#include <boost/container/small_vector.hpp>

#include "alloca.hpp"
#include "fixed.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "fnv1a.hpp"
#include "convert.hpp"
#include "eternal_new.hpp"
#include "mods.hpp"
#include "hw_reg.hpp"
#include "asm.hpp"
#include "loop_test.hpp"
#include "text.hpp"
#include "string.hpp"
#include "hex.hpp"

namespace fs = ::std::filesystem;
using namespace lex;

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
            while(token.type == TOK_eol)
                parse_line_ending();

            parse_func(count++);

            mill_eol();

            if(token.type == r)
                break;

            if(token.type != TOK_comma)
                compiler_error(fmt("Expecting , or %.", token_string(r)));

            parse_token();

            if(TrailingComma)
            {
                mill_eol();

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
        if(!LOOP_TEST(func))
            break;
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
std::unique_ptr<mods_t> parser_t<P>::parse_mods(int base_indent)
{
     std::unique_ptr<mods_t> mods;

    auto const handle_group_list = [this, &mods](mod_list_t listf)
    {
        mods->explicit_lists |= listf;
        while(token.type == TOK_fslash)
        {
            pstring_t const group_ident = parse_group_ident();
            auto& mapped = mods->lists[policy().lookup_group(group_ident)->handle()];

            if(mapped.lists & listf)
                compiler_warning("Duplicate group modifier.");

            mapped.pstring = group_ident;
            mapped.lists |= listf;
        }
    };

    while(token.type == TOK_colon && indent == base_indent)
    {
        parse_token();

        while(token.type != TOK_eol)
        {
            if(!mods)
                mods.reset(new mods_t());

            switch(token.type)
            {
            case TOK_vars: parse_token(); handle_group_list(MODL_VARS); break;
            case TOK_data: parse_token(); handle_group_list(MODL_DATA); break;
            case TOK_preserves: parse_token(); handle_group_list(MODL_PRESERVES); break;
            case TOK_employs: 
                {
                    parse_token(); 
                    if(token.type == TOK_vars)
                    {
                        parse_token();
                        handle_group_list(MODL_EMPLOYS_VARS); 
                    }
                    else if(token.type == TOK_data)
                    {
                        parse_token();
                        handle_group_list(MODL_EMPLOYS_DATA); 
                    }
                    else
                        handle_group_list(MODL_EMPLOYS); 
                    break;
                }
                break;
            case TOK_stows: 
                {
                    parse_token();
                    bool const omni = token.type == TOK_omni;
                    if(omni)
                    {
                        parse_token();
                        mods->details |= MODD_STOWS_OMNI;
                    }
                    handle_group_list(MODL_STOWS); 
                }
                break;

            case TOK_omni:
                compiler_error("Unknown modifier. Did you mean 'data'?");
                break;

            case TOK_nmi:
                {
                    if(mods->nmi)
                        compiler_error("Multiple nmi modifiers.");

                    parse_token();
                    pstring_t const pstring = parse_ident();
                    mods->nmi = &policy().lookup_global(pstring);
                }
                break;

            case TOK_irq:
                {
                    if(mods->irq)
                        compiler_error("Multiple irq modifiers.");

                    parse_token();
                    pstring_t const pstring = parse_ident();
                    mods->irq = &policy().lookup_global(pstring);
                }
                break;

            case TOK_plus:
            case TOK_minus:
                while(token.type == TOK_plus || token.type == TOK_minus)
                {
                    bool const is_plus = token.type == TOK_plus;
                    parse_token();

                    pstring_t const pstring = token.pstring;
                    std::string_view const view = token.pstring.view(source());
                    mod_flags_t const flag = ::parse_mod_flag(token.pstring.view(source()));

                    parse_token(TOK_ident);

                    if(flag)
                    {
                        if(is_plus)
                        {
                            if(mods->disable & flag)
                                compiler_warning(pstring, fmt("Ignoring conflicting flags: ", view));
                            if(mods->enable & flag)
                                compiler_warning(pstring, fmt("Duplicate flag: +", view));
                            mods->enable |= flag;
                        }
                        else
                        {
                            if(mods->enable & flag)
                                compiler_warning(pstring, fmt("Ignoring conflicting flags: ", view));
                            if(mods->disable & flag)
                                compiler_warning(pstring, fmt("Duplicate flag: ", view));
                            mods->disable |= flag;
                        }
                    }
                    else
                        compiler_warning(pstring, fmt("Unknown flag: %", view));
                }
                break;
            default:
                compiler_error("Unknown modifier.");
            }
        }
        parse_line_ending();
    }

    if(mods)
        mods->remove_conflicting_flags();

    return mods;
}

template<typename P>
template<typename Fn>
std::unique_ptr<mods_t> parser_t<P>::parse_mods_after(Fn const& fn)
{
    int const base_indent = indent;
    unsigned pre_line_number = line_number;

    fn();

    pstring_t line_break = {};
    if(line_number != pre_line_number && indent != base_indent)
        line_break = { .offset = line_source - source(), .size = token_source - line_source, .file_i = file_i() };

    parse_line_ending();

    pre_line_number = line_number;
    return parse_mods(base_indent);
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
    auto const parse_int = [this](int base)
    {
        token_t::int_type value = 0;
        token_t::int_type frac = 0;
        unsigned frac_scale = 1;

        for(char const* it = token_source; it != next_char; ++it)
        {
            if(*it == '.')
            {
                for(++it; it != next_char; ++it)
                {
                    if(frac_scale <= (1ull << fixed_t::shift))
                    {
                        frac_scale *= base;
                        frac *= base;
                        int const x = char_to_int(*it);
                        if(x >= 0)
                            frac += x;
                    }
                }

                frac *= 1ull << fixed_t::shift;
                frac /= frac_scale;
                passert(frac < 1ull << fixed_t::shift, frac, 1ull << fixed_t::shift);

                token.type = TOK_real;
                goto not_int;
            }
            else
            {
                value *= base;
                int const x = char_to_int(*it);
                if(x >= 0)
                    value += x;
                if(value >= (1ull << 31))
                    compiler_error("Integer literal is too large.");
            }
        }
        token.type = TOK_int;
    not_int:
        value <<= fixed_t::shift;
        value |= frac;
        return value;
    };

restart:
    // Lex 1 token
    token_source = next_char;
    assert(next_char);
    token_type_t lexed = TOK_START;
    while(lexed > TOK_LAST_STATE)
    {
#if 1 // Enable to debug
        passert(next_char < source() + file.size(), next_char - source(), file.size(), file_i());
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
        token.value = parse_int(10);
        return false;

    case TOK_hex:
        token.value = parse_int(16);
        return false;

    case TOK_binary:
        token.value = parse_int(2);
        return false;

    default: 
        return false;
    }
}

template<typename P>
asm_lex::token_type_t parser_t<P>::parse_asm_token(pstring_t from)
{
    asm_lex::token_type_t lexed = asm_lex::TOK_START;
    int size_left = from.size;
    for(char const* ptr = source() + from.offset; lexed > asm_lex::TOK_LAST_STATE; ++ptr)
    {
        if(size_left-- == -1)
            ::compiler_error(from, "Invalid assembly token.");
        unsigned char const c = *ptr;
        lexed = asm_lex::lexer_transition_table[lexed + asm_lex::lexer_ec_table[c]];
    }
    if(lexed == asm_lex::TOK_ERROR)
        ::compiler_error(from, "Invalid assembly token.");
    return lexed;
}

template<typename P>
bool parser_t<P>::parse_line_ending()
{
    if(token.type != TOK_eol)
        compiler_error("Unexpected token. Expecting line ending.");
    return parse_indented_token();
}

template<typename P>
void parser_t<P>::mill_eol()
{
    while(token.type == TOK_eol)
        parse_line_ending();
}

template<typename P>
bool parser_t<P>::parse_indented_token()
{
retry:

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

    if(token.type == TOK_ml_comment_begin)
    {
        while(true)
        {
            if(*next_char == '*')
            {
                ++next_char;

                if(*next_char == '\0')
                    compiler_error("Unterminated multi-line comment.");

                if(*next_char == '/')
                    break;
            }

            ++next_char;
        }

        ++next_char;
        parse_token();
        if(token.type != TOK_eol)
            compiler_error("Unexpected token. Expecting line ending following multi-line comment.");
        goto retry;
    }

    return false;
}

template<typename P>
template<typename Fn>
auto parser_t<P>::parse_file(token_type_t tt, Fn const& fn)
{
    pstring_t const file_pstring = token.pstring;
    pstring_t script;
    std::vector<convert_arg_t> args;

    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    { 
        parse_token(tt);

        unsigned const argn = parse_args(TOK_lparen, TOK_rparen, [&](unsigned arg)
        {

            switch(arg)
            {
            case 0: script = parse_ident(); return true;
            default: 
                pstring_t const pstring = token.pstring;
                switch(token.type)
                {
                case TOK_ident: 
                    args.push_back({ parse_ident(), pstring }); 
                    return true;
                case TOK_true:
                    args.push_back({ true, pstring });
                    parse_token(); 
                    return true;
                case TOK_false: 
                    args.push_back({ false, pstring });
                    parse_token();
                    return true;
                case TOK_dquote:
                    args.push_back({ parse_string_literal(true), pstring }); 
                    return true;
                case TOK_int: 
                    args.push_back({ std::uint64_t(token.value), pstring }); 
                    parse_token();
                    return true;
                default: 
                    compiler_error("Unexpected token."); 
                    return false;
                }
            }
        });

        if(argn < 1)
            compiler_error(file_pstring, "Wrong number of arguments.");
    });

    fs::path preferred_dir = file.path();
    preferred_dir.remove_filename();

    fn(file_pstring, script, std::filesystem::absolute(preferred_dir), std::move(mods), std::move(args));
}

template<typename P>
pstring_t parser_t<P>::parse_ident()
{
    pstring_t ident = token.pstring;
    parse_token(TOK_ident);
    return ident;
}

template<typename P>
string_literal_t parser_t<P>::parse_string_literal(bool open_parens, token_type_t first, char last)
{
    expect_token(first);

    string_literal_t ret = {};
    while(token.type == first)
    {
        assert(next_char == token_source + 1);
        try
        {
            next_char = ::parse_string_literal(ret, source(), token_source, last, file_i());
        }
        catch(std::exception const& e)
        {
            pstring_t const at = { next_char - source(), 1, file_i() };
            compiler_error(at, e.what());
        }
        catch(...) { throw; }

        parse_token();

        if(open_parens)
            mill_eol();
    }

    return ret;
}

template<typename P>
string_literal_t parser_t<P>::parse_char_literal(bool open_parens)
{
    return parse_string_literal(open_parens, TOK_quote, '\'');
}

template<typename P>
ast_node_t parser_t<P>::parse_string_or_char_expr(bool open_parens)
{
    bool const is_char = token.type == TOK_quote;
    bool const compressed = token.type == TOK_backtick;

    string_literal_t literal; 
    if(is_char)
        literal = parse_char_literal(open_parens);
    else if(compressed)
        literal = parse_string_literal(open_parens, TOK_backtick, '`');
    else
        literal = parse_string_literal(open_parens);

    global_t const* charmap;
    if(token.type == TOK_ident)
        charmap = &policy().lookup_global(parse_ident());
    else
        charmap = &global_t::default_charmap(literal.pstring);

    policy().uses_charmap(charmap);

    ast_node_t ast = {};
    ast.token.pstring = literal.pstring;
    ast.charmap = charmap;

    if(is_char)
    {
        // Parse the character:
        char const* begin = literal.string.data();
        char const* ptr = begin;
        char32_t const utf32 = escaped_utf8_to_utf32(ptr);
        if(!ptr)
            compiler_error(literal.pstring, "Invalid character literal.");
        if(ptr != literal.string.data() + literal.string.size())
            compiler_error(literal.pstring, "Character literal with more than one character.");

        ast.token.type = TOK_character;
        ast.token.value = utf32;
    }
    else
    {
        if(compressed)
            ast.token.type = TOK_string_compressed;
        else
            ast.token.type = TOK_string_uncompressed;

        // Add it to the string literal manager:
        ast.token.value = sl_manager.add_string(charmap, literal.pstring, literal.string, compressed);
    }

    return ast;
}

template<typename P>
pstring_t parser_t<P>::parse_group_ident()
{
    pstring_t const slash = token.pstring;
    parse_token(TOK_fslash);
    if(slash.end() != token.pstring.offset)
        return slash;
    pstring_t const ident = token.pstring;
    parse_token(TOK_ident);
    return fast_concat(slash, ident);
}

template<typename P>
std::uint16_t parser_t<P>::get_hw_reg(token_type_t token_type)
{
    switch(token_type)
    {
    case TOK_PPUCTRL:   return PPUCTRL;
    case TOK_PPUMASK:   return PPUMASK;
    case TOK_PPUSTATUS: return PPUSTATUS;
    case TOK_PPUSCROLL: return PPUSCROLL;
    case TOK_PPUADDR:   return PPUADDR;
    case TOK_PPUDATA:   return PPUDATA;
    case TOK_OAMADDR:   return OAMADDR;
    case TOK_OAMDATA:   return OAMDATA;
    case TOK_OAMDMA:    return OAMDMA;
    default: return 0;
    }
}

template<typename P>
std::uint16_t parser_t<P>::parse_hw_reg()
{
    if(std::uint16_t hw_reg = get_hw_reg(token.type))
    {
        parse_token();
        return hw_reg;
    }
    compiler_error("Expecting hardware register.");
}

template<typename P>
ast_node_t parser_t<P>::parse_expr_atom(int starting_indent, int open_parens)
{
    auto const type_info_impl = [&](token_type_t expr_token, std::size_t(type_t::*fn)() const) -> ast_node_t
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
                return { .token = t };
            }
            else
                return { .token = { .type = TOK_int, .pstring = t.pstring, .value = size << fixed_t::shift }};
        }
        else
        {
            int const paren_indent = indent;

            if(token.type != TOK_lparen)
                compiler_error("Unexpected token. Expecting type or '('.");

            parse_token();
            ast_node_t child = parse_expr(paren_indent, open_parens+1);
            parse_token(TOK_rparen);

            t.pstring.size = token.pstring.offset - t.pstring.offset;
            t.type = expr_token;

            return { .token = t, .children = eternal_emplace<ast_node_t>(child) };
        }
    };

    auto const system = [this](nes_system_t s)
    {
        pstring_t at = token.pstring;
        ast_node_t ast = { .token = { .type = TOK_int, .pstring = at, .value = s << fixed_t::shift } };
        parse_token();
        return ast;
    };

retry:
    if(indent < starting_indent)
        compiler_error("Multi-line expressions must be indented. "
                       "(Did you forget to close a parenthesis?)");
    switch(token.type)
    {
    case TOK_lparen:
        {
            int const paren_indent = indent;
            parse_token();
            ast_node_t ast = parse_expr(paren_indent, open_parens+1);
            if(token.type != TOK_rparen)
                compiler_error("Unmatched parenthesis. Expecting ')'.");
            parse_token();
            return ast;
        }
    case TOK_eol:
        if(open_parens == 0)
            compiler_error("Line ended with an incomplete expression.");
        parse_indented_token();
        goto retry;

    case TOK_quote:
    case TOK_dquote:
    case TOK_backtick:
        return parse_string_or_char_expr(open_parens);

    case TOK_charmap:
        {
            global_t const* charmap = &global_t::default_charmap(token.pstring);
            assert(charmap);
            policy().uses_charmap(charmap);

            ast_node_t ast = { .token = token };
            ast.token.type = TOK_global_ident;
            ast.token.set_ptr(charmap);

            parse_token();

            return ast;
        }

    case TOK_ident:
    case TOK_return:
#ifndef NDEBUG
        token.value = ~0ull; // May catch a bug if the AST isn't properly converted.
#endif
        //fall-through
    case TOK_int:
    case TOK_real:
    case TOK_true:
    case TOK_false:
    case TOK_ready:
    case TOK_system:
    case TOK___mapper_detail:
    case TOK___mapper_reset:
    case TOK_nmi_counter:
        {
            ast_node_t ast = { .token = token };
            parse_token();
            return ast;
        }

    case TOK_SYSTEM_NTSC:    return system(NES_SYSTEM_NTSC);
    case TOK_SYSTEM_PAL:     return system(NES_SYSTEM_PAL);
    case TOK_SYSTEM_DENDY:   return system(NES_SYSTEM_DENDY);
    case TOK_SYSTEM_UNKNOWN: return system(NES_SYSTEM_UNKNOWN);

    case TOK_bitwise_and:
        token.type = TOK_unary_ref;
        goto unary;
    case TOK_plus:
        token.type = TOK_unary_plus;
        goto unary;
    case TOK_minus:
        token.type = TOK_unary_minus;
        goto unary;
    case TOK_at:
    case TOK_unary_xor:
    case TOK_unary_negate:
    unary:
        {
            ast_node_t ast = { .token = token };
            parse_token();

            // Handling numbers separately may grant a tiny speedup
            // (note the code would still work without this)
            if(token.type == TOK_int || token.type == TOK_real)
            {
                if(ast.token.type == TOK_unary_plus)
                    token.value = token.value;
                else if(ast.token.type == TOK_unary_minus)
                    token.value = -token.value;
                else
                    goto done_unary_number;
                token.pstring = fast_concat(ast.token.pstring, token.pstring);
                ast = { .token = token };
                parse_token();
                return ast;
            }
        done_unary_number:

            ast.children = eternal_emplace<ast_node_t>(parse_expr(
                starting_indent, open_parens, operator_precedence(ast.token.type)));
            ast.token.pstring = fast_concat(ast.token.pstring, ast.children->token.pstring);
            return ast;
        }

    case TOK_sizeof:
        return type_info_impl(TOK_sizeof_expr, &type_t::size_of);

    case TOK_len:
        return type_info_impl(TOK_len_expr, &type_t::array_length);

    case TOK_abs:
        {
            ast_node_t ast = { .token = token };
            parse_token();
            parse_token(TOK_lparen);
            ast.children = eternal_emplace<ast_node_t>(parse_expr(indent, open_parens+1));
            ast.token.pstring = fast_concat(ast.token.pstring, token.pstring);
            parse_token(TOK_rparen);
            return ast;
        }

    case TOK_min:
    case TOK_max:
        {
            ast_node_t ast = { .token = token };
            parse_token();

            bc::small_vector<ast_node_t, 2> children;
            parse_args(TOK_lparen, TOK_rparen,
                [&](unsigned){ children.push_back(parse_expr(indent, open_parens+1)); });

            ast.token.value = children.size();
            ast.children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());
            ast.token.pstring = fast_concat(ast.token.pstring, children.back().token.pstring);

            if(children.size() < 2)
                compiler_error(ast.token.pstring, fmt("Too few arguments to %. Expecting 2 or more.", token_string(ast.token.type)));

            return ast;
        }

    case TOK_read:
    case TOK_write:
        {
            ast_node_t ast = { .token = token };
            parse_token();

            auto src_type = parse_type(false, false, {});
            bc::small_vector<ast_node_t, 3> children = { ast_node_t{ .token = token_t::make_ptr( 
                TOK_cast_type, src_type.pstring, type_t::new_type(src_type.type)) }};

            unsigned const argn = parse_args(TOK_lparen, TOK_rparen,
                [&](unsigned){ children.push_back(parse_expr(indent, open_parens+1)); });
            unsigned const req = ast.token.type == TOK_read ? 1 : 2;
            if(argn != req)
                compiler_error(ast.token.pstring, fmt("Wrong number of arguments to read. Expecting %.", req));

            ast.children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());
            ast.token.pstring = fast_concat(ast.token.pstring, children.back().token.pstring);

            return ast;
        }

    case TOK_state:
        {
            ast_node_t ast = { .token = token };
            parse_token();
            parse_token(TOK_lparen);

            if(token.type != TOK_rparen)
            {
                ast.token.type = TOK_write_state;
                ast.children = eternal_emplace<ast_node_t>(parse_expr(indent, open_parens+1));
            }

            ast.token.pstring = fast_concat(ast.token.pstring, token.pstring);
            parse_token(TOK_rparen);

            return ast;
        }

    case TOK_push:
    case TOK_pop:
        {
            ast_node_t ast = { .token = token };
            parse_token();

            bc::small_vector<ast_node_t, 2> children;
            unsigned const argn = parse_args(TOK_lparen, TOK_rparen,
                [&](unsigned){ children.push_back(parse_expr(indent, open_parens+1)); });
            if(argn != ast.num_children())
                compiler_error(ast.token.pstring, fmt("Wrong number of arguments to %. Expecting %.", 
                                                      token_string(ast.token.type), ast.num_children()));
            ast.children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());
            ast.token.pstring = fast_concat(ast.token.pstring, token.pstring);

            return ast;
        }

    case TOK_lbrace:
        {
            bc::static_vector<ast_node_t, 2> children;

            // braces define a register value
            pstring_t const pstring = token.pstring;
            parse_token();
            children.push_back(parse_expr(indent, open_parens+1));
            parse_token(TOK_rbrace);
            parse_token(TOK_lparen);

            ast_node_t ast = {};

            if(token.type == TOK_rparen)
                ast.token.type = TOK_read_hw;
            else
            {
                ast.token.type = TOK_write_hw;
                children.push_back(parse_expr(indent, open_parens+1));
            }

            ast.token.pstring = fast_concat(pstring, token.pstring);
            ast.children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());
            parse_token(TOK_rparen);

            return ast;
        }

    default:
        if(is_type_prefix(token.type))
        {
            src_type_t src_type;
            return parse_cast(src_type, open_parens+1);
        }
        else 
        {
            if(std::uint16_t hw_reg = get_hw_reg(token.type))
            {
                token_t t = { .type = TOK_hw_addr, .pstring = token.pstring, .value = hw_reg };
                parse_token();
                return { .token = t };
            }
            else if(token.type == TOK_increment || token.type == TOK_decrement)
                throw compiler_error_t(fmt_error(token.pstring, "Unexpected token while parsing expression.")
                                       + fmt_note("++ and -- are reserved for future use.")
                                       + fmt_note("To increment and decrement, use '+= 1' and '-= 1'."));
            else
                compiler_error("Unexpected token while parsing expression.");
        }
    }
}

template<typename P>
ast_node_t parser_t<P>::parse_expr()
{
    return parse_expr(indent, 0);
}

template<typename P>
ast_node_t parser_t<P>::parse_expr(int starting_indent, int open_parens, int min_precedence)
{
    ast_node_t result = parse_expr_atom(starting_indent, open_parens);
    assert(result.token.type != TOK_eol);

    auto const array_index = [&](token_type_t token_type, token_type_t end_token)
    {
        ast_node_t* children = eternal_new<ast_node_t>(2);
        children[0] = result;

        char const* begin = token_source;
        int const bracket_indent = indent;
        parse_token();
        children[1] = parse_expr(bracket_indent, open_parens+1);

        parse_token(end_token);
        char const* end = token_source;

        result.token.type = token_type;
        result.token.pstring = concat(result.token.pstring, { begin - source(), end - begin, file_i() });
        result.children = children;
    };

    while(true)
    {
        if(operator_precedence(token.type) > min_precedence)
            break;

        switch(token.type)
        {
        case TOK_lbracket:
            array_index(TOK_index8, TOK_rbracket);
            break;

        case TOK_lbrace:
            array_index(TOK_index16, TOK_rbrace);
            break;

        case TOK_lparen:
            {
                int const fn_indent = indent;
                char const* begin = token_source;
                bc::small_vector<ast_node_t, 8> children = { result };

                parse_args(TOK_lparen, TOK_rparen,
                    [&](unsigned){ children.push_back(parse_expr(fn_indent, open_parens+1)); });

                char const* end = token_source;

                result.token.type = TOK_apply;
                result.token.pstring = { begin - source(), end - begin, file_i() };
                result.token.value = children.size();
                result.children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());
            }
            break;

        case TOK_period:
            {
                ast_node_t* children = eternal_new<ast_node_t>(1);
                children[0] = result;

                result.token = token;
                parse_token();

                if(token.type != TOK_ident && token.type != TOK_return)
                    compiler_error("Unexpected token. Expecting identifier or 'return'.");

                result.token.pstring = fast_concat(result.token.pstring, token.pstring);
                result.token.value = fnv1a<std::uint64_t>::hash(token.pstring.view(source()));
                result.children = children;

                parse_token();
            }
            break;

        default:
            if(is_operator(token.type))
            {
                int const p = operator_precedence(token.type) - !operator_right_assoc(token.type);
                token_t const t = token;
                parse_token();
                ast_node_t rhs = parse_expr(starting_indent, open_parens, p);

                ast_node_t* children = eternal_new<ast_node_t>(2);
                children[0] = result;
                children[1] = rhs;
                result = { .token = t, .children = children };
                result.token.pstring = concat(children[0].token.pstring, children[1].token.pstring);
            }
            else
                goto done_loop;
            break;
        }
    }
done_loop:

    if(open_parens)
        mill_eol();

    assert(result.token.type != TOK_eol);
    return result;
}

template<typename P>
ast_node_t parser_t<P>::parse_cast(src_type_t& src_type, int open_parens)
{
    int const cast_indent = indent;
    char const* begin = token_source;

    src_type = parse_type(false, true, {});

    bc::small_vector<ast_node_t, 16> children = { ast_node_t{ .token = token_t::make_ptr( 
        TOK_cast_type, src_type.pstring, type_t::new_type(src_type.type)) }};

    unsigned argument_count = parse_args<true>(TOK_lparen, TOK_rparen,
        [&](unsigned){ children.push_back(parse_expr(cast_indent, open_parens+1)); });

    char const* end = token_source;

    if(src_type.type.is_unsized_array())
        src_type.type.set_array_length(argument_count, src_type.pstring);

    ast_node_t ast = {};
    ast.token.type = TOK_cast; 
    ast.token.pstring = { begin - source(), end - begin, file_i() };
    ast.token.value = children.size();
    ast.children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());

    return ast;
}

template<typename P>
src_type_t parser_t<P>::parse_type(bool allow_void, bool allow_blank_size, group_ht group, 
                                   bool allow_groupless_paa)
{
    type_name_t tn;

    auto const parse_groups = [this]()
    {
        bc::small_vector<group_ht, 8> groups;
        while(token.type == TOK_fslash)
            groups.push_back(policy().lookup_group(parse_group_ident())->handle());
        return groups;
    };

    src_type_t result = { token.pstring, TYPE_VOID };
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
    case TOK_AAA:    parse_token(); result.type = TYPE_BANKED_APTR; break;
    case TOK_AA:     parse_token(); result.type = TYPE_APTR; break;

    case TOK_PPP: tn = TYPE_BANKED_PPTR; goto ptr;
    case TOK_PP:  tn = TYPE_PPTR; goto ptr;
    case TOK_CCC: tn = TYPE_BANKED_CPTR; goto ptr;
    case TOK_CC:  tn = TYPE_CPTR; goto ptr;
    case TOK_MMM: tn = TYPE_BANKED_MPTR; goto ptr;
    case TOK_MM:  tn = TYPE_MPTR; goto ptr;
    ptr:
        {
            parse_token();
            auto groups = parse_groups();
            result.type = type_t::ptr(&*groups.begin(), &*groups.end(), tn);
            break;
        }

    // PAA
    case TOK_lbracket:
        {
            if(!group && !allow_groupless_paa)
                compiler_error("Pointer-addressable array types can only appear in group or assembly contexts.");

            pstring_t const start_pstring = token.pstring;
            parse_token();

            if(token.type == TOK_rbracket)
            {
                if(allow_blank_size)
                    result.type = type_t::paa(group);
                else
                    compiler_error("Array length must be specified in this context.");
            }
            else
            {
                ast_node_t expr = parse_expr(indent, 1);
                policy().convert_ast(expr, IDEP_TYPE);

                if(expr.token.type == TOK_int)
                    result.type = type_t::paa(expr.token.signed_() >> fixed_t::shift, group, expr.token.pstring);
                else
                    result.type = type_t::paa_thunk(fast_concat(start_pstring, token.pstring), expr, group);
            }

            parse_token(TOK_rbracket);
        }
        break;

    case TOK_type_ident:
        {
            global_t const& global = policy().lookup_global(token.pstring);
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

    while(true)
    {
        if(token.type == TOK_lbracket) // TEA
        {
            pstring_t const start_pstring = token.pstring;
            parse_token();

            if(token.type == TOK_rbracket)
            {
                if(allow_blank_size)
                {
                    result.type = type_t::tea(result.type);
                    assert(result.type.unsized());
                }
                else
                    compiler_error("Array length must be specified in this context.");
            }
            else
            {
                int const array_indent = indent;
                ast_node_t expr = parse_expr(array_indent, 1);
                policy().convert_ast(expr, IDEP_TYPE);

                if(!is_scalar(result.type.name()) && !is_struct(result.type.name()))
                    compiler_error(fmt("% is an invalid array element type.", result.type));

                if(expr.token.type == TOK_int)
                    result.type = type_t::tea(result.type, expr.token.signed_() >> fixed_t::shift, expr.token.pstring);
                else
                    result.type = type_t::tea_thunk(fast_concat(start_pstring, token.pstring), result.type, expr);
            }

            parse_token(TOK_rbracket);
        }
        else if(token.type == TOK_lbrace) // Vec
        {
            parse_token();
            parse_token(TOK_rbrace);
            result.type = type_t::vec(result.type);
        }
        else
            break;
    }

    result.pstring.size = token.pstring.offset - result.pstring.offset;
    return result;

}

template<typename P>
var_decl_t parser_t<P>::parse_var_decl(bool block_init, group_ht group, bool allow_groupless_paa)
{
    return { parse_type(false, block_init, group, allow_groupless_paa), parse_ident() };
}

template<typename P>
template<typename Children>
bool parser_t<P>::parse_byte_block(pstring_t decl, int block_indent, global_t& global, bool is_banked, Children& children)
{
    bool proc = false;

    auto const call = [&](token_type_t tt, bool weak = false)
    {
        pstring_t pstring = token.pstring;
        ast_node_t expr;

        std::unique_ptr<mods_t> mods = parse_mods_after([&]{ expr = parse_expr(block_indent, 0); });

        if(weak)
            expr.weaken_idents();

        children.push_back(policy().byte_block_call(tt, pstring, std::move(expr), std::move(mods)));
    };

    auto const is_reg = [this](pstring_t pstring, char ch)
        { return pstring.size == 1 && std::tolower(pstring.view(source())[0]) == ch; };

    maybe_parse_block(block_indent, [&]
    {
        switch(token.type)
        {
        case TOK_lparen:
            {
                int const paren_indent = indent;
                parse_token();
                children.push_back(parse_expr(paren_indent, 1));
                parse_token(TOK_rparen);
                parse_line_ending();
            }
            break;

        case TOK_ct:
            parse_local_ct();
            break;

        case TOK_label:
        case TOK_default:
            {
                proc = true;
                unsigned const label_indent = indent;
                bool const is_default = token.type == TOK_default;

                pstring_t name = token.pstring;
                parse_token();
                if(!is_default)
                {
                    name = token.pstring;
                    parse_token(TOK_ident);
                }
                parse_line_ending();
                children.push_back(policy().byte_block_label(name, global.handle(), is_default, is_banked, nullptr));

                proc |= parse_byte_block(decl, label_indent, global, is_banked, children);
            }
            return;

        case TOK_file:
            parse_file(TOK_file, [&](pstring_t file_pstring, pstring_t script, fs::path const& preferred_dir, 
                           std::unique_ptr<mods_t> mods, std::vector<convert_arg_t> args)
            {
                if(args.empty())
                    compiler_error(file_pstring, "Expecting filename.");

                string_literal_t const filename = args[0].filename();

                conversion_t c = convert_file(source(), script, preferred_dir, filename, mods.get(),
                                              args.data() + 1, args.size() - 1);

                if(auto* vec = std::get_if<std::vector<std::uint8_t>>(&c.data))
                {
                    children.push_back({ 
                        .token = token_t::make_ptr(
                            TOK_byte_block_byte_array, filename.pstring, 
                            eternal_emplace<std::vector<std::uint8_t>>(std::move(*vec)))
                    });
                }
                else if(auto* vec = std::get_if<std::vector<locator_t>>(&c.data))
                {
                    children.push_back({ 
                        .token = token_t::make_ptr(
                            TOK_byte_block_locator_array, filename.pstring, 
                            eternal_emplace<std::vector<locator_t>>(std::move(*vec)))
                    });
                }
                else if(auto* asm_proc = std::get_if<asm_proc_t>(&c.data))
                {
                    proc = true;

                    // Prepare the proc:
                    policy().byte_block_sub_proc(filename.pstring, *asm_proc, global.handle());

                    children.push_back({ 
                        .token = token_t::make_ptr(
                            TOK_byte_block_sub_proc, filename.pstring, 
                            eternal_emplace<asm_proc_t>(std::move(*asm_proc)))
                    });
                }

                for(auto const& named_value : c.named_values)
                {
                    policy().byte_block_named_value(file_pstring, named_value.name, named_value.value);
                }
            });
            break;

        case TOK_fn:
            proc = true;
            parse_token();
            call(TOK_byte_block_call);
            break;

        case TOK_goto:
            proc = true;
            parse_token();
            if(token.type == TOK_mode)
            {
                parse_token();
                call(TOK_byte_block_goto_mode, true);
            }
            else
                call(TOK_byte_block_goto);
            break;

        case TOK_nmi:
            {
                proc = true;
                pstring_t const pstring = token.pstring;
                parse_token();
                children.push_back(policy().byte_block_wait_nmi(pstring, parse_mods(indent)));
                parse_line_ending();
            }
            break;

        case TOK_switch:
            {
                using namespace std::literals;

                pstring_t const pstring = token.pstring;
                parse_token();

                token_type_t tt;

                if(!is_ident(token.type))
                    compiler_error("Expecting X or Y.");
                if(is_reg(token.pstring, 'x'))
                    tt = TOK_byte_block_bank_switch_x;
                else if(is_reg(token.pstring, 'y'))
                    tt = TOK_byte_block_bank_switch_y;
                else if(to_lower(token.pstring.string(source())) == "ax"sv)
                    tt = TOK_byte_block_bank_switch_ax;
                else
                    compiler_error("Expecting X, Y, AX, or AY.");

                parse_token();
                children.push_back(policy().byte_block_bank_switch(pstring, tt, parse_mods(indent)));
                parse_line_ending();
            }
            break;

        default:
            if(is_type_prefix(token.type))
            {
                src_type_t cast_type;
                children.push_back(parse_cast(cast_type));
                if(!is_thunk(cast_type.type))
                {
                    unsigned const cast_size = cast_type.type.size_of();
                    if(cast_size == 0 && !is_vec(cast_type.type.name()))
                        compiler_error(cast_type.pstring, fmt("Type % cannot appear in pointer-addressable array.", cast_type.type));
                }
                parse_line_ending();
            }
            else if(is_ident(token.type))
            {
                proc = true;
                pstring_t const pstring = token.pstring;
                asm_lex::token_type_t const asm_token = parse_asm_token(token.pstring);

                parse_token();

                op_name_t name;
                switch(asm_token)
                {
#define OP_NAME(NAME) case asm_lex::TOK_##NAME: name = NAME; break;
#include "lex_op_name.inc"
#undef OP_NAME
                default: name = BAD_OP_NAME; break;
                }

                addr_mode_t mode;
                ast_node_t expr = {}, *maybe_expr = nullptr;

                switch(token.type)
                {
                case TOK_eol:
                    mode = MODE_IMPLIED;
                    break;

                case TOK_hash:
                    parse_token();
                    expr = parse_expr();
                    maybe_expr = &expr;
                    mode = MODE_IMMEDIATE;
                    break;

                case TOK_lparen:
                    parse_token();
                    expr = parse_expr();
                    maybe_expr = &expr;

                    if(token.type == TOK_comma)
                    {
                        parse_token();
                        if(!is_ident(token.type) || !is_reg(token.pstring, 'x'))
                            compiler_error("Expecting X.");
                        parse_token();
                        parse_token(TOK_rparen);
                        mode = MODE_INDIRECT_X;
                    }
                    else
                    {
                        parse_token(TOK_rparen);

                        if(token.type == TOK_comma)
                        {
                            parse_token();
                            if(!is_ident(token.type) || !is_reg(token.pstring, 'y'))
                                compiler_error("Expecting Y.");
                            parse_token();
                            mode = MODE_INDIRECT_Y;
                        }
                        else
                            mode = MODE_INDIRECT;
                    }
                    break;

                default:
                    expr = parse_expr();
                    maybe_expr = &expr;

                    if(token.type == TOK_comma)
                    {
                        parse_token();
                        if(!is_ident(token.type))
                            compiler_error("Expecting X or Y.");
                        if(is_reg(token.pstring, 'x'))
                            mode = MODE_ABSOLUTE_X;
                        else if(is_reg(token.pstring, 'y'))
                            mode = MODE_ABSOLUTE_Y;
                        else
                            compiler_error("Expecting X or Y.");
                        parse_token();
                    }
                    else
                    {
                        if(get_op(name, MODE_RELATIVE))
                            mode = MODE_RELATIVE;
                        else
                            mode = MODE_ABSOLUTE;
                    }
                    break;
                }

                children.push_back(policy().byte_block_asm_op(pstring, name, mode, maybe_expr));
                parse_line_ending();
            }
            else
                compiler_error("Unexpected token in byte block.");
            break;
        }
    });

    return proc;
}

template<typename P>
ast_node_t parser_t<P>::parse_byte_block(pstring_t decl, int block_indent, global_t& global, bool is_banked)
{
    bc::small_vector<ast_node_t, 32> children;

    bool const proc = parse_byte_block(decl, block_indent, global, is_banked, children);

    return
    {
        .token = 
        {
            .type = proc ? TOK_byte_block_proc : TOK_byte_block_data,
            .pstring = decl,
            .value = children.size()
        },
        .children = eternal_new<ast_node_t>(&*children.begin(), &*children.end())
    };
}


// Returns true if the var init contains an expression.
template<typename P>
bool parser_t<P>::parse_var_init(var_decl_t& var_decl, ast_node_t& expr, std::unique_ptr<mods_t>* mods,
                                 global_t** block_init_global, group_ht group, bool is_banked)
{
    bool const block_init = block_init_global;
    bool const allow_groupless_paa = !group && block_init_global;
    int const var_indent = indent;

    if(block_init)
        policy().prepare_global();

    var_decl = parse_var_decl(block_init, group, allow_groupless_paa);

    if(block_init)
        *block_init_global = policy().prepare_var_init(var_decl.name);

    bool const is_paa = ::is_paa(var_decl.src_type.type.name());

    if(token.type == TOK_assign && !is_paa)
    {
        parse_token();
        expr = parse_expr();

        if(block_init)
            parse_line_ending();

        if(mods)
            *mods = parse_mods(var_indent);

        return true;
    }
    else if(block_init)
    {
        parse_line_ending();

        if(mods)
            *mods = parse_mods(var_indent);

        if(is_paa)
        {
            expr = parse_byte_block(var_decl.name, var_indent, **block_init_global, is_banked);
            return expr.num_children();
        }
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
    case TOK_mode: 
    case TOK_nmi: 
    case TOK_irq: 
        return parse_fn();
    case TOK_asm: 
        parse_token();
        return parse_fn(TOK_asm);
    case TOK_vars: 
        return parse_group_vars();
    case TOK_omni: 
    case TOK_data: 
        return parse_group_data();
    case TOK_struct: 
        return parse_struct();
    case TOK_charmap:
        return parse_charmap();
    case TOK_chrrom:
        return parse_chrrom();
    case TOK_ct:
        parse_token();
        if(token.type == TOK_fn)
            return parse_fn(TOK_ct);
        else
            return parse_const();
    case TOK_audio:
        return parse_audio();
    case TOK_mapfab:
        return parse_mapfab();
    case TOK_macro:
        return parse_macro();
    case TOK_eof:
        return;
    default:
        compiler_error("Unexpected token at top level.");
    }
}

template<typename P>
void parser_t<P>::parse_chrrom()
{
    pstring_t const decl = token.pstring;
    int const chrrom_indent = indent;

    ast_node_t expr;
    ast_node_t* expr_ptr = nullptr;

    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ 
        parse_token(TOK_chrrom); 
        if(token.type != TOK_eol)
        {
            expr = parse_expr();
            expr_ptr = &expr;
        }
    });

    auto& pair = global_t::new_chrrom(decl);
    ast_node_t ast = parse_byte_block(decl, chrrom_indent, *pair.first, false);

    policy().chrrom(pair, decl, ast, std::move(mods), expr_ptr);
}

template<typename P>
void parser_t<P>::parse_macro()
{
    pstring_t pstring = token.pstring;
    parse_token(TOK_macro);
    macro_invocation_t invoke;

    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    { 
        parse_args(TOK_lparen, TOK_rparen, [&](unsigned arg)
        {
            if(arg == 0)
                invoke.name = parse_string_literal(true).string; 
            else
                invoke.args.push_back(parse_string_literal(true).string); 
            pstring = fast_concat(pstring, token.pstring);
        });
    });

    policy().macro(pstring, std::move(invoke), std::move(mods));
}

template<typename P>
void parser_t<P>::parse_audio()
{
    parse_file(TOK_audio, [&](pstring_t audio_pstring, pstring_t script, fs::path const& preferred_dir,
                              std::unique_ptr<mods_t> mods, std::vector<convert_arg_t> args)
    {
        policy().audio(audio_pstring, script, preferred_dir, std::move(args), std::move(mods));
    });
}

template<typename P>
void parser_t<P>::parse_mapfab()
{
    parse_file(TOK_mapfab, [&](pstring_t mapfab_pstring, pstring_t script, fs::path const& preferred_dir,
                                 std::unique_ptr<mods_t> mods, std::vector<convert_arg_t> args)
    {
        policy().mapfab(mapfab_pstring, script, preferred_dir, std::move(args), std::move(mods));
    });
}

template<typename P>
void parser_t<P>::parse_charmap()
{
    pstring_t charmap_name;
    bool is_default = false;
    string_literal_t characters, sentinel;
        
    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    {
        // Parse the declaration
        charmap_name = token.pstring;
        parse_token(TOK_charmap);

        if(token.type == TOK_lparen)
            is_default = true;
        else if(token.type == TOK_ident)
            charmap_name = parse_ident();
        else 
            compiler_error("Unexpected token. Expecting identifier or 'default'.");

        unsigned const argn = parse_args(TOK_lparen, TOK_rparen, [&](unsigned arg)
        {
            switch(arg)
            {
            case 0:
                characters = parse_string_literal(true);
                break;
            case 1:
                sentinel = parse_char_literal(true);
                break;
            default:
                compiler_error("Too many arguments to charmap.");
            }
        });

        if(argn < 1)
            compiler_error(charmap_name, "Too few arguments to charmap.");

    });

    policy().charmap(charmap_name, is_default, characters, sentinel, std::move(mods));
}

template<typename P>
void parser_t<P>::parse_struct()
{
    int const struct_indent = indent;

    pstring_t struct_name;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    {
        // Parse the declaration
        parse_token(TOK_struct);
        struct_name = token.pstring;
        parse_token(TOK_type_ident);
    });

    auto struct_ = policy().begin_struct(struct_name);

    maybe_parse_block(struct_indent, [&]
    { 
        var_decl_t var_decl;
        ast_node_t expr;
        if(parse_var_init(var_decl, expr, nullptr, {}, {}, false)) // TODO: Allow default values in structs.
            compiler_error(var_decl.name, "Variables in struct block cannot have an initial value.");
        else
            policy().struct_field(struct_, var_decl, nullptr);
        parse_line_ending();
    });

    policy().end_struct(struct_);
}

template<typename P>
void parser_t<P>::parse_group_vars()
{
    int const vars_indent = indent;

    // Parse the declaration
    pstring_t group_name = {};
    std::unique_ptr<mods_t> base_mods = parse_mods_after([&]
    {
        parse_token(TOK_vars);
        if(token.type != TOK_eol)
            group_name = parse_group_ident();
    });

    auto d = policy().begin_group_vars(group_name);

    maybe_parse_block(vars_indent, 
    [&]{ 
        global_t* global;
        var_decl_t var_decl;
        ast_node_t expr;
        policy().begin_global_var();

        std::unique_ptr<mods_t> mods;
        bool const has_expr = parse_var_init(var_decl, expr, &mods, &global, d.group ? d.group->handle() : group_ht{}, false);

        inherit(mods, base_mods);

        policy().global_var(d, var_decl, has_expr ? &expr : nullptr, std::move(mods));
        policy().end_global_var();
    });

    policy().end_group();
}

template<typename P>
void parser_t<P>::parse_group_data()
{
    int const group_indent = indent;

    bool omni = false;
    if(token.type == TOK_omni)
    {
        omni = true;
        parse_token();
    }

    // Parse the declaration
    pstring_t group_name = {};
    std::unique_ptr<mods_t> base_mods = parse_mods_after([&]
    {
        parse_token(TOK_data);
        if(!omni || token.type != TOK_eol)
            group_name = parse_group_ident();
    });

    auto d = policy().begin_group_data(group_name, omni);

    maybe_parse_block(group_indent, [&]
    { 
        global_t* global;
        var_decl_t var_decl;
        ast_node_t expr;
        policy().begin_global_var();

        std::unique_ptr<mods_t> mods;
        if(!parse_var_init(var_decl, expr, &mods, &global, d.group ? d.group->handle() : group_ht{}, !omni))
            compiler_error(var_decl.name, "Constants must be assigned a value.");

        inherit(mods, base_mods);

        policy().global_const(true, d, omni, var_decl, expr, std::move(mods));
        policy().end_global_var();
    });

    policy().end_group();
}

template<typename P>
void parser_t<P>::parse_const()
{
    global_t* global;
    var_decl_t var_decl;
    ast_node_t expr;

    int const const_indent = indent;

    policy().begin_global_var();

    if(!parse_var_init(var_decl, expr, nullptr, &global, {}, true))
        compiler_error(var_decl.name, "Constants must be assigned a value.");

    if(var_decl.src_type.type.name() == TYPE_PAA)
        compiler_error(var_decl.name, "Pointer-addressable arrays cannot be defined at top-level.");

    policy().global_const(false, {}, false, var_decl, expr, parse_mods(const_indent));
    policy().end_global_var();
}

template<typename P>
void parser_t<P>::parse_fn(token_type_t prefix)
{
    fn_class_t fclass;

    if(prefix == TOK_ct)
    {
        expect_token(TOK_fn);
        fclass = FN_CT;
    }
    else switch(token.type)
    {
    case TOK_fn:   fclass = FN_FN; break;
    case TOK_mode: fclass = FN_MODE; break;
    case TOK_nmi:  fclass = FN_NMI; break;
    case TOK_irq:  fclass = FN_IRQ; break;
    default: compiler_error("Unknown function prefix.");
    }

    int const fn_indent = indent;

    global_t* global;
    pstring_t fn_name;
    bc::small_vector<var_decl_t, 8> params;
    src_type_t return_type = {};
    assert(return_type.type.name() == TYPE_VOID);

    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    {
        // Parse the declaration
        parse_token();
        fn_name = parse_ident();
        global = policy().prepare_fn(fn_name);

        // Parse the arguments
        if(fclass == FN_NMI || fclass == FN_IRQ)
        {
            parse_token(TOK_lparen);
            parse_token(TOK_rparen);
        }
        else
            parse_args(TOK_lparen, TOK_rparen, [&](unsigned){ params.push_back(parse_var_decl(false, {})); });

        // Parse the return type
        if(fclass == FN_FN || fclass == FN_CT)
            return_type = parse_type(true, false, {});
    });


    auto state = policy().fn_decl(fn_name, &*params.begin(), &*params.end(), return_type, prefix == TOK_asm);

    if(prefix == TOK_asm)
    {
        // Parse the local vars of this fn:
        while(token.type == TOK_vars)
        {
            int const vars_indent = indent;
            std::unique_ptr<mods_t> base_mods = parse_mods_after([&]{ parse_token(); });

            maybe_parse_block(vars_indent, [&]
            { 
                var_decl_t decl;
                std::unique_ptr<mods_t> mods = parse_mods_after([&]
                {
                    decl = parse_var_decl(false, {}, true);
                });

                inherit(mods, base_mods);

                policy().asm_fn_var(decl, std::move(mods));
            });
        }

        ast_node_t ast = parse_byte_block(fn_name, fn_indent, *global, false);

        policy().end_asm_fn(std::move(state), fclass, ast, std::move(mods));
    }
    else
    {
        // Parse the body of the function
        parse_block_statement(fn_indent);
        policy().end_fn(std::move(state), fclass, std::move(mods));
    }
}

template<typename P>
void parser_t<P>::parse_statement()
{
    switch(token.type)
    {
    case TOK_if:       return parse_if();
    case TOK_do:       return parse_do();
    case TOK_while:    return parse_while();
    case TOK_for:      return parse_for();
    case TOK_return:   return parse_return();
    case TOK_break:    return parse_break();
    case TOK_continue: return parse_continue();
    case TOK_goto:     return parse_goto();
    case TOK_label:    return parse_label();
    case TOK_switch:   return parse_switch();
    case TOK_case:     return parse_case();
    case TOK_default:  return parse_default();
    case TOK_nmi:      return parse_nmi_statement();
    case TOK_irq:      return parse_irq_statement();
    case TOK_fence:    return parse_fence();
    case TOK_swap:     return parse_swap();
    case TOK_ct:       return parse_local_ct();
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
    case TOK_if:      return parse_if();
    case TOK_do:      return parse_do();
    case TOK_while:   return parse_while();
    case TOK_for:     return parse_for();
    case TOK_switch:  return parse_switch();
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
    policy().expr_statement(parse_expr());
    parse_line_ending();
}

template<typename P>
void parser_t<P>::parse_var_init_statement()
{
    var_decl_t var_decl;
    ast_node_t expr;
    if(parse_var_init(var_decl, expr, nullptr, {}, {}, false))
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

    ast_node_t expr;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ expr = parse_expr(); });

    auto if_state = policy().begin_if(pstring, expr, std::move(mods));
    parse_block_statement(if_indent);

    if(indent == if_indent && token.type == TOK_else)
    {
        pstring = token.pstring;
        parse_token();
        typename P::else_d else_state;

        if(token.type != TOK_eol)
        {
            else_state = policy().end_if_begin_else(std::move(if_state), pstring, std::unique_ptr<mods_t>());
            parse_flow_statement();
        }
        else
        {
            parse_line_ending();
            else_state = policy().end_if_begin_else(std::move(if_state), pstring, parse_mods(if_indent));
            parse_block_statement(if_indent);
        }

        policy().end_else(std::move(else_state));
    }
    else
        policy().end_if(std::move(if_state));
}

template<typename P>
void parser_t<P>::parse_do()
{
    parse_token(TOK_do);

    if(token.type == TOK_while)
        parse_while(true);
    else if(token.type == TOK_for)
        parse_for(true);
    else
        compiler_error("Unexpected token. Expecting while or for.");
}

template<typename P>
void parser_t<P>::parse_while(bool is_do)
{
    int const while_indent = indent;
    pstring_t pstring = token.pstring;
    parse_token(TOK_while);

    ast_node_t expr;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ expr = parse_expr(); });

    auto while_state = policy().begin_while(is_do, pstring, expr, std::move(mods));
    parse_block_statement(while_indent);
    policy().end_while(is_do, std::move(while_state));
}

template<typename P>
void parser_t<P>::parse_for(bool is_do)
{
    int const for_indent = indent;
    pstring_t pstring = token.pstring;

    auto parse_statement_separator = [&]()
    {
        if(token.type == TOK_eol)
            parse_line_ending();
        parse_token(TOK_semicolon);
        if(indent != for_indent)
            compiler_error("Multi-line for loop statements must use same indentation.");
    };

    ast_node_t init_expr, *maybe_init_expr = nullptr;
    var_decl_t var_init, *maybe_var_init = nullptr;
    ast_node_t condition, *maybe_condition = nullptr;
    ast_node_t effect, *maybe_effect = nullptr;

    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    {
        parse_token(TOK_for);

        if(token.type != TOK_semicolon && token.type != TOK_eol)
        {
            if(is_type_prefix(token.type))
            {
                if(parse_var_init(var_init, init_expr, nullptr, {}, {}, false))
                    maybe_init_expr = &init_expr;
                maybe_var_init = &var_init;
            }
            else
                maybe_init_expr = &(init_expr = parse_expr());
        }

        parse_statement_separator();

        if(token.type != TOK_semicolon && token.type != TOK_eol)
            maybe_condition = &(condition = parse_expr());

        parse_statement_separator();

        if(token.type != TOK_semicolon && token.type != TOK_eol)
            maybe_effect = &(effect = parse_expr());
    });

    auto for_state = policy().begin_for(
        is_do, pstring, maybe_var_init, maybe_init_expr, 
        maybe_condition, maybe_effect, std::move(mods));

    parse_block_statement(for_indent);
    policy().end_for(is_do, std::move(for_state));
}

template<typename P>
void parser_t<P>::parse_return()
{
    int const return_indent = indent;
    pstring_t pstring = token.pstring;
    parse_token(TOK_return);

    if(token.type == TOK_eol)
    {
        parse_line_ending();
        policy().return_statement(pstring, nullptr, parse_mods(return_indent));
    }
    else
    {
        ast_node_t ast;
        std::unique_ptr<mods_t> mods = parse_mods_after([&]{ ast = parse_expr(); });
        policy().return_statement(pstring, &ast, std::move(mods));
    }
}

template<typename P>
void parser_t<P>::parse_break()
{
    pstring_t const pstring = token.pstring;
    policy().break_statement(pstring, parse_mods_after([&]{ parse_token(TOK_break); }));
}

template<typename P>
void parser_t<P>::parse_continue()
{
    pstring_t const pstring = token.pstring;
    policy().continue_statement(pstring, parse_mods_after([&]{ parse_token(TOK_continue); }));
}

template<typename P>
void parser_t<P>::parse_goto()
{
    int const goto_indent = indent;
    parse_token(TOK_goto);

    if(token.type == TOK_mode)
    {
        parse_token();

        // Parse like a fn call:
        pstring_t mode;
        ast_node_t ast = {};
        std::unique_ptr<mods_t> mods = parse_mods_after([&]
        {
            bc::small_vector<ast_node_t, 16> children;
            mode = parse_ident();
            children.push_back({ .token = { .type = TOK_weak_ident, .pstring = mode }});

            char const* begin = token_source;
            int const mode_indent = indent;

            parse_args(TOK_lparen, TOK_rparen,
                [&](unsigned){ children.push_back(parse_expr(mode_indent, 1)); });

            char const* end = token_source;

            ast.token.type = TOK_mode_apply; 
            ast.token.pstring = { begin - source(), end - begin, file_i() };
            ast.token.value = children.size();
            ast.children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());
        });

        policy().goto_mode_statement(mode, ast, std::move(mods));
    }
    else
    {
        pstring_t const label = parse_ident();
        parse_line_ending();
        policy().goto_statement(label, parse_mods(goto_indent));
    }
}

template<typename P>
void parser_t<P>::parse_label()
{
    int const label_indent = indent;
    parse_token(TOK_label);
    pstring_t label;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ label = parse_ident(); });
    policy().begin_label(label, std::move(mods));
    parse_block_statement(label_indent);
    policy().end_label();
}

template<typename P>
void parser_t<P>::parse_case()
{
    int const label_indent = indent;
    pstring_t const at = token.pstring;
    parse_token(TOK_case);
    ast_node_t switch_expr;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ switch_expr = parse_expr(); });
    policy().begin_case_label(at, switch_expr, std::move(mods));
    parse_block_statement(label_indent);
    policy().end_label();
}

template<typename P>
void parser_t<P>::parse_default()
{
    int const label_indent = indent;
    pstring_t const at = token.pstring;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ parse_token(TOK_default); });
    policy().begin_default_label(at, std::move(mods));
    parse_block_statement(label_indent);
    policy().end_label();
}

template<typename P>
void parser_t<P>::parse_switch()
{
    int const switch_indent = indent;
    pstring_t const at = token.pstring;
    parse_token(TOK_switch);
    ast_node_t switch_expr;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ switch_expr = parse_expr(); });

    policy().begin_switch(at, switch_expr, std::move(mods));
    parse_block_statement(switch_indent);
    policy().end_switch(at);
}

template<typename P>
void parser_t<P>::parse_nmi_statement()
{
    pstring_t pstring = token.pstring;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ parse_token(TOK_nmi); });
    policy().nmi_statement(pstring, std::move(mods));
}

template<typename P>
void parser_t<P>::parse_irq_statement()
{
    pstring_t pstring = token.pstring;
    parse_token(TOK_irq); 

    ast_node_t expr;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ expr = parse_expr(); });

    policy().irq_statement(pstring, expr, std::move(mods));
}

template<typename P>
void parser_t<P>::parse_fence()
{
    pstring_t pstring = token.pstring;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]{ parse_token(TOK_fence); });
    policy().fence_statement(pstring, std::move(mods));
}

template<typename P>
void parser_t<P>::parse_swap()
{
    pstring_t pstring = token.pstring;
    ast_node_t a;
    ast_node_t b;
    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    { 
        parse_token(TOK_swap); 
        a = parse_expr();
        parse_token(TOK_comma);
        b = parse_expr();
        pstring = fast_concat(pstring, token.pstring);
    });
    policy().swap_statement(pstring, std::move(mods), a, b);
}

template<typename P>
void parser_t<P>::parse_local_ct()
{
    parse_token(TOK_ct);

    var_decl_t var_decl;
    ast_node_t expr;

    std::unique_ptr<mods_t> mods = parse_mods_after([&]
    {
        if(!parse_var_init(var_decl, expr, nullptr, {}, {}, true))
            compiler_error(var_decl.name, "Constants must be assigned a value.");
    });

    policy().local_ct(var_decl, expr, std::move(mods));
}


// The policies for the parser:
#include "pass1.hpp"
template class parser_t<pass1_t>;

