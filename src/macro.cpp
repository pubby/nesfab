#include "macro.hpp"

#include <boost/container/small_vector.hpp>

#include "robin/set.hpp"

#include "macro_lex_tables.hpp"
#include "text.hpp"
#include "file.hpp"

namespace bc = ::boost::container;
using namespace macro_lex;

static token_type_t do_lex(char const*& str)
{
    char const* start = str;
    token_type_t lexed = macro_lex::TOK_START;
    for(; lexed > TOK_LAST_STATE; ++str)
    {
        unsigned char const c = *str;
        lexed = lexer_transition_table[lexed + lexer_ec_table[c]];
        if(!c)
            break;
    }
    --str;
    if(lexed == TOK_ERROR)
        str = start;
    return lexed;
}

std::string invoke_macro(unsigned file_i, std::vector<std::string> const& args)
{
    file_contents_t file(file_i);

    std::string ret;
    std::vector<std::string> params;

    char quote;

    char const* str = file.source();
    assert(str && *str);

    while(*str)
    {
        char const* begin = str;

        auto error = [&](std::string const& msg){ throw macro_error_t(msg, { begin - file.source(), str - begin, file_i }); };
        
        auto const find = [&](std::string const& name)
        {
            auto it = std::find(params.begin(), params.end(), name);
            if(it == params.end())
                error(fmt("Macro parameter #%# must be declared before use.", name));
            return it - params.begin();
        };

        bool next_upper = false;

        switch(do_lex(str))
        {
        case TOK_eof: 
            goto done;

        case TOK_dquote:   quote = '"';  goto quote;
        case TOK_quote:    quote = '\''; goto quote;
        case TOK_backtick: quote = '`';  goto quote;
        quote:
            {
                string_literal_t literal = {};
                try { str = parse_string_literal(literal, file.source(), str-1, quote, file_i); }
                catch(std::exception const& e) { error(e.what()); }
                catch(...) { throw; }
                ret.push_back(quote);
                ret += literal.string;
                ret.push_back(quote);
            }
            break;

        case TOK_ident:
            {
                unsigned const i = find(std::string(begin+1, str-1));
                if(i < args.size())
                    ret += args[i];
            }
            break;

        case TOK_eq_ident: 
            next_upper = true;
            // fall-through
        case TOK_dash_ident:
            {
                unsigned const i = find(std::string(begin+2, str-2));
                if(i < args.size())
                {
                    for(unsigned j = 0; j < args[i].size(); ++j)
                    {
                        char const c = args[i][j];

                        if(c == '_')
                        {
                            if(j != 0)
                            {
                                next_upper = true;
                                continue;
                            }
                        }
                        else if(next_upper)
                        {
                            ret.push_back(std::toupper(c));
                            next_upper = false;
                            continue;
                        }

                        ret.push_back(c);
                    }
                }
            };
            break;

        case TOK_dquote_ident:   quote = '"';  goto quote_replace;
        case TOK_quote_ident:    quote = '\''; goto quote_replace;
        case TOK_backtick_ident: quote = '`';  goto quote_replace;
        quote_replace:
            {
                unsigned const i = find(std::string(begin+2, str-2));
                ret.push_back(quote);
                if(i < args.size())
                    ret += escape(args[i]);
                ret.push_back(quote);
            }
            break;

        case TOK_colon_ident:
            {
                std::string name(begin+2, str-2);
                auto it = std::find(params.begin(), params.end(), name);
                if(it != params.end())
                    error("Macro parameters declared twice.");
                params.emplace_back(std::move(name));
            }
            break;

        default:
            ret.insert(ret.end(), begin, str);
            break;

        case TOK_ERROR:
            ++str;
            ret.push_back(*begin);
            break;
        }
    }
done:

    return ret;
}
