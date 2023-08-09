#include <cstdint>
#include <string_view>
namespace macro_lex
{
using token_type_t = std::uint16_t;
constexpr token_type_t TOK_ERROR = 0;
constexpr token_type_t TOK_eof = 1;
constexpr token_type_t TOK_backtick = 2;
constexpr token_type_t TOK_dquote = 3;
constexpr token_type_t TOK_quote = 4;
constexpr token_type_t TOK_comment = 5;
constexpr token_type_t TOK_ml_comment = 6;
constexpr token_type_t TOK_ident = 7;
constexpr token_type_t TOK_dquote_ident = 8;
constexpr token_type_t TOK_quote_ident = 9;
constexpr token_type_t TOK_backtick_ident = 10;
constexpr token_type_t TOK_dash_ident = 11;
constexpr token_type_t TOK_eq_ident = 12;
constexpr token_type_t TOK_colon_ident = 13;
constexpr token_type_t TOK_END = 14;
inline std::string_view token_name(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_eof: return "eof"sv;
    case TOK_backtick: return "backtick"sv;
    case TOK_dquote: return "dquote"sv;
    case TOK_quote: return "quote"sv;
    case TOK_comment: return "comment"sv;
    case TOK_ml_comment: return "ml_comment"sv;
    case TOK_ident: return "ident"sv;
    case TOK_dquote_ident: return "dquote_ident"sv;
    case TOK_quote_ident: return "quote_ident"sv;
    case TOK_backtick_ident: return "backtick_ident"sv;
    case TOK_dash_ident: return "dash_ident"sv;
    case TOK_eq_ident: return "eq_ident"sv;
    case TOK_colon_ident: return "colon_ident"sv;
    }
}
inline std::string_view token_string(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_eof: return "file ending"sv;
    case TOK_backtick: return "backtick"sv;
    case TOK_dquote: return "dquote"sv;
    case TOK_quote: return "quote"sv;
    case TOK_comment: return "single-line comment"sv;
    case TOK_ml_comment: return "multi-line comment"sv;
    case TOK_ident: return "macro identifier"sv;
    case TOK_dquote_ident: return "quoted macro identifier"sv;
    case TOK_quote_ident: return "quoted macro identifier"sv;
    case TOK_backtick_ident: return "quoted macro identifier"sv;
    case TOK_dash_ident: return "quoted macro identifier"sv;
    case TOK_eq_ident: return "quoted macro identifier"sv;
    case TOK_colon_ident: return "macro identifier declaration"sv;
    }
}
#define macro_lex_TOK_KEY_CASES \
    case TOK_backtick:\
    case TOK_dquote:\
    case TOK_quote:\

constexpr token_type_t TOK_LAST_STATE = 13;
constexpr token_type_t TOK_START = 19;
extern unsigned const lexer_ec_table[256];
extern token_type_t const lexer_transition_table[976];
} // namespace macro_lex
