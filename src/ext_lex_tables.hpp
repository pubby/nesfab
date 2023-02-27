#include <cstdint>
#include <string_view>
namespace ext_lex
{
using token_type_t = std::uint16_t;
constexpr token_type_t TOK_ERROR = 0;
constexpr token_type_t TOK_bin = 1;
constexpr token_type_t TOK_chr = 2;
constexpr token_type_t TOK_nam = 3;
constexpr token_type_t TOK_png = 4;
constexpr token_type_t TOK_txt = 5;
constexpr token_type_t TOK_END = 6;
inline std::string_view token_name(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_bin: return "bin"sv;
    case TOK_chr: return "chr"sv;
    case TOK_nam: return "nam"sv;
    case TOK_png: return "png"sv;
    case TOK_txt: return "txt"sv;
    }
}
inline std::string_view token_string(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_bin: return "bin"sv;
    case TOK_chr: return "chr"sv;
    case TOK_nam: return "nam"sv;
    case TOK_png: return "png"sv;
    case TOK_txt: return "txt"sv;
    }
}
#define ext_lex_TOK_KEY_CASES \

constexpr token_type_t TOK_LAST_STATE = 5;
constexpr token_type_t TOK_START = 19;
extern unsigned const lexer_ec_table[256];
extern token_type_t const lexer_transition_table[800];
} // namespace ext_lex
