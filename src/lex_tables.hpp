#include <cstdint>
#include <string_view>
using token_type_t = std::uint32_t;
constexpr token_type_t TOK_ERROR = 0;
constexpr token_type_t TOK_eof = 1;
constexpr token_type_t TOK_comment = 2;
constexpr token_type_t TOK_eol = 3;
constexpr token_type_t TOK_whitespace = 4;
constexpr token_type_t TOK_if = 5;
constexpr token_type_t TOK_else = 6;
constexpr token_type_t TOK_for = 7;
constexpr token_type_t TOK_while = 8;
constexpr token_type_t TOK_do = 9;
constexpr token_type_t TOK_break = 10;
constexpr token_type_t TOK_continue = 11;
constexpr token_type_t TOK_then = 12;
constexpr token_type_t TOK_return = 13;
constexpr token_type_t TOK_fn = 14;
constexpr token_type_t TOK_goto = 15;
constexpr token_type_t TOK_label = 16;
constexpr token_type_t TOK_lbrace = 17;
constexpr token_type_t TOK_rbrace = 18;
constexpr token_type_t TOK_lbracket = 19;
constexpr token_type_t TOK_rbracket = 20;
constexpr token_type_t TOK_pointer = 21;
constexpr token_type_t TOK_semicolon = 22;
constexpr token_type_t TOK_comma = 23;
constexpr token_type_t TOK_lparen = 24;
constexpr token_type_t TOK_apply = 25;
constexpr token_type_t TOK_plus = 26;
constexpr token_type_t TOK_minus = 27;
constexpr token_type_t TOK_lshift = 28;
constexpr token_type_t TOK_rshift = 29;
constexpr token_type_t TOK_bitwise_and = 30;
constexpr token_type_t TOK_bitwise_xor = 31;
constexpr token_type_t TOK_bitwise_or = 32;
constexpr token_type_t TOK_lt = 33;
constexpr token_type_t TOK_lte = 34;
constexpr token_type_t TOK_gt = 35;
constexpr token_type_t TOK_gte = 36;
constexpr token_type_t TOK_eq = 37;
constexpr token_type_t TOK_not_eq = 38;
constexpr token_type_t TOK_logical_and = 39;
constexpr token_type_t TOK_end_logical_and = 40;
constexpr token_type_t TOK_logical_or = 41;
constexpr token_type_t TOK_end_logical_or = 42;
constexpr token_type_t TOK_bitwise_not = 43;
constexpr token_type_t TOK_logical_not = 44;
constexpr token_type_t TOK_plus_plus = 45;
constexpr token_type_t TOK_minus_minus = 46;
constexpr token_type_t TOK_assign = 47;
constexpr token_type_t TOK_plus_assign = 48;
constexpr token_type_t TOK_minus_assign = 49;
constexpr token_type_t TOK_times_assign = 50;
constexpr token_type_t TOK_div_assign = 51;
constexpr token_type_t TOK_bitwise_and_assign = 52;
constexpr token_type_t TOK_logical_and_assign = 53;
constexpr token_type_t TOK_bitwise_or_assign = 54;
constexpr token_type_t TOK_logical_or_assign = 55;
constexpr token_type_t TOK_bitwise_not_assign = 56;
constexpr token_type_t TOK_bitwise_xor_assign = 57;
constexpr token_type_t TOK_lshift_assign = 58;
constexpr token_type_t TOK_rshift_assign = 59;
constexpr token_type_t TOK_rparen = 60;
constexpr token_type_t TOK_void = 61;
constexpr token_type_t TOK_bool = 62;
constexpr token_type_t TOK_byte = 63;
constexpr token_type_t TOK_short = 64;
constexpr token_type_t TOK_int = 65;
constexpr token_type_t TOK_fixed = 66;
constexpr token_type_t TOK_ident = 67;
constexpr token_type_t TOK_decimal = 68;
constexpr token_type_t TOK_number = 69;
constexpr token_type_t TOK_global_ident = 70;
constexpr token_type_t TOK_END = 71;
inline std::string_view token_name(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_eof: return "eof"sv;
    case TOK_comment: return "comment"sv;
    case TOK_eol: return "eol"sv;
    case TOK_whitespace: return "whitespace"sv;
    case TOK_if: return "if"sv;
    case TOK_else: return "else"sv;
    case TOK_for: return "for"sv;
    case TOK_while: return "while"sv;
    case TOK_do: return "do"sv;
    case TOK_break: return "break"sv;
    case TOK_continue: return "continue"sv;
    case TOK_then: return "then"sv;
    case TOK_return: return "return"sv;
    case TOK_fn: return "fn"sv;
    case TOK_goto: return "goto"sv;
    case TOK_label: return "label"sv;
    case TOK_lbrace: return "lbrace"sv;
    case TOK_rbrace: return "rbrace"sv;
    case TOK_lbracket: return "lbracket"sv;
    case TOK_rbracket: return "rbracket"sv;
    case TOK_pointer: return "pointer"sv;
    case TOK_semicolon: return "semicolon"sv;
    case TOK_comma: return "comma"sv;
    case TOK_lparen: return "lparen"sv;
    case TOK_apply: return "apply"sv;
    case TOK_plus: return "plus"sv;
    case TOK_minus: return "minus"sv;
    case TOK_lshift: return "lshift"sv;
    case TOK_rshift: return "rshift"sv;
    case TOK_bitwise_and: return "bitwise_and"sv;
    case TOK_bitwise_xor: return "bitwise_xor"sv;
    case TOK_bitwise_or: return "bitwise_or"sv;
    case TOK_lt: return "lt"sv;
    case TOK_lte: return "lte"sv;
    case TOK_gt: return "gt"sv;
    case TOK_gte: return "gte"sv;
    case TOK_eq: return "eq"sv;
    case TOK_not_eq: return "not_eq"sv;
    case TOK_logical_and: return "logical_and"sv;
    case TOK_end_logical_and: return "end_logical_and"sv;
    case TOK_logical_or: return "logical_or"sv;
    case TOK_end_logical_or: return "end_logical_or"sv;
    case TOK_bitwise_not: return "bitwise_not"sv;
    case TOK_logical_not: return "logical_not"sv;
    case TOK_plus_plus: return "plus_plus"sv;
    case TOK_minus_minus: return "minus_minus"sv;
    case TOK_assign: return "assign"sv;
    case TOK_plus_assign: return "plus_assign"sv;
    case TOK_minus_assign: return "minus_assign"sv;
    case TOK_times_assign: return "times_assign"sv;
    case TOK_div_assign: return "div_assign"sv;
    case TOK_bitwise_and_assign: return "bitwise_and_assign"sv;
    case TOK_logical_and_assign: return "logical_and_assign"sv;
    case TOK_bitwise_or_assign: return "bitwise_or_assign"sv;
    case TOK_logical_or_assign: return "logical_or_assign"sv;
    case TOK_bitwise_not_assign: return "bitwise_not_assign"sv;
    case TOK_bitwise_xor_assign: return "bitwise_xor_assign"sv;
    case TOK_lshift_assign: return "lshift_assign"sv;
    case TOK_rshift_assign: return "rshift_assign"sv;
    case TOK_rparen: return "rparen"sv;
    case TOK_void: return "void"sv;
    case TOK_bool: return "bool"sv;
    case TOK_byte: return "byte"sv;
    case TOK_short: return "short"sv;
    case TOK_int: return "int"sv;
    case TOK_fixed: return "fixed"sv;
    case TOK_ident: return "ident"sv;
    case TOK_decimal: return "decimal"sv;
    case TOK_number: return "number"sv;
    case TOK_global_ident: return "global_ident"sv;
    }
}
inline std::string_view token_string(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_eof: return "file ending"sv;
    case TOK_comment: return "comment"sv;
    case TOK_eol: return "line ending"sv;
    case TOK_whitespace: return "space"sv;
    case TOK_if: return "if"sv;
    case TOK_else: return "else"sv;
    case TOK_for: return "for"sv;
    case TOK_while: return "while"sv;
    case TOK_do: return "do"sv;
    case TOK_break: return "break"sv;
    case TOK_continue: return "continue"sv;
    case TOK_then: return "then"sv;
    case TOK_return: return "return"sv;
    case TOK_fn: return "fn"sv;
    case TOK_goto: return "goto"sv;
    case TOK_label: return "label"sv;
    case TOK_lbrace: return "lbrace"sv;
    case TOK_rbrace: return "rbrace"sv;
    case TOK_lbracket: return "lbracket"sv;
    case TOK_rbracket: return "rbracket"sv;
    case TOK_pointer: return "pointer"sv;
    case TOK_semicolon: return "semicolon"sv;
    case TOK_comma: return "comma"sv;
    case TOK_lparen: return "lparen"sv;
    case TOK_apply: return "apply"sv;
    case TOK_plus: return "plus"sv;
    case TOK_minus: return "minus"sv;
    case TOK_lshift: return "lshift"sv;
    case TOK_rshift: return "rshift"sv;
    case TOK_bitwise_and: return "bitwise_and"sv;
    case TOK_bitwise_xor: return "bitwise_xor"sv;
    case TOK_bitwise_or: return "bitwise_or"sv;
    case TOK_lt: return "lt"sv;
    case TOK_lte: return "lte"sv;
    case TOK_gt: return "gt"sv;
    case TOK_gte: return "gte"sv;
    case TOK_eq: return "eq"sv;
    case TOK_not_eq: return "not_eq"sv;
    case TOK_logical_and: return "logical_and"sv;
    case TOK_end_logical_and: return "end_logical_and"sv;
    case TOK_logical_or: return "logical_or"sv;
    case TOK_end_logical_or: return "end_logical_or"sv;
    case TOK_bitwise_not: return "bitwise_not"sv;
    case TOK_logical_not: return "logical_not"sv;
    case TOK_plus_plus: return "plus_plus"sv;
    case TOK_minus_minus: return "minus_minus"sv;
    case TOK_assign: return "assign"sv;
    case TOK_plus_assign: return "plus_assign"sv;
    case TOK_minus_assign: return "minus_assign"sv;
    case TOK_times_assign: return "times_assign"sv;
    case TOK_div_assign: return "div_assign"sv;
    case TOK_bitwise_and_assign: return "bitwise_and_assign"sv;
    case TOK_logical_and_assign: return "logical_and_assign"sv;
    case TOK_bitwise_or_assign: return "bitwise_or_assign"sv;
    case TOK_logical_or_assign: return "logical_or_assign"sv;
    case TOK_bitwise_not_assign: return "bitwise_not_assign"sv;
    case TOK_bitwise_xor_assign: return "bitwise_xor_assign"sv;
    case TOK_lshift_assign: return "lshift_assign"sv;
    case TOK_rshift_assign: return "rshift_assign"sv;
    case TOK_rparen: return "rparen"sv;
    case TOK_void: return "void type"sv;
    case TOK_bool: return "bool type"sv;
    case TOK_byte: return "byte type"sv;
    case TOK_short: return "short type"sv;
    case TOK_int: return "int type"sv;
    case TOK_fixed: return "fixed type"sv;
    case TOK_ident: return "identifier"sv;
    case TOK_decimal: return "number"sv;
    case TOK_number: return "number"sv;
    case TOK_global_ident: return "global identifier"sv;
    }
}
constexpr unsigned char token_precedence_table[] =
{
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    4,
    6,
    6,
    1,
    7,
    11,
    11,
    12,
    12,
    13,
    14,
    15,
    16,
    16,
    16,
    16,
    17,
    17,
    18,
    18,
    19,
    19,
    1,
    1,
    1,
    1,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    30,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
};
#define TOK_KEY_CASES \
    case TOK_if:\
    case TOK_else:\
    case TOK_for:\
    case TOK_while:\
    case TOK_do:\
    case TOK_break:\
    case TOK_continue:\
    case TOK_then:\
    case TOK_return:\
    case TOK_fn:\
    case TOK_goto:\
    case TOK_label:\
    case TOK_lbrace:\
    case TOK_rbrace:\
    case TOK_lbracket:\
    case TOK_rbracket:\
    case TOK_pointer:\
    case TOK_semicolon:\
    case TOK_comma:\
    case TOK_lparen:\
    case TOK_apply:\
    case TOK_plus:\
    case TOK_minus:\
    case TOK_lshift:\
    case TOK_rshift:\
    case TOK_bitwise_and:\
    case TOK_bitwise_xor:\
    case TOK_bitwise_or:\
    case TOK_lt:\
    case TOK_lte:\
    case TOK_gt:\
    case TOK_gte:\
    case TOK_eq:\
    case TOK_not_eq:\
    case TOK_logical_and:\
    case TOK_end_logical_and:\
    case TOK_logical_or:\
    case TOK_end_logical_or:\
    case TOK_bitwise_not:\
    case TOK_logical_not:\
    case TOK_plus_plus:\
    case TOK_minus_minus:\
    case TOK_assign:\
    case TOK_plus_assign:\
    case TOK_minus_assign:\
    case TOK_times_assign:\
    case TOK_div_assign:\
    case TOK_bitwise_and_assign:\
    case TOK_logical_and_assign:\
    case TOK_bitwise_or_assign:\
    case TOK_logical_or_assign:\
    case TOK_bitwise_not_assign:\
    case TOK_bitwise_xor_assign:\
    case TOK_lshift_assign:\
    case TOK_rshift_assign:\
    case TOK_rparen:\

constexpr token_type_t TOK_LAST_STATE = 70;
constexpr token_type_t TOK_START = 71;
extern unsigned const lexer_ec_table[256];
extern token_type_t const lexer_transition_table[9555];
