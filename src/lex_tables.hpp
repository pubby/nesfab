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
constexpr token_type_t TOK_mode = 17;
constexpr token_type_t TOK_struct = 18;
constexpr token_type_t TOK_vars = 19;
constexpr token_type_t TOK_once = 20;
constexpr token_type_t TOK_many = 21;
constexpr token_type_t TOK_lbrace = 22;
constexpr token_type_t TOK_rbrace = 23;
constexpr token_type_t TOK_lbracket = 24;
constexpr token_type_t TOK_rbracket = 25;
constexpr token_type_t TOK_semicolon = 26;
constexpr token_type_t TOK_comma = 27;
constexpr token_type_t TOK_lparen = 28;
constexpr token_type_t TOK_period = 29;
constexpr token_type_t TOK_apply = 30;
constexpr token_type_t TOK_index = 31;
constexpr token_type_t TOK_unary_minus = 32;
constexpr token_type_t TOK_unary_xor = 33;
constexpr token_type_t TOK_asterisk = 34;
constexpr token_type_t TOK_fslash = 35;
constexpr token_type_t TOK_plus = 36;
constexpr token_type_t TOK_minus = 37;
constexpr token_type_t TOK_lshift = 38;
constexpr token_type_t TOK_rshift = 39;
constexpr token_type_t TOK_bitwise_and = 40;
constexpr token_type_t TOK_bitwise_xor = 41;
constexpr token_type_t TOK_bitwise_or = 42;
constexpr token_type_t TOK_lt = 43;
constexpr token_type_t TOK_lte = 44;
constexpr token_type_t TOK_gt = 45;
constexpr token_type_t TOK_gte = 46;
constexpr token_type_t TOK_eq = 47;
constexpr token_type_t TOK_not_eq = 48;
constexpr token_type_t TOK_logical_and = 49;
constexpr token_type_t TOK_end_logical_and = 50;
constexpr token_type_t TOK_logical_or = 51;
constexpr token_type_t TOK_end_logical_or = 52;
constexpr token_type_t TOK_bitwise_not = 53;
constexpr token_type_t TOK_logical_not = 54;
constexpr token_type_t TOK_plus_plus = 55;
constexpr token_type_t TOK_minus_minus = 56;
constexpr token_type_t TOK_assign = 57;
constexpr token_type_t TOK_plus_assign = 58;
constexpr token_type_t TOK_minus_assign = 59;
constexpr token_type_t TOK_times_assign = 60;
constexpr token_type_t TOK_div_assign = 61;
constexpr token_type_t TOK_bitwise_and_assign = 62;
constexpr token_type_t TOK_logical_and_assign = 63;
constexpr token_type_t TOK_bitwise_or_assign = 64;
constexpr token_type_t TOK_logical_or_assign = 65;
constexpr token_type_t TOK_bitwise_not_assign = 66;
constexpr token_type_t TOK_bitwise_xor_assign = 67;
constexpr token_type_t TOK_lshift_assign = 68;
constexpr token_type_t TOK_rshift_assign = 69;
constexpr token_type_t TOK_rparen = 70;
constexpr token_type_t TOK_Void = 71;
constexpr token_type_t TOK_F = 72;
constexpr token_type_t TOK_FF = 73;
constexpr token_type_t TOK_FFF = 74;
constexpr token_type_t TOK_U = 75;
constexpr token_type_t TOK_UU = 76;
constexpr token_type_t TOK_UUU = 77;
constexpr token_type_t TOK_UF = 78;
constexpr token_type_t TOK_UUF = 79;
constexpr token_type_t TOK_UUUF = 80;
constexpr token_type_t TOK_UFF = 81;
constexpr token_type_t TOK_UUFF = 82;
constexpr token_type_t TOK_UUUFF = 83;
constexpr token_type_t TOK_UFFF = 84;
constexpr token_type_t TOK_UUFFF = 85;
constexpr token_type_t TOK_UUUFFF = 86;
constexpr token_type_t TOK_S = 87;
constexpr token_type_t TOK_SS = 88;
constexpr token_type_t TOK_SSS = 89;
constexpr token_type_t TOK_SF = 90;
constexpr token_type_t TOK_SSF = 91;
constexpr token_type_t TOK_SSSF = 92;
constexpr token_type_t TOK_SFF = 93;
constexpr token_type_t TOK_SSFF = 94;
constexpr token_type_t TOK_SSSFF = 95;
constexpr token_type_t TOK_SFFF = 96;
constexpr token_type_t TOK_SSFFF = 97;
constexpr token_type_t TOK_SSSFFF = 98;
constexpr token_type_t TOK_PP = 99;
constexpr token_type_t TOK_PPP = 100;
constexpr token_type_t TOK_Num = 101;
constexpr token_type_t TOK_Bool = 102;
constexpr token_type_t TOK_group_ident = 103;
constexpr token_type_t TOK_ident = 104;
constexpr token_type_t TOK_type_ident = 105;
constexpr token_type_t TOK_decimal = 106;
constexpr token_type_t TOK_number = 107;
constexpr token_type_t TOK_global_ident = 108;
constexpr token_type_t TOK_weak_ident = 109;
constexpr token_type_t TOK_END = 110;
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
    case TOK_mode: return "mode"sv;
    case TOK_struct: return "struct"sv;
    case TOK_vars: return "vars"sv;
    case TOK_once: return "once"sv;
    case TOK_many: return "many"sv;
    case TOK_lbrace: return "lbrace"sv;
    case TOK_rbrace: return "rbrace"sv;
    case TOK_lbracket: return "lbracket"sv;
    case TOK_rbracket: return "rbracket"sv;
    case TOK_semicolon: return "semicolon"sv;
    case TOK_comma: return "comma"sv;
    case TOK_lparen: return "lparen"sv;
    case TOK_period: return "period"sv;
    case TOK_apply: return "apply"sv;
    case TOK_index: return "index"sv;
    case TOK_unary_minus: return "unary_minus"sv;
    case TOK_unary_xor: return "unary_xor"sv;
    case TOK_asterisk: return "asterisk"sv;
    case TOK_fslash: return "fslash"sv;
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
    case TOK_Void: return "Void"sv;
    case TOK_F: return "F"sv;
    case TOK_FF: return "FF"sv;
    case TOK_FFF: return "FFF"sv;
    case TOK_U: return "U"sv;
    case TOK_UU: return "UU"sv;
    case TOK_UUU: return "UUU"sv;
    case TOK_UF: return "UF"sv;
    case TOK_UUF: return "UUF"sv;
    case TOK_UUUF: return "UUUF"sv;
    case TOK_UFF: return "UFF"sv;
    case TOK_UUFF: return "UUFF"sv;
    case TOK_UUUFF: return "UUUFF"sv;
    case TOK_UFFF: return "UFFF"sv;
    case TOK_UUFFF: return "UUFFF"sv;
    case TOK_UUUFFF: return "UUUFFF"sv;
    case TOK_S: return "S"sv;
    case TOK_SS: return "SS"sv;
    case TOK_SSS: return "SSS"sv;
    case TOK_SF: return "SF"sv;
    case TOK_SSF: return "SSF"sv;
    case TOK_SSSF: return "SSSF"sv;
    case TOK_SFF: return "SFF"sv;
    case TOK_SSFF: return "SSFF"sv;
    case TOK_SSSFF: return "SSSFF"sv;
    case TOK_SFFF: return "SFFF"sv;
    case TOK_SSFFF: return "SSFFF"sv;
    case TOK_SSSFFF: return "SSSFFF"sv;
    case TOK_PP: return "PP"sv;
    case TOK_PPP: return "PPP"sv;
    case TOK_Num: return "Num"sv;
    case TOK_Bool: return "Bool"sv;
    case TOK_group_ident: return "group_ident"sv;
    case TOK_ident: return "ident"sv;
    case TOK_type_ident: return "type_ident"sv;
    case TOK_decimal: return "decimal"sv;
    case TOK_number: return "number"sv;
    case TOK_global_ident: return "global_ident"sv;
    case TOK_weak_ident: return "weak_ident"sv;
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
    case TOK_mode: return "mode"sv;
    case TOK_struct: return "struct"sv;
    case TOK_vars: return "vars"sv;
    case TOK_once: return "once"sv;
    case TOK_many: return "many"sv;
    case TOK_lbrace: return "lbrace"sv;
    case TOK_rbrace: return "rbrace"sv;
    case TOK_lbracket: return "lbracket"sv;
    case TOK_rbracket: return "rbracket"sv;
    case TOK_semicolon: return "semicolon"sv;
    case TOK_comma: return "comma"sv;
    case TOK_lparen: return "lparen"sv;
    case TOK_period: return "period"sv;
    case TOK_apply: return "apply"sv;
    case TOK_index: return "index"sv;
    case TOK_unary_minus: return "unary -"sv;
    case TOK_unary_xor: return "~"sv;
    case TOK_asterisk: return "asterisk"sv;
    case TOK_fslash: return "fslash"sv;
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
    case TOK_Void: return "void type"sv;
    case TOK_F: return "F type"sv;
    case TOK_FF: return "FF type"sv;
    case TOK_FFF: return "FFF type"sv;
    case TOK_U: return "U type"sv;
    case TOK_UU: return "UU type"sv;
    case TOK_UUU: return "UUU type"sv;
    case TOK_UF: return "UF type"sv;
    case TOK_UUF: return "UUF type"sv;
    case TOK_UUUF: return "UUUF type"sv;
    case TOK_UFF: return "UFF type"sv;
    case TOK_UUFF: return "UUFF type"sv;
    case TOK_UUUFF: return "UUUFF type"sv;
    case TOK_UFFF: return "UFFF type"sv;
    case TOK_UUFFF: return "UUFFF type"sv;
    case TOK_UUUFFF: return "UUUFFF type"sv;
    case TOK_S: return "S type"sv;
    case TOK_SS: return "SS type"sv;
    case TOK_SSS: return "SSS type"sv;
    case TOK_SF: return "SF type"sv;
    case TOK_SSF: return "SSF type"sv;
    case TOK_SSSF: return "SSSF type"sv;
    case TOK_SFF: return "SFF type"sv;
    case TOK_SSFF: return "SSFF type"sv;
    case TOK_SSSFF: return "SSSFF type"sv;
    case TOK_SFFF: return "SFFF type"sv;
    case TOK_SSFFF: return "SSFFF type"sv;
    case TOK_SSSFFF: return "SSSFFF type"sv;
    case TOK_PP: return "PP type"sv;
    case TOK_PPP: return "PPP type"sv;
    case TOK_Num: return "Num type"sv;
    case TOK_Bool: return "Bool type"sv;
    case TOK_group_ident: return "group identifier"sv;
    case TOK_ident: return "identifier"sv;
    case TOK_type_ident: return "type identifier"sv;
    case TOK_decimal: return "number"sv;
    case TOK_number: return "number"sv;
    case TOK_global_ident: return "global identifier"sv;
    case TOK_weak_ident: return "weak identifier"sv;
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
    1,
    1,
    1,
    1,
    1,
    6,
    6,
    1,
    5,
    7,
    7,
    8,
    8,
    10,
    10,
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
    case TOK_mode:\
    case TOK_struct:\
    case TOK_vars:\
    case TOK_once:\
    case TOK_many:\
    case TOK_lbrace:\
    case TOK_rbrace:\
    case TOK_lbracket:\
    case TOK_rbracket:\
    case TOK_semicolon:\
    case TOK_comma:\
    case TOK_lparen:\
    case TOK_period:\
    case TOK_apply:\
    case TOK_index:\
    case TOK_unary_minus:\
    case TOK_unary_xor:\
    case TOK_asterisk:\
    case TOK_fslash:\
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

constexpr token_type_t TOK_LAST_STATE = 109;
constexpr token_type_t TOK_START = 111;
extern unsigned const lexer_ec_table[256];
extern token_type_t const lexer_transition_table[16740];
