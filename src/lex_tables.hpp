#include <cstdint>
#include <string_view>
namespace lex
{
using token_type_t = std::uint16_t;
constexpr token_type_t TOK_ERROR = 0;
constexpr token_type_t TOK_eof = 1;
constexpr token_type_t TOK_comment = 2;
constexpr token_type_t TOK_ml_comment_begin = 3;
constexpr token_type_t TOK_eol = 4;
constexpr token_type_t TOK_whitespace = 5;
constexpr token_type_t TOK_if = 6;
constexpr token_type_t TOK_else = 7;
constexpr token_type_t TOK_for = 8;
constexpr token_type_t TOK_while = 9;
constexpr token_type_t TOK_do = 10;
constexpr token_type_t TOK_break = 11;
constexpr token_type_t TOK_continue = 12;
constexpr token_type_t TOK_return = 13;
constexpr token_type_t TOK_fn = 14;
constexpr token_type_t TOK_ct = 15;
constexpr token_type_t TOK_mode = 16;
constexpr token_type_t TOK_nmi = 17;
constexpr token_type_t TOK_irq = 18;
constexpr token_type_t TOK_goto = 19;
constexpr token_type_t TOK_label = 20;
constexpr token_type_t TOK_file = 21;
constexpr token_type_t TOK_struct = 22;
constexpr token_type_t TOK_vars = 23;
constexpr token_type_t TOK_data = 24;
constexpr token_type_t TOK_omni = 25;
constexpr token_type_t TOK_asm = 26;
constexpr token_type_t TOK_ready = 27;
constexpr token_type_t TOK_fence = 28;
constexpr token_type_t TOK_switch = 29;
constexpr token_type_t TOK_case = 30;
constexpr token_type_t TOK_default = 31;
constexpr token_type_t TOK_charmap = 32;
constexpr token_type_t TOK_chrrom = 33;
constexpr token_type_t TOK_employs = 34;
constexpr token_type_t TOK_preserves = 35;
constexpr token_type_t TOK_stows = 36;
constexpr token_type_t TOK_audio = 37;
constexpr token_type_t TOK_system = 38;
constexpr token_type_t TOK_state = 39;
constexpr token_type_t TOK_abs = 40;
constexpr token_type_t TOK_min = 41;
constexpr token_type_t TOK_max = 42;
constexpr token_type_t TOK_swap = 43;
constexpr token_type_t TOK_macro = 44;
constexpr token_type_t TOK___mapper_detail = 45;
constexpr token_type_t TOK___mapper_reset = 46;
constexpr token_type_t TOK___mapper = 47;
constexpr token_type_t TOK___illegal = 48;
constexpr token_type_t TOK___controllers = 49;
constexpr token_type_t TOK___expansion_audio = 50;
constexpr token_type_t TOK___sector_size = 51;
constexpr token_type_t TOK_nmi_counter = 52;
constexpr token_type_t TOK_read = 53;
constexpr token_type_t TOK_write = 54;
constexpr token_type_t TOK_mapfab = 55;
constexpr token_type_t TOK_push = 56;
constexpr token_type_t TOK_pop = 57;
constexpr token_type_t TOK_true = 58;
constexpr token_type_t TOK_false = 59;
constexpr token_type_t TOK_PPUCTRL = 60;
constexpr token_type_t TOK_PPUMASK = 61;
constexpr token_type_t TOK_PPUSTATUS = 62;
constexpr token_type_t TOK_PPUSCROLL = 63;
constexpr token_type_t TOK_PPUADDR = 64;
constexpr token_type_t TOK_PPUDATA = 65;
constexpr token_type_t TOK_OAMADDR = 66;
constexpr token_type_t TOK_OAMDATA = 67;
constexpr token_type_t TOK_OAMDMA = 68;
constexpr token_type_t TOK_SYSTEM_NTSC = 69;
constexpr token_type_t TOK_SYSTEM_PAL = 70;
constexpr token_type_t TOK_SYSTEM_DENDY = 71;
constexpr token_type_t TOK_SYSTEM_UNKNOWN = 72;
constexpr token_type_t TOK_colon = 73;
constexpr token_type_t TOK_hash = 74;
constexpr token_type_t TOK_backtick = 75;
constexpr token_type_t TOK_dquote = 76;
constexpr token_type_t TOK_quote = 77;
constexpr token_type_t TOK_semicolon = 78;
constexpr token_type_t TOK_comma = 79;
constexpr token_type_t TOK_sizeof = 80;
constexpr token_type_t TOK_sizeof_expr = 81;
constexpr token_type_t TOK_len = 82;
constexpr token_type_t TOK_len_expr = 83;
constexpr token_type_t TOK_unary_plus = 84;
constexpr token_type_t TOK_unary_minus = 85;
constexpr token_type_t TOK_unary_xor = 86;
constexpr token_type_t TOK_unary_negate = 87;
constexpr token_type_t TOK_unary_ref = 88;
constexpr token_type_t TOK_at = 89;
constexpr token_type_t TOK_period = 90;
constexpr token_type_t TOK_apply = 91;
constexpr token_type_t TOK_mode_apply = 92;
constexpr token_type_t TOK_cast = 93;
constexpr token_type_t TOK_cast_type = 94;
constexpr token_type_t TOK_index8 = 95;
constexpr token_type_t TOK_index16 = 96;
constexpr token_type_t TOK_lbrace = 97;
constexpr token_type_t TOK_rbrace = 98;
constexpr token_type_t TOK_lbracket = 99;
constexpr token_type_t TOK_rbracket = 100;
constexpr token_type_t TOK_lparen = 101;
constexpr token_type_t TOK_increment = 102;
constexpr token_type_t TOK_decrement = 103;
constexpr token_type_t TOK_asterisk = 104;
constexpr token_type_t TOK_fslash = 105;
constexpr token_type_t TOK_plus = 106;
constexpr token_type_t TOK_minus = 107;
constexpr token_type_t TOK_rol = 108;
constexpr token_type_t TOK_ror = 109;
constexpr token_type_t TOK_ror_flip = 110;
constexpr token_type_t TOK_lshift = 111;
constexpr token_type_t TOK_rshift = 112;
constexpr token_type_t TOK_bitwise_and = 113;
constexpr token_type_t TOK_bitwise_xor = 114;
constexpr token_type_t TOK_bitwise_or = 115;
constexpr token_type_t TOK_lt = 116;
constexpr token_type_t TOK_lte = 117;
constexpr token_type_t TOK_gt = 118;
constexpr token_type_t TOK_gte = 119;
constexpr token_type_t TOK_eq = 120;
constexpr token_type_t TOK_not_eq = 121;
constexpr token_type_t TOK_logical_and = 122;
constexpr token_type_t TOK_end_logical_and = 123;
constexpr token_type_t TOK_logical_or = 124;
constexpr token_type_t TOK_end_logical_or = 125;
constexpr token_type_t TOK_rol_assign = 126;
constexpr token_type_t TOK_ror_assign = 127;
constexpr token_type_t TOK_ror_assign_flip = 128;
constexpr token_type_t TOK_assign = 129;
constexpr token_type_t TOK_plus_assign = 130;
constexpr token_type_t TOK_minus_assign = 131;
constexpr token_type_t TOK_times_assign = 132;
constexpr token_type_t TOK_div_assign = 133;
constexpr token_type_t TOK_bitwise_and_assign = 134;
constexpr token_type_t TOK_bitwise_or_assign = 135;
constexpr token_type_t TOK_bitwise_xor_assign = 136;
constexpr token_type_t TOK_lshift_assign = 137;
constexpr token_type_t TOK_rshift_assign = 138;
constexpr token_type_t TOK_rparen = 139;
constexpr token_type_t TOK_Void = 140;
constexpr token_type_t TOK_Fn = 141;
constexpr token_type_t TOK_F = 142;
constexpr token_type_t TOK_FF = 143;
constexpr token_type_t TOK_FFF = 144;
constexpr token_type_t TOK_U = 145;
constexpr token_type_t TOK_UU = 146;
constexpr token_type_t TOK_UUU = 147;
constexpr token_type_t TOK_UF = 148;
constexpr token_type_t TOK_UUF = 149;
constexpr token_type_t TOK_UUUF = 150;
constexpr token_type_t TOK_UFF = 151;
constexpr token_type_t TOK_UUFF = 152;
constexpr token_type_t TOK_UUUFF = 153;
constexpr token_type_t TOK_UFFF = 154;
constexpr token_type_t TOK_UUFFF = 155;
constexpr token_type_t TOK_UUUFFF = 156;
constexpr token_type_t TOK_S = 157;
constexpr token_type_t TOK_SS = 158;
constexpr token_type_t TOK_SSS = 159;
constexpr token_type_t TOK_SF = 160;
constexpr token_type_t TOK_SSF = 161;
constexpr token_type_t TOK_SSSF = 162;
constexpr token_type_t TOK_SFF = 163;
constexpr token_type_t TOK_SSFF = 164;
constexpr token_type_t TOK_SSSFF = 165;
constexpr token_type_t TOK_SFFF = 166;
constexpr token_type_t TOK_SSFFF = 167;
constexpr token_type_t TOK_SSSFFF = 168;
constexpr token_type_t TOK_AA = 169;
constexpr token_type_t TOK_AAA = 170;
constexpr token_type_t TOK_PP = 171;
constexpr token_type_t TOK_PPP = 172;
constexpr token_type_t TOK_CC = 173;
constexpr token_type_t TOK_CCC = 174;
constexpr token_type_t TOK_MM = 175;
constexpr token_type_t TOK_MMM = 176;
constexpr token_type_t TOK_Int = 177;
constexpr token_type_t TOK_Real = 178;
constexpr token_type_t TOK_Bool = 179;
constexpr token_type_t TOK_ident = 180;
constexpr token_type_t TOK_type_ident = 181;
constexpr token_type_t TOK_decimal = 182;
constexpr token_type_t TOK_hex = 183;
constexpr token_type_t TOK_binary = 184;
constexpr token_type_t TOK_int = 185;
constexpr token_type_t TOK_real = 186;
constexpr token_type_t TOK_global_ident = 187;
constexpr token_type_t TOK_weak_ident = 188;
constexpr token_type_t TOK_hw_addr = 189;
constexpr token_type_t TOK_read_hw = 190;
constexpr token_type_t TOK_write_hw = 191;
constexpr token_type_t TOK_group_set = 192;
constexpr token_type_t TOK_rpair = 193;
constexpr token_type_t TOK_ssa = 194;
constexpr token_type_t TOK_implicit_cast = 195;
constexpr token_type_t TOK_shift_atom = 196;
constexpr token_type_t TOK_replace_atom = 197;
constexpr token_type_t TOK_write_state = 198;
constexpr token_type_t TOK_byte_vec = 199;
constexpr token_type_t TOK_locator_vec = 200;
constexpr token_type_t TOK_character = 201;
constexpr token_type_t TOK_string_uncompressed = 202;
constexpr token_type_t TOK_string_compressed = 203;
constexpr token_type_t TOK_byte_block_proc = 204;
constexpr token_type_t TOK_byte_block_data = 205;
constexpr token_type_t TOK_byte_block_asm_op = 206;
constexpr token_type_t TOK_byte_block_label = 207;
constexpr token_type_t TOK_byte_block_call = 208;
constexpr token_type_t TOK_byte_block_goto = 209;
constexpr token_type_t TOK_byte_block_goto_mode = 210;
constexpr token_type_t TOK_byte_block_wait_nmi = 211;
constexpr token_type_t TOK_byte_block_bank_switch_a = 212;
constexpr token_type_t TOK_byte_block_bank_switch_x = 213;
constexpr token_type_t TOK_byte_block_bank_switch_y = 214;
constexpr token_type_t TOK_byte_block_bank_switch_ax = 215;
constexpr token_type_t TOK_byte_block_byte_array = 216;
constexpr token_type_t TOK_byte_block_locator_array = 217;
constexpr token_type_t TOK_byte_block_sub_proc = 218;
constexpr token_type_t TOK_byte_block_if = 219;
constexpr token_type_t TOK_END = 220;
inline std::string_view token_name(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_eof: return "eof"sv;
    case TOK_comment: return "comment"sv;
    case TOK_ml_comment_begin: return "ml_comment_begin"sv;
    case TOK_eol: return "eol"sv;
    case TOK_whitespace: return "whitespace"sv;
    case TOK_if: return "if"sv;
    case TOK_else: return "else"sv;
    case TOK_for: return "for"sv;
    case TOK_while: return "while"sv;
    case TOK_do: return "do"sv;
    case TOK_break: return "break"sv;
    case TOK_continue: return "continue"sv;
    case TOK_return: return "return"sv;
    case TOK_fn: return "fn"sv;
    case TOK_ct: return "ct"sv;
    case TOK_mode: return "mode"sv;
    case TOK_nmi: return "nmi"sv;
    case TOK_irq: return "irq"sv;
    case TOK_goto: return "goto"sv;
    case TOK_label: return "label"sv;
    case TOK_file: return "file"sv;
    case TOK_struct: return "struct"sv;
    case TOK_vars: return "vars"sv;
    case TOK_data: return "data"sv;
    case TOK_omni: return "omni"sv;
    case TOK_asm: return "asm"sv;
    case TOK_ready: return "ready"sv;
    case TOK_fence: return "fence"sv;
    case TOK_switch: return "switch"sv;
    case TOK_case: return "case"sv;
    case TOK_default: return "default"sv;
    case TOK_charmap: return "charmap"sv;
    case TOK_chrrom: return "chrrom"sv;
    case TOK_employs: return "employs"sv;
    case TOK_preserves: return "preserves"sv;
    case TOK_stows: return "stows"sv;
    case TOK_audio: return "audio"sv;
    case TOK_system: return "system"sv;
    case TOK_state: return "state"sv;
    case TOK_abs: return "abs"sv;
    case TOK_min: return "min"sv;
    case TOK_max: return "max"sv;
    case TOK_swap: return "swap"sv;
    case TOK_macro: return "macro"sv;
    case TOK___mapper_detail: return "__mapper_detail"sv;
    case TOK___mapper_reset: return "__mapper_reset"sv;
    case TOK___mapper: return "__mapper"sv;
    case TOK___illegal: return "__illegal"sv;
    case TOK___controllers: return "__controllers"sv;
    case TOK___expansion_audio: return "__expansion_audio"sv;
    case TOK___sector_size: return "__sector_size"sv;
    case TOK_nmi_counter: return "nmi_counter"sv;
    case TOK_read: return "read"sv;
    case TOK_write: return "write"sv;
    case TOK_mapfab: return "mapfab"sv;
    case TOK_push: return "push"sv;
    case TOK_pop: return "pop"sv;
    case TOK_true: return "true"sv;
    case TOK_false: return "false"sv;
    case TOK_PPUCTRL: return "PPUCTRL"sv;
    case TOK_PPUMASK: return "PPUMASK"sv;
    case TOK_PPUSTATUS: return "PPUSTATUS"sv;
    case TOK_PPUSCROLL: return "PPUSCROLL"sv;
    case TOK_PPUADDR: return "PPUADDR"sv;
    case TOK_PPUDATA: return "PPUDATA"sv;
    case TOK_OAMADDR: return "OAMADDR"sv;
    case TOK_OAMDATA: return "OAMDATA"sv;
    case TOK_OAMDMA: return "OAMDMA"sv;
    case TOK_SYSTEM_NTSC: return "SYSTEM_NTSC"sv;
    case TOK_SYSTEM_PAL: return "SYSTEM_PAL"sv;
    case TOK_SYSTEM_DENDY: return "SYSTEM_DENDY"sv;
    case TOK_SYSTEM_UNKNOWN: return "SYSTEM_UNKNOWN"sv;
    case TOK_colon: return "colon"sv;
    case TOK_hash: return "hash"sv;
    case TOK_backtick: return "backtick"sv;
    case TOK_dquote: return "dquote"sv;
    case TOK_quote: return "quote"sv;
    case TOK_semicolon: return "semicolon"sv;
    case TOK_comma: return "comma"sv;
    case TOK_sizeof: return "sizeof"sv;
    case TOK_sizeof_expr: return "sizeof_expr"sv;
    case TOK_len: return "len"sv;
    case TOK_len_expr: return "len_expr"sv;
    case TOK_unary_plus: return "unary_plus"sv;
    case TOK_unary_minus: return "unary_minus"sv;
    case TOK_unary_xor: return "unary_xor"sv;
    case TOK_unary_negate: return "unary_negate"sv;
    case TOK_unary_ref: return "unary_ref"sv;
    case TOK_at: return "at"sv;
    case TOK_period: return "period"sv;
    case TOK_apply: return "apply"sv;
    case TOK_mode_apply: return "mode_apply"sv;
    case TOK_cast: return "cast"sv;
    case TOK_cast_type: return "cast_type"sv;
    case TOK_index8: return "index8"sv;
    case TOK_index16: return "index16"sv;
    case TOK_lbrace: return "lbrace"sv;
    case TOK_rbrace: return "rbrace"sv;
    case TOK_lbracket: return "lbracket"sv;
    case TOK_rbracket: return "rbracket"sv;
    case TOK_lparen: return "lparen"sv;
    case TOK_increment: return "increment"sv;
    case TOK_decrement: return "decrement"sv;
    case TOK_asterisk: return "asterisk"sv;
    case TOK_fslash: return "fslash"sv;
    case TOK_plus: return "plus"sv;
    case TOK_minus: return "minus"sv;
    case TOK_rol: return "rol"sv;
    case TOK_ror: return "ror"sv;
    case TOK_ror_flip: return "ror_flip"sv;
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
    case TOK_rol_assign: return "rol_assign"sv;
    case TOK_ror_assign: return "ror_assign"sv;
    case TOK_ror_assign_flip: return "ror_assign_flip"sv;
    case TOK_assign: return "assign"sv;
    case TOK_plus_assign: return "plus_assign"sv;
    case TOK_minus_assign: return "minus_assign"sv;
    case TOK_times_assign: return "times_assign"sv;
    case TOK_div_assign: return "div_assign"sv;
    case TOK_bitwise_and_assign: return "bitwise_and_assign"sv;
    case TOK_bitwise_or_assign: return "bitwise_or_assign"sv;
    case TOK_bitwise_xor_assign: return "bitwise_xor_assign"sv;
    case TOK_lshift_assign: return "lshift_assign"sv;
    case TOK_rshift_assign: return "rshift_assign"sv;
    case TOK_rparen: return "rparen"sv;
    case TOK_Void: return "Void"sv;
    case TOK_Fn: return "Fn"sv;
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
    case TOK_AA: return "AA"sv;
    case TOK_AAA: return "AAA"sv;
    case TOK_PP: return "PP"sv;
    case TOK_PPP: return "PPP"sv;
    case TOK_CC: return "CC"sv;
    case TOK_CCC: return "CCC"sv;
    case TOK_MM: return "MM"sv;
    case TOK_MMM: return "MMM"sv;
    case TOK_Int: return "Int"sv;
    case TOK_Real: return "Real"sv;
    case TOK_Bool: return "Bool"sv;
    case TOK_ident: return "ident"sv;
    case TOK_type_ident: return "type_ident"sv;
    case TOK_decimal: return "decimal"sv;
    case TOK_hex: return "hex"sv;
    case TOK_binary: return "binary"sv;
    case TOK_int: return "int"sv;
    case TOK_real: return "real"sv;
    case TOK_global_ident: return "global_ident"sv;
    case TOK_weak_ident: return "weak_ident"sv;
    case TOK_hw_addr: return "hw_addr"sv;
    case TOK_read_hw: return "read_hw"sv;
    case TOK_write_hw: return "write_hw"sv;
    case TOK_group_set: return "group_set"sv;
    case TOK_rpair: return "rpair"sv;
    case TOK_ssa: return "ssa"sv;
    case TOK_implicit_cast: return "implicit_cast"sv;
    case TOK_shift_atom: return "shift_atom"sv;
    case TOK_replace_atom: return "replace_atom"sv;
    case TOK_write_state: return "write_state"sv;
    case TOK_byte_vec: return "byte_vec"sv;
    case TOK_locator_vec: return "locator_vec"sv;
    case TOK_character: return "character"sv;
    case TOK_string_uncompressed: return "string_uncompressed"sv;
    case TOK_string_compressed: return "string_compressed"sv;
    case TOK_byte_block_proc: return "byte_block_proc"sv;
    case TOK_byte_block_data: return "byte_block_data"sv;
    case TOK_byte_block_asm_op: return "byte_block_asm_op"sv;
    case TOK_byte_block_label: return "byte_block_label"sv;
    case TOK_byte_block_call: return "byte_block_call"sv;
    case TOK_byte_block_goto: return "byte_block_goto"sv;
    case TOK_byte_block_goto_mode: return "byte_block_goto_mode"sv;
    case TOK_byte_block_wait_nmi: return "byte_block_wait_nmi"sv;
    case TOK_byte_block_bank_switch_a: return "byte_block_bank_switch_a"sv;
    case TOK_byte_block_bank_switch_x: return "byte_block_bank_switch_x"sv;
    case TOK_byte_block_bank_switch_y: return "byte_block_bank_switch_y"sv;
    case TOK_byte_block_bank_switch_ax: return "byte_block_bank_switch_ax"sv;
    case TOK_byte_block_byte_array: return "byte_block_byte_array"sv;
    case TOK_byte_block_locator_array: return "byte_block_locator_array"sv;
    case TOK_byte_block_sub_proc: return "byte_block_sub_proc"sv;
    case TOK_byte_block_if: return "byte_block_if"sv;
    }
}
inline std::string_view token_string(token_type_t type)
{
    using namespace std::literals;
    switch(type)
    {
    default: return "?BAD?"sv;
    case TOK_eof: return "file ending"sv;
    case TOK_comment: return "single-line comment"sv;
    case TOK_ml_comment_begin: return "multi-line comment"sv;
    case TOK_eol: return "line ending"sv;
    case TOK_whitespace: return "space"sv;
    case TOK_if: return "if"sv;
    case TOK_else: return "else"sv;
    case TOK_for: return "for"sv;
    case TOK_while: return "while"sv;
    case TOK_do: return "do"sv;
    case TOK_break: return "break"sv;
    case TOK_continue: return "continue"sv;
    case TOK_return: return "return"sv;
    case TOK_fn: return "fn"sv;
    case TOK_ct: return "ct"sv;
    case TOK_mode: return "mode"sv;
    case TOK_nmi: return "nmi"sv;
    case TOK_irq: return "irq"sv;
    case TOK_goto: return "goto"sv;
    case TOK_label: return "label"sv;
    case TOK_file: return "file"sv;
    case TOK_struct: return "struct"sv;
    case TOK_vars: return "vars"sv;
    case TOK_data: return "data"sv;
    case TOK_omni: return "omni"sv;
    case TOK_asm: return "asm"sv;
    case TOK_ready: return "ready"sv;
    case TOK_fence: return "fence"sv;
    case TOK_switch: return "switch"sv;
    case TOK_case: return "case"sv;
    case TOK_default: return "default"sv;
    case TOK_charmap: return "charmap"sv;
    case TOK_chrrom: return "chrrom"sv;
    case TOK_employs: return "employs"sv;
    case TOK_preserves: return "preserves"sv;
    case TOK_stows: return "stows"sv;
    case TOK_audio: return "audio"sv;
    case TOK_system: return "system"sv;
    case TOK_state: return "state"sv;
    case TOK_abs: return "abs"sv;
    case TOK_min: return "min"sv;
    case TOK_max: return "max"sv;
    case TOK_swap: return "swap"sv;
    case TOK_macro: return "macro"sv;
    case TOK___mapper_detail: return "__mapper_detail"sv;
    case TOK___mapper_reset: return "__mapper_reset"sv;
    case TOK___mapper: return "__mapper"sv;
    case TOK___illegal: return "__illegal"sv;
    case TOK___controllers: return "__controllers"sv;
    case TOK___expansion_audio: return "__expansion_audio"sv;
    case TOK___sector_size: return "__sector_size"sv;
    case TOK_nmi_counter: return "nmi_counter"sv;
    case TOK_read: return "read"sv;
    case TOK_write: return "write"sv;
    case TOK_mapfab: return "mapfab"sv;
    case TOK_push: return "push"sv;
    case TOK_pop: return "pop"sv;
    case TOK_true: return "true"sv;
    case TOK_false: return "false"sv;
    case TOK_PPUCTRL: return "PPUCTRL"sv;
    case TOK_PPUMASK: return "PPUMASK"sv;
    case TOK_PPUSTATUS: return "PPUSTATUS"sv;
    case TOK_PPUSCROLL: return "PPUSCROLL"sv;
    case TOK_PPUADDR: return "PPUADDR"sv;
    case TOK_PPUDATA: return "PPUDATA"sv;
    case TOK_OAMADDR: return "OAMADDR"sv;
    case TOK_OAMDATA: return "OAMDATA"sv;
    case TOK_OAMDMA: return "OAMDMA"sv;
    case TOK_SYSTEM_NTSC: return "SYSTEM_NTSC"sv;
    case TOK_SYSTEM_PAL: return "SYSTEM_PAL"sv;
    case TOK_SYSTEM_DENDY: return "SYSTEM_DENDY"sv;
    case TOK_SYSTEM_UNKNOWN: return "SYSTEM_UNKNOWN"sv;
    case TOK_colon: return "colon"sv;
    case TOK_hash: return "hash"sv;
    case TOK_backtick: return "backtick"sv;
    case TOK_dquote: return "dquote"sv;
    case TOK_quote: return "quote"sv;
    case TOK_semicolon: return "semicolon"sv;
    case TOK_comma: return "comma"sv;
    case TOK_sizeof: return "sizeof"sv;
    case TOK_sizeof_expr: return "sizeof"sv;
    case TOK_len: return "len"sv;
    case TOK_len_expr: return "len"sv;
    case TOK_unary_plus: return "unary +"sv;
    case TOK_unary_minus: return "unary -"sv;
    case TOK_unary_xor: return "~"sv;
    case TOK_unary_negate: return "!"sv;
    case TOK_unary_ref: return "unary &"sv;
    case TOK_at: return "at"sv;
    case TOK_period: return "period"sv;
    case TOK_apply: return "apply"sv;
    case TOK_mode_apply: return "mode_apply"sv;
    case TOK_cast: return "cast"sv;
    case TOK_cast_type: return "cast_type"sv;
    case TOK_index8: return "index []"sv;
    case TOK_index16: return "index {}"sv;
    case TOK_lbrace: return "lbrace"sv;
    case TOK_rbrace: return "rbrace"sv;
    case TOK_lbracket: return "lbracket"sv;
    case TOK_rbracket: return "rbracket"sv;
    case TOK_lparen: return "lparen"sv;
    case TOK_increment: return "++"sv;
    case TOK_decrement: return "--"sv;
    case TOK_asterisk: return "*"sv;
    case TOK_fslash: return "/"sv;
    case TOK_plus: return "+"sv;
    case TOK_minus: return "-"sv;
    case TOK_rol: return "<-<"sv;
    case TOK_ror: return ">->"sv;
    case TOK_ror_flip: return "ror_flip"sv;
    case TOK_lshift: return "<<"sv;
    case TOK_rshift: return ">>"sv;
    case TOK_bitwise_and: return "&"sv;
    case TOK_bitwise_xor: return "^"sv;
    case TOK_bitwise_or: return "|"sv;
    case TOK_lt: return "<"sv;
    case TOK_lte: return "<="sv;
    case TOK_gt: return ">"sv;
    case TOK_gte: return ">="sv;
    case TOK_eq: return "=="sv;
    case TOK_not_eq: return "!="sv;
    case TOK_logical_and: return "&&"sv;
    case TOK_end_logical_and: return "end_logical_and"sv;
    case TOK_logical_or: return "||"sv;
    case TOK_end_logical_or: return "end_logical_or"sv;
    case TOK_rol_assign: return "<=<"sv;
    case TOK_ror_assign: return ">=>"sv;
    case TOK_ror_assign_flip: return "ror_assign_flip"sv;
    case TOK_assign: return "="sv;
    case TOK_plus_assign: return "+="sv;
    case TOK_minus_assign: return "-="sv;
    case TOK_times_assign: return "*="sv;
    case TOK_div_assign: return "/="sv;
    case TOK_bitwise_and_assign: return "&="sv;
    case TOK_bitwise_or_assign: return "|="sv;
    case TOK_bitwise_xor_assign: return "^="sv;
    case TOK_lshift_assign: return "<<="sv;
    case TOK_rshift_assign: return ">>="sv;
    case TOK_rparen: return "rparen"sv;
    case TOK_Void: return "Void type"sv;
    case TOK_Fn: return "Fn type"sv;
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
    case TOK_AA: return "AA type"sv;
    case TOK_AAA: return "AAA type"sv;
    case TOK_PP: return "PP type"sv;
    case TOK_PPP: return "PPP type"sv;
    case TOK_CC: return "CC type"sv;
    case TOK_CCC: return "CCC type"sv;
    case TOK_MM: return "MM type"sv;
    case TOK_MMM: return "MMM type"sv;
    case TOK_Int: return "Int type"sv;
    case TOK_Real: return "Real type"sv;
    case TOK_Bool: return "Bool type"sv;
    case TOK_ident: return "identifier"sv;
    case TOK_type_ident: return "type identifier"sv;
    case TOK_decimal: return "number"sv;
    case TOK_hex: return "number"sv;
    case TOK_binary: return "number"sv;
    case TOK_int: return "int"sv;
    case TOK_real: return "real"sv;
    case TOK_global_ident: return "global identifier"sv;
    case TOK_weak_ident: return "weak identifier"sv;
    case TOK_hw_addr: return "hardware address"sv;
    case TOK_read_hw: return "read hardware"sv;
    case TOK_write_hw: return "write hardware"sv;
    case TOK_group_set: return "group set"sv;
    case TOK_rpair: return "rpair"sv;
    case TOK_ssa: return "ssa"sv;
    case TOK_implicit_cast: return "implicit_cast"sv;
    case TOK_shift_atom: return "shift_atom"sv;
    case TOK_replace_atom: return "replace_atom"sv;
    case TOK_write_state: return "write_state"sv;
    case TOK_byte_vec: return "byte_vec"sv;
    case TOK_locator_vec: return "locator_vec"sv;
    case TOK_character: return "character literal"sv;
    case TOK_string_uncompressed: return "uncompressed string literal"sv;
    case TOK_string_compressed: return "compressed string literal"sv;
    case TOK_byte_block_proc: return "byte block"sv;
    case TOK_byte_block_data: return "byte block"sv;
    case TOK_byte_block_asm_op: return "assembly instruction"sv;
    case TOK_byte_block_label: return "assembly label"sv;
    case TOK_byte_block_call: return "assembly fn call"sv;
    case TOK_byte_block_goto: return "assembly goto"sv;
    case TOK_byte_block_goto_mode: return "assembly goto mode"sv;
    case TOK_byte_block_wait_nmi: return "assembly wait nmi"sv;
    case TOK_byte_block_bank_switch_a: return "assembly bank switch A"sv;
    case TOK_byte_block_bank_switch_x: return "assembly bank switch X"sv;
    case TOK_byte_block_bank_switch_y: return "assembly bank switch Y"sv;
    case TOK_byte_block_bank_switch_ax: return "assembly bank switch AX"sv;
    case TOK_byte_block_byte_array: return "byte block array"sv;
    case TOK_byte_block_locator_array: return "byte block locator array"sv;
    case TOK_byte_block_sub_proc: return "byte block sub proc"sv;
    case TOK_byte_block_if: return "byte block if"sv;
    }
}
constexpr unsigned char token_precedence_table[] =
{
    0,
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
    0,
    1,
    0,
    8,
    8,
    8,
    8,
    8,
    4,
    5,
    7,
    7,
    7,
    7,
    7,
    7,
    6,
    6,
    6,
    6,
    6,
    8,
    8,
    10,
    10,
    11,
    11,
    12,
    141,
    13,
    14,
    14,
    15,
    16,
    17,
    18,
    18,
    18,
    18,
    19,
    19,
    20,
    20,
    21,
    21,
    156,
    29,
    29,
    158,
    158,
    158,
    158,
    158,
    158,
    158,
    158,
    158,
    158,
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
    0,
    0,
};
constexpr bool token_right_assoc_table[] =
{
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
    1,
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
    0,
    0,
    0,
};
#define lex_TOK_KEY_CASES \
    case TOK_if:\
    case TOK_else:\
    case TOK_for:\
    case TOK_while:\
    case TOK_do:\
    case TOK_break:\
    case TOK_continue:\
    case TOK_return:\
    case TOK_fn:\
    case TOK_ct:\
    case TOK_mode:\
    case TOK_nmi:\
    case TOK_irq:\
    case TOK_goto:\
    case TOK_label:\
    case TOK_file:\
    case TOK_struct:\
    case TOK_vars:\
    case TOK_data:\
    case TOK_omni:\
    case TOK_asm:\
    case TOK_ready:\
    case TOK_fence:\
    case TOK_switch:\
    case TOK_case:\
    case TOK_default:\
    case TOK_charmap:\
    case TOK_chrrom:\
    case TOK_employs:\
    case TOK_preserves:\
    case TOK_stows:\
    case TOK_audio:\
    case TOK_system:\
    case TOK_state:\
    case TOK_abs:\
    case TOK_min:\
    case TOK_max:\
    case TOK_swap:\
    case TOK_macro:\
    case TOK___mapper_detail:\
    case TOK___mapper_reset:\
    case TOK___mapper:\
    case TOK___illegal:\
    case TOK___controllers:\
    case TOK___expansion_audio:\
    case TOK___sector_size:\
    case TOK_nmi_counter:\
    case TOK_read:\
    case TOK_write:\
    case TOK_mapfab:\
    case TOK_push:\
    case TOK_pop:\
    case TOK_true:\
    case TOK_false:\
    case TOK_PPUCTRL:\
    case TOK_PPUMASK:\
    case TOK_PPUSTATUS:\
    case TOK_PPUSCROLL:\
    case TOK_PPUADDR:\
    case TOK_PPUDATA:\
    case TOK_OAMADDR:\
    case TOK_OAMDATA:\
    case TOK_OAMDMA:\
    case TOK_SYSTEM_NTSC:\
    case TOK_SYSTEM_PAL:\
    case TOK_SYSTEM_DENDY:\
    case TOK_SYSTEM_UNKNOWN:\
    case TOK_colon:\
    case TOK_hash:\
    case TOK_backtick:\
    case TOK_dquote:\
    case TOK_quote:\
    case TOK_semicolon:\
    case TOK_comma:\
    case TOK_sizeof:\
    case TOK_len:\
    case TOK_unary_plus:\
    case TOK_unary_minus:\
    case TOK_unary_xor:\
    case TOK_unary_negate:\
    case TOK_unary_ref:\
    case TOK_at:\
    case TOK_period:\
    case TOK_apply:\
    case TOK_mode_apply:\
    case TOK_cast:\
    case TOK_cast_type:\
    case TOK_index8:\
    case TOK_index16:\
    case TOK_lbrace:\
    case TOK_rbrace:\
    case TOK_lbracket:\
    case TOK_rbracket:\
    case TOK_lparen:\
    case TOK_increment:\
    case TOK_decrement:\
    case TOK_asterisk:\
    case TOK_fslash:\
    case TOK_plus:\
    case TOK_minus:\
    case TOK_rol:\
    case TOK_ror:\
    case TOK_ror_flip:\
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
    case TOK_rol_assign:\
    case TOK_ror_assign:\
    case TOK_ror_assign_flip:\
    case TOK_assign:\
    case TOK_plus_assign:\
    case TOK_minus_assign:\
    case TOK_times_assign:\
    case TOK_div_assign:\
    case TOK_bitwise_and_assign:\
    case TOK_bitwise_or_assign:\
    case TOK_bitwise_xor_assign:\
    case TOK_lshift_assign:\
    case TOK_rshift_assign:\
    case TOK_rparen:\

constexpr token_type_t TOK_LAST_STATE = 219;
constexpr token_type_t TOK_START = 296;
extern unsigned const lexer_ec_table[256];
extern token_type_t const lexer_transition_table[56700];
} // namespace lex
