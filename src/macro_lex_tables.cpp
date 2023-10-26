#include "macro_lex_tables.hpp"
namespace macro_lex
{
extern unsigned const lexer_ec_table[256] = {
    0, 61, 61, 61, 61, 61, 61, 61, 61, 61, 122, 61, 61, 183, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 244, 305, 61, 61, 61, 366, 61, 61, 427, 61, 61, 488, 61, 549,
    610, 610, 610, 610, 610, 610, 610, 610, 610, 610, 671, 61, 61, 732, 61, 61,
    61, 793, 793, 793, 793, 793, 793, 793, 793, 793, 793, 793, 793, 793, 793, 793,
    793, 793, 793, 793, 793, 793, 793, 793, 793, 793, 793, 61, 61, 61, 61, 610,
    854, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915,
    915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
};
extern token_type_t const lexer_transition_table[976] = {

    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 31, 6, 16, 1,
    3, 0, 4, 0, 2, 7, 5, 5, 5, 0, 10, 0, 0, 9, 0, 0,
    0, 8, 0, 0, 12, 0, 0, 0, 13, 0, 0, 0, 11, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 27, 0, 6, 16, 1, 3, 0, 4,
    0, 2, 7, 5, 5, 5, 0, 10, 0, 0, 9, 0, 0, 0, 8, 0,
    0, 12, 0, 0, 0, 13, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 39, 0, 6, 16, 1, 3, 0, 4, 0, 2, 7,
    5, 5, 38, 0, 10, 0, 0, 9, 0, 0, 0, 8, 0, 0, 12, 0,
    0, 0, 13, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 40, 0, 6, 16, 1, 3, 0, 4, 0, 2, 7, 5, 38, 5,
    0, 10, 0, 0, 9, 0, 0, 0, 8, 0, 0, 12, 0, 0, 0, 13,
    0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 27,
    32, 6, 16, 1, 3, 21, 4, 0, 2, 7, 5, 5, 5, 0, 10, 0,
    0, 9, 48, 48, 0, 8, 0, 0, 12, 0, 0, 0, 13, 0, 0, 0,
    11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 37,
    0, 16, 0, 0, 0, 0, 0, 0, 0, 37, 0, 0, 27, 33, 6, 16,
    1, 3, 0, 4, 0, 2, 7, 5, 5, 5, 42, 10, 0, 45, 9, 0,
    0, 49, 8, 0, 52, 12, 0, 0, 56, 13, 0, 0, 60, 11, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0,
    0, 0, 0, 0, 0, 44, 0, 0, 0, 27, 34, 6, 16, 1, 3, 22,
    4, 0, 2, 7, 5, 5, 5, 0, 10, 44, 0, 9, 0, 0, 0, 8,
    0, 0, 12, 0, 0, 0, 13, 0, 0, 0, 11, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 27, 0, 6, 16, 1, 3, 0, 4, 16, 2,
    7, 5, 5, 5, 0, 10, 0, 0, 9, 0, 0, 0, 8, 0, 0, 12,
    0, 0, 0, 13, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 27, 0, 6, 16, 1, 3, 17, 4, 0, 2, 7, 5, 5,
    5, 0, 10, 0, 0, 9, 0, 0, 0, 8, 0, 0, 12, 0, 0, 0,
    13, 59, 59, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    27, 35, 6, 29, 1, 3, 0, 4, 27, 2, 7, 5, 5, 5, 0, 10,
    0, 0, 9, 0, 0, 0, 8, 0, 0, 12, 0, 0, 0, 13, 0, 0,
    0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    14, 15, 16, 0, 0, 0, 20, 0, 0, 23, 24, 25, 0, 27, 0, 6,
    16, 1, 3, 0, 4, 0, 2, 7, 5, 5, 5, 0, 10, 43, 0, 9,
    46, 47, 0, 8, 50, 0, 12, 53, 54, 0, 13, 57, 58, 0, 11, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 27, 0, 6, 16, 1, 3,
    18, 4, 0, 2, 7, 5, 5, 5, 0, 10, 0, 0, 9, 0, 0, 0,
    8, 0, 0, 12, 55, 55, 0, 13, 0, 0, 0, 11, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0,
    0, 0, 0, 0, 0, 51, 0, 27, 0, 6, 16, 1, 3, 19, 4, 0,
    2, 7, 5, 5, 5, 0, 10, 0, 0, 9, 0, 0, 0, 8, 51, 0,
    12, 0, 0, 0, 13, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 14, 15, 16, 57, 53, 50, 0, 46, 43,
    0, 0, 0, 15, 27, 0, 6, 16, 1, 3, 14, 4, 0, 2, 7, 5,
    5, 5, 0, 10, 43, 0, 9, 46, 0, 0, 8, 50, 0, 12, 53, 0,
    0, 13, 57, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 41, 16, 0, 0, 0, 41, 0, 0, 0, 0, 0,
    0, 27, 36, 6, 16, 1, 3, 26, 4, 0, 2, 7, 5, 5, 5, 0,
    10, 0, 0, 9, 0, 0, 0, 8, 0, 0, 12, 0, 0, 0, 13, 0,
    0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 16, 58, 54, 25, 20, 47, 23, 23, 24, 25, 20, 27, 0,
    6, 16, 1, 3, 24, 4, 0, 2, 7, 5, 5, 5, 0, 10, 0, 0,
    9, 0, 47, 0, 8, 0, 0, 12, 0, 54, 0, 13, 0, 58, 0, 11,
};
} // namespace macro_lex
