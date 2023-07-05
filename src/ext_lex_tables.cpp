#include "ext_lex_tables.hpp"
namespace ext_lex
{
extern unsigned const lexer_ec_table[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 36, 72, 108, 0, 0, 0, 144, 180, 216, 0, 0, 252, 288, 324, 0,
    360, 0, 396, 0, 432, 0, 0, 0, 468, 0, 0, 0, 0, 0, 0, 0,
    0, 504, 540, 576, 0, 0, 0, 612, 648, 684, 0, 0, 720, 756, 792, 0,
    828, 0, 864, 0, 900, 0, 0, 0, 936, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};
extern token_type_t const lexer_transition_table[972] = {

    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6, 0, 3, 0, 2,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6,
    0, 3, 0, 2, 0, 1, 12, 34, 0, 0, 0, 0, 0, 0, 0, 5,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8,
    0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 9, 0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6, 0, 3, 0, 2,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0,
    0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 4, 6,
    0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    16, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 11, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6, 13, 3, 0, 2,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 0, 0, 4, 6,
    0, 3, 0, 2, 17, 1, 0, 35, 0, 0, 0, 0, 0, 0, 0, 5,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19,
    0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 4, 6, 0, 3, 15, 2, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 4, 6, 0, 3, 0, 2,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0,
    0, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6,
    0, 3, 0, 2, 0, 1, 0, 0, 33, 0, 0, 0, 0, 0, 0, 5,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 30,
    28, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 21, 0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 22, 0, 0, 4, 6, 0, 3, 0, 2,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6,
    0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
    0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 31, 0,
    0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0,
    0, 32, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6, 0, 3, 0, 2,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 11, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6,
    0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
    0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 23,
    0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0,
    29, 0, 0, 5, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 24, 0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 6, 0, 3, 0, 2,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 15,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 4, 6,
    0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 27, 5,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 4, 6, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 26, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0,
};
} // namespace ext_lex
