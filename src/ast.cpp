#include "ast.hpp"

unsigned ast_node_t::num_children() const
{
    using namespace lex;

    switch(token.type)
    {
    case TOK_apply:
    case TOK_cast:
    case TOK_push_paa:
        assert(children);
        return token.value;

    case TOK_unary_minus:
    case TOK_unary_xor:
    case TOK_unary_negate:
    case TOK_unary_ref:
    case TOK_sizeof_expr:
    case TOK_len_expr:
    case TOK_period:
    case TOK_read_hw:
        assert(children);
        return 1;

    case TOK_write_hw:
        return 2;

    default:
        if(is_operator(token.type))
        {
            assert(children);
            return 2;
        }

        assert(!children);
        return 0;
    }
}
