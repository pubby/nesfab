#include "pass1.hpp"

#include "alloca.hpp"

token_t* pass1_t::convert_expr(expr_temp_t& expr)
{
    for(token_t& token : expr)
    {
        // Lookup identifiers and replace their '.value' with an index.
        if(token.type == TOK_ident)
        {
            if(unsigned const* handle = 
               symbol_table.find(token.pstring.view()))
            {
                token.value = *handle;
            }
            else
            {
                token.type = TOK_global_ident;
                token.value = globals().get_index(token.pstring);
                active_global->deps.insert(&globals()[token.value]);
            }
        }
    }

    // Store the expression in 'expr_pool' and return a pointer to it.
    expr.push_back({});
    return expr_pool.insert(expr.begin(), expr.end());
}

