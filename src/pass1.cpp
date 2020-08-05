#include "pass1.hpp"

#include "alloca.hpp"

token_t const* pass1_t::convert_expr(expr_temp_t& expr)
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
                global_t& g = global_t::lookup(token.pstring);
                token.type = TOK_global_ident;
                token.set_ptr(&g);
                ideps.insert(&g);
            }
        }
    }

    // Null-like terminator
    expr.push_back({});

    // Store the expression in 'expr_pool' and return a pointer to it.
    return global_t::new_expr(&*expr.begin(), &*expr.end());
}

