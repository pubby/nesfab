#include "pass1.hpp"

#include "alloca.hpp"

void pass1_t::this_uses_type(type_t type)
{
    if(type.name() == TYPE_STRUCT_THUNK)
        ideps.insert(const_cast<global_t*>(&type.global()));
    else if(has_type_tail(type.name()))
    {
        unsigned const size = type.type_tail_size();
        for(unsigned i = 0; i < size; ++i)
            this_uses_type(type.type(i));
    }
}

token_t const* pass1_t::convert_expr(expr_temp_t& expr)
{
    for(token_t& token : expr)
    {
        // Lookup identifiers and replace their '.value'.
        if(token.type == TOK_ident || token.type == TOK_weak_ident)
        {
            if(unsigned const* handle = symbol_table.find(token.pstring.view(source())))
                token.value = *handle;
            else
            {
                global_t& g = global_t::lookup(file.source(), token.pstring);
                if(token.type != TOK_weak_ident)
                    ideps.insert(&g);
                else
                    weak_ideps.insert(&g);
                token.type = TOK_global_ident;
                token.set_ptr(&g);
            }
        }
        else if(token.type == TOK_type_ident)
        {
            global_t& g = global_t::lookup(file.source(), token.pstring);
            ideps.insert(&g);
            token.set_ptr(&g);
        }
    }

    // Null-like terminator
    expr.push_back({});

    // Store the expression in 'expr_pool' and return a pointer to it.
    return stmt_t::new_expr(&*expr.begin(), &*expr.end());
}

