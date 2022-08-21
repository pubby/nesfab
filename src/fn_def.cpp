#include "fn_def.hpp"

using namespace lex;

pstring_t fn_def_t::find_global(global_t const* global) const
{
    for(stmt_t const& stmt : stmts)
    {
        if(!has_expression(stmt.name))
            continue;
        for(token_t const* token = stmt.expr; token->type; ++token)
            if(token->type == TOK_global_ident && token->ptr<global_t>() == global)
                return token->pstring;
    }
    return {};
}


stmt_ht fn_def_t::push_stmt(stmt_t stmt) 
{ 
    stmt_ht const handle = next_stmt();
    stmts.push_back(stmt); 
    return handle;
}

stmt_ht fn_def_t::push_var_init(unsigned name, token_t const* expr, pstring_t pstring)
{ 
    return push_stmt({ static_cast<stmt_name_t>(~name), {}, {}, pstring, expr }); 
}

stmt_mods_ht fn_def_t::push_mods(std::unique_ptr<mods_t> m)
{
    if(!m)
        return {};
    stmt_mods_ht const handle = { mods.size() };
    mods.push_back(std::move(*m));
    return handle;
}

mods_t const* fn_def_t::mods_of(stmt_ht h) const 
{
    if(stmt_mods_ht m = operator[](h).mods)
        return &operator[](m);
    return nullptr;
}
