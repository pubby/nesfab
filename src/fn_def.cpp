#include "fn_def.hpp"

using namespace lex;

static pstring_t _find_global(ast_node_t const& ast, global_t const* global)
{
    if(ast.token.type == TOK_global_ident && ast.token.ptr<global_t const>() == global)
        return ast.token.pstring;
    unsigned const n = ast.num_children();
    for(unsigned i = 0; i != n; ++i)
        if(pstring_t pstring = _find_global(ast.children[i], global))
           return pstring;
    return {};
}

pstring_t fn_def_t::find_global(global_t const* global) const
{
    for(stmt_t const& stmt : stmts)
        if(has_expression(stmt.name))
            if(pstring_t pstring = _find_global(*stmt.expr, global))
                return pstring;
    return {};
}


stmt_ht fn_def_t::push_stmt(stmt_t stmt) 
{ 
    stmt_ht const handle = next_stmt();
    stmts.push_back(stmt); 
    return handle;
}

stmt_ht fn_def_t::push_var_init(unsigned name, ast_node_t const* expr, pstring_t pstring)
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
