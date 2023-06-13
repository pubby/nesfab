#include "pass1.hpp"

#include "alloca.hpp"

using namespace lex;

void pass1_t::uses_type(type_t type, idep_class_t calc)
{
    if(type.name() == TYPE_STRUCT_THUNK || type.name() == TYPE_STRUCT)
        ideps.emplace(const_cast<global_t*>(&type.global()), idep_pair_t{ .calc = calc, .depends_on = IDEP_TYPE });
    else if(has_type_tail(type.name()))
    {
        unsigned const size = type.type_tail_size();
        for(unsigned i = 0; i < size; ++i)
            uses_type(type.type(i));
    }
}

void pass1_t::uses_charmap(global_t const* charmap, idep_class_t calc)
{
    ideps.emplace(const_cast<global_t*>(charmap), idep_pair_t{ .calc = calc, .depends_on = IDEP_TYPE });
}

ast_node_t* pass1_t::eternal_expr(ast_node_t const* expr)
{
    // Store the expression and return a pointer to it.
    if(expr)
        return eternal_emplace<ast_node_t>(*expr);
    return nullptr;
}

ast_node_t* pass1_t::convert_eternal_expr(ast_node_t const* expr, idep_class_t calc)
{
    // Store the expression and return a pointer to it.
    if(expr)
    {
        ast_node_t* ret = eternal_emplace<ast_node_t>(*expr);
        convert_ast(*ret, calc);
        return ret;
    }
    return nullptr;
}

void pass1_t::convert_ast(ast_node_t& ast, idep_class_t calc, idep_class_t depends_on)
{
    switch(ast.token.type)
    {
    case TOK_weak_ident:
        if(depends_on)
            depends_on = IDEP_TYPE;

        // fall-through
    case TOK_ident:
        if(int const* handle = symbol_table.find(ast.token.pstring.view(source())))
        {
            ast.token.value = *handle;
            ast.token.type = TOK_ident;
            assert(ast.token.signed_() == *handle);
        }
        else
        {
            global_t& g = lookup_global(ast.token.pstring);
            if(depends_on)
                add_idep(ideps, &g, { .calc = calc, .depends_on = depends_on });
            ast.token.type = TOK_global_ident;
            ast.token.set_ptr(&g);
        }
        break;

    case TOK_type_ident:
        {
            global_t& g = lookup_global(ast.token.pstring);
            add_idep(ideps, &g, { .calc = calc, .depends_on = IDEP_TYPE });
            ast.token.set_ptr(&g);
        }
        break;

    case TOK_at:
    case TOK_unary_ref:
        depends_on = BAD_IDEP;
        goto do_children;

    case TOK_sizeof_expr:
    case TOK_len_expr:
        depends_on = IDEP_TYPE;
        goto do_children;

    case TOK_sizeof:
    case TOK_len:
        depends_on = IDEP_TYPE;
        uses_type(*ast.token.ptr<type_t const>());
        goto do_children;

    case TOK_cast_type:
        uses_type(*ast.token.ptr<type_t const>());
        // fall-through
    default:
        depends_on = IDEP_VALUE;
        // fall-through
    do_children:
        unsigned const n = ast.num_children();
        for(unsigned i = 0; i < n; ++i)
            convert_ast(ast.children[i], calc, depends_on);
    }
}

global_t& pass1_t::lookup_global(pstring_t pstring)
{
    std::string_view const view = pstring.view(file.source());
    if(view.starts_with('_'))
        return private_globals.lookup(pstring, view);
    return global_t::lookup_sourceless(pstring, view);
}

group_t* pass1_t::lookup_group(pstring_t pstring)
{
    std::string_view const view = pstring.view(file.source());
    if(view.starts_with("/_"))
        return &private_groups.lookup(pstring, view);
    return group_t::lookup_sourceless(pstring, view);
}
