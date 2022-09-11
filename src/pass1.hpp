#ifndef PASS1_HPP
#define PASS1_HPP

#include <memory>
#include <variant>
#include <vector>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "flat/small_map.hpp"
#include "flat/small_multimap.hpp"
#include "robin/map.hpp"

#include "alloca.hpp"
#include "compiler_error.hpp"
#include "fnv1a.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "parser_decl.hpp"
#include "symbol_table.hpp"
#include "type.hpp"
#include "mods.hpp"
#include "iasm.hpp"
#include "eternal_new.hpp"

namespace bc = boost::container;

class pass1_t
{
private:
    file_contents_t const& file;
    global_t* active_global = nullptr;
    ideps_map_t ideps;
    fn_def_t fn_def;
    field_map_t field_map;

    symbol_table_t symbol_table;
    fc::small_map<pstring_t, stmt_ht, 4, pstring_less_t> label_map;
    fc::small_multimap<pstring_t, stmt_ht, 4, pstring_less_t> unlinked_gotos;

    bc::small_vector<bc::small_vector<stmt_ht, 4>, 8> break_stack;
    bc::small_vector<bc::small_vector<stmt_ht, 4>, 8> continue_stack;

    struct nothing_t {};

    void validate_mods(
        char const* keyword, pstring_t pstring, std::unique_ptr<mods_t> const& mods, 
        mod_flags_t accepts_flags = 0, 
        bool accepts_vars = false, bool accepts_data = false, 
        bool accepts_nmi = false)
    {
        if(!mods)
            return;

        char const* const title = keyword ? keyword : "Construct";

        if(!accepts_vars && mods->explicit_group_vars)
        {
            compiler_error(pstring, 
                fmt("% does not support vars modifiers.", title), &file);
        }

        if(!accepts_data && mods->explicit_group_data)
        {
            compiler_error(pstring, 
                fmt("% does not support data modifiers.", title), &file);
        }

        if(!accepts_nmi && mods->nmi)
        {
            compiler_error(pstring, 
                fmt("% does not support nmi modifier.", title), &file);
        }

        mod_flags_t const bad_enable = mods->enable & ~accepts_flags;
        mod_flags_t const bad_disable = mods->disable & ~accepts_flags;

        char const* const in = keyword ? " attached to " : "";

        if(bad_enable | bad_disable)
        {
            bitset_for_each(bad_enable, [&](unsigned bit)
            {
                compiler_warning(pstring, 
                    fmt("Ignoring modifier +%%%.", 
                        to_string(mod_flags_t(1ull << bit)), in, keyword), &file);
            });

            bitset_for_each(bad_disable, [&](unsigned bit)
            {
                compiler_warning(pstring, 
                    fmt("Ignoring modifier -%%%.", 
                        to_string(mod_flags_t(1ull << bit)), in, keyword), &file);
            });
        }
    }

public:
    explicit pass1_t(file_contents_t const& file) 
    : file(file)
    , label_map(pstring_less_t{ file.source() })
    , unlinked_gotos(pstring_less_t{ file.source() })
    {}

    // Helpers
    char const* source() { return file.source(); }
    void uses_type(type_t type, idep_class_t calc = IDEP_TYPE);
    //token_t* eternal_expr(expr_temp_t& expr); TODO
    //void convert_expr(token_t* begin); TODO
    //token_t const* convert_expr(expr_temp_t& expr); TODO
    ast_node_t* eternal_expr(ast_node_t const* expr);
    ast_node_t* convert_eternal_expr(ast_node_t const* expr, idep_class_t calc = IDEP_VALUE);
    void convert_ast(ast_node_t& ast, idep_class_t calc, idep_class_t depends_on = IDEP_VALUE);
    //ast_node_t process_ast(ast_node_t ast);
    //global_t const* at_ident(pstring_t pstring);

    [[gnu::always_inline]]
    void prepare_global()
    {
        assert(ideps.empty());
        assert(label_map.empty());
        assert(unlinked_gotos.empty());
        assert(symbol_table.empty());
    }

    [[gnu::always_inline]]
    void prepare_fn(pstring_t fn_name)
    {
        assert(ideps.empty());
        assert(label_map.empty());
        assert(unlinked_gotos.empty());

        // Reset the fn_def and iasm:
        fn_def = fn_def_t();

        // Find the global
        active_global = &global_t::lookup(file.source(), fn_name);

        // Create a scope for the parameters.
        assert(symbol_table.empty());
        symbol_table.push_scope();
    }

    // implementation detail:
    int _add_symbol(var_decl_t const& var_decl, bool local_const,
                    std::unique_ptr<mods_t> mods = {},
                    ast_node_t* local_const_expr = nullptr)
    {
        assert(!local_const_expr || local_const);

        int const handle = local_const ? -int(fn_def.local_consts.size())-1 : int(fn_def.local_vars.size());
        if(int const* existing = symbol_table.new_def(handle, var_decl.name.view(source())))
        {
            // Already have a variable defined in this scope.
            throw compiler_error_t(
                fmt_error(var_decl.name, 
                          fmt("Identifier % already in use.", 
                              var_decl.name.view(source())), &file)
                + fmt_error(fn_def.var_decl(*existing).name, 
                            "Previous definition here:", &file));
        }
        if(local_const)
            fn_def.local_consts.emplace_back(var_decl, std::move(mods), convert_eternal_expr(local_const_expr));
        else
            fn_def.local_vars.emplace_back(var_decl, std::move(mods));
        return handle;
    }

    // Functions
    [[gnu::always_inline]]
    var_decl_t fn_decl(
        pstring_t fn_name, 
        var_decl_t const* params_begin, var_decl_t const* params_end, 
        src_type_t return_type, bool assembly)
    {
        // Add the parameters to the symbol table.
        fn_def.num_params = params_end - params_begin;
        for(unsigned i = 0; i < fn_def.num_params; ++i)
            _add_symbol(params_begin[i], false);

        // Add the parameters to 'name_hashes':
        fn_def.name_hashes.resize(fn_def.num_params);
        for(unsigned i = 0; i < fn_def.num_params; ++i)
            fn_def.name_hashes[i] = fnv1a<std::uint64_t>::hash(params_begin[i].name.view(file.source()));

        pstring_t pstring = return_type.pstring;

        fn_def.return_type = return_type;

        // Find and store the fn's type:
        type_t* types = ALLOCA_T(type_t, fn_def.num_params + 1);
        for(unsigned i = 0; i != fn_def.num_params; ++i)
        {
            types[i] = params_begin[i].src_type.type;
            pstring = concat(params_begin[i].src_type.pstring, pstring);
        }
        types[fn_def.num_params] = return_type.type;
        type_t fn_type = type_t::fn(types, types + fn_def.num_params + 1);
        assert(fn_type.return_type() == return_type.type);

        // Track it!
        uses_type(fn_type);

        // Create a scope for the fn body.
        if(!assembly)
            symbol_table.push_scope();

        return { { pstring, fn_type }, fn_name };
    }

    [[gnu::always_inline]]
    void end_fn(var_decl_t decl, fn_class_t fclass, std::unique_ptr<mods_t> mods)
    {
        symbol_table.pop_scope(); // fn body scope
        symbol_table.pop_scope(); // param scope
        label_map.clear();
        assert(symbol_table.empty());
        fn_def.push_stmt({ STMT_END_FN, {}, {}, decl.name });

        if(!unlinked_gotos.empty())
        {
            auto it = unlinked_gotos.begin();
            compiler_error(it->first, "Label not in scope.", &file);
        }

        validate_mods(fn_class_keyword(fclass), decl.name, mods, 
            0, // flags
            fclass != FN_CT, // vars
            fclass != FN_CT, // data
            fclass == FN_MODE // nmi
            );

        // Create the global:
        active_global->define_fn(
            decl.name, std::move(ideps),
            decl.src_type.type, std::move(fn_def), std::move(mods), fclass, false);
        ideps.clear();
    }

    [[gnu::always_inline]]
    void asm_fn_var(var_decl_t const& var_decl, std::unique_ptr<mods_t> mods)
    {
        validate_mods("var", var_decl.name, mods, MOD_zero_page);

        _add_symbol(var_decl, false, std::move(mods));
        uses_type(var_decl.src_type.type);
    }

    [[gnu::always_inline]]
    void byte_block_ct_value(var_decl_t const& var_decl, ast_node_t& ast, std::unique_ptr<mods_t> mods)
    {
        validate_mods("ct", var_decl.name, mods);

        // We'll save the expr, but *don't* convert it yet.
        _add_symbol(var_decl, true, std::move(mods), &ast);
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_label(pstring_t label, bool is_default, std::unique_ptr<mods_t> mods)
    {
        int const i = -_add_symbol({{ label, type_t::addr(false) }, label }, true)-1;
        assert(i >= 0);

        if(is_default)
        {
            if(fn_def.default_label != ENTRY_LABEL)
                compiler_error(label, "Multiple default labels.");
            fn_def.default_label = i;
        }
        else
            fn_def.name_hashes.push_back(fnv1a<std::uint64_t>::hash(label.view(file.source())));

        validate_mods("label", label, mods);

        ast_node_t ast = { .token = { .type = lex::TOK_byte_block_label, .pstring = label, .value = i }};
        if(mods)
            ast.mods = eternal_emplace<mods_t>(std::move(*mods));

        return ast;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_asm_op(pstring_t pstring, op_name_t name, addr_mode_t mode, ast_node_t* expr)
    {
        op_t op = get_op(name, mode);
        if(!op)
            op = get_op(name, zp_equivalent(mode));
        if(!op)
            compiler_error(pstring, fmt("% lacks addressing mode %.", to_string(name), to_string(mode)));

        return ast_node_t
        { 
            .token = { .type = lex::TOK_byte_block_asm_op, .pstring = pstring, .value = op },
            .children = eternal_expr(expr) // Don't convert yet. Wait until every label has been processed.
        };
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_call(lex::token_type_t tt, pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        if(symbol_table.find(pstring.view(source())))
            compiler_error(pstring, "Cannot call a local variable.");

        global_t& g = global_t::lookup(file.source(), pstring);
        ideps.emplace(&g, idep_pair_t{ .calc = IDEP_VALUE, .depends_on = IDEP_VALUE });

        if(tt == lex::TOK_byte_block_goto_mode)
            validate_mods("goto mode", pstring, mods, 0, true, false);
        else if(tt == lex::TOK_byte_block_goto)
            validate_mods("goto", pstring, mods);
        else
            validate_mods("fn", pstring, mods);

        ast_node_t ast = { .token = token_t::make_ptr(tt, pstring, &g) };
        if(mods)
            ast.mods = eternal_emplace<mods_t>(std::move(*mods));

        return ast;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_wait_nmi(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        ast_node_t ast = { .token = { .type = lex::TOK_byte_block_wait_nmi, .pstring =pstring }};

        validate_mods("nmi", pstring, mods);

        if(mods)
            ast.mods = eternal_emplace<mods_t>(std::move(*mods));
        return ast;
    }

    [[gnu::always_inline]]
    void end_asm_fn(
        var_decl_t decl, fn_class_t fclass, ast_node_t ast,
        std::unique_ptr<mods_t> mods)
    {
        // Set the default label
        /* TODO: remove?
        if(fn_def.default_label < 0)
        {
            for(unsigned i = 0; i < fn_def.local_consts.size(); ++i)
            {
                if(fn_def.local_consts[i].is_label())
                {
                    fn_def.default_label = i;
                    goto found_default_label;
                }
            }
        found_default_label:;
        }
        */

        assert(ast.token.type == lex::TOK_byte_block_proc
               || ast.token.type == lex::TOK_byte_block_data);
        ast.token.type = lex::TOK_byte_block_proc;

        //Convert all expressions
        for(auto& c : fn_def.local_consts)
            if(c.expr)
                convert_ast(*const_cast<ast_node_t*>(c.expr), IDEP_VALUE);
        fn_def.push_stmt({ STMT_EXPR, {}, {}, {}, convert_eternal_expr(&ast) });
        fn_def.push_stmt({ STMT_END_FN, {}, {}, decl.name });

        if(fn_def.local_vars.size() > MAX_ASM_LOCAL_VARS)
            compiler_error(decl.name, fmt("Too many local variables. Max %.", MAX_ASM_LOCAL_VARS));

        symbol_table.pop_scope(); // param scope

        label_map.clear();
        assert(symbol_table.empty());

        if(fclass != FN_FN && fclass != FN_NMI)
            compiler_error(decl.name, fmt("% does not support inline assembly.", fn_class_keyword(fclass)));

        validate_mods(fn_class_keyword(fclass), decl.name, mods, 
            0, // flags
            true, // vars
            true, // data
            false // nmi
            );

        // Create the global:
        active_global->define_fn(
            decl.name, std::move(ideps),
            decl.src_type.type, std::move(fn_def), std::move(mods), fclass, true);

        ideps.clear();
    }

    [[gnu::always_inline]]
    pstring_t begin_struct(pstring_t struct_name)
    {
        assert(ideps.empty());

        // Reset the struct:
        field_map.clear();

        // Find the global
        active_global = &global_t::lookup(file.source(), struct_name);

        return struct_name;
    }

    void end_struct(pstring_t struct_name)
    {
        active_global->define_struct(
            struct_name, std::move(ideps), std::move(field_map));
    }

    void struct_field(pstring_t struct_name, var_decl_t const& var_decl, ast_node_t* expr)
    {
        assert(symbol_table.empty());

        auto const hash = fnv1a<std::uint64_t>::hash(var_decl.name.view(file.source()));

        auto result = field_map.insert({ hash, field_t{ .decl = var_decl }});

        if(!result.second)
        {
            pstring_t const map_pstring = result.first->second.decl.name;

            assert(var_decl.name.file_i == map_pstring.file_i);

            if(var_decl.name.view(file.source()) == map_pstring.view(file.source()))
            {
                // Already have a field with that name.
                throw compiler_error_t(
                    fmt_error(var_decl.name, 
                              fmt("Multiple definitions of % in %.", 
                                  var_decl.name.view(source()),
                                  struct_name.view(source())), &file)
                    + fmt_error(map_pstring, "Previous definition here:", &file));
            }
            else
            {
                throw compiler_error_t(
                    fmt_error(var_decl.name, 
                              fmt("Hash collisision! % in %...", 
                                  var_decl.name.view(source()),
                                  struct_name.view(source())), &file)
                    + fmt_error(map_pstring, "...has the same fnv1a hash as:", &file)
                    + fmt_note("Rename one to avoid this issue."));
            }
        }

        uses_type(var_decl.src_type.type);
    }

    [[gnu::always_inline]]
    std::pair<group_vars_t*, group_vars_ht> begin_group_vars(pstring_t group_name)
    {
        return group_t::lookup(file.source(), group_name).define_vars(group_name);
    }

    [[gnu::always_inline]]
    std::pair<group_data_t*, group_data_ht> begin_group_data(pstring_t group_name, bool once)
    {
        return group_t::lookup(file.source(), group_name).define_data(group_name, once);
    }

    [[gnu::always_inline]]
    void end_group()
    {}

    [[gnu::always_inline]]
    void global_var(std::pair<group_vars_t*, group_vars_ht> group, var_decl_t const& var_decl, 
                    ast_node_t* expr, std::unique_ptr<mods_t> mods)
    {
        uses_type(var_decl.src_type.type);

        validate_mods("var", var_decl.name, mods, MOD_zero_page);

        active_global = &global_t::lookup(file.source(), var_decl.name);
        active_global->define_var(var_decl.name, std::move(ideps), var_decl.src_type, group, convert_eternal_expr(expr), std::move(mods));
        ideps.clear();
    }

    [[gnu::always_inline]]
    void global_const(std::pair<group_data_t*, group_data_ht> group, var_decl_t const& var_decl, 
                      ast_node_t const& expr, std::unique_ptr<mods_t> mods)
    {
        uses_type(var_decl.src_type.type);

        validate_mods("ct", var_decl.name, mods);

        active_global = &global_t::lookup(file.source(), var_decl.name);
        active_global->define_const(var_decl.name, std::move(ideps), var_decl.src_type, group, convert_eternal_expr(&expr), std::move(mods));
        ideps.clear();
    }

    [[gnu::always_inline]]
    void expr_statement(ast_node_t const& expr)
    {
        fn_def.push_stmt({ STMT_EXPR, {}, {}, {}, convert_eternal_expr(&expr) });
    }

    [[gnu::always_inline]]
    void local_var(var_decl_t var_decl, ast_node_t* expr)
    {
        // Create the var.
        unsigned const handle = _add_symbol(var_decl, false);
        fn_def.push_var_init(handle, convert_eternal_expr(expr), var_decl.src_type.pstring);
        uses_type(var_decl.src_type.type);
    }

    struct flow_d
    {
        stmt_ht begin;
        stmt_mods_ht mods;
    };

    struct if_d : flow_d {};

    [[gnu::always_inline]]
    if_d begin_if(pstring_t pstring, ast_node_t const& condition, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();

        validate_mods("if", pstring, mods);

        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        stmt_ht const begin_if = fn_def.push_stmt({ STMT_IF, mods_h, {}, pstring, convert_eternal_expr(&condition) });
        return { begin_if, mods_h };
    }

    [[gnu::always_inline]]
    void end_if(if_d d) 
    { 
        fn_def[d.begin].link = fn_def.push_stmt({ STMT_END_IF, d.mods, d.begin }) + 1;
        symbol_table.pop_scope();
    }

    struct else_d : flow_d {};

    [[gnu::always_inline]]
    else_d end_if_begin_else(if_d d, pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        fn_def[d.begin].link = fn_def.push_stmt({ STMT_END_IF, d.mods, d.begin }) + 1;
        symbol_table.pop_scope();
        symbol_table.push_scope();

        validate_mods("else", pstring, mods);

        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        stmt_ht const begin_else = fn_def.push_stmt({ STMT_ELSE, mods_h, {}, pstring });
        return { begin_else, mods_h };
    }

    [[gnu::always_inline]]
    void end_else(else_d d) 
    { 
        fn_def[d.begin].link = fn_def.push_stmt({ STMT_END_IF, d.mods, d.begin }) + 1;
        symbol_table.pop_scope();
    }

    struct do_d : flow_d {};

    [[gnu::always_inline]]
    do_d begin_do_while(pstring_t pstring, std::unique_ptr<mods_t> mods) 
    { 
        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();

        validate_mods("do", pstring, mods);

        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        stmt_ht const begin_while = fn_def.push_stmt({ STMT_DO, mods_h, {}, pstring });
        return { begin_while, mods_h };
    }

    [[gnu::always_inline]]
    void end_do_while(do_d d, pstring_t pstring, ast_node_t const& condition)
    {
        stmt_ht const cond = fn_def.push_stmt({ STMT_END_DO, d.mods, d.begin, pstring, convert_eternal_expr(&condition) });
        fn_def[d.begin].link = cond;

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = cond + 1;
        for(stmt_ht h : continue_stack.back())
            fn_def[h].link = cond;

        break_stack.pop_back();
        continue_stack.pop_back();
        symbol_table.pop_scope();
    }

    struct while_d : flow_d {};

    [[gnu::always_inline]]
    while_d begin_while(pstring_t pstring, ast_node_t const& condition, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();

        validate_mods("while", pstring, mods);

        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        stmt_ht const begin_while = fn_def.push_stmt({ STMT_WHILE, mods_h, {}, pstring, convert_eternal_expr(&condition) });

        return { begin_while, mods_h};
    }

    [[gnu::always_inline]]
    void end_while(while_d d)
    {
        stmt_ht const exit = fn_def.push_stmt({ STMT_END_WHILE, d.mods, d.begin });
        fn_def[d.begin].link = exit + 1;

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = exit + 1;
        for(stmt_ht h : continue_stack.back())
            fn_def[h].link = d.begin;

        break_stack.pop_back();
        continue_stack.pop_back();
        symbol_table.pop_scope();
    }

    struct for_d : flow_d
    {
        ast_node_t* effect;
    };

    [[gnu::always_inline]]
    for_d begin_for(pstring_t pstring, var_decl_t* var_decl, ast_node_t* init, 
                    ast_node_t* condition, ast_node_t* effect, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();

        if(var_decl)
            local_var(*var_decl, init);
        else if(init)
            expr_statement(*init);

        validate_mods("for", pstring, mods);

        stmt_ht begin_for;
        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));

        begin_for = fn_def.push_stmt({ STMT_FOR, mods_h, {}, pstring, convert_eternal_expr(condition) });

        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();
        
        return { begin_for, mods_h, effect };
    }

    [[gnu::always_inline]]
    void end_for(for_d d)
    {
        stmt_ht const effect = fn_def.push_stmt({ STMT_FOR_EFFECT, d.mods, d.begin, {}, convert_eternal_expr(d.effect) });
        symbol_table.pop_scope();
        stmt_ht const exit = fn_def.push_stmt({ STMT_END_FOR, d.mods, d.begin });
        fn_def[d.begin].link = exit + 1;

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = exit + 1;
        for(stmt_ht h : continue_stack.back())
            fn_def[h].link = effect;

        break_stack.pop_back();
        continue_stack.pop_back();
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    void return_statement(pstring_t pstring, ast_node_t* expr, std::unique_ptr<mods_t> mods)
    {
        validate_mods("return", pstring, mods);
        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        fn_def.push_stmt({ STMT_RETURN, mods_h, {}, pstring, convert_eternal_expr(expr) });
    }

    [[gnu::always_inline]]
    void break_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        validate_mods("break", pstring, mods);
        break_stack.back().push_back(fn_def.push_stmt(
            { STMT_BREAK, fn_def.push_mods(std::move(mods)), {}, pstring }));
    }

    [[gnu::always_inline]]
    void continue_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        validate_mods("continue", pstring, mods);
        break_stack.back().push_back(fn_def.push_stmt(
            { STMT_CONTINUE, fn_def.push_mods(std::move(mods)), {}, pstring }));
    }

    [[gnu::always_inline]]
    void label_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        validate_mods("label", pstring, mods);

        // Create a new label
        stmt_ht label = fn_def.push_stmt(
            { STMT_LABEL, fn_def.push_mods(std::move(mods)), {}, pstring, { .use_count = 0 } });

        // Add it to the label map
        auto pair = label_map.emplace(pstring, label);
        if(!pair.second)
        {
            throw compiler_error_t(
                fmt_error(pstring, "Label name already in use.", &file)
                + fmt_error(pair.first->first, "Previous definition here:", &file));
        }

        // Link up the unlinked gotos that jump to this label.
        auto lower = unlinked_gotos.lower_bound(pstring);
        auto upper = unlinked_gotos.upper_bound(pstring);
        for(auto it = lower; it < upper; ++it)
        {
            assert(fn_def[it->second].name == STMT_GOTO);
            fn_def[it->second].link = label;
        }
        fn_def[label].use_count = std::distance(lower, upper);
        unlinked_gotos.erase(lower, upper);
    }

    [[gnu::always_inline]]
    void goto_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        validate_mods("goto", pstring, mods);
        stmt_ht const goto_h = fn_def.push_stmt(
            { STMT_GOTO, fn_def.push_mods(std::move(mods)), {}, pstring });

        auto it = label_map.find(pstring);
        if(it == label_map.end())
        {
            // Label wasn't defined yet.
            // We'll fill in the jump_h once it is.
            unlinked_gotos.emplace(pstring, goto_h);
        }
        else
        {
            assert(fn_def[goto_h].name == STMT_GOTO);
            assert(fn_def[it->second].name == STMT_GOTO);

            fn_def[goto_h].link = it->second;
            fn_def[it->second].use_count += 1;
        }
    }

    [[gnu::always_inline]]
    void goto_mode_statement(pstring_t mode, ast_node_t const& expr, std::unique_ptr<mods_t> mods)
    {
        validate_mods("goto mode", mode, mods, 0, true, false);

        if(!mods || !mods->explicit_group_vars)
            compiler_error(mode, "Missing vars modifier.");

        fn_def.push_stmt({ STMT_GOTO_MODE, fn_def.push_mods(std::move(mods)), {}, mode, convert_eternal_expr(&expr) });
    }

    [[gnu::always_inline]]
    void nmi_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        validate_mods("nmi", pstring, mods);
        fn_def.push_stmt({ STMT_NMI, fn_def.push_mods(std::move(mods)), {}, pstring });
    }

    [[gnu::always_inline]]
    void fence_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        validate_mods("fence", pstring, mods);
        fn_def.push_stmt({ STMT_FENCE, fn_def.push_mods(std::move(mods)), {}, pstring });
    }
};


#endif
