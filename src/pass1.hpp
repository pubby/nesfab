#ifndef PASS1_HPP
#define PASS1_HPP

#include <memory>
#include <variant>
#include <vector>

#include "flat/small_map.hpp"
#include "flat/small_multimap.hpp"

#include "alloca.hpp"
#include "compiler_error.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "parser_types.hpp"
#include "symbol_table.hpp"
#include "types.hpp"

class pass1_t
{
private:
    file_contents_t const& file;
    global_t* active_global = nullptr;
    global_t::ideps_set_t ideps;
    global_t::ideps_set_t weak_ideps;
    fn_def_t fn_def;

    symbol_table_t symbol_table;
    fc::small_map<pstring_t, label_t*, 4, pstring_less_t>
        label_map;
    fc::small_multimap<pstring_t, stmt_ht, 4, pstring_less_t> 
        unlinked_gotos;

    struct nothing_t {};
public:
    explicit pass1_t(file_contents_t const& file) 
    : file(file)
    , label_map(pstring_less_t{ file.source() })
    , unlinked_gotos(pstring_less_t{ file.source() })
    {}

    // Helpers
    char const* source() { return file.source(); }
    token_t const* convert_expr(expr_temp_t& expr);

    // Functions
    [[gnu::always_inline]]
    var_decl_t begin_fn(pstring_t fn_name, var_decl_t const* params_begin, 
                        var_decl_t const* params_end, type_t return_type)
    {
        assert(ideps.empty());
        assert(weak_ideps.empty());
        assert(label_map.empty());
        assert(unlinked_gotos.empty());

        // Reset the fn_def:
        fn_def = fn_def_t();

        // Find the global
        active_global = &global_t::lookup(file.source(), fn_name);

        // Create a scope for the parameters.
        assert(symbol_table.empty());
        symbol_table.push_scope();

        // Add the parameters to the symbol table.
        fn_def.num_params = params_end - params_begin;
        for(unsigned i = 0; i < fn_def.num_params; ++i)
        {
            symbol_table.new_def(i, params_begin[i].name.view(source()));
            fn_def.local_vars.push_back(params_begin[i]);
        }

        // Find and store the fn's type:
        type_t* types = ALLOCA_T(type_t, fn_def.num_params + 1);
        for(unsigned i = 0; i != fn_def.num_params; ++i)
            types[i] = params_begin[i].type;
        types[fn_def.num_params] = return_type;
        type_t fn_type = type_t::fn(types, types + fn_def.num_params + 1);

        // Create a scope for the fn body.
        symbol_table.push_scope();

        return { fn_type, fn_name };
    }

    [[gnu::always_inline]]
    void end_fn(var_decl_t decl)
    {
        symbol_table.pop_scope(); // fn body scope
        symbol_table.pop_scope(); // param scope
        label_map.clear();
        assert(symbol_table.empty());
        fn_def.push_stmt({ STMT_END_BLOCK });

        if(!unlinked_gotos.empty())
        {
            auto it = unlinked_gotos.begin();
            compiler_error(file, it->first, "Label not in scope.");
        }

        // Create the global:
        active_global->define_fn(decl.name, 
                                 std::move(ideps), std::move(weak_ideps),
                                 decl.type, std::move(fn_def), false);
        ideps.clear();
        weak_ideps.clear();
    }

    // Functions
    [[gnu::always_inline]]
    var_decl_t begin_mode(pstring_t mode_name, 
                          var_decl_t const* params_begin, var_decl_t const* params_end)
    {
        assert(ideps.empty());
        assert(weak_ideps.empty());
        assert(label_map.empty());
        assert(unlinked_gotos.empty());

        // Reset the fn_def:
        fn_def = fn_def_t();

        // Find the global
        active_global = &global_t::lookup(file.source(), mode_name);

        // Create a scope for the parameters.
        assert(symbol_table.empty());
        symbol_table.push_scope();

        // Add the parameters to the symbol table.
        fn_def.num_params = params_end - params_begin;
        for(unsigned i = 0; i < fn_def.num_params; ++i)
        {
            symbol_table.new_def(i, params_begin[i].name.view(source()));
            fn_def.local_vars.push_back(params_begin[i]);
        }

        // Find and store the fn's type:
        type_t* types = ALLOCA_T(type_t, fn_def.num_params + 1);
        for(unsigned i = 0; i != fn_def.num_params; ++i)
            types[i] = params_begin[i].type;
        types[fn_def.num_params] = TYPE_VOID;
        type_t fn_type = type_t::fn(types, types + fn_def.num_params + 1);

        // Create a scope for the fn body.
        symbol_table.push_scope();

        return { fn_type, mode_name };
    }

    [[gnu::always_inline]]
    void end_mode(var_decl_t decl)
    {
        symbol_table.pop_scope(); // mode body scope
        symbol_table.pop_scope(); // param scope
        label_map.clear();
        assert(symbol_table.empty());
        fn_def.push_stmt({ STMT_END_BLOCK });

        if(!unlinked_gotos.empty())
        {
            auto it = unlinked_gotos.begin();
            compiler_error(file, it->first, "Label not in scope.");
        }

        // Create the global:
        active_global->define_fn(decl.name, 
                                 std::move(ideps), std::move(weak_ideps),
                                 decl.type, std::move(fn_def), true);
        ideps.clear();
        weak_ideps.clear();
    }

    [[gnu::always_inline]]
    group_vars_t& begin_vars_group(pstring_t group_name)
    {
        return group_t::lookup(file.source(), group_name).define_vars(group_name);
    }

    [[gnu::always_inline]]
    void end_vars_group(group_vars_t&)
    {}

    // Global variables
    [[gnu::always_inline]]
    void global_var(group_vars_t& vars_group, var_decl_t const& var_decl, expr_temp_t* expr)
    {
        assert(ideps.empty());

        active_global = &global_t::lookup(file.source(), var_decl.name);
        active_global->define_var(var_decl.name, std::move(ideps), var_decl.type, vars_group);
        ideps.clear();
    }

    [[gnu::always_inline]]
    void expr_statement(expr_temp_t& expr)
    {
        fn_def.push_stmt({ STMT_EXPR, {}, convert_expr(expr) });
    }

    [[gnu::always_inline]]
    void local_var(var_decl_t var_decl, expr_temp_t* expr)
    {
        // Create the var.
        unsigned handle = fn_def.local_vars.size();
        if(unsigned const* existing = 
           symbol_table.new_def(handle, var_decl.name.view(source())))
        {
            // Already have a variable defined in this scope.
            throw compiler_error_t(
                fmt_error(file, var_decl.name, 
                          fmt("Identifier % already in use.", 
                              var_decl.name.view(source())))
                + fmt_error(file, fn_def.local_vars[*existing].name, 
                            "Previous definition here:"));
        }
        fn_def.local_vars.push_back(var_decl);
        fn_def.push_var_init(handle, expr ? convert_expr(*expr) : nullptr);
    }

    [[gnu::always_inline]]
    nothing_t begin_if(pstring_t pstring, expr_temp_t& condition)
    {
        symbol_table.push_scope();
        fn_def.stmts.push_back({ STMT_IF, pstring, convert_expr(condition) });
        return {};
    }

    [[gnu::always_inline]]
    void end_if(nothing_t) 
    { 
        fn_def.push_stmt({ STMT_END_BLOCK });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    nothing_t end_if_begin_else(nothing_t, pstring_t pstring)
    {
        fn_def.push_stmt({ STMT_END_BLOCK });
        fn_def.push_stmt({ STMT_ELSE, pstring });
        symbol_table.pop_scope();
        symbol_table.push_scope();
        return {};
    }

    [[gnu::always_inline]]
    void end_else(nothing_t) 
    { 
        fn_def.push_stmt({ STMT_END_BLOCK });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    nothing_t begin_do_while(pstring_t pstring) 
    { 
        symbol_table.push_scope();
        fn_def.push_stmt({ STMT_DO, pstring });
        return {};
    }

    [[gnu::always_inline]]
    void end_do_while(nothing_t, pstring_t pstring, expr_temp_t& condition)
    {
        fn_def.push_stmt({ STMT_END_BLOCK });
        fn_def.push_stmt({ STMT_WHILE, pstring, convert_expr(condition) });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    nothing_t begin_while(pstring_t pstring, expr_temp_t& condition)
    {
        symbol_table.push_scope();
        fn_def.push_stmt({ STMT_WHILE, pstring, convert_expr(condition) });
        return {};
    }

    [[gnu::always_inline]]
    void end_while(nothing_t)
    {
        fn_def.push_stmt({ STMT_END_BLOCK });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    expr_temp_t* begin_for(pstring_t pstring,
                           var_decl_t* var_decl, expr_temp_t* init, 
                           expr_temp_t* condition, 
                           expr_temp_t* effect)
    {
        symbol_table.push_scope();

        if(var_decl)
            local_var(*var_decl, init);
        else if(init)
            expr_statement(*init);

        if(condition)
            fn_def.push_stmt({ STMT_WHILE, pstring, convert_expr(*condition) });
        else
            fn_def.push_stmt({ STMT_WHILE, pstring, nullptr });

        symbol_table.push_scope();
        
        return effect;
    }

    [[gnu::always_inline]]
    void end_for(expr_temp_t* effect)
    {
        symbol_table.pop_scope();
        if(effect)
            expr_statement(*effect);
        fn_def.push_stmt({ STMT_END_BLOCK });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    void return_statement(pstring_t pstring, expr_temp_t* expr)
    {
        if(expr)
            fn_def.push_stmt({ STMT_RETURN, pstring, convert_expr(*expr) });
        else
            fn_def.push_stmt({ STMT_RETURN, pstring, nullptr });
    }

    [[gnu::always_inline]]
    void break_statement(pstring_t pstring)
    {
        fn_def.push_stmt({ STMT_BREAK, pstring });
    }

    [[gnu::always_inline]]
    void continue_statement(pstring_t pstring)
    {
        fn_def.push_stmt({ STMT_CONTINUE, pstring });
    }

    [[gnu::always_inline]]
    void label_statement(pstring_t pstring)
    {
        // Create a new label
        label_t* label = stmt_t::new_label();
        label->stmt_h = fn_def.push_stmt(
            { STMT_LABEL, pstring, { .label = label} });

        // Add it to the label map
        auto pair = label_map.emplace(pstring, label);
        if(!pair.second)
        {
            throw compiler_error_t(
                fmt_error(file, pstring, "Label name already in use.")
                + fmt_error(file, pair.first->first, 
                            "Previous definition here:"));
        }

        // Link up the unlinked gotos that jump to this label.
        auto lower = unlinked_gotos.lower_bound(pstring);
        auto upper = unlinked_gotos.upper_bound(pstring);
        for(auto it = lower; it < upper; ++it)
            fn_def[it->second].label = label;
        label->goto_count = std::distance(lower, upper);
        unlinked_gotos.erase(lower, upper);
    }

    [[gnu::always_inline]]
    void goto_statement(pstring_t pstring)
    {
        stmt_ht goto_h = fn_def.push_stmt({ STMT_GOTO, pstring });

        auto it = label_map.find(pstring);
        if(it == label_map.end())
        {
            // Label wasn't defined yet.
            // We'll fill in the jump_h once it is.
            unlinked_gotos.emplace(pstring, goto_h);
        }
        else
        {
            fn_def[goto_h].label = it->second;
            it->second->goto_count += 1;
        }
    }

    [[gnu::always_inline]]
    void goto_mode_statement(pstring_t mode, expr_temp_t& expr)
    {
        fn_def.push_stmt({ STMT_GOTO_MODE, mode, convert_expr(expr) });
    }
};


#endif
