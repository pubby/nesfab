#ifndef PASS1_HPP
#define PASS1_HPP

// First pass of the compiler:
// - Define types.

#include <memory>
#include <variant>
#include <vector>

#include "flat/small_map.hpp"
#include "flat/small_multimap.hpp"

#include "compiler_error.hpp"
#include "globals.hpp"
#include "nothing.hpp"
#include "parser_types.hpp"
#include "symbol_table.hpp"
#include "types.hpp"

class pass1_t
{
private:
    global_manager_t* global_manager_ptr;
    global_t* active_global;
    symbol_table_t symbol_table;
    fc::small_map<pstring_t, label_t*, 4, pstring_less_t> label_map;
    fc::small_multimap<pstring_t, stmt_handle_t, 4, pstring_less_t> 
        unlinked_gotos;
public:
    explicit pass1_t(global_manager_t& globals)
    : global_manager_ptr(&globals)
    {}

    // Helpers
    token_t* convert_expr(expr_temp_t& expr);

    // Functions
    [[gnu::always_inline]]
    void begin_fn(pstring_t fn_name, var_decl_t const* params_begin, 
                  var_decl_t const* params_end, type_t return_type)
    {
        assert(label_map.empty());
        assert(unlinked_gotos.empty());

        // Create the fn global
        active_global = &globals().new_fn(
            fn_name, params_begin, params_end, return_type);

        // Create a scope for the parameters.
        assert(symbol_table.empty());
        symbol_table.push_scope();

        // Add the parameters to the symbol table.
        unsigned const num_params = params_end - params_begin;
        for(unsigned i = 0; i < num_params; ++i)
            symbol_table.new_def(i, params_begin[i].name.view());

        // Create a scope for the fn body.
        symbol_table.push_scope();
    }

    [[gnu::always_inline]]
    void end_fn()
    {
        symbol_table.pop_scope(); // fn body scope
        symbol_table.pop_scope(); // param scope
        label_map.clear();
        assert(symbol_table.empty());
        fn().push_stmt({ STMT_END_BLOCK });

        if(!unlinked_gotos.empty())
        {
            auto it = unlinked_gotos.begin();
            compiler_error(it->first, "Label not in scope.");
        }
    }

    // Global variables
    // TODO
    [[gnu::always_inline]]
    void global_var(var_decl_t const&)
    {
    }

    [[gnu::always_inline]]
    void expr_statement(expr_temp_t& expr)
    {
        fn().push_stmt({ STMT_EXPR, {}, convert_expr(expr) });
    }

    [[gnu::always_inline]]
    void local_var(var_decl_t var_decl, expr_temp_t* expr)
    {
        // Create the var.
        unsigned handle = fn().local_vars.size();
        if(unsigned const* existing = 
           symbol_table.new_def(handle, var_decl.name.view()))
        {
            // Already have a variable defined in this scope.
            throw compiler_error_t(
                fmt_error(var_decl.name, "Identifier already in use.")
                + fmt_error(fn().local_vars[*existing].name, 
                            "Previous declaration here."));
        }
        fn().local_vars.push_back(var_decl);
        fn().push_var_init(handle, expr ? convert_expr(*expr) : nullptr);
    }

    [[gnu::always_inline]]
    nothing_t begin_if(pstring_t pstring, expr_temp_t& condition)
    {
        symbol_table.push_scope();
        fn().stmts.push_back({ STMT_IF, pstring, convert_expr(condition) });
        return {};
    }

    [[gnu::always_inline]]
    void end_if(nothing_t) 
    { 
        fn().push_stmt({ STMT_END_BLOCK });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    nothing_t end_if_begin_else(nothing_t, pstring_t pstring)
    {
        fn().push_stmt({ STMT_END_BLOCK });
        fn().push_stmt({ STMT_ELSE, pstring });
        symbol_table.pop_scope();
        symbol_table.push_scope();
        return {};
    }

    [[gnu::always_inline]]
    void end_else(nothing_t) 
    { 
        fn().push_stmt({ STMT_END_BLOCK });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    nothing_t begin_do_while(pstring_t pstring) 
    { 
        symbol_table.push_scope();
        fn().push_stmt({ STMT_DO, pstring });
        return {};
    }

    [[gnu::always_inline]]
    void end_do_while(nothing_t, pstring_t pstring, expr_temp_t& condition)
    {
        fn().push_stmt({ STMT_END_BLOCK });
        fn().push_stmt({ STMT_WHILE, pstring, convert_expr(condition) });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    nothing_t begin_while(pstring_t pstring, expr_temp_t& condition)
    {
        symbol_table.push_scope();
        fn().push_stmt({ STMT_WHILE, pstring, convert_expr(condition) });
        return {};
    }

    [[gnu::always_inline]]
    void end_while(nothing_t)
    {
        fn().push_stmt({ STMT_END_BLOCK });
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
            fn().push_stmt({ STMT_WHILE, pstring, convert_expr(*condition) });
        else
            fn().push_stmt({ STMT_WHILE, pstring, nullptr });

        symbol_table.push_scope();
        
        return effect;
    }

    [[gnu::always_inline]]
    void end_for(expr_temp_t* effect)
    {
        symbol_table.pop_scope();
        if(effect)
            expr_statement(*effect);
        fn().push_stmt({ STMT_END_BLOCK });
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    void return_statement(pstring_t pstring, expr_temp_t* expr)
    {
        if(expr)
            fn().push_stmt({ STMT_RETURN, pstring, convert_expr(*expr) });
        else
            fn().push_stmt({ STMT_RETURN, pstring, nullptr });
    }

    [[gnu::always_inline]]
    void break_statement(pstring_t pstring)
    {
        fn().push_stmt({ STMT_BREAK, pstring });
    }

    [[gnu::always_inline]]
    void continue_statement(pstring_t pstring)
    {
        fn().push_stmt({ STMT_CONTINUE, pstring });
    }

    [[gnu::always_inline]]
    void label_statement(pstring_t pstring)
    {
        // Create a new label
        label_t* label = globals().new_label();
        label->stmt_h = fn().push_stmt(
            { STMT_LABEL, pstring, { .label = label} });

        // Add it to the label map
        auto pair = label_map.emplace(pstring, label);
        if(!pair.second)
        {
            throw compiler_error_t(
                fmt_error(pstring, "Label name already in use.")
                + fmt_error(pair.first->first, "Previous definition here."));
        }

        // Link up the unlinked gotos that jump to this label.
        auto lower = unlinked_gotos.lower_bound(pstring);
        auto upper = unlinked_gotos.upper_bound(pstring);
        for(auto it = lower; it < upper; ++it)
            fn()[it->second].label = label;
        label->goto_count = std::distance(lower, upper);
        unlinked_gotos.erase(lower, upper);
    }

    [[gnu::always_inline]]
    void goto_statement(pstring_t pstring)
    {
        stmt_handle_t goto_h = fn().push_stmt({ STMT_GOTO, pstring });

        auto it = label_map.find(pstring);
        if(it == label_map.end())
        {
            // Label wasn't defined yet.
            // We'll fill in the jump_h once it is.
            unlinked_gotos.emplace(pstring, goto_h);
        }
        else
        {
            fn()[goto_h].label = it->second;
            it->second->goto_count += 1;
        }
    }

private:
    global_manager_t& globals() { return *global_manager_ptr; }
    fn_t& fn() { return *active_global->fn; }
};


#endif
