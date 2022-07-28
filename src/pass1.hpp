#ifndef PASS1_HPP
#define PASS1_HPP

#include <memory>
#include <variant>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_map.hpp"
#include "flat/small_multimap.hpp"

#include "alloca.hpp"
#include "compiler_error.hpp"
#include "fnv1a.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "parser_decl.hpp"
#include "symbol_table.hpp"
#include "type.hpp"

namespace bc = boost::container;

class pass1_t
{
private:
    file_contents_t const& file;
    global_t* active_global = nullptr;
    global_t::ideps_set_t ideps;
    global_t::ideps_set_t weak_ideps;
    fn_def_t fn_def;
    field_map_t field_map;

    symbol_table_t symbol_table;
    fc::small_map<pstring_t, stmt_ht, 4, pstring_less_t> label_map;
    fc::small_multimap<pstring_t, stmt_ht, 4, pstring_less_t> unlinked_gotos;

    bc::small_vector<bc::small_vector<stmt_ht, 4>, 8> break_stack;
    bc::small_vector<bc::small_vector<stmt_ht, 4>, 8> continue_stack;

    struct nothing_t {};
public:
    explicit pass1_t(file_contents_t const& file) 
    : file(file)
    , label_map(pstring_less_t{ file.source() })
    , unlinked_gotos(pstring_less_t{ file.source() })
    {}

    // Helpers
    char const* source() { return file.source(); }
    void uses_type(type_t type);
    token_t const* convert_expr(expr_temp_t& expr);

    [[gnu::always_inline]]
    void prepare_global()
    {
        assert(ideps.empty());
        assert(weak_ideps.empty());
        assert(label_map.empty());
        assert(unlinked_gotos.empty());
        assert(symbol_table.empty());
    }

    [[gnu::always_inline]]
    void prepare_fn(pstring_t fn_name)
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
    }

    // Functions
    [[gnu::always_inline]]
    var_decl_t fn_decl(pstring_t fn_name, var_decl_t 
                       const* params_begin, var_decl_t const* params_end, 
                       src_type_t return_type)
    {
        // Add the parameters to the symbol table.
        fn_def.num_params = params_end - params_begin;
        for(unsigned i = 0; i < fn_def.num_params; ++i)
        {
            symbol_table.new_def(i, params_begin[i].name.view(source()));
            fn_def.local_vars.push_back(params_begin[i]);
        }

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

        // Track it!
        uses_type(fn_type);

        // Create a scope for the fn body.
        symbol_table.push_scope();

        return { { pstring, fn_type }, fn_name };
    }

    [[gnu::always_inline]]
    void end_fn(var_decl_t decl, fclass_t fclass, type_t using_vars)
    {
        symbol_table.pop_scope(); // fn body scope
        symbol_table.pop_scope(); // param scope
        label_map.clear();
        assert(symbol_table.empty());
        fn_def.push_stmt({ STMT_END_FN, {}, decl.name });

        if(!unlinked_gotos.empty())
        {
            auto it = unlinked_gotos.begin();
            compiler_error(file, it->first, "Label not in scope.");
        }

        // Create the global:
        active_global->define_fn(decl.name, 
                                 std::move(ideps), std::move(weak_ideps),
                                 decl.src_type.type, std::move(fn_def), fclass);//,
                                 //using_vars);
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
        assert(mode_name);

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
            types[i] = params_begin[i].src_type.type;
        types[fn_def.num_params] = TYPE_VOID;
        type_t fn_type = type_t::fn(types, types + fn_def.num_params + 1);

        // Track it!
        uses_type(fn_type);

        // Create a scope for the fn body.
        symbol_table.push_scope();

        assert(mode_name);
        return { .src_type = { mode_name, fn_type }, .name = mode_name };
    }

    [[gnu::always_inline]]
    void end_mode(var_decl_t decl, type_t using_vars)
    {
        assert(decl.name);
        symbol_table.pop_scope(); // mode body scope
        symbol_table.pop_scope(); // param scope
        label_map.clear();
        assert(symbol_table.empty());
        fn_def.push_stmt({ STMT_END_FN });

        if(!unlinked_gotos.empty())
        {
            auto it = unlinked_gotos.begin();
            compiler_error(file, it->first, "Label not in scope.");
        }

        // Create the global:
        assert(decl.name);
        active_global->define_fn(decl.name, 
                                 std::move(ideps), std::move(weak_ideps),
                                 decl.src_type.type, std::move(fn_def), FN_MODE);//,
                                 //using_vars);
        ideps.clear();
        weak_ideps.clear();
    }

    [[gnu::always_inline]]
    pstring_t begin_struct(pstring_t struct_name)
    {
        assert(ideps.empty());
        assert(weak_ideps.empty());

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

    void struct_field(pstring_t struct_name, var_decl_t const& var_decl, expr_temp_t* expr)
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
                    fmt_error(file, var_decl.name, 
                              fmt("Multiple definitions of % in %.", 
                                  var_decl.name.view(source()),
                                  struct_name.view(source())))
                    + fmt_error(file, map_pstring, 
                                "Previous definition here:"));
            }
            else
            {
                throw compiler_error_t(
                    fmt_error(file, var_decl.name, 
                              fmt("Hash collisision! % in %...", 
                                  var_decl.name.view(source()),
                                  struct_name.view(source())))
                    + fmt_error(file, map_pstring, 
                                "...has the same fnv1a hash as:")
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
    void global_var(std::pair<group_vars_t*, group_vars_ht> group, var_decl_t const& var_decl, expr_temp_t* expr_temp)
    {
        uses_type(var_decl.src_type.type);

        token_t const* init_expr = nullptr;
        if(expr_temp)
            init_expr = convert_expr(*expr_temp);

        active_global = &global_t::lookup(file.source(), var_decl.name);
        active_global->define_var(var_decl.name, std::move(ideps), var_decl.src_type, group, init_expr);
        ideps.clear();
    }

    [[gnu::always_inline]]
    void global_const(std::pair<group_data_t*, group_data_ht> group, var_decl_t const& var_decl, expr_temp_t& expr_temp)
    {
        uses_type(var_decl.src_type.type);

        token_t const* init_expr = convert_expr(expr_temp);

        active_global = &global_t::lookup(file.source(), var_decl.name);
        active_global->define_const(var_decl.name, std::move(ideps), var_decl.src_type, group, init_expr);
        ideps.clear();
    }

    [[gnu::always_inline]]
    void expr_statement(expr_temp_t& expr)
    {
        fn_def.push_stmt({ STMT_EXPR, {}, {}, convert_expr(expr) });
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
        fn_def.push_var_init(handle, expr ? convert_expr(*expr) : nullptr, var_decl.src_type.pstring);
        uses_type(var_decl.src_type.type);
    }

    [[gnu::always_inline]]
    stmt_ht begin_if(pstring_t pstring, expr_temp_t& condition)
    {
        symbol_table.push_scope();
        return fn_def.push_stmt({ STMT_IF, {}, pstring, convert_expr(condition) });
    }

    [[gnu::always_inline]]
    void end_if(stmt_ht if_begin) 
    { 
        fn_def[if_begin].link = fn_def.push_stmt({ STMT_END_IF, if_begin }) + 1;
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    stmt_ht end_if_begin_else(stmt_ht if_begin, pstring_t pstring)
    {
        fn_def[if_begin].link = fn_def.push_stmt({ STMT_END_IF, if_begin }) + 1;
        symbol_table.pop_scope();
        symbol_table.push_scope();
        return fn_def.push_stmt({ STMT_ELSE, {}, pstring });
    }

    [[gnu::always_inline]]
    void end_else(stmt_ht else_begin) 
    { 
        fn_def[else_begin].link = fn_def.push_stmt({ STMT_END_IF, else_begin }) + 1;
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    stmt_ht begin_do_while(pstring_t pstring) 
    { 
        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();
        return fn_def.push_stmt({ STMT_DO, {}, pstring });
    }

    [[gnu::always_inline]]
    void end_do_while(stmt_ht begin_do, pstring_t pstring, expr_temp_t& condition)
    {
        stmt_ht const cond = fn_def.push_stmt({ STMT_END_DO, begin_do, pstring, convert_expr(condition) });
        fn_def[begin_do].link = cond;

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = cond + 1;
        for(stmt_ht h : continue_stack.back())
            fn_def[h].link = cond;

        break_stack.pop_back();
        continue_stack.pop_back();
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    stmt_ht begin_while(pstring_t pstring, expr_temp_t& condition)
    {
        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();
        return fn_def.push_stmt({ STMT_WHILE, {}, pstring, convert_expr(condition) });
    }

    [[gnu::always_inline]]
    void end_while(stmt_ht begin_while)
    {
        stmt_ht const exit = fn_def.push_stmt({ STMT_END_WHILE, begin_while });
        fn_def[begin_while].link = exit + 1;

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = exit + 1;
        for(stmt_ht h : continue_stack.back())
            fn_def[h].link = begin_while;

        break_stack.pop_back();
        continue_stack.pop_back();
        symbol_table.pop_scope();
    }

    struct for_d
    {
        stmt_ht begin_for;
        expr_temp_t* effect;
    };

    [[gnu::always_inline]]
    for_d begin_for(pstring_t pstring, var_decl_t* var_decl, expr_temp_t* init, 
                    expr_temp_t* condition, expr_temp_t* effect)
    {
        symbol_table.push_scope();

        if(var_decl)
            local_var(*var_decl, init);
        else if(init)
            expr_statement(*init);

        stmt_ht begin_for;

        if(condition)
            begin_for = fn_def.push_stmt({ STMT_FOR, {}, pstring, convert_expr(*condition) });
        else
            begin_for = fn_def.push_stmt({ STMT_FOR, {}, pstring, nullptr });

        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();
        
        return { begin_for, effect };
    }

    [[gnu::always_inline]]
    void end_for(for_d d)
    {
        stmt_ht const effect = fn_def.push_stmt({ STMT_FOR_EFFECT, {}, {}, d.effect ? convert_expr(*d.effect) : nullptr });
        symbol_table.pop_scope();
        stmt_ht const exit = fn_def.push_stmt({ STMT_END_FOR, d.begin_for });
        fn_def[d.begin_for].link = exit + 1;

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = exit + 1;
        for(stmt_ht h : continue_stack.back())
            fn_def[h].link = effect;

        break_stack.pop_back();
        continue_stack.pop_back();
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    void return_statement(pstring_t pstring, expr_temp_t* expr)
    {
        if(expr)
            fn_def.push_stmt({ STMT_RETURN, {}, pstring, convert_expr(*expr) });
        else
            fn_def.push_stmt({ STMT_RETURN, {}, pstring, nullptr });
    }

    [[gnu::always_inline]]
    void break_statement(pstring_t pstring)
    {
        break_stack.back().push_back(fn_def.push_stmt({ STMT_BREAK, {}, pstring }));
    }

    [[gnu::always_inline]]
    void continue_statement(pstring_t pstring)
    {
        break_stack.back().push_back(fn_def.push_stmt({ STMT_CONTINUE, {}, pstring }));
    }

    [[gnu::always_inline]]
    void label_statement(pstring_t pstring)
    {
        // Create a new label
        stmt_ht label = fn_def.push_stmt({ STMT_LABEL, {}, pstring, { .use_count = 0 } });

        // Add it to the label map
        auto pair = label_map.emplace(pstring, label);
        if(!pair.second)
        {
            throw compiler_error_t(
                fmt_error(file, pstring, "Label name already in use.")
                + fmt_error(file, pair.first->first, "Previous definition here:"));
        }

        // Link up the unlinked gotos that jump to this label.
        auto lower = unlinked_gotos.lower_bound(pstring);
        auto upper = unlinked_gotos.upper_bound(pstring);
        for(auto it = lower; it < upper; ++it)
            fn_def[it->second].link = label + 1;
        fn_def[label].use_count = std::distance(lower, upper);
        unlinked_gotos.erase(lower, upper);
    }

    [[gnu::always_inline]]
    void goto_statement(pstring_t pstring)
    {
        stmt_ht goto_h = fn_def.push_stmt({ STMT_GOTO, {}, pstring });

        auto it = label_map.find(pstring);
        if(it == label_map.end())
        {
            // Label wasn't defined yet.
            // We'll fill in the jump_h once it is.
            unlinked_gotos.emplace(pstring, goto_h);
        }
        else
        {
            fn_def[goto_h].link = it->second + 1;
            fn_def[it->second].use_count += 1;
        }
    }

    [[gnu::always_inline]]
    void goto_mode_statement(pstring_t mode, expr_temp_t& expr)
    {
        fn_def.push_stmt({ STMT_GOTO_MODE, {}, mode, convert_expr(expr) });
    }
};


#endif
