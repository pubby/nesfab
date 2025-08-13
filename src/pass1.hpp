#ifndef PASS1_HPP
#define PASS1_HPP

#include <memory>
#include <variant>
#include <vector>

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
#include "eternal_new.hpp"
#include "puf.hpp"
#include "convert.hpp"
#include "macro.hpp"
#include "ident_map.hpp"
#include "mapfab.hpp"
#include "xfab.hpp"

namespace bc = boost::container;

struct macro_todo_t
{
    pstring_t at;
    macro_invocation_t invoke;
    std::unique_ptr<mods_t> mods;
};

struct mapfab_todo_t
{
    lpstring_t at;
    mapfab_convert_type_t ct;
    std::vector<std::uint8_t> data;
    fs::path path;
    mapfab_macros_t macros;
    std::unique_ptr<mods_t> mods;
};

struct xfab_todo_t
{
    lpstring_t at;
    xfab_convert_type_t ct;
    std::vector<std::uint8_t> data;
    fs::path path;
    xfab_macros_t macros;
    std::unique_ptr<mods_t> mods;
};

class pass1_t
{
private:
    file_contents_t const& file;
    global_t* active_global = nullptr;
    ideps_map_t ideps;
    fn_def_t fn_def;
    field_map_t field_map;
    std::string_view prev_label_name;
    unsigned num_minor_labels = 0;

    symbol_table_t symbol_table;
    ident_map_t<global_ht> private_globals;
    ident_map_t<group_ht> private_groups;
    fc::small_map<pstring_t, stmt_ht, 4, pstring_less_t> label_map;
    fc::small_multimap<pstring_t, stmt_ht, 4, pstring_less_t> unlinked_gotos;

    bc::deque<macro_todo_t> todo_macros;
    bc::deque<mapfab_todo_t> todo_mapfabs;
    bc::deque<xfab_todo_t> todo_xfabs;

    struct switch_info_t
    {
        stmt_ht prev_case;
        stmt_ht default_case;
    };

    bc::small_vector<bc::small_vector<stmt_ht, 4>, 8> break_stack;
    bc::small_vector<bc::small_vector<stmt_ht, 4>, 8> continue_stack;
    bc::small_vector<switch_info_t, 4> switch_stack;

    struct nothing_t {};

public:
    explicit pass1_t(file_contents_t const& file) 
    : file(file)
    , label_map(pstring_less_t{ file.source() })
    , unlinked_gotos(pstring_less_t{ file.source() })
    {
        if(file.private_globals())
            private_globals = *file.private_globals();
        if(file.private_groups())
            private_groups = *file.private_groups();
    }

    ~pass1_t()
    {
        finish_macros();
        finish_mapfabs();
    }

    // Helpers
    char const* source() { return file.source(); }
    void uses_type(type_t type, idep_class_t calc = IDEP_TYPE);
    void uses_charmap(global_t const* charmap, idep_class_t calc = IDEP_TYPE);
    ast_node_t* eternal_expr(ast_node_t const* expr);
    ast_node_t* convert_eternal_expr(ast_node_t const* expr, idep_class_t calc = IDEP_VALUE);
    void convert_ast(ast_node_t& ast, idep_class_t calc, idep_class_t depends_on = IDEP_VALUE);
    global_t& lookup_global(pstring_t pstring);
    group_t* lookup_group(pstring_t pstring);
    fn_set_t& lookup_fn_set(lpstring_t pstring);

    void begin_global_var()
    {
        assert(symbol_table.empty());
        symbol_table.push_scope();
        prev_label_name = {};
    }

    void end_global_var()
    {
        symbol_table.pop_scope();
        assert(symbol_table.empty());
    }

    void prepare_global()
    {
        assert(ideps.empty());
        assert(label_map.empty());
        assert(unlinked_gotos.empty());
    }

    [[gnu::always_inline]]
    global_t* prepare_var_init(pstring_t name)
    {
        // Reset the fn_def and iasm:
        fn_def = fn_def_t();
        num_minor_labels = 0;

        // Find the global:
        active_global = &lookup_global(name);

        return active_global;
    }

    [[gnu::always_inline]]
    global_t* prepare_fn(pstring_t fn_name, lpstring_t fn_set_name)
    {
        assert(ideps.empty());
        assert(label_map.empty());
        assert(unlinked_gotos.empty());

        // Reset the fn_def and iasm:
        fn_def = fn_def_t();
        num_minor_labels = 0;

        // Find the global
        if(fn_set_name)
        {
            fn_set_t& set = lookup_fn_set(fn_set_name);
            active_global = &set.lookup(source(), fn_name);
        }
        else
            active_global = &lookup_global(fn_name);

        // Create a scope for the parameters.
        assert(symbol_table.empty());
        symbol_table.push_scope();

        return active_global;
    }

    // implementation detail:
    int _add_symbol(var_decl_t const& var_decl, bool local_const,
                    std::unique_ptr<mods_t> mods = {},
                    ast_node_t* local_const_expr = nullptr)
    {
        assert(!local_const_expr || local_const);

        int const handle = local_const ? -int(fn_def.local_consts.size())-1 : int(fn_def.local_vars.size());
        assert((handle < 0) == local_const);
        if(int const* existing = symbol_table.new_def(handle, var_decl.name.view(source())))
        {
            // Already have a variable defined in this scope.
            throw compiler_error_t(
                fmt_error(var_decl.name, 
                          fmt("Identifier % already in use.", 
                              var_decl.name.view(source())), &file)
                + fmt_note(fn_def.var_decl(*existing).name, 
                           "Previous definition here:", &file));
        }

        if(local_const)
        {
            fn_def.local_consts.emplace_back(var_decl, std::move(mods), eternal_expr(local_const_expr));
            fn_def.name_hashes.push_back(fnv1a<std::uint64_t>::hash(var_decl.name.view(file.source())));
        }
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
            pstring = pstring ? concat(params_begin[i].src_type.pstring, pstring) : pstring;
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

    mod_flags_t fn_mods(fn_class_t fclass)
    {
        switch(fclass)
        {
        default:      return 0;
        case FN_CT:   return 0;
        case FN_FN:   return MOD_zero_page | MOD_align | MOD_inline | MOD_graphviz | MOD_static | MOD_info | MOD_sloppy | MOD_static_fixed;
        case FN_MODE: return MOD_zero_page | MOD_align | MOD_graphviz | MOD_static | MOD_info | MOD_sloppy | MOD_static_fixed;
        case FN_NMI:  return MOD_zero_page | MOD_align | MOD_graphviz | MOD_static | MOD_info | MOD_sloppy | MOD_static_fixed;
        case FN_IRQ:  return MOD_zero_page | MOD_align | MOD_graphviz | MOD_static | MOD_info | MOD_sloppy | MOD_solo_interrupt | MOD_static_fixed;
        }
    }

    [[gnu::always_inline]]
    void end_fn(var_decl_t decl, unsigned line, fn_class_t fclass, lpstring_t fn_set_name, std::unique_ptr<mods_t> mods)
    {
        // Convert local consts now:
        for(auto& c : fn_def.local_consts)
            if(c.expr)
                convert_ast(*const_cast<ast_node_t*>(c.expr), IDEP_TYPE);

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

        if(mods)
        {
            mods->validate(
                decl.name, 
                fn_mods(fclass), // flags
                fclass == FN_CT ? 0 : (MODL_VARS | MODL_DATA | MODL_EMPLOYS_ANY), // lists
                fclass == FN_MODE // nmi / irq
                );
        }

        fn_set_t* fn_set = nullptr;
        if(fn_set_name)
            fn_set = &lookup_fn_set(fn_set_name);

        // Create the global:
        active_global->define_fn(
            extend(decl.name, line), std::move(ideps),
            decl.src_type.type, std::move(fn_def), std::move(mods), fclass, false, fn_set);
        ideps.clear();
    }

    [[gnu::always_inline]]
    void end_asm_fn(
        var_decl_t decl, unsigned line, fn_class_t fclass, lpstring_t fn_set_name, ast_node_t ast,
        std::unique_ptr<mods_t> mods)
    {
        assert(ast.token.type == lex::TOK_byte_block_proc
               || ast.token.type == lex::TOK_byte_block_data);
        ast.token.type = lex::TOK_byte_block_proc;

        //Convert all expressions
        for(auto& c : fn_def.local_consts)
            if(c.expr)
                convert_ast(*const_cast<ast_node_t*>(c.expr), IDEP_TYPE);
        fn_def.push_stmt({ STMT_EXPR, {}, {}, {}, convert_eternal_expr(&ast) });
        fn_def.push_stmt({ STMT_END_FN, {}, {}, decl.name });

        if(fn_def.local_vars.size() > MAX_ASM_LOCAL_VARS)
            compiler_error(decl.name, fmt("Too many local variables. Max %.", MAX_ASM_LOCAL_VARS));

        symbol_table.pop_scope(); // param scope

        label_map.clear();
        assert(symbol_table.empty());

        if(fclass != FN_FN && (fclass != FN_IRQ || !mod_test(mods.get(), MOD_solo_interrupt)))
            compiler_error(decl.name, fmt("% does not support inline assembly.", fn_class_keyword(fclass)));

        if(mods)
        {
            mods->validate(
                decl.name, 
                fn_mods(fclass), // flags
                MODL_VARS | MODL_DATA | MODL_EMPLOYS_ANY, // lists
                false // nmi
                );
        }

        fn_set_t* fn_set = nullptr;
        if(fn_set_name)
            fn_set = &lookup_fn_set(fn_set_name);

        // Create the global:
        active_global->define_fn(
            extend(decl.name, line), std::move(ideps),
            decl.src_type.type, std::move(fn_def), std::move(mods), fclass, true, fn_set);
        ideps.clear();
    }

    [[gnu::always_inline]]
    void asm_fn_var(var_decl_t const& var_decl, std::unique_ptr<mods_t> mods)
    {
        if(mods)
        {
            mods->validate(
                var_decl.name, 
                MOD_zero_page // flags
                );
        }

        _add_symbol(var_decl, false, std::move(mods));
        uses_type(var_decl.src_type.type);
    }

    void byte_block_named_value(pstring_t at, char const* orig_name, ssa_value_t value)
    {
        char const* name;
        std::string name_str;

        if(prev_label_name.size())
        {
            name_str = prev_label_name;
            name_str.push_back('_');
            name_str += orig_name;
            name = name_str.data();
        }
        else
            name = orig_name;

        int const handle = -int(fn_def.local_consts.size())-1;

        if(int const* existing = symbol_table.new_def(handle, name))
        {
            // Already have a variable defined in this scope.
            throw compiler_error_t(
                fmt_error(at, fmt("Identifier % (defined from file) already in use.", name, &file))
                + fmt_note(fn_def.var_decl(*existing).name, "Previous definition here:", &file));
        }

        var_decl_t const decl = {{ at, value.type() }, at };
        ast_node_t const ast = {{ .type = lex::TOK_ssa, .pstring = at, .value = value.target() }};

        assert(fn_def.local_consts.size() == fn_def.name_hashes.size());
        fn_def.local_consts.emplace_back(std::move(decl), std::unique_ptr<mods_t>(), eternal_expr(&ast));
        fn_def.name_hashes.push_back(fnv1a<std::uint64_t>::hash(name));
    }

    [[gnu::always_inline]]
    void byte_block_sub_proc(pstring_t at, asm_proc_t& proc, global_ht global)
    {
        assert(fn_def.local_consts.size() == fn_def.name_hashes.size());

        unsigned const prev_minor = num_minor_labels;
        unsigned new_minor = prev_minor;

        unsigned const prev_size = fn_def.local_consts.size();
        unsigned new_size = prev_size;

        for(asm_inst_t& inst : proc.code)
        {
            assert(inst.alt.lclass() != LOC_MINOR_LABEL);
            assert(inst.alt.lclass() != LOC_NAMED_LABEL);

            if(inst.arg.lclass() == LOC_MINOR_LABEL)
            {
                unsigned const i = prev_minor + inst.arg.data();
                new_minor = std::max(new_minor, i);
                inst.arg.set_data(i);
            }
            else if(inst.arg.lclass() == LOC_NAMED_LABEL && inst.arg.global() == global)
            {
                unsigned const i = prev_size + inst.arg.data();
                new_size = std::max(new_size, i);
                inst.arg.set_data(i);
            }
        }

        var_decl_t const decl = {{ at, type_t::addr(true) }, at };

        assert(new_size >= prev_size);
        while(fn_def.local_consts.size() < new_size)
            fn_def.local_consts.emplace_back(decl, std::unique_ptr<mods_t>());
        fn_def.name_hashes.resize(new_size, 0);

        num_minor_labels = new_minor;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_label(pstring_t label, global_ht global, group_ht group, 
                                bool is_vars, bool is_default, bool is_banked, bool is_chrrom, std::unique_ptr<mods_t> mods)
    {
        prev_label_name = label.view(source());

        type_t type;
        if(group)
        {
            if(is_vars)
                type = type_t::ptr(group, is_banked ? TYPE_BANKED_MPTR : TYPE_MPTR);
            else
                type = type_t::ptr(group, is_banked ? TYPE_BANKED_CPTR : TYPE_CPTR);
        }
        else if(is_chrrom)
            type = TYPE_INT;
        else
            type = type_t::addr(is_banked);

        int const i = -_add_symbol({{ label, type }, label }, true)-1;
        assert(i >= 0);

        if(is_default)
        {
            if(fn_def.default_label != ENTRY_LABEL)
                compiler_error(label, "Multiple default labels.");
            fn_def.default_label = i;
        }

        if(mods)
            mods->validate(label);

        assert(active_global);
        std::uint64_t const value = i | (std::uint64_t(global.id) << 32);

        ast_node_t ast = { .token = { .type = lex::TOK_byte_block_label, .pstring = label, .value = value }};
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
            op = get_op(name, buggy_equivalent(mode));
        if(!op)
            compiler_error(pstring, fmt("% lacks addressing mode %.", to_string(name), to_string(mode)));

        return ast_node_t
        { 
            .token = { .type = lex::TOK_byte_block_asm_op, .pstring = pstring, .value = op },
            .children = eternal_expr(expr) // Don't convert yet. Wait until every label has been processed.
        };
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_call(lex::token_type_t tt, pstring_t pstring, ast_node_t call, std::unique_ptr<mods_t> mods)
    {
        ast_node_t ast = { .token = token_t::make_ptr(tt, pstring, &mods), .children = eternal_expr(&call) };

        if(tt == lex::TOK_byte_block_goto_mode)
        {
            if(mods)
            {
                mods->validate(
                    pstring, 
                    0, // flags
                    MODL_PRESERVES // lists
                    );
            }

            if(!mods || !(mods->explicit_lists & MODL_PRESERVES))
                compiler_error(pstring, "Missing preserves modifier.");
        }
        else if(mods)
            mods->validate(pstring);

        return ast;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_wait_nmi(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        ast_node_t ast = { .token = { .type = lex::TOK_byte_block_wait_nmi, .pstring = pstring }};

        if(mods)
        {
            mods->validate(pstring);
            ast.mods = eternal_emplace<mods_t>(std::move(*mods));
        }

        return ast;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_bank_switch(pstring_t pstring, lex::token_type_t tt, std::unique_ptr<mods_t> mods)
    {
        ast_node_t ast = { .token = { .type = tt, .pstring = pstring }};

        if(mods)
        {
            mods->validate(pstring);
            ast.mods = eternal_emplace<mods_t>(std::move(*mods));
        }

        return ast;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_push(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        ast_node_t ast = { .token = { .type = lex::TOK_byte_block_push, .pstring = pstring }};

        if(mods)
        {
            mods->validate(pstring);
            ast.mods = eternal_emplace<mods_t>(std::move(*mods));
        }

        return ast;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_pop(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        ast_node_t ast = { .token = { .type = lex::TOK_byte_block_pop, .pstring = pstring }};

        if(mods)
        {
            mods->validate(pstring);
            ast.mods = eternal_emplace<mods_t>(std::move(*mods));
        }

        return ast;
    }

    [[gnu::always_inline]]
    ast_node_t byte_block_if(pstring_t pstring, ast_node_t* children, unsigned num_children, std::unique_ptr<mods_t> mods)
    {
        if(num_children > 0)
            convert_ast(children[0], IDEP_TYPE);

        for(unsigned i = 0; i < num_children; ++i)
            children[i].num_children();

        return ast_node_t
        { 
            .token = { .type = lex::TOK_byte_block_if, .pstring = pstring, .value = num_children },
            .children = children,
        };
    }

    [[gnu::always_inline]]
    void local_ct(var_decl_t const& var_decl, ast_node_t& ast, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(var_decl.name);

        // We'll save the expr, but *don't* convert it yet.
        int i = _add_symbol(var_decl, true, std::move(mods), &ast);
        assert(i < 0);
    }

    [[gnu::always_inline]]
    lpstring_t begin_struct(lpstring_t struct_name)
    {
        assert(ideps.empty());

        // Reset the struct:
        field_map.clear();

        // Find the global
        active_global = &lookup_global(struct_name);

        return struct_name;
    }

    void end_struct(lpstring_t struct_name)
    {
        active_global->define_struct(
            struct_name, std::move(ideps), std::move(field_map));
        ideps.clear();
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
                    + fmt_note(map_pstring, "Previous definition here:", &file));
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
    defined_group_vars_t begin_group_vars(lpstring_t group_name)
    {
        if(group_t* group = lookup_group(group_name))
            return group->define_vars(group_name);
        else
            return {};
    }

    [[gnu::always_inline]]
    defined_group_data_t begin_group_data(lpstring_t group_name, bool omni)
    {
        if(group_t* group = lookup_group(group_name))
            return group->define_data(group_name, omni);
        else
            return {};
    }

    [[gnu::always_inline]]
    void end_group()
    {}

    [[gnu::always_inline]]
    void global_var(defined_group_vars_t const& d, var_decl_t const& var_decl, 
                    unsigned line, ast_node_t* expr, std::unique_ptr<mods_t> mods)
    {
        uses_type(var_decl.src_type.type);

        if(mods)
        {
            mods->validate(var_decl.name, MOD_zero_page | MOD_align | MOD_sram | MOD_unused);
            if(((mods->enable) & MOD_sram) && !mapper().sram)
                compiler_warning(var_decl.name, "sram modifier set, but mapper does not use SRAM.");
        }

        std::unique_ptr<paa_def_t> paa_def;
        if(is_paa(var_decl.src_type.type.name()))
            paa_def = std::make_unique<paa_def_t>(std::move(fn_def.local_consts), std::move(fn_def.name_hashes));

        active_global = &lookup_global(var_decl.name);
        active_global->define_var(
            extend(var_decl.name, line), std::move(ideps), var_decl.src_type, d, convert_eternal_expr(expr, IDEP_TYPE),
            std::move(paa_def), std::move(mods));
        ideps.clear();
    }

    [[gnu::always_inline]]
    void global_const(bool in_group, defined_group_data_t const& d, bool omni, var_decl_t const& var_decl, 
                      unsigned line, ast_node_t const& expr, std::unique_ptr<mods_t> mods)
    {
        if(in_group && !is_paa(var_decl.src_type.type.name()))
            compiler_error(var_decl.name, "Expecting pointer-addressable array inside 'data' block.");

        uses_type(var_decl.src_type.type);

        if(mods)
            mods->validate(var_decl.name, MOD_align | MOD_dpcm | MOD_static | MOD_sector | MOD_unused | MOD_static_fixed);

        std::unique_ptr<paa_def_t> paa_def;
        if(is_paa(var_decl.src_type.type.name()))
            paa_def = std::make_unique<paa_def_t>(std::move(fn_def.local_consts), std::move(fn_def.name_hashes));

        active_global = &lookup_global(var_decl.name);
        active_global->define_const(
            extend(var_decl.name, line), std::move(ideps), var_decl.src_type, d, omni, nullptr, convert_eternal_expr(&expr, IDEP_TYPE), 
            std::move(paa_def), std::move(mods));
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

        if(mods)
            mods->validate(pstring);

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

        if(mods)
            mods->validate(pstring);

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

    struct while_d : flow_d 
    {
        ast_node_t const* condition;
        pstring_t pstring;
    };

    [[gnu::always_inline]]
    while_d begin_while(bool is_do, pstring_t pstring, ast_node_t const& condition, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();

        if(mods)
            mods->validate(pstring, MOD_unroll | MOD_unloop);
        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        stmt_ht begin_while;

        if(is_do)
            begin_while = fn_def.push_stmt({ STMT_DO_WHILE, mods_h, {}, pstring });
        else
            begin_while = fn_def.push_stmt({ STMT_WHILE, mods_h, {}, pstring, convert_eternal_expr(&condition) });

        return { begin_while, mods_h, &condition, pstring };
    }

    [[gnu::always_inline]]
    void end_while(bool is_do, while_d d)
    {
        stmt_ht exit;

        if(is_do)
        {
            exit = fn_def.push_stmt({ STMT_END_DO_WHILE, d.mods, d.begin, d.pstring, convert_eternal_expr(d.condition) });
            fn_def[d.begin].link = exit;
        }
        else
        {
            exit = fn_def.push_stmt({ STMT_END_WHILE, d.mods, d.begin });
            fn_def[d.begin].link = exit + 1;
        }

        assert(break_stack.size());
        assert(continue_stack.size());

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = exit + 1;
        for(stmt_ht h : continue_stack.back())
            fn_def[h].link = is_do ? exit : d.begin;

        break_stack.pop_back();
        continue_stack.pop_back();
        symbol_table.pop_scope();
    }

    struct for_d : flow_d
    {
        ast_node_t* condition;
        ast_node_t* effect;
        pstring_t pstring;
    };

    [[gnu::always_inline]]
    for_d begin_for(bool is_do, pstring_t pstring, var_decl_t* var_decl, ast_node_t* init, 
                    ast_node_t* condition, ast_node_t* effect, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();

        if(var_decl)
            local_var(*var_decl, init);
        else if(init)
            expr_statement(*init);

        if(mods)
            mods->validate(pstring, MOD_unroll | MOD_unloop);

        stmt_ht begin_for;
        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));

        if(is_do)
            begin_for = fn_def.push_stmt({ STMT_DO_FOR, mods_h, {}, pstring });
        else
            begin_for = fn_def.push_stmt({ STMT_FOR, mods_h, {}, pstring, convert_eternal_expr(condition) });

        symbol_table.push_scope();
        break_stack.emplace_back();
        continue_stack.emplace_back();
        
        return { begin_for, mods_h, condition, effect, pstring };
    }

    [[gnu::always_inline]]
    void end_for(bool is_do, for_d d)
    {
        stmt_ht const effect = fn_def.push_stmt({ STMT_FOR_EFFECT, d.mods, d.begin, d.pstring, convert_eternal_expr(d.effect) });
        symbol_table.pop_scope();

        stmt_ht exit;
        if(is_do)
        {
            exit = fn_def.push_stmt({ STMT_END_DO_FOR, d.mods, d.begin, d.pstring, convert_eternal_expr(d.condition) });
            fn_def[d.begin].link = exit;
        }
        else
        {
            exit = fn_def.push_stmt({ STMT_END_FOR, d.mods, d.begin });
            fn_def[d.begin].link = exit + 1;
        }

        assert(break_stack.size());
        assert(continue_stack.size());

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
        if(mods)
            mods->validate(pstring);
        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        fn_def.push_stmt({ STMT_RETURN, mods_h, {}, pstring, convert_eternal_expr(expr) });
    }

    [[gnu::always_inline]]
    void break_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(pstring);

        if(break_stack.empty())
            compiler_error(pstring, "break cannot be used here.");

        break_stack.back().push_back(fn_def.push_stmt(
            { STMT_BREAK, fn_def.push_mods(std::move(mods)), {}, pstring }));
    }

    [[gnu::always_inline]]
    void continue_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(pstring);

        if(continue_stack.empty())
            compiler_error(pstring, "continue cannot be used here.");

        continue_stack.back().push_back(fn_def.push_stmt(
            { STMT_CONTINUE, fn_def.push_mods(std::move(mods)), {}, pstring }));
    }

    [[gnu::always_inline]]
    void begin_switch(pstring_t pstring, ast_node_t const& condition, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();
        break_stack.emplace_back();

        if(mods)
            mods->validate(pstring);

        stmt_mods_ht const mods_h = fn_def.push_mods(std::move(mods));
        stmt_ht const begin_switch = fn_def.push_stmt({ STMT_SWITCH, mods_h, {}, pstring, convert_eternal_expr(&condition) });

        switch_stack.push_back({ .prev_case = begin_switch });
    }

    [[gnu::always_inline]]
    void end_switch(pstring_t pstring)
    {
        if(!switch_stack.back().default_case)
        {
            begin_default_label(pstring, {});
            end_label();
            assert(switch_stack.back().default_case);
        }

        stmt_ht const exit = fn_def.push_stmt({ STMT_END_SWITCH });
        
        fn_def[switch_stack.back().prev_case].link = switch_stack.back().default_case;
        fn_def[switch_stack.back().default_case].link = exit;

        for(stmt_ht h : break_stack.back())
            fn_def[h].link = exit + 1;

        break_stack.pop_back();
        switch_stack.pop_back();
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    void begin_case_label(pstring_t pstring, ast_node_t const& expr, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();

        if(mods)
            mods->validate(pstring);

        // Create a new label
        stmt_ht label = fn_def.push_stmt(
            { STMT_CASE, fn_def.push_mods(std::move(mods)), {}, pstring, convert_eternal_expr(&expr) });

        if(switch_stack.empty())
            compiler_error(pstring, "case without switch.");

        fn_def[switch_stack.back().prev_case].link = label;
        switch_stack.back().prev_case = label;
    }

    [[gnu::always_inline]]
    void begin_default_label(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();

        if(mods)
            mods->validate(pstring);

        if(switch_stack.back().default_case)
            compiler_error(pstring, "Multiple default labels inside switch.");

        // Create a new label
        switch_stack.back().default_case = fn_def.push_stmt(
            { STMT_DEFAULT, fn_def.push_mods(std::move(mods)), {}, pstring });

        if(switch_stack.empty())
            compiler_error(pstring, "default without switch.");
    }

    [[gnu::always_inline]]
    void begin_label(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        symbol_table.push_scope();

        if(mods)
            mods->validate(pstring);

        // Create a new label
        stmt_ht label = fn_def.push_stmt(
            { STMT_LABEL, fn_def.push_mods(std::move(mods)), {}, pstring, { .use_count = 0 } });

        // Add it to the label map
        auto pair = label_map.emplace(pstring, label);
        if(!pair.second)
        {
            throw compiler_error_t(
                fmt_error(pstring, "Label name already in use.", &file)
                + fmt_note(pair.first->first, "Previous definition here:", &file));
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

    void end_label()
    {
        symbol_table.pop_scope();
    }

    [[gnu::always_inline]]
    void goto_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(pstring);
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
            passert(fn_def[it->second].name == STMT_LABEL, fn_def[it->second].name);

            fn_def[goto_h].link = it->second;
            fn_def[it->second].use_count += 1;
        }
    }

    [[gnu::always_inline]]
    void goto_mode_statement(pstring_t mode, ast_node_t const& expr, std::unique_ptr<mods_t> mods)
    {
        if(mods)
        {
            mods->validate(
                mode, 
                0, // flags
                MODL_PRESERVES // list
                );
        }

        if(!mods || !(mods->explicit_lists & MODL_PRESERVES))
            compiler_error(mode, "Missing preserves modifier.");

        fn_def.push_stmt({ STMT_GOTO_MODE, fn_def.push_mods(std::move(mods)), {}, mode, convert_eternal_expr(&expr) });
    }

    [[gnu::always_inline]]
    void nmi_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(pstring);
        fn_def.push_stmt({ STMT_NMI, fn_def.push_mods(std::move(mods)), {}, pstring });
    }

    [[gnu::always_inline]]
    void irq_statement(pstring_t pstring, ast_node_t const& expr, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(pstring);
        fn_def.push_stmt({ STMT_IRQ, fn_def.push_mods(std::move(mods)), {}, pstring, convert_eternal_expr(&expr) });
    }

    [[gnu::always_inline]]
    void fence_statement(pstring_t pstring, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(pstring);
        fn_def.push_stmt({ STMT_FENCE, fn_def.push_mods(std::move(mods)), {}, pstring });
    }

    [[gnu::always_inline]]
    void swap_statement(pstring_t pstring, std::unique_ptr<mods_t> mods, ast_node_t const& a, ast_node_t const& b)
    {
        if(mods)
            mods->validate(pstring);
        fn_def.push_stmt({ STMT_SWAP_FIRST, fn_def.push_mods(std::move(mods)), {}, pstring, convert_eternal_expr(&a) });
        fn_def.push_stmt({ STMT_SWAP_SECOND, {}, {}, pstring, convert_eternal_expr(&b) });
    }

    [[gnu::always_inline]]
    void macro(pstring_t at, macro_invocation_t&& invoke, std::unique_ptr<mods_t> mods)
    {
        if(mods)
            mods->validate(at, MOD_fork_scope);
        todo_macros.push_back({ at, std::move(invoke), std::move(mods) });
    }

    void finish_macros()
    {
        for(macro_todo_t& tm : todo_macros)
        {
            try 
            { 
                if(mod_test(tm.mods.get(), MOD_fork_scope))
                    invoke_macro(std::move(tm.invoke), private_globals, private_groups); 
                else
                    invoke_macro(std::move(tm.invoke)); 
            }
            catch(macro_error_t const& e) 
            { 
                throw compiler_error_t(
                    fmt_error(tm.at, "While parsing macro file...")
                    + fmt_error(e.pstring, e.what()));
            }
            catch(std::exception const& e) { compiler_error(tm.at, e.what()); }
            catch(...) { throw; }
        }
    }

    void charmap(lpstring_t charmap_name, bool is_default, 
                 string_literal_t const& characters, 
                 string_literal_t const& sentinel, 
                 unsigned offset,
                 std::unique_ptr<mods_t> mods)
    {
        using namespace std::literals;

        if(mods)
        {
            mods->validate(
                charmap_name, 
                0, // flags
                MODL_STOWS // lists
                );
        }

        if(is_default)
            active_global = &global_t::default_charmap(charmap_name);
        else
            active_global = &lookup_global(charmap_name);

        assert(active_global);

        active_global->define_charmap(
            charmap_name, is_default, characters, sentinel, offset, std::move(mods));
        ideps.clear();
    }

    global_t const& lookup_charmap(pstring_t at, pstring_t name = {})
    {
        global_t const* ret;
        if(name)
            ret = &lookup_global(name);
        else
            ret = &global_t::default_charmap(extend(at));

        uses_charmap(ret);

        assert(ret);
        return *ret;
    }
    
    void begin_chrrom()
    {
        symbol_table.push_scope();
    }

    void end_chrrom(global_t* global, lpstring_t decl, ast_node_t& ast, 
                std::unique_ptr<mods_t> mods, ast_node_t* expr)
    {
        if(mods)
            mods->validate(decl);

        std::unique_ptr<paa_def_t> paa_def = std::make_unique<paa_def_t>(
            std::move(fn_def.local_consts), std::move(fn_def.name_hashes));

        active_global = global;
        active_global->define_const(
            decl, std::move(ideps), { decl, type_t::paa(0, {}) }, {}, false, convert_eternal_expr(expr),
            convert_eternal_expr(&ast), std::move(paa_def), std::move(mods));
        ideps.clear();
        symbol_table.pop_scope();
    }

    fs::path get_path(fs::path const& preferred_dir, convert_arg_t const& v) const
    {
        string_literal_t filename = v.filename();
        fs::path path;
        if(!resource_path(preferred_dir, fs::path(filename.string), path))
            compiler_error(filename.pstring, fmt("Missing file: %", filename.string));
        return std::filesystem::absolute(path);
    }

    void audio(lpstring_t decl, pstring_t script, fs::path preferred_dir, std::vector<convert_arg_t> args, std::unique_ptr<mods_t> mods)
    {
        using namespace std::literals;

        if(mods)
            mods->validate(decl);

        auto const check_argn = [&](unsigned expected)
        {
            if(args.size() != expected)
                compiler_error(decl, fmt("Wrong number of arguments. Expecting %.", expected + 1));
        };

        std::string_view const view = script.view(source());

        try
        {
            if(view == "puf1_music"sv)
            {
                if(args.empty())
                    convert_puf_music(nullptr, 0, decl);
                else
                {
                    check_argn(1);
                    std::vector<std::uint8_t> txt_data = read_binary_file(get_path(preferred_dir, args[0]).string(), decl);
                    convert_puf_music(reinterpret_cast<char const*>(txt_data.data()), txt_data.size(), decl);
                }
            }
            else if(view == "puf1_sfx"sv)
            {
                if(args.empty())
                    convert_puf_sfx(nullptr, 0, nullptr, 0, decl);
                else
                {
                    check_argn(2);
                    std::vector<std::uint8_t> txt_data = read_binary_file(get_path(preferred_dir, args[0]).string(), decl);
                    std::vector<std::uint8_t> nsf_data = read_binary_file(get_path(preferred_dir, args[1]).string(), decl);
                    convert_puf_sfx(reinterpret_cast<char const*>(txt_data.data()), txt_data.size(), 
                                    nsf_data.data(), nsf_data.size(), 
                                    decl);
                }
            }
            else
                compiler_error(script, fmt("Unknown audio format: %", view));
        }
        catch(compiler_error_t const& e)
        {
            throw e;
        }
        catch(std::exception const& e)
        {
            compiler_error(script, e.what());
        }
    }

    void mapfab(lpstring_t decl, pstring_t script, fs::path preferred_dir, std::vector<convert_arg_t> args, std::unique_ptr<mods_t> mods)
    {
        using namespace std::literals;

        if(mods)
            mods->validate(decl, MOD_fork_scope);

        auto const check_argn = [&](unsigned expected)
        {
            if(args.size() != expected)
                compiler_error(decl, fmt("Wrong number of arguments. Expecting %.", expected + 1));
        };

        std::string_view const view = script.view(source());

        auto const get_macro = [&](unsigned i) -> std::string
        {
            if(auto const* str = std::get_if<string_literal_t>(&args[i].value))
                return str->string;
            compiler_error(args[i].pstring, "Expecting macro name string literal as argument.");
        };

        check_argn(5);

        mapfab_macros_t mm;
        mm.chr = get_macro(1);
        mm.palette = get_macro(2);
        mm.metatiles = get_macro(3);
        mm.level = get_macro(4);

        try
        {
            mapfab_convert_type_t ct = MAPFAB_INVALID;

            if(view == "raw"sv)
                ct = MAPFAB_RAW;
            else if(view == "rlz"sv)
                ct = MAPFAB_RLZ;
            else if(view == "pbz"sv)
                ct = MAPFAB_PBZ;
            else if(view == "mmt_32"sv)
                ct = MAPFAB_MMT_32;

            if(ct == MAPFAB_INVALID)
                compiler_error(script, fmt("Unknown MapFab format: %", view));

            fs::path mapfab_path = get_path(preferred_dir, args[0]);
            std::vector<std::uint8_t> data = read_binary_file(mapfab_path.string(), decl);

            todo_mapfabs.push_back({
                decl,
                ct,
                std::move(data),
                std::move(mapfab_path),
                std::move(mm),
                std::move(mods),
            });
        }
        catch(compiler_error_t const& e)
        {
            throw e;
        }
        catch(std::exception const& e)
        {
            compiler_error(script, e.what());
        }
    }
    void xfab(lpstring_t decl, pstring_t script, fs::path preferred_dir, std::vector<convert_arg_t> args, std::unique_ptr<mods_t> mods)
    {
        using namespace std::literals;

        if(mods)
            mods->validate(decl, MOD_fork_scope);

        auto const check_argn = [&](unsigned expected)
        {
            if(args.size() != expected)
                compiler_error(decl, fmt("Wrong number of arguments. Expecting %.", expected + 1));
        };

        std::string_view const view = script.view(source());

        auto const get_macro = [&](unsigned i) -> std::string
        {
            if(auto const* str = std::get_if<string_literal_t>(&args[i].value))
                return str->string;
            compiler_error(args[i].pstring, "Expecting macro name string literal as argument.");
        };

        check_argn(4);

        xfab_macros_t mm;
        mm.chr = get_macro(1);
        mm.palette = get_macro(2);
        mm.level = get_macro(3);

        try
        {
            xfab_convert_type_t ct = XFAB_INVALID;

            if(view == "raw"sv)
                ct = XFAB_RAW;
            else if(view == "rlz"sv)
                ct = XFAB_RLZ;
            else if(view == "pbz"sv)
                ct = XFAB_PBZ;

            if(ct == XFAB_INVALID)
                compiler_error(script, fmt("Unknown XFab format: %", view));

            fs::path xfab_path = get_path(preferred_dir, args[0]);
            std::vector<std::uint8_t> data = read_binary_file(xfab_path.string(), decl);

            todo_xfabs.push_back({
                decl,
                ct,
                std::move(data),
                std::move(xfab_path),
                std::move(mm),
                std::move(mods),
            });
        }
        catch(compiler_error_t const& e)
        {
            throw e;
        }
        catch(std::exception const& e)
        {
            compiler_error(script, e.what());
        }
    }

    void finish_mapfabs()
    {
        for(mapfab_todo_t& tm : todo_mapfabs)
        {
            try 
            { 
                ident_map_t<global_ht>* private_globals = nullptr;
                ident_map_t<group_ht>* private_groups = nullptr;

                if(mod_test(tm.mods.get(), MOD_fork_scope))
                {
                    private_globals = &this->private_globals;
                    private_groups = &this->private_groups;
                }

                convert_mapfab(tm.ct, tm.data.data(), tm.data.size(), tm.at, std::move(tm.path), 
                               std::move(tm.macros), private_globals, private_groups);
            }
            catch(macro_error_t const& e) 
            { 
                throw compiler_error_t(
                    fmt_error(tm.at, "While parsing mapfab file...")
                    + fmt_error(e.pstring, e.what()));
            }
            catch(std::exception const& e) { compiler_error(tm.at, e.what()); }
            catch(...) { throw; }
        }

        for(xfab_todo_t& tm : todo_xfabs)
        {
            try 
            { 
                ident_map_t<global_ht>* private_globals = nullptr;
                ident_map_t<group_ht>* private_groups = nullptr;

                if(mod_test(tm.mods.get(), MOD_fork_scope))
                {
                    private_globals = &this->private_globals;
                    private_groups = &this->private_groups;
                }

                convert_xfab(tm.ct, tm.data.data(), tm.data.size(), tm.at, std::move(tm.path), 
                               std::move(tm.macros), private_globals, private_groups);
            }
            catch(macro_error_t const& e) 
            { 
                throw compiler_error_t(
                    fmt_error(tm.at, "While parsing xfab file...")
                    + fmt_error(e.pstring, e.what()));
            }
            catch(std::exception const& e) { compiler_error(tm.at, e.what()); }
            catch(...) { throw; }
        }
    }
};

#endif
