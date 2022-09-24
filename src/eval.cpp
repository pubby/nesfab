#include "eval.hpp"

#include <cassert>
#include <chrono>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "alloca.hpp"
#include "rval.hpp"
#include "decl.hpp"
#include "globals.hpp"
#include "file.hpp"
#include "options.hpp"
#include "ir.hpp"
#include "stmt.hpp"
#include "eternal_new.hpp"
#include "lt.hpp"
#include "group.hpp"
#include "fnv1a.hpp"
#include "ast.hpp"
#include "compiler_error.hpp"
#include "asm_proc.hpp"
#include "text.hpp"

namespace sc = std::chrono;
namespace bc = boost::container;
using namespace lex;

using ssa_value_array_t = bc::small_vector<ssa_value_t, 1>;

// Data associated with each block node, to be used when making IRs.
struct block_d
{
    using vector_t = std::vector<ssa_value_array_t>;

    // An array of size 'num_ssa_vars()'
    // Keeps track of which ssa node a var refers to.
    // A handle of {} means the local var isn't in the block.
    vector_t vars;

    // An array of size 'num_ssa_vars()'
    // Phi nodes in the block which have yet to be sealed.
    vector_t unsealed_phis;

    // Only used for labels.
    pstring_t label_name = {};

    // A CFG node is sealed when all its predecessors are set.
    bool sealed = false;
};

class eval_t
{
private:
    pstring_t pstring = {};
    fn_t const* fn = nullptr;
    stmt_t const* stmt = nullptr;
    ir_t* ir = nullptr;
    bc::small_vector<rval_t, 8> interpret_locals;
    bc::small_vector<type_t, 8> var_types;

    using clock = sc::steady_clock;
    sc::time_point<clock> start_time;

    struct logical_data_t
    {
        cfg_ht branch_node;
        pstring_t lhs_pstring;
    };

    struct label_t
    {
        cfg_ht node = {};
        bc::small_vector<cfg_ht, 2> inputs;
    };

    // Data used by the ir builder can go inside this struct (for organization).
    struct ir_builder_t
    {
        cfg_ht cfg = {}; // The current CFG node

        bc::small_vector<logical_data_t, 8> logical_stack;

        bc::small_vector<bc::small_vector<cfg_ht, 4>, 4> break_stack;
        bc::small_vector<bc::small_vector<cfg_ht, 4>, 4> continue_stack;
        bc::small_vector<cfg_ht, 4> switch_stack;

        bc::small_vector<ssa_value_array_t, 8> return_values;
        bc::small_vector<cfg_ht, 8> return_jumps;

        rh::robin_map<stmt_t const*, label_t> label_map;

        void clear()
        {
            cfg = {};

            logical_stack.clear();

            break_stack.clear();
            continue_stack.clear();
            
            return_values.clear();
            return_jumps.clear();

            label_map.clear();
        }
    };

    static thread_local ir_builder_t builder;
public:
    rpair_t final_result;
    byte_block_data_t byte_block_data; // Only used when defining byte blocks
    local_const_t const* local_consts = nullptr;
    precheck_tracked_t* precheck_tracked = nullptr; // Various things callers of 'eval_t' may want.

    enum do_t
    {
        CHECK,         // Resolves types and syntax, but not values.
        INTERPRET_CE,  // Like INTERPRET, but can't read/write locals.
        INTERPRET_ASM, // Like INTERPRET, but for inline assembly
        INTERPRET,     // Calculates values at compile-time.
        COMPILE,       // Generates the SSA IR.
        LINK
    };

    static constexpr bool is_check(do_t d) { return d == CHECK; }
    static constexpr bool is_interpret(do_t d) { return d == INTERPRET_CE || d == INTERPRET_ASM || d == INTERPRET; }
    static constexpr bool is_compile(do_t d) { return d == COMPILE; }
    static constexpr bool is_link(do_t d) { return d == LINK; }

    stmt_ht stmt_handle() const { return { stmt - fn->def().stmts.data() }; }
    pstring_mods_t stmt_pstring_mods() const { return { stmt->pstring, fn->def().mods_of(stmt_handle()) }; }

    template<do_t Do>
    struct do_wrapper_t { static constexpr auto D = Do; };

    template<do_t D>
    eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const* fn_ref,
           ast_node_t const& expr, type_t expected_type, 
           local_const_t const* local_consts = nullptr);

    template<do_t D>
    eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const& fn_ref, 
           precheck_tracked_t* tracked, rval_t const* args, unsigned num_args,
           local_const_t const* local_consts = nullptr);

    eval_t(ir_t& ir_ref, fn_t const& fn_ref, local_const_t const* local_consts);

    struct access_t
    {
        type_t type = {};
        unsigned member = 0;
        ssa_value_t index = {};
    };

    void check_time();

    template<do_t D>
    void interpret_stmts();

    void compile_block();

    template<do_t D>
    expr_value_t do_var_init_expr(unsigned var_i, ast_node_t const& expr);

    template<eval_t::do_t D>
    expr_value_t do_expr(ast_node_t const& ast);

    template<do_t D>
    void do_expr_result(ast_node_t const&, type_t expected_result);

    template<do_t D>
    expr_value_t do_assign(expr_value_t lhs, expr_value_t rhs, token_t const& token);

    expr_value_t compile_binary_operator(
        expr_value_t const& lhs, expr_value_t const& rhs, 
        ssa_op_t op, type_t result_type, bool carry = false);

    template<typename Policy>
    expr_value_t do_compare(expr_value_t lhs, expr_value_t rhs, token_t const& token);

    template<typename Policy>
    expr_value_t do_arith(expr_value_t lhs, expr_value_t rhs, token_t const& token);

    template<typename Policy>
    expr_value_t do_shift(expr_value_t lhs, expr_value_t rhs, token_t const& token);

    //template<typename Policy>
    //expr_value_t interpret_shift(expr_value_t lhs, expr_value_t rhs, token_t const& token);

    template<typename Policy>
    expr_value_t do_assign_arith(expr_value_t lhs, expr_value_t rhs, token_t const& token);

    template<typename Policy>
    expr_value_t do_assign_shift(expr_value_t lhs, expr_value_t rhs, token_t const& token);

    template<do_t D>
    expr_value_t do_logical(ast_node_t const& ast);

    void req_quantity(token_t const& token, expr_value_t const& value);
    void req_quantity(token_t const& token, expr_value_t const& lhs, expr_value_t const& rhs);

    template<do_t D>
    expr_value_t force_truncate(expr_value_t value, type_t to_type, pstring_t cast_pstring);
    template<do_t D>
    expr_value_t force_promote(expr_value_t value, type_t to_type, pstring_t cast_pstring);
    template<do_t D>
    expr_value_t force_intify_ptr(expr_value_t value, type_t to_type, pstring_t cast_pstring);
    template<do_t D>
    expr_value_t force_ptrify_int(expr_value_t value, expr_value_t* bank, type_t to_type, pstring_t cast_pstring);
    template<do_t D>
    expr_value_t force_convert_int(expr_value_t value, type_t to_type, bool implicit, pstring_t cast_pstring);
    template<do_t D>
    expr_value_t force_round_real(expr_value_t value, type_t to_type, bool implicit, pstring_t cast_pstring);
    template<do_t D>
    expr_value_t force_boolify(expr_value_t value, pstring_t cast_pstring);
    template<do_t D>
    expr_value_t force_resize_tea(expr_value_t value, type_t to_type, pstring_t cast_pstring);

    template<do_t D>
    bool cast(expr_value_t& v, type_t to_type, bool implicit, pstring_t pstring = {});

    template<do_t D>
    expr_value_t throwing_cast(expr_value_t value, type_t to_type, bool implicit, pstring_t pstring = {});

    template<do_t D>
    int cast_args(pstring_t pstring, expr_value_t* begin, expr_value_t* end, type_t const* type_begin, bool implicit);

    std::size_t num_local_vars() const { assert(fn); return fn->def().local_vars.size(); }

    type_t var_i_type(unsigned var_i) const;
    void init_rval(access_t a, rval_t& rval);
    //access_t access(rpn_value_t const& rpn_value) const;
    ssa_value_t const& get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index) const;
    ssa_value_t& get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index);

    template<eval_t::do_t D>
    ssa_value_t from_variant(ct_variant_t const& v, type_t type);

    template<do_t D>
    expr_value_t to_rval(expr_value_t v);

    ///////////////////////
    // compiler-specific //
    ///////////////////////

    std::size_t num_vars() const { assert(ir); return num_local_vars() + ir->gmanager.num_locators(); }

    unsigned to_var_i(gmanager_t::index_t index) const { assert(index); return index.id + num_local_vars(); }
    template<typename T>
    unsigned to_var_i(T gvar) const { assert(ir); return to_var_i(ir->gmanager.var_i(gvar)); }

    // Block and local variable functions
    void seal_block(block_d& block_data);
    void fill_phi_args(ssa_ht phi, unsigned var_i, unsigned member);
    ssa_value_t var_lookup(cfg_ht node, unsigned var_i, unsigned member);
    rval_t var_lookup(cfg_ht node, unsigned var_i);
    ssa_value_array_t from_rval(rval_t const& rval, type_t type);
    ssa_value_array_t from_rval(expr_value_t const& value);

    cfg_ht insert_cfg(bool seal, pstring_t label_name = {});
    void cfg_exits_with_jump();
    void cfg_exits_with_branch(ssa_value_t condition);
    cfg_ht compile_goto();

    //void compile_binary_operator(rpn_stack_t&, ssa_op_t op, type_t result_type, bool carry = false);

    ///////////////////
    // link-specific //
    ///////////////////

    //void make_lt(rpn_stack_t& rpn_stack, unsigned argn,
                 //token_t const* op_begin, token_t const* op_end);

    //void make_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op);

    //template<eval_t::do_t D>
    //rval_t prep_lt(rpn_value_t const& rpn_value, token_t const* op_begin, token_t const* op_end);

    //template<eval_t::do_t D>
    //rval_t prep_lt(rpn_value_t const& rpn_value, token_t const& op);

    //template<eval_t::do_t D>
    //bool handle_lt(rpn_stack_t& rpn_stack, unsigned argn,
                   //token_t const* op_begin, token_t const* op_end);

    //template<eval_t::do_t D>
    //bool handle_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op);
};

thread_local eval_t::ir_builder_t eval_t::builder;

rpair_t interpret_local_const(pstring_t pstring, fn_t const* fn, ast_node_t const& expr,
                              type_t expected_type, local_const_t const* local_consts)
{
    eval_t i(eval_t::do_wrapper_t<eval_t::INTERPRET_CE>{}, pstring, fn, expr, expected_type, local_consts);
    return i.final_result;
}

void check_local_const(pstring_t pstring, fn_t const* fn, ast_node_t const& expr,
                       local_const_t const* local_consts)
{
    eval_t i(eval_t::do_wrapper_t<eval_t::CHECK>{}, pstring, fn, expr, TYPE_VOID, local_consts);
}

rpair_t interpret_expr(pstring_t pstring, ast_node_t const& expr, type_t expected_type, eval_t* env)
{
    if(env)
    {
        env->do_expr_result<eval_t::INTERPRET_CE>(expr, expected_type);
        return env->final_result;
    }
    else
    {
        eval_t i(eval_t::do_wrapper_t<eval_t::INTERPRET>{}, pstring, nullptr, expr, expected_type);
        return i.final_result;
    }
}


byte_block_data_t interpret_byte_block(pstring_t pstring, ast_node_t const& expr, fn_t const* fn, 
                                       local_const_t const* local_consts)
{
    eval_t i(eval_t::do_wrapper_t<eval_t::INTERPRET>{}, pstring, fn, expr, TYPE_VOID, local_consts);
    return std::move(i.byte_block_data);
}

precheck_tracked_t build_tracked(fn_t const& fn, local_const_t const* local_consts)
{
    precheck_tracked_t tracked;
    eval_t eval(eval_t::do_wrapper_t<eval_t::CHECK>{}, {}, fn, &tracked, nullptr, 0, local_consts);
    return tracked;
}

void build_ir(ir_t& ir, fn_t const& fn, local_const_t const* local_consts)
{
    assert(cfg_data_pool::array_size() == 0);
    assert(ssa_data_pool::array_size() == 0);
    cfg_data_pool::scope_guard_t<block_d> cg(0);

    eval_t eval(ir, fn, local_consts);
}

template<eval_t::do_t D>
eval_t::eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const* fn_ref,
               ast_node_t const& expr, type_t expected_type, local_const_t const* local_consts)
: pstring(pstring)
, fn(fn_ref)
, start_time(clock::now())
, local_consts(local_consts)
{
    if(fn)
    {
        unsigned const nlocals = num_local_vars();
        var_types.resize(nlocals);
        for(unsigned i = 0; i < nlocals; ++i)
            var_types[i] = ::dethunkify(fn->def().local_vars[i].decl.src_type, true, this);
    }
    do_expr_result<D>(expr, expected_type);
}

template<eval_t::do_t D>
eval_t::eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const& fn_ref, 
               precheck_tracked_t* tracked, rval_t const* args, unsigned num_args,
               local_const_t const* local_consts)
: pstring(pstring)
, fn(&fn_ref)
, stmt(fn_ref.def().stmts.data())
, start_time(clock::now())
, local_consts(local_consts)
, precheck_tracked(tracked)
{
    unsigned const nlocals = num_local_vars();

    var_types.resize(nlocals);
    for(unsigned i = 0; i < nlocals; ++i)
        var_types[i] = ::dethunkify(fn->def().local_vars[i].decl.src_type, true, this);

    if(D == COMPILE)
    {
        // Reset the static thread-local state:
        builder.clear();

        // Add global vars to 'var_types':
        var_types.reserve(var_types.size() + ir->gmanager.num_gvar_locators());
        ir->gmanager.for_each_gvar([&](gvar_ht gvar, auto) { var_types.push_back(gvar->type()); });
        assert(num_vars() >= var_types.size());
        var_types.resize(num_vars(), TYPE_VOID);
    }
    else
    {
        if(!is_check(D))
        {
            interpret_locals.resize(nlocals);

            assert(args);
            assert(num_args <= nlocals);
            for(unsigned i = 0; i < num_args; ++i)
            {
                assert(args[i].size() == num_members(var_types[i]));
                interpret_locals[i] = args[i];
            }
        }

        interpret_stmts<D>();
    }
}

eval_t::eval_t(ir_t& ir_ref, fn_t const& fn_ref, local_const_t const* local_consts)
: fn(&fn_ref)
, stmt(fn_ref.def().stmts.data())
, ir(&ir_ref)
, start_time(clock::now())
, local_consts(local_consts)
{
    // Reset the static thread-local state:
    builder.clear(); // TODO: make sure this isn't called in recursion
    ir->gmanager.init(fn->handle());

    unsigned const nlocals = num_local_vars();

    var_types.resize(nlocals);
    for(unsigned i = 0; i < nlocals; ++i)
        var_types[i] = ::dethunkify(fn->def().local_vars[i].decl.src_type, true, this);

    // Add global vars to 'var_types':
    var_types.reserve(num_vars());
    ir->gmanager.for_each_gvar([&](gvar_ht gvar, auto) { var_types.push_back(gvar->type()); });
    assert(num_vars() >= var_types.size());
    var_types.resize(num_vars(), TYPE_VOID);

    // OK! var_types is built.

    ir->root = builder.cfg = insert_cfg(true);

    ssa_ht const entry = ir->root->emplace_ssa(SSA_entry, TYPE_VOID);
    entry->append_daisy();

    // Insert nodes for the arguments
    unsigned const nparams = fn->def().num_params;
    for(unsigned i = 0; i < nparams; ++i)
    {
        type_t const type = var_types[i];
        unsigned nmember = ::num_members(type);

        assert(ir->root.data<block_d>().vars[i].size() == nmember);

        for(unsigned m = 0; m < nmember; ++m)
        {
            ir->root.data<block_d>().vars[i][m] = ir->root->emplace_ssa(
                SSA_read_global, member_type(type, m), entry, 
                locator_t::arg(fn->handle(), i, m, 0));
        }
    }

    // Insert nodes for gmember reads
    ir->gmanager.for_each_gvar([&](gvar_ht gvar, gmanager_t::index_t i)
    {
        unsigned const var_i = to_var_i(i);
        auto& vars = ir->root.data<block_d>().vars;
        assert(vars.size() == var_types.size());

        for(gmember_ht m = gvar->begin(); m != gvar->end(); ++m)
        {
            assert(var_i < vars.size());
            assert(m->member() < vars[var_i].size());

            vars[var_i][m->member()] = ir->root->emplace_ssa(
                SSA_read_global, member_type(var_types[var_i], m->member()), entry, locator_t::gmember(m, 0));
        }
    });

    // Insert nodes for gmember set reads
    ir->gmanager.for_each_gmember_set(fn->handle(),
    [&](bitset_uint_t const* gmember_set, gmanager_t::index_t i, locator_t locator)
    {
        unsigned const var_i = to_var_i(i);
        auto& vars = ir->root.data<block_d>().vars;
        assert(vars.size() == var_types.size());
        assert(vars[var_i].size() == 1);

        vars[var_i][0] = ir->root->emplace_ssa(
            SSA_read_global, var_types[var_i], entry, locator);
    });

    // Create all of the SSA graph, minus the exit node:
    compile_block();
    cfg_exits_with_jump();
    cfg_ht const end = builder.cfg;

    // Now create the exit block.
    // All return statements create a jump, which will jump to the exit node.
    type_t const return_type = fn->type().return_type();
    if(return_type != TYPE_VOID)
    {
        ssa_value_array_t array;

        unsigned const num_m = ::num_members(return_type);
        for(unsigned m = 0; m < num_m; ++m)
            array.push_back(end->emplace_ssa(SSA_uninitialized, ::member_type(return_type, m)));

        builder.return_values.push_back(std::move(array));
    }

    ir->exit = insert_cfg(true);

    for(cfg_ht node : builder.return_jumps)
        node->build_set_output(0, ir->exit);
    end->build_set_output(0, ir->exit);

    // Write all globals at the exit:
    std::vector<ssa_value_t> return_inputs;
    return_inputs.reserve(ir->gmanager.num_locators() * 2);

    ir->gmanager.for_each_gvar([&](gvar_ht gvar, gmanager_t::index_t i)
    {
        unsigned const var_i = to_var_i(i);

        for(gmember_ht m = gvar->begin(); m != gvar->end(); ++m)
        {
            return_inputs.push_back(var_lookup(ir->exit, var_i, m->member()));
            return_inputs.push_back(locator_t::gmember(m, 0));
        }
    });

    ssa_ht ret = ir->exit->emplace_ssa(SSA_return, TYPE_VOID);

    // Append the return value, if it exists:
    if(return_type != TYPE_VOID)
    {
        unsigned const num_m = ::num_members(return_type);
        for(unsigned m = 0; m < num_m; ++m)
        {
            ssa_ht phi = ir->exit->emplace_ssa(SSA_phi, member_type(return_type, m));
            
            unsigned const size = builder.return_values.size();
            phi->alloc_input(size);

            for(unsigned i = 0; i < size; ++i)
                phi->build_set_input(i, builder.return_values[i][m]);

            return_inputs.push_back(phi);
            return_inputs.push_back(locator_t::ret(fn->handle(), m, 0));
        }
    }

    assert(return_inputs.size() % 2 == 0);
    ret->assign_input(&*return_inputs.begin(), &*return_inputs.end());
    ret->append_daisy();

#ifndef NDEBUG
    for(cfg_ht h = ir->cfg_begin(); h; ++h)
        assert(h.data<block_d>().sealed);
#endif
}

template<eval_t::do_t D>
void eval_t::do_expr_result(ast_node_t const& expr, type_t expected_type)
{
    expr_value_t v = to_rval<D>(do_expr<D>(expr));

    if(expected_type.name() != TYPE_VOID)
    {
        if(!can_size_unsized_array(v.type, expected_type))
            v = throwing_cast<D>(std::move(v), expected_type, true);

        if(is_interpret(D))
            final_result.value = v.rval();

        final_result.type = v.type;
    }
    else
        final_result.type = TYPE_VOID;
}

void eval_t::check_time()
{
    auto elapsed = clock::now() - start_time;
    if(compiler_options().time_limit > 0)
    {
        if(elapsed > sc::milliseconds(compiler_options().time_limit))
        {
            throw out_of_time_t(
                fmt_error(this->pstring, "Ran out of time executing expression.")
                + fmt_note("Computation is likely divergent.\n")
                + fmt_note(fmt("Use compiler flag --timelimit 0 to ignore this error.\n", compiler_options().time_limit)));
        }
    }
}

///////////////////////////////////////////////////////////////////////////////

template<eval_t::do_t D>
expr_value_t eval_t::do_var_init_expr(unsigned var_i, ast_node_t const& expr)
{
    expr_value_t v = do_expr<D>(expr);

    if(can_size_unsized_array(v.type, var_types[var_i]))
        var_types[var_i].set_array_length(v.type.array_length(), v.pstring);

    return throwing_cast<D>(std::move(v), var_types[var_i], true);
}

template<eval_t::do_t D>
void eval_t::interpret_stmts()
{
    static_assert(D != COMPILE);

    auto const do_condition = [&]() -> bool
    { 
        expr_value_t v = throwing_cast<D>(do_expr<D>(*stmt->expr), TYPE_BOOL, true);
        return !is_interpret(D) || v.fixed();
    };

    while(true)
    {
        check_time();

        switch(stmt->name)
        {
        default: // Handles var inits
            if(is_var_init(stmt->name))
            {
                if(D == INTERPRET_CE)
                    compiler_error(stmt->pstring, "Expression cannot be evaluated at compile-time.");

                unsigned const var_i = ::get_local_var_i(stmt->name);

                // Prepare the type.
                assert(var_i < var_types.size());
                if(var_types[var_i].name() == TYPE_VOID)
                    var_types[var_i] = dethunkify(fn->def().local_vars[var_i].decl.src_type, true, this);

                if(stmt->expr)
                {
                    expr_value_t v = do_var_init_expr<D>(var_i, *stmt->expr);

                    if(is_interpret(D))
                    {
                        assert(interpret_locals[var_i].empty());
                        interpret_locals[var_i] = std::move(v.rval());
                    }
                }
                else if(is_interpret(D))
                {
                    type_t const type = var_types[var_i];
                    unsigned const num = num_members(type);
                    assert(num > 0);

                    rval_t rval;
                    rval.reserve(num);

                    for(unsigned i = 0; i < num; ++i)
                    {
                        type_t const mt = member_type(type, i);
                        if(mt.name() == TYPE_TEA)
                            rval.emplace_back(make_ct_array(mt.array_length()));
                        else
                            rval.emplace_back();
                    }

                    interpret_locals[var_i] = std::move(rval);
                }

                ++stmt;
            }
            else
                compiler_error(stmt->pstring, "Statement cannot appear in constant evaluation.");
            break;

        case STMT_GOTO_MODE:
            if(!is_check(D))
                compiler_error(stmt->pstring, "Statement cannot appear in constant evaluation.");
            // fall-through
        case STMT_EXPR:
        case STMT_FOR_EFFECT:
            do_expr<D>(*stmt->expr);
            ++stmt;
            break;

        case STMT_DO:
        case STMT_END_IF:
        case STMT_LABEL:
        case STMT_END_SWITCH:
        case STMT_CASE:
        case STMT_DEFAULT:
            ++stmt;
            break;

        case STMT_ELSE:
        case STMT_END_WHILE:
        case STMT_END_FOR:
        case STMT_BREAK:
        case STMT_CONTINUE:
        case STMT_GOTO:
            if(is_interpret(D))
                stmt = &fn->def()[stmt->link];
            else
                ++stmt;
            break;

        case STMT_IF:
            if(do_condition())
                ++stmt;
            else
            {
                stmt = &fn->def()[stmt->link];
                if(stmt->name == STMT_ELSE)
                    ++stmt;
            }
            break;

        case STMT_WHILE:
        case STMT_FOR:
            if(do_condition())
                ++stmt;
            else
                stmt = &fn->def()[stmt->link];
            break;

        case STMT_END_DO:
            if(do_condition())
                stmt = &fn->def()[stmt->link];
            else
                ++stmt;
            break;

        case STMT_SWITCH:
            {
                expr_value_t switch_expr = do_expr<D>(*stmt->expr);
                switch_expr = throwing_cast<D>(std::move(switch_expr), is_signed(switch_expr.type.name()) ? TYPE_S : TYPE_U, true);

                if(!is_interpret(D))
                    ++stmt;
                else while(true)
                {
                    assert(stmt->link);
                    stmt = &fn->def()[stmt->link];

                    if(stmt->name == STMT_CASE)
                    {
                        expr_value_t case_expr = throwing_cast<D>(do_expr<D>(*stmt->expr), switch_expr.type, true);

                        if(switch_expr.fixed() == case_expr.fixed())
                        {
                            ++stmt;
                            break;
                        }
                    }
                    else if(stmt->name == STMT_DEFAULT)
                    {
                        ++stmt;
                        break;
                    }
                    else
                        assert(false);
                }
            }

            break;

        case STMT_RETURN:
            {
                type_t const return_type = fn->type().return_type();
                if(stmt->expr)
                {
                    expr_value_t v = throwing_cast<D>(do_expr<D>(*stmt->expr), return_type, true);
                    if(is_interpret(D))
                        final_result.value = std::move(v.rval());
                    final_result.type = std::move(v.type);
                }
                else if(return_type.name() != TYPE_VOID)
                {
                    compiler_error(stmt->pstring, fmt(
                        "Expecting return expression of type %.", return_type));
                }
            }
            return;

        case STMT_END_FN:
            {
                type_t return_type = fn->type().return_type();
                if(return_type.name() != TYPE_VOID)
                {
                    compiler_error(stmt->pstring, fmt(
                        "Interpreter reached end of function without returning %.", return_type));
                }
            }
            return;

        case STMT_NMI:
            if(!is_check(D))
                compiler_error(stmt->pstring, "Cannot wait for nmi at compile-time.");
            if(precheck_tracked)
                precheck_tracked->wait_nmis.push_back(stmt_pstring_mods());
            ++stmt;
            break;

        case STMT_FENCE:
            if(precheck_tracked)
                precheck_tracked->fences.push_back(stmt_pstring_mods());
            ++stmt;
            break;
        }
    }
    assert(false);
}

static ssa_value_t _interpret_shift_atom(ssa_value_t v, int shift)
{
    fixed_t result = { v.fixed() };
    if(shift < 0)
        result.value <<= -shift * 8;
    else
        result.value >>= shift * 8;
    result.value &= numeric_bitmask(TYPE_U);
    return ssa_value_t(result, TYPE_U);
}

void eval_t::compile_block()
{
    while(true)
    {
    switch(stmt->name)
    {
    default: // Handles var inits
        if(is_var_init(stmt->name))
        {
            unsigned const var_i = get_local_var_i(stmt->name);
            type_t const type = var_types[var_i];

            if(is_ct(type))
                compiler_error(stmt->pstring, fmt("Variables of type % are only valid inside ct functions.", type));

            ssa_value_array_t value;

            if(stmt->expr)
                value = from_rval(do_var_init_expr<COMPILE>(var_i, *stmt->expr));
            else
            {
                unsigned const num = num_members(type);
                assert(num > 0);

                value.reserve(num);
                
                for(unsigned i = 0; i < num; ++i)
                {
                    type_t const mt = member_type(type, i);
                    value.emplace_back(builder.cfg->emplace_ssa(SSA_uninitialized, mt));
                }
            }

            builder.cfg.data<block_d>().vars[var_i] = std::move(value);
            ++stmt;
        }
        else
            throw std::runtime_error("Unimplemented stmt.");
        break;

    case STMT_END_IF:
    case STMT_END_WHILE:
    case STMT_END_FN:
    case STMT_END_FOR:
        ++stmt;
        // fall-through
    case STMT_END_DO:
    case STMT_FOR_EFFECT:
        return;

    case STMT_EXPR:
        do_expr<COMPILE>(*stmt->expr);
        ++stmt;
        break;

    case STMT_IF:
        {
            // Branch the active node.
            expr_value_t v = do_expr<COMPILE>(*stmt->expr);
            ++stmt;
            cfg_ht branch = builder.cfg;
            v = throwing_cast<COMPILE>(std::move(v), TYPE_BOOL, true);
            cfg_exits_with_branch(v.ssa());

            // Create new cfg_node for the 'true' branch.
            cfg_ht const begin_true = builder.cfg = insert_cfg(true);
            branch->build_set_output(1, begin_true);
            compile_block();
            cfg_exits_with_jump();
            cfg_ht const end_true = builder.cfg;

            if(stmt->name == STMT_ELSE)
            {
                // Create new block for the 'false' branch.
                ++stmt;
                cfg_ht const begin_false = builder.cfg = insert_cfg(true);
                branch->build_set_output(0, begin_false);
                compile_block();

                // Repurpose 'branch' to hold end of the 'false' branch.
                // Simplifies the assignment that follows.
                branch = builder.cfg;
                cfg_exits_with_jump();
            }

            // Merge the two nodes.
            builder.cfg = insert_cfg(true);
            end_true->build_set_output(0, builder.cfg);
            branch->build_set_output(0, builder.cfg);
        }
        break;

    case STMT_FOR:
    case STMT_WHILE:
        {
            bool const is_for = stmt->name == STMT_FOR;
            cfg_ht const entry = builder.cfg;

            // The loop condition will go in its own block.
            cfg_exits_with_jump();
            cfg_ht const begin_branch = builder.cfg = insert_cfg(false);
            entry->build_set_output(0, begin_branch);

            expr_value_t v = do_expr<COMPILE>(*stmt->expr);
            ++stmt;
            cfg_ht const end_branch = builder.cfg;
            v = throwing_cast<COMPILE>(std::move(v), TYPE_BOOL, true);
            cfg_exits_with_branch(v.ssa());

            builder.continue_stack.emplace_back();
            builder.break_stack.emplace_back();

            // Compile the body.
            cfg_ht const begin_body = builder.cfg = insert_cfg(true);
            end_branch->build_set_output(1, begin_body);
            compile_block();
            cfg_ht const end_body = builder.cfg;
            cfg_exits_with_jump();

            cfg_ht begin_for_expr = begin_branch; // Will be 'begin_branch' only for WHILE stmts, otherwise see below:
            if(is_for)
            {
                // Compile the 'for' expr in its own block:
                assert(stmt->name == STMT_FOR_EFFECT);
                begin_for_expr = builder.cfg = insert_cfg(true);
                end_body->build_set_output(0, begin_for_expr);

                do_expr<COMPILE>(*stmt->expr);
                ++stmt;
                assert(stmt->name == STMT_END_FOR);
                ++stmt;
                cfg_ht const end_for_expr = builder.cfg;
                cfg_exits_with_jump();
                end_for_expr->build_set_output(0, begin_branch);
            }
            else
                end_body->build_set_output(0, begin_for_expr);

            // All continue statements jump to the 'begin_for_expr'.
            for(cfg_ht node : builder.continue_stack.back())
                node->build_set_output(0, begin_for_expr);
            seal_block(begin_branch.data<block_d>());

            // Create the exit node.
            cfg_ht const begin_exit = builder.cfg = insert_cfg(true);
            end_branch->build_set_output(0, begin_exit);
            for(cfg_ht node : builder.break_stack.back())
                node->build_set_output(0, begin_exit);

            builder.continue_stack.pop_back();
            builder.break_stack.pop_back();
        }
        break;

    case STMT_DO:
        {
            cfg_ht const entry = builder.cfg;

            ++stmt;
            builder.continue_stack.emplace_back();
            builder.break_stack.emplace_back();

            // Compile the loop body
            cfg_exits_with_jump();
            cfg_ht const begin_body = builder.cfg = insert_cfg(false);
            entry->build_set_output(0, begin_body);
            compile_block();
            cfg_ht const end_body = builder.cfg;

            assert(stmt->name == STMT_END_DO);

            // The loop condition can go in its own block, which is
            // necessary to implement 'continue'.
            cfg_exits_with_jump();
            cfg_ht const begin_branch = builder.cfg = insert_cfg(true);
            end_body->build_set_output(0, begin_branch);

            expr_value_t v = do_expr<COMPILE>(*stmt->expr);
            ++stmt;
            cfg_ht const end_branch = builder.cfg;
            throwing_cast<COMPILE>(std::move(v), TYPE_BOOL, true);
            cfg_exits_with_branch(v.ssa());

            end_branch->build_set_output(1, begin_body);
            seal_block(begin_body.data<block_d>());

            // All continue statements jump to the branch node.
            for(cfg_ht node : builder.continue_stack.back())
                node->build_set_output(0, begin_branch);

            // Create the exit cfg_node.
            cfg_ht const begin_exit = builder.cfg = insert_cfg(true);
            end_branch->build_set_output(0, begin_exit);
            for(cfg_ht node : builder.break_stack.back())
                node->build_set_output(0, begin_exit);

            builder.continue_stack.pop_back();
            builder.break_stack.pop_back();
        }
        break;

    case STMT_RETURN:
        {
            type_t const return_type = fn->type().return_type();

            if(stmt->expr)
            {
                expr_value_t v = throwing_cast<COMPILE>(do_expr<COMPILE>(*stmt->expr), return_type, true);
                rval_t& rval = v.rval();

                ssa_value_array_t array;
                array.reserve(rval.size());

                for(unsigned i = 0; i < rval.size(); ++i)
                    array.push_back(from_variant<COMPILE>(rval[i], member_type(v.type, i)));

                builder.return_values.push_back(std::move(array));
            }
            else if(return_type.name() != TYPE_VOID)
            {
                compiler_error(stmt->pstring, fmt(
                    "Expecting return expression of type %.", return_type));
            }

            builder.return_jumps.push_back(builder.cfg);
            builder.cfg = compile_goto();
            ++stmt;
        }
        break;

    case STMT_BREAK:
        if(builder.break_stack.empty())
            compiler_error(stmt->pstring, "break statement outside of loop or switch.");
        builder.break_stack.back().push_back(builder.cfg);
        builder.cfg = compile_goto();
        ++stmt;
        break;

    case STMT_CONTINUE:
        if(builder.continue_stack.empty())
            compiler_error(stmt->pstring, "continue statement outside of loop.");
        builder.continue_stack.back().push_back(builder.cfg);
        builder.cfg = compile_goto();
        ++stmt;
        break;

    case STMT_SWITCH:
        // TODO
        {
            cfg_ht const dead_branch = compile_goto();
            cfg_ht const switch_cfg = insert_cfg(true);
            switch_cfg->alloc_output(1);
            builder.cfg->build_set_output(0, switch_cfg);

            expr_value_t v = do_expr<COMPILE>(*stmt->expr);
            ++stmt;
            v = throwing_cast<COMPILE>(std::move(v), is_signed(v.type.name()) ? TYPE_S : TYPE_U, true);

            ssa_ht const switch_ssa = switch_cfg->emplace_ssa(SSA_switch, TYPE_VOID, v.ssa());
            switch_ssa->append_daisy();

            assert(switch_cfg->last_daisy()->op() == SSA_switch);

            builder.break_stack.emplace_back();
            builder.switch_stack.push_back(switch_cfg);
            builder.cfg = dead_branch;
        }
        break;

    case STMT_END_SWITCH:
        {
            cfg_ht const exit = insert_cfg(true);
            cfg_exits_with_jump();
            builder.cfg->build_set_output(0, exit);
            builder.cfg = exit;

            for(cfg_ht node : builder.break_stack.back())
                node->build_set_output(0, exit);

            builder.break_stack.pop_back();
            builder.switch_stack.pop_back();

            ++stmt;
        }
        break;

    case STMT_CASE:
        // TODO
        {
            cfg_ht const entry = builder.cfg;
            cfg_exits_with_jump();

            cfg_ht const case_cfg = builder.cfg = insert_cfg(true);
            entry->build_set_output(0, case_cfg);

            assert(builder.switch_stack.size() > 0);

            assert(builder.switch_stack.size() > 0);
            cfg_ht const switch_cfg = builder.switch_stack.back();
            ssa_ht const switch_ssa = switch_cfg->last_daisy();
            passert(switch_ssa->op() == SSA_switch, switch_ssa->op());

            expr_value_t case_value = do_expr<INTERPRET_CE>(*stmt->expr);
            ++stmt;
            case_value = throwing_cast<INTERPRET_CE>(std::move(case_value), switch_ssa->input(0).type(), true);

            // TODO: handle non-consts
            //if(!v.ssa().is_num())
                //compiler_error(TODO);

            switch_cfg->build_append_output(case_cfg);
            assert(!switch_cfg->output(0));
            assert(switch_cfg->output_size() >= 1);
            switch_ssa->link_append_input(case_value.ssa());
        }
        break;

    case STMT_DEFAULT:
        {
            cfg_ht const entry = builder.cfg;
            cfg_exits_with_jump();

            cfg_ht const case_cfg = builder.cfg = insert_cfg(true);
            entry->build_set_output(0, case_cfg);

            assert(builder.switch_stack.size() > 0);
            cfg_ht const switch_cfg = builder.switch_stack.back();
            ssa_ht const switch_ssa = switch_cfg->last_daisy();
            passert(switch_ssa->op() == SSA_switch, switch_ssa->op());

            assert(switch_cfg->output_size() >= 1);
            assert(!switch_cfg->output(0));
            switch_cfg->build_set_output(0, case_cfg);

            ++stmt;
        }
        break;

    case STMT_LABEL:
        {
            label_t& label = builder.label_map[stmt];
            unsigned const use_count = stmt->use_count;
            pstring_t const label_name = stmt->pstring;
            ++stmt;

            // If there's no goto to this label, just ignore it.
            if(use_count == 0)
                break;

            cfg_exits_with_jump();
            label.inputs.push_back(builder.cfg);

            if(use_count + 1 == label.inputs.size())
            {
                // All the gotos to this label have been compiled,
                // that means this block can be sealed immediately!
                label.node = builder.cfg = insert_cfg(true, label_name);
                for(cfg_ht node : label.inputs)
                    node->build_set_output(0, label.node);
            }
            else // Otherwise, seal the node at a later time.
                label.node = builder.cfg = insert_cfg(false, label_name);
        }
        break;

    case STMT_GOTO:
        {
            label_t& label = builder.label_map[&fn->def()[stmt->link]];

            stmt_t const& label_stmt = fn->def()[stmt->link];
            assert(label_stmt.name == STMT_LABEL);
            unsigned const label_use_count = label_stmt.use_count;
            assert(label_use_count > 0);

            ++stmt;

            // Add the jump to the label.
            label.inputs.push_back(builder.cfg);
            builder.cfg = compile_goto();

            // If this is the last goto, finish and seal the node.
            if(label_use_count + 1 == label.inputs.size())
            {
                assert(label.node);
                for(cfg_ht node : label.inputs)
                    node->build_set_output(0, label.node);
                // Seal the block.
                seal_block(label.node.data<block_d>());
            }
            break;
        }

    case STMT_GOTO_MODE:
        {
            assert(stmt->expr);

            cfg_ht const branch = builder.cfg;
            cfg_ht const dead = compile_goto();
            cfg_ht const mode = builder.cfg = insert_cfg(true);
            branch->build_set_output(0, mode);
            do_expr<COMPILE>(*stmt->expr);
            ++stmt;
            builder.cfg = dead;
        }
        break;

    case STMT_NMI:
        if(fn->fclass == FN_NMI)
            compiler_error(stmt->pstring, "Cannot wait for nmi inside nmi.");
        if(precheck_tracked)
            precheck_tracked->wait_nmis.push_back(stmt_pstring_mods());
        goto do_fence;
    case STMT_FENCE:
        if(precheck_tracked)
            precheck_tracked->fences.push_back(stmt_pstring_mods());
        // fall-through
    do_fence:
        {
            assert(fn->precheck_fences());
            bc::small_vector<ssa_value_t, 32> inputs;

            block_d& block_data = builder.cfg.data<block_d>();
            ssa_ht const fenced = builder.cfg->emplace_ssa(
                stmt->name == STMT_NMI ? SSA_wait_nmi : SSA_fence, TYPE_VOID);
            fenced->append_daisy();

            ir->gmanager.for_each_gvar([&](gvar_ht gvar, gmanager_t::index_t index)
            {
                for(gmember_ht m : gvar->handles())
                {
                    if(fn->fence_rw().test(m.id))
                    {
                        inputs.push_back(var_lookup(builder.cfg, to_var_i(index), m->member()));
                        inputs.push_back(locator_t::gmember(m, 0));

                        // Create writes after reads:
                        ssa_ht const read = builder.cfg->emplace_ssa(
                            SSA_read_global, m->type(), fenced, locator_t::gmember(m, 0));
                        block_data.vars[to_var_i(index)][m->member()] = read;
                    }
                }
            });

            xbitset_t<gmember_ht> rw(0);

            ir->gmanager.for_each_gmember_set(fn->handle(),
            [&](bitset_uint_t const* gmember_set, gmanager_t::index_t index,locator_t locator)
            {
                rw = fn->fence_rw();

                bitset_and(rw.size(), rw.data(), gmember_set);

                if(!rw.all_clear())
                {
                    inputs.push_back(var_lookup(builder.cfg, to_var_i(index), 0));
                    inputs.push_back(locator);

                    // Create writes after reads:
                    ssa_ht const read = builder.cfg->emplace_ssa(
                        SSA_read_global, TYPE_VOID, fenced, locator);
                    block_data.vars[to_var_i(index)][0] = read;
                }
            });

            fenced->link_append_input(&*inputs.begin(), &*inputs.end());
        }
        ++stmt;
        break;
    }
    }
    assert(false);
}

template<eval_t::do_t D>
expr_value_t eval_t::do_expr(ast_node_t const& ast)
{
    using S = fixed_sint_t;

    auto const make_ptr = [&](locator_t loc, type_t type, bool banked) -> expr_value_t
    {
        rval_t rval = { loc.with_is(IS_PTR) };
        if(banked)
            rval.push_back(loc.with_is(IS_BANK));

        return
        {
            .val = std::move(rval),
            .type = type,
            .pstring = ast.token.pstring,
        };
    };

    auto const infix = [&](auto const& fn, bool flipped = false, bool lhs_lval = false) -> expr_value_t
    {
        expr_value_t lhs = do_expr<D>(ast.children[0]);
        if(!lhs_lval)
            lhs = to_rval<D>(std::move(lhs));
        expr_value_t rhs = to_rval<D>(do_expr<D>(ast.children[1]));
        if(flipped)
            std::swap(lhs, rhs);
        return (this->*fn)(std::move(lhs), std::move(rhs), ast.token);
    };

    // Declare cross-label vars before switch.
    ssa_value_t common_value;
    type_t common_type;

    switch(ast.token.type)
    {
    default:
        throw std::runtime_error(fmt("Invalid token '%' in expression.", token_string(ast.token.type)));

    case TOK_ident:
        if(ast.token.signed_() < 0) // If we have a local const
        {
            assert(local_consts);
            unsigned const const_i = ~ast.token.value;

            expr_value_t result =
            {
                .type = local_consts[const_i].type(),
                .pstring = ast.token.pstring,
            };

            if(!is_check(D))
                result.val = local_consts[const_i].value;

            return result;
        }
        else
        {
            if(is_link(D)) // TODO: perhaps this should move to 'to_rval'.
                compiler_error(ast.token.pstring, "Expression cannot be evaluated at link-time.");

            unsigned const var_i = ast.token.value;
            if(fn && fn->iasm)
                std::printf("asm var_i %i\n", var_i);
            assert(var_i < var_types.size());
            assert(var_i < num_local_vars());

            expr_value_t result =
            {
                .val = lval_t{ .vvar_i = var_i },
                .type = var_types[var_i],
                .pstring = ast.token.pstring,
            };

            if(is_compile(D))
                result.time = RT;
            else if(D == INTERPRET && interpret_locals[var_i].empty())
            {
                compiler_error(ast.token.pstring, 
                    "Variable is invalid because goto jumped past its initialization.");
            }

            assert(result.is_lval());
            return result;
        }

    case TOK_global_ident:
        {
            if(is_link(D))
                compiler_error(ast.token.pstring, "Expression cannot be evaluated at link-time.");

            global_t const* global = ast.token.ptr<global_t>();
            assert(global);

            switch(global->gclass())
            {
            default: 
                throw std::runtime_error(fmt("Unimplemented global in expression. (%)", global));

            case GLOBAL_VAR:
                {
                    if(precheck_tracked)
                        precheck_tracked->gvars_used.emplace(global->handle<gvar_ht>(), ast.token.pstring);

                    expr_value_t result =
                    {
                        .val = lval_t{ .flags = LVALF_IS_GLOBAL, .vglobal = global },
                        .type = global->impl<gvar_t>().type(),
                        .pstring = ast.token.pstring,
                    };

                    if(is_compile(D))
                        result.time = RT;

                    return result;
                }

            case GLOBAL_CONST:
                {
                    const_t const& c = global->impl<const_t>();
                    assert(!is_thunk(c.type().name()));

                    expr_value_t result =
                    {
                        .val = lval_t{ .flags = LVALF_IS_GLOBAL, .vglobal = global },
                        .type = c.type(),
                        .pstring = ast.token.pstring,
                    };

                    return result;
                }

            case GLOBAL_FN:
                return 
                {
                    .val = lval_t{ .flags = LVALF_IS_GLOBAL, .vglobal = global },
                    .type = global->impl<fn_t>().type(), 
                    .pstring = ast.token.pstring 
                };

            case GLOBAL_CHARMAP:
                return
                {
                    .val = lval_t{ .flags = LVALF_IS_GLOBAL, .vglobal = global },
                    .type = TYPE_CHARMAP, 
                    .pstring = ast.token.pstring 
                };
            }
        }
        break;

    case TOK_return:
        {
            if(is_link(D)) // TODO: perhaps this should move to 'to_rval'.
                compiler_error(ast.token.pstring, "Expression cannot be evaluated at link-time.");

            expr_value_t result =
            {
                .val = lval_t{ .flags = LVALF_IS_GLOBAL, .arg = lval_t::RETURN_ARG, .vglobal = &fn->global },
                .type = fn->type().return_type(),
                .pstring = ast.token.pstring,
            };

            if(is_compile(D))
                result.time = RT;

            assert(result.is_lval());
            return result;
        }
        break;

    case TOK_at:
        {
            expr_value_t value = do_expr<D>(ast.children[0]);

            strval_t const* strval;

            if(lval_t const* lval = value.is_lval())
            {
                if(!(lval->flags & LVALF_IS_GLOBAL) || !is_paa(value.type.name()))
                    goto at_error;

                switch(lval->global().gclass())
                {
                case GLOBAL_CONST:
                    {
                        const_t const& c = lval->global().impl<const_t>();
                        bool const banked = c.group_data->banked_ptrs();

                        if(!c.is_paa)
                            goto at_error;

                        return make_ptr(locator_t::gconst(c.handle()), type_t::ptr(c.group(), false, banked), banked);
                    }

                case GLOBAL_VAR:
                    {
                        gvar_t const& v = lval->global().impl<gvar_t>();

                        if(!v.is_paa)
                            goto at_error;

                        return make_ptr(locator_t::gmember(v.begin()), type_t::ptr(v.group(), false, false), false);
                    }

                default: 
                    goto at_error;
                }
            }
            else if((strval = value.is_strval()))
            {
                rom_array_ht const rom_array = sl_manager.get_rom_array(&strval->charmap->global, strval->index, strval->compressed);
                assert(rom_array);
                assert(strval->charmap->group_data());

                group_data_ht const group = strval->charmap->group_data();
                bool const banked = group->banked_ptrs();

                return make_ptr(locator_t::rom_array(rom_array), type_t::ptr(group->group.handle(), false, banked), banked);
            }
            else
            {
            at_error:
                compiler_error(ast.token.pstring, "Cannot get pointer. String literal or pointer-addressable array lvalue required as unary '@' operand.");
            }

        }
        break;


    case TOK_unary_ref:
        {
            expr_value_t value = do_expr<D>(ast.children[0]);
            if(lval_t const* lval = value.is_lval())
            {
                type_t base_type = value.type;
                if(is_paa(base_type.name()))
                    base_type = TYPE_U;
                else if(is_tea(base_type.name()))
                {
                    base_type = base_type.elem_type();
                    if(total_bytes(base_type.name()) > 1 && lval->atom < 0)
                        goto err_mb;
                }
                else if(total_bytes(base_type.name()) > 1 && lval->atom < 0)
                {
                err_mb:
                    char const* s = is_tea(base_type.name()) ? "array" : "type";
                    throw compiler_error_t(
                        fmt_error(value.pstring, fmt("Cannot get address of multi-byte % using unary '&'.", s))
                        + fmt_note(fmt("Type is %.", base_type))
                        + fmt_note(fmt("Use the '.' operator to get a single byte %.", s)));
                }

                if(!is_arithmetic(base_type.name()))
                {
                    throw compiler_error_t(
                        fmt_error(value.pstring, "Cannot get address of non-arithmetic value using unary '&'.")
                        + fmt_note(fmt("Type is %.", base_type)));
                }

                std::uint16_t offset = 0;
                if(lval->index)
                {
                    if(lval->index.is_num())
                        offset = lval->index.whole();
                    else
                    {
                        // TODO
                        assert(false);
                    }
                }

                if(lval->is_global())
                {
                    switch(lval->global().gclass())
                    {
                    case GLOBAL_CONST:
                        {
                            const_t const& c = lval->global().impl<const_t>();

                            if(!c.group_data)
                                compiler_error(value.pstring, "Cannot get address of a constant that doesn't belong to a group.");

                            bool const banked = c.group_data->banked_ptrs();

                            // TODO: not every const has an address. This is buggy.
                            return make_ptr(locator_t::gconst(c.handle(), lval->member, lval->uatom(), offset), 
                                            type_t::addr(banked), banked);
                        }

                    case GLOBAL_VAR:
                        {
                            gvar_t const& gvar = lval->global().impl<gvar_t>();
                            return make_ptr(locator_t::gmember(gvar.begin() + lval->member, lval->uatom(), offset), 
                                            type_t::addr(false), false);
                        }

                    case GLOBAL_FN:
                        {
                            fn_ht const fn = lval->global().handle<fn_ht>();
                            if(lval->arg < 0)
                                return make_ptr(locator_t::fn(fn, lval->ulabel(), offset), type_t::addr(true), true);
                            else
                            {
                                // Referencing a parameter.
                                fn->mark_referenced_param(lval->arg);
                                locator_t loc;
                                if(lval->arg == lval_t::RETURN_ARG)
                                    loc = locator_t::ret(fn, lval->member, lval->uatom(), offset); 
                                else
                                    loc = locator_t::arg(fn, lval->arg, lval->member, lval->uatom(), offset); 
                                return make_ptr(loc, type_t::addr(false), false);
                            }
                        }

                    default: 
                        throw std::runtime_error("Unimplemented global in expression.");
                    }
                }
                else
                {
                    if(!fn)
                        compiler_error(value.pstring, "Cannot get address.");

                    locator_t loc;
                    unsigned const var_i = lval->var_i();

                    if(var_i < fn->def().num_params)
                        loc = locator_t::arg(fn->handle(), lval->var_i(), lval->member, lval->uatom());
                    else
                        loc = locator_t::asm_local_var(fn->handle(), lval->var_i(), lval->member, lval->uatom());

                    return make_ptr(loc, type_t::addr(false), false);
                }
            }
            else if(strval_t const* strval = value.is_strval())
            {
                rom_array_ht const rom_array = sl_manager.get_rom_array(&strval->charmap->global, strval->index, strval->compressed);
                assert(rom_array);
                assert(strval->charmap->group_data());

                group_data_ht const group = strval->charmap->group_data();
                bool const banked = group->banked_ptrs();

                return make_ptr(locator_t::rom_array(rom_array), type_t::addr(banked), banked);
            }
            else
                compiler_error(ast.token.pstring, "lvalue or string literal required as unary '&' operand.");
        }
        break;

    case TOK_true:
    case TOK_false:
        {
            expr_value_t result = { .type = TYPE_BOOL, .pstring = ast.token.pstring };
            if(!is_check(D))
                result.val = rval_t{ ssa_value_t(unsigned(ast.token.type == TOK_true), TYPE_BOOL) };
            return result;
        }

    case TOK_int:
        common_value.set(mask_numeric(fixed_t{ ast.token.value }, TYPE_INT), TYPE_INT);
    push_int:
        {
            expr_value_t result = { .type = TYPE_INT, .pstring = ast.token.pstring };
            if(!is_check(D))
                result.val = rval_t{ common_value };
            return result;
        }

    case TOK_real:
        common_value.set(mask_numeric(fixed_t{ ast.token.value }, TYPE_REAL), TYPE_REAL);
        {
            expr_value_t result = { .type = TYPE_REAL, .pstring = ast.token.pstring };
            if(!is_check(D))
                result.val = rval_t{ common_value };
            return result;
        }
        break;

    case TOK_period:
        {
            // Periods represent struct member access.
            
            using namespace std::literals;

            expr_value_t lhs = do_expr<D>(ast.children[0]);
            std::uint64_t const hash = ast.token.value;

            type_t elem_type = lhs.type;
            unsigned tea_length = 0;
            bool const is_tea = ::is_tea(elem_type.name());
            if(is_tea)
            {
                tea_length = elem_type.size();
                elem_type = elem_type.elem_type();
            }

            if(lval_t const* lval = lhs.is_lval())
            {
                if(lval->is_global()) switch(lval->global().gclass())
                {
                case GLOBAL_CHARMAP:
                    {
                        charmap_t const& charmap = lval->global().impl<charmap_t>();
                        switch(hash)
                        {
                        case fnv1a<std::uint64_t>::hash("size"sv): 
                            lhs.type = TYPE_INT;
                            lhs.val = rval_t{ ssa_value_t(charmap.size(), TYPE_INT) };
                            break;
                        case fnv1a<std::uint64_t>::hash("sentinel"sv): 
                            if(charmap.sentinel() < 0)
                                goto bad_global_accessor;
                            lhs.type = TYPE_U;
                            lhs.val = rval_t{ ssa_value_t(charmap.sentinel(), TYPE_U) };
                            break;
                        default:
                            goto bad_global_accessor;
                        }

                    }
                    goto finish_period;

                case GLOBAL_FN:
                    {
                        if(!lhs.is_lval())
                            compiler_error(ast.token.pstring, "Expecting lvalue.");

                        fn_t const& fn = *to_rval<D>(lhs).ssa().locator().fn();

                        using namespace std::literals;
                        if(hash == fnv1a<std::uint64_t>::hash("return"sv))
                        {
                            lhs.lval().arg = lval_t::RETURN_ARG;
                            lhs.type = fn.type().return_type();
                        }
                        else
                        {
                            auto const& hashes = fn.def().name_hashes;
                            auto it = std::find(hashes.begin(), hashes.end(), hash);

                            if(it == hashes.end())
                                goto bad_global_accessor;

                            unsigned i = it - hashes.begin();

                            if(i < fn.def().num_params) // If it's a param
                            {
                                lhs.lval().arg = i;
                                lhs.type = fn.def().local_vars[i].decl.src_type.type;
                            }
                            else // It's a label
                            {
                                if(lhs.lval().label >= 0)
                                    goto bad_global_accessor;

                                assert(fn.iasm);

                                // Determine the corresponding 'local_consts' index to this label:

                                i -= fn.def().num_params;
                                for(unsigned j = 0; j < fn.def().local_consts.size(); ++j)
                                {
                                    if(fn.def().local_consts[j].is_label() && i-- == 0)
                                    {
                                        lhs.lval().label = j; // OK! Found the label.
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    goto finish_period;

                default:
                    break;

                bad_global_accessor:
                    file_contents_t file(ast.token.pstring.file_i);
                    compiler_error(ast.token.pstring, fmt(
                        "% isn't a member of %.", 
                        ast.token.pstring.view(file.source()), lval->global().name));
                }
            }

            if(elem_type.name() == TYPE_STRUCT)
            {
                struct_t const& s = elem_type.struct_();
                auto const ptr = s.fields().lookup(hash);

                if(!ptr)
                    goto bad_accessor;

                unsigned const field_i = ptr - s.fields().begin();
                unsigned const member_i = member_index(lhs.type, field_i);

                if(lhs.is_lval())
                    lhs.lval().member += member_i;
                else if(lhs.is_rval())
                {
                    if(!is_check(D))
                    {
                        rval_t& rval = lhs.rval();

                        // Shrink the rval to only contain the specified field.
                        unsigned const size = num_members(ptr->second.type());

                        assert(rval.size() == num_members(lhs.type));
                        assert(size + member_i <= rval.size());

                        if(member_i != 0)
                            for(unsigned i = 0; i < size; ++i)
                                rval[i] = std::move(rval[i + member_i]);
                        rval.resize(size);
                    }
                }
                else
                    goto bad_accessor;

                if(is_tea)
                    lhs.type = type_t::tea(ptr->second.type(), tea_length);
                else
                    lhs.type = ptr->second.type();
            }
            else if(lhs.type.name() == TYPE_FN)
            {
                if(!lhs.is_lval())
                    compiler_error(ast.token.pstring, "Expecting lvalue.");

                fn_t const& fn = *to_rval<D>(lhs).ssa().locator().fn();

                using namespace std::literals;
                if(hash == fnv1a<std::uint64_t>::hash("return"sv))
                {
                    lhs.lval().arg = lval_t::RETURN_ARG;
                    lhs.type = fn.type().return_type();
                }
                else
                {
                    auto const& hashes = fn.def().name_hashes;
                    auto it = std::find(hashes.begin(), hashes.end(), hash);

                    if(it == hashes.end())
                        goto bad_accessor;

                    unsigned i = it - hashes.begin();

                    if(i < fn.def().num_params) // If it's a param
                    {
                        lhs.lval().arg = i;
                        lhs.type = fn.def().local_vars[i].decl.src_type.type;
                    }
                    else // It's a label
                    {
                        if(lhs.lval().label >= 0)
                            goto bad_accessor;

                        assert(fn.iasm);

                        // Determine the corresponding 'local_consts' index to this label:

                        i -= fn.def().num_params;
                        for(unsigned j = 0; j < fn.def().local_consts.size(); ++j)
                        {
                            if(fn.def().local_consts[j].is_label() && i-- == 0)
                            {
                                lhs.lval().label = j; // OK! Found the label.
                                break;
                            }
                        }
                    }
                }
            }
            else if(is_scalar(elem_type.name()) || is_banked_ptr(elem_type.name()))
            {
                int shift;
                switch(hash)
                {
                case fnv1a<std::uint64_t>::hash('c'): shift =  2; break;
                case fnv1a<std::uint64_t>::hash('b'): shift =  1; break;
                case fnv1a<std::uint64_t>::hash('a'): shift =  0; break;
                case fnv1a<std::uint64_t>::hash('x'): shift = -1; break;
                case fnv1a<std::uint64_t>::hash('y'): shift = -2; break;
                case fnv1a<std::uint64_t>::hash('z'): shift = -3; break;
                default:
                bad_accessor:
                    file_contents_t file(ast.token.pstring.file_i);
                    compiler_error(ast.token.pstring, fmt(
                        "% isn't a member of %.", 
                        ast.token.pstring.view(file.source()), lhs.type), &file);
                }

                int const atom = shift + frac_bytes(lhs.type.name());

                type_t result_type = TYPE_U;
                if(is_tea)
                    result_type = type_t::tea(TYPE_U, tea_length);

                if(atom < 0 || atom >= int(total_bytes(elem_type.name())))
                    goto bad_accessor;

                if(lhs.is_lval())
                {
                    assert(lhs.lval().atom < 0 || atom == 0);
                    lhs.lval().atom = atom;
                }
                else if(is_interpret(D) && lhs.is_rval())
                {
                    if(tea_length > 0)
                    {
                        ct_array_t const& from = std::get<ct_array_t>(lhs.rval()[0]);
                        ct_array_t to = make_ct_array(tea_length);

                        for(unsigned i = 0; i < tea_length; ++i)
                            to[i] = _interpret_shift_atom(from[i], shift);

                        lhs.rval() = { std::move(to) };
                    }
                    else
                        lhs.rval() = { _interpret_shift_atom(lhs.ssa(), shift) };
                }
                else if(is_compile(D) && lhs.is_rval())
                {
                    ssa_ht const h = builder.cfg->emplace_ssa(
                        tea_length ? SSA_array_get_byte : SSA_get_byte, 
                        result_type, 
                        lhs.ssa(), ssa_value_t(atom, TYPE_U));
                    lhs.rval() = { h };
                }

                lhs.type = result_type;
            }
            else
                goto bad_accessor;

        finish_period:
            lhs.pstring = concat(lhs.pstring, ast.token.pstring);
            return lhs;
        }

    case TOK_apply:
        {
            // TOK_apply is a psuedo token used to represent application. 
            // The token's 'value' stores the application's arity:
            std::size_t const num_children = ast.token.value;
            std::size_t const num_args = num_children - 1;

            bc::small_vector<expr_value_t, 8> exprs(num_children);
            for(unsigned i = 0; i < num_children; ++i)
                exprs[i] = to_rval<D>(do_expr<D>(ast.children[i]));

            //if(handle_lt<D>(rpn_stack, num_args+1, *token)) // TODO
                //break;

            // The first expression is the function:
            expr_value_t& fn_expr = exprs[0];
            expr_value_t* args = exprs.data() + 1;

            if(fn_expr.type.name() != TYPE_FN)
            {
                compiler_error(ast.children[0].token.pstring, fmt(
                    "Expecting function type. Got %.", fn_expr.type));
            }

            assert(fn_expr.is_rval());
            fn_ht const call = fn_expr.ssa().locator().fn();
            pstring_t const call_pstring = concat(fn_expr.pstring, ast.token.pstring);

            if(call->fclass == FN_NMI)
                compiler_error(call_pstring, "Cannot call nmi function.");

            // TODO: make sure modes can't be called normally.
            if(call->fclass == FN_MODE && is_interpret(D))
                compiler_error(call_pstring, "Cannot goto mode at compile-time.");

            std::size_t const num_params = fn_expr.type.num_params();
            type_t const* const params = fn_expr.type.types();

            if(num_args != num_params)
            {
                compiler_error(
                    fn_expr.pstring, fmt(
                    "Passed % arguments to a function of type %. "
                    "Expecting % arguments.",
                    num_args, fn_expr.type, num_params));
            }

            // Now for the arguments.
            // Cast all arguments to match the fn signature.
            int const cast_result = cast_args<D>(fn_expr.pstring, args, args+num_args, params, true);

            if(cast_result >= 0)
            {
                compiler_error(
                    args[cast_result].pstring, fmt(
                    "Unable to convert type % "
                    "to type % in function application.\n"
                    "Expected signature: % ",
                    args[cast_result].type, params[cast_result], fn_expr.type));
            }

            if(precheck_tracked)
            {
                if(call->fclass == FN_MODE) // Track that we're going to a mode here:
                    precheck_tracked->goto_modes.push_back(std::make_pair(call, stmt_pstring_mods()));
                else if(call->fclass != FN_CT)
                    precheck_tracked->calls.emplace(call, call_pstring);
            }

            // Now do the call!

            expr_value_t result =
            {
                .type = fn_expr.type.return_type(), 
                .pstring = call_pstring,
            };

            if(is_interpret(D))
            {
            interpret_fn:
                bc::small_vector<rval_t, 8> rval_args(num_args);
                for(unsigned i = 0; i < num_args; ++i)
                {
                    if(!args[i].is_ct())
                        compiler_error(args[i].pstring, "Expecting compile-time constant value.");
                    rval_args[i] = args[i].rval();
                }

                try
                {
                    // NOTE: call as INTERPRET, not D.
                    eval_t sub(do_wrapper_t<INTERPRET>{}, call_pstring, *call, nullptr, rval_args.data(), rval_args.size());
                    result.val = std::move(sub.final_result.value);
                }
                catch(out_of_time_t& e)
                {
                    e.msg += fmt_note(this->pstring, "Backtrace:");
                    throw;
                }
            }
            else if(is_compile(D))
            {
                if(call->fclass == FN_CT)
                    goto interpret_fn;
                // TODO: Interpret in other situations, too.

                bc::small_vector<ssa_value_t, 32> fn_inputs;

                // The [0] argument holds the fn_t ptr.
                fn_inputs.push_back(fn_expr.ssa());
                
                // For modes, the [1] argument references the stmt:
                if(call->fclass == FN_MODE)
                    fn_inputs.push_back(locator_t::stmt(stmt_handle()));

                // Prepare the input globals

                //bool const is_idep = fn->global.ideps().count(&call->global);
                //assert(is_idep || call->fclass == FN_MODE);

                std::size_t const gmember_bs_size = gmanager_t::bitset_size();
                bitset_uint_t* const temp_bs = ALLOCA_T(bitset_uint_t, gmember_bs_size);

                // Prepare global inputs:

                if(call->fclass == FN_MODE)
                {
                    // 'goto mode's use their modifiers to determine inputs.

                    assert(stmt->name == STMT_GOTO_MODE);
                    mods_t const& mods = fn->def()[stmt->mods];

                    bitset_uint_t* const preserved_bs = ALLOCA_T(bitset_uint_t, gmember_bs_size);
                    bitset_clear_all(gmember_bs_size, preserved_bs);
                    mods.for_each_group_vars([&](group_vars_ht gv)
                    {
                        assert(gmember_bs_size == gv->gmembers().size());
                        bitset_or(gmember_bs_size, preserved_bs, gv->gmembers().data());
                    });

                    ir->gmanager.for_each_gmember_set(fn->handle(),
                    [&](bitset_uint_t const* gmember_set, gmanager_t::index_t index,locator_t locator)
                    {
                        bitset_copy(gmember_bs_size, temp_bs, preserved_bs);
                        bitset_and(gmember_bs_size, temp_bs, gmember_set);
                        if(bitset_all_clear(gmember_bs_size, temp_bs))
                            return;
                        fn_inputs.push_back(var_lookup(builder.cfg, to_var_i(index), 0));
                        fn_inputs.push_back(locator);
                    });

                    ir->gmanager.for_each_gvar(
                    [&](gvar_ht gvar, gmanager_t::index_t index)
                    {
                        if(!mods.group_vars.count(gvar->group()))
                            return;

                        for(gmember_ht m : gvar->handles())
                        {
                            fn_inputs.push_back(var_lookup(builder.cfg, to_var_i(index), m->member()));
                            fn_inputs.push_back(locator_t::gmember(m, 0));
                        }
                    });
                }
                else
                {
                    assert(call->ir_reads().size() == gmember_bs_size);

                    // Use 'ir_reads()' to determine which members are needed by the called fn.

                    ir->gmanager.for_each_gmember_set(fn->handle(),
                    [&](bitset_uint_t const* gmember_set, gmanager_t::index_t index,locator_t locator)
                    {
                        bitset_copy(gmember_bs_size, temp_bs, call->ir_reads().data());
                        bitset_and(gmember_bs_size, temp_bs, gmember_set);
                        if(bitset_all_clear(gmember_bs_size, temp_bs))
                            return;
                        fn_inputs.push_back(var_lookup(builder.cfg, to_var_i(index), 0));
                        fn_inputs.push_back(locator);
                    });

                    ir->gmanager.for_each_gvar(
                    [&](gvar_ht gvar, gmanager_t::index_t index)
                    {
                        for(gmember_ht m = gvar->begin(); m != gvar->end(); ++m)
                        {
                            if(call->ir_reads().test(m.id))
                            {
                                fn_inputs.push_back(var_lookup(builder.cfg, to_var_i(index), m->member()));
                                fn_inputs.push_back(locator_t::gmember(m, 0));
                            }
                        }
                    });
                }

                // Prepare the arguments
                for(unsigned i = 0; i < num_params; ++i)
                {
                    type_t const param_type = call->type().type(i);
                    unsigned const num_param_members = ::num_members(param_type);

                    for(unsigned j = 0; j < num_param_members; ++j)
                    {
                        locator_t const loc = locator_t::arg(call, i, j, 0);

                        type_t const member_type = ::member_type(param_type, j);

                        fn_inputs.push_back(from_variant<COMPILE>(args[i].rval()[j], member_type));
                        fn_inputs.push_back(loc);
                    }
                }

                // Create the dependent node.
                ssa_op_t const op = (call->fclass == FN_MODE) ? SSA_goto_mode : SSA_fn_call;
                ssa_ht const fn_node = builder.cfg->emplace_ssa(op, TYPE_VOID);
                fn_node->link_append_input(&*fn_inputs.begin(), &*fn_inputs.end());

                if(call->fclass == FN_MODE || !call->ir_io_pure()/* || ir->gmanager.num_locators() > 0*/)
                    fn_node->append_daisy();

                if(call->fclass != FN_MODE)
                {
                    assert(fn->global.ideps().count(&call->global));

                    // After the fn is called, read all the globals it has written to:

                    ir->gmanager.for_each_gvar([&](gvar_ht gvar, gmanager_t::index_t index)
                    {
                        for(gmember_ht m = gvar->begin(); m != gvar->end(); ++m)
                        {
                            if(call->ir_writes().test(m.id))
                            {
                                ssa_ht read = builder.cfg->emplace_ssa(
                                    SSA_read_global, m->type(), fn_node, locator_t::gmember(m, 0));
                                block_d& block_data = builder.cfg.data<block_d>();
                                block_data.vars[to_var_i(index)][m->member()] = read;
                            }
                        }
                    });

                    ir->gmanager.for_each_gmember_set(fn->handle(),
                    [&](bitset_uint_t const* gvar_set, gmanager_t::index_t index, locator_t locator)
                    {
                        bitset_copy(gmember_bs_size, temp_bs, gvar_set);
                        bitset_and(gmember_bs_size, temp_bs, call->ir_writes().data());
                        if(!bitset_all_clear(gmember_bs_size, temp_bs))
                        {
                            ssa_ht read = builder.cfg->emplace_ssa(
                                SSA_read_global, TYPE_VOID, fn_node, locator);
                            block_d& block_data = builder.cfg.data<block_d>();
                            block_data.vars[to_var_i(index)][0] = read;
                        }
                    });

                    type_t const return_type = fn_expr.type.return_type();
                    unsigned const return_members = ::num_members(return_type);

                    for(unsigned m = 0; m < return_members; ++m)
                    {
                        ssa_ht ret = builder.cfg->emplace_ssa(
                            SSA_read_global, member_type(return_type, m), fn_node, locator_t::ret(call, m, 0));

                        result.rval().push_back(ret);
                    }
                }
            }

            return result;
        }

    case TOK_hw_addr:
        {
            expr_value_t result = { .type = TYPE_APTR, .pstring = ast.token.pstring };
            if(!is_check(D))
                result.val = rval_t{ ssa_value_t(ast.token.value, TYPE_APTR) };
            return result;
        }

    case TOK_read_hw:
        {
            if(is_interpret(D))
                compiler_error(stmt->pstring, "Hardware expression cannot be evaluated at compile-time.");

            expr_value_t addr = throwing_cast<D>(do_expr<D>(ast.children[0]), TYPE_APTR, true);

            expr_value_t result = 
            {
                .type = TYPE_U, 
                .pstring = ast.token.pstring,
            };

            if(is_compile(D))
            {
                ssa_ht const h = builder.cfg->emplace_ssa(
                    SSA_read_ptr_hw, TYPE_U, 
                    addr.ssa(), ssa_value_t(), ssa_value_t(), 
                    ssa_value_t(0u, TYPE_U));
                h->append_daisy();
                result.val = rval_t{ h };
            }

            return result;
        }

    case TOK_write_hw:
        {
            if(is_interpret(D))
                compiler_error(stmt->pstring, "Hardware expression cannot be evaluated at compile-time.");

            expr_value_t addr = throwing_cast<D>(do_expr<D>(ast.children[0]), TYPE_APTR, true);
            expr_value_t arg  = throwing_cast<D>(do_expr<D>(ast.children[1]), TYPE_U, true);

            expr_value_t result = 
            {
                .type = TYPE_U, 
                .pstring = ast.token.pstring,
            };

            if(is_compile(D))
            {
                ssa_ht const h = builder.cfg->emplace_ssa(
                    SSA_write_ptr_hw, TYPE_VOID, 
                    addr.ssa(), ssa_value_t(), ssa_value_t(), 
                    ssa_value_t(0u, TYPE_U), arg.ssa());
                h->append_daisy();
                result.val = rval_t{ h };
            }

            return result;
        }

    case TOK_byte_block_data:
        if(!is_check(D))
        {
            std::vector<locator_t> paa;

            unsigned const n = ast.num_children();
            for(unsigned i = 0; i < n; ++i)
            {
                ast_node_t const& sub = ast.children[i];

                switch(sub.token.type)
                {
                case TOK_byte_block_byte_array:
                    {
                        auto const* data = sub.token.ptr<std::vector<std::uint8_t>>();
                        paa.reserve(paa.size() + data->size());
                        for(std::uint8_t i : *data)
                            paa.push_back(locator_t::const_byte(i));
                    }
                    break;

                case TOK_byte_block_locator_array:
                    {
                        auto const* data = sub.token.ptr<std::vector<locator_t>>();
                        paa.insert(paa.end(), data->begin(), data->end());
                    }
                    break;

                default:
                    {
                        expr_value_t arg = to_rval<D>(do_expr<D>(sub));
                        if(sub.token.type != TOK_cast && is_ct(arg.type))
                            throw compiler_error_t(
                                fmt_error(sub.token.pstring, fmt("Expression of type % in byte block.", arg.type))
                                + fmt_note("Use an explicit cast to override."));
                        ::append_locator_bytes(paa, arg.rval(), arg.type, arg.pstring);
                    }
                    break;
                }
            }

            byte_block_data = std::move(paa);
        }
        return {};

    case TOK_byte_block_proc:
        if(!is_check(D))
        {
            asm_proc_t proc;
            proc.entry_label = locator_t::minor_label(ENTRY_LABEL);
            proc.push_inst({ .op = ASM_LABEL, .arg = proc.entry_label });

            unsigned const n = ast.num_children();
            for(unsigned i = 0; i < n; ++i)
            {
                ast_node_t const& sub = ast.children[i];

                switch(ast.children[i].token.type)
                {
                case TOK_byte_block_asm_op:
                    {
                        op_t const asm_op = op_t(sub.token.value);
                        locator_t value = {};

                        if(sub.children)
                        {
                            assert(op_addr_mode(asm_op) != MODE_IMPLIED);

                            expr_value_t arg = throwing_cast<INTERPRET_CE>(
                                do_expr<INTERPRET_CE>(*sub.children), 
                                op_addr_mode(asm_op) == MODE_IMMEDIATE ? TYPE_U : TYPE_APTR, 
                                false);

                            assert(arg.rval().size() == 1);

                            ssa_value_t const v = arg.ssa();

                            if(v.is_locator())
                                value = v.locator();
                            else if(v.is_num())
                            {
                                if(op_addr_mode(asm_op) == MODE_IMMEDIATE)
                                    value = locator_t::const_byte(v.whole());
                                else
                                    value = locator_t::addr(v.whole());
                            }
                            else
                            {
                                // Likely a bug if this fires:
                                assert(false);
                                compiler_error(sub.token.pstring, "Unable to compile assembly instruction.");
                            }
                        }

                        proc.push_inst({ .op = asm_op, .iasm_child = i, .arg = value });
                        
                    }
                    break;

                case TOK_byte_block_label:
                    proc.push_inst({ .op = ASM_LABEL, .iasm_child = i, .arg = locator_t::minor_label(sub.token.value) });
                    break;

                case TOK_byte_block_call:
                case TOK_byte_block_goto:
                    {
                        global_t const* g = sub.token.ptr<global_t>();

                        if(g->gclass() != GLOBAL_FN || g->impl<fn_t>().fclass != FN_FN)
                            compiler_error(sub.token.pstring, fmt("% is not a callable function.", g->name));

                        if(precheck_tracked)
                            precheck_tracked->calls.emplace(g->handle<fn_ht>(), sub.token.pstring);

                        op_t const op = sub.token.type == TOK_byte_block_call ? BANKED_Y_JSR : BANKED_Y_JMP;
                        proc.push_inst({ .op = LDY_IMMEDIATE, .iasm_child = i, .arg = locator_t::fn(g->handle<fn_ht>()).with_is(IS_BANK) });
                        proc.push_inst({ .op = op, .iasm_child = i, .arg = locator_t::fn(g->handle<fn_ht>()) });
                    }
                    break;

                case TOK_byte_block_goto_mode:
                    {
                        global_t const* g = sub.token.ptr<global_t>();

                        if(g->gclass() != GLOBAL_FN || g->impl<fn_t>().fclass != FN_MODE)
                            compiler_error(sub.token.pstring, fmt("% is not a mode.", g->name));

                        if(precheck_tracked)
                            precheck_tracked->goto_modes.push_back({ g->handle<fn_ht>(), { sub.token.pstring, sub.mods }});

                        // TODO
                        assert(false);
                        throw std::runtime_error("unimplemented");
                    }
                    break;

                case TOK_byte_block_wait_nmi:
                    if(precheck_tracked)
                        precheck_tracked->wait_nmis.push_back({ sub.token.pstring, sub.mods });
                    proc.push_inst({ .op = JSR_ABSOLUTE, .iasm_child = i, .arg = locator_t::runtime_rom(RTROM_wait_nmi) });
                    break;

                case TOK_byte_block_byte_array:
                    {
                        auto const* data = sub.token.ptr<std::vector<std::uint8_t>>();
                        proc.code.reserve(proc.code.size() + data->size());
                        for(std::uint8_t i : *data)
                            proc.push_inst({ .op = ASM_DATA, .iasm_child = i, .arg = locator_t::const_byte(i) });
                    }
                    break;

                case TOK_byte_block_locator_array:
                    {
                        auto const* data = sub.token.ptr<std::vector<locator_t>>();
                        proc.code.reserve(proc.code.size() + data->size());
                        for(locator_t loc : *data)
                            proc.push_inst({ .op = ASM_DATA, .iasm_child = i, .arg = loc });
                    }
                    break;

                default:
                    {
                        // TODO! this is SLOWW
                        std::puts("TODO: FIX SLOW");
                        thread_local loc_vec_t vec_temp;
                        vec_temp.clear();

                        expr_value_t arg = to_rval<D>(do_expr<D>(sub));
                        ::append_locator_bytes(vec_temp, arg.rval(), arg.type, arg.pstring);

                        for(locator_t loc : vec_temp)
                            proc.push_inst({ .op = ASM_DATA, .iasm_child = i, .arg = loc });
                    }
                }
            }

            byte_block_data = std::move(proc);
        }
        return {};

    case TOK_cast:
        {
            // TOK_cast are pseudo tokens used to implement type casts.

            // Extract how many args this cast parsed:
            std::size_t const num_children = ast.token.value;
            std::size_t const num_args = num_children - 1;

            bc::small_vector<expr_value_t, 8> args(num_args);
            for(unsigned i = 0; i < num_args; ++i)
                args[i] = to_rval<D>(do_expr<D>(ast.children[i + 1]));

            // The first expr holds the type.
            assert(ast.children[0].token.type == TOK_cast_type);
            type_t type = dethunkify({ ast.children[0].token.pstring, *ast.children[0].token.ptr<type_t const>() }, true, this);

            // Only handle LT for non-aggregates.
            // TODO
            //if(!is_aggregate(type.name()) && handle_lt<D>(rpn_stack, argn, token-1, token+1))
                //break;

            auto const check_argn = [&](unsigned size)
            { 
                if(num_args != size)
                    compiler_error(ast.token.pstring, fmt(
                        "Too % arguments to %. Expecting %.", 
                        num_args < size ? "few" : "many", type, size));
            };

            if(is_aggregate(type.name()))
            {
                expr_value_t result = { .pstring = ast.token.pstring };

                if(type.name() == TYPE_STRUCT)
                {
                    struct_t const& s = type.struct_();
                    check_argn(s.fields().size());

                    type_t* const types = ALLOCA_T(type_t, s.fields().size());
                    for(unsigned i = 0; i < s.fields().size(); ++i)
                    {
                        types[i] = s.field(i).type();
                        assert(!is_thunk(types[i].name()));
                    }

                    int const cast_result = cast_args<D>(
                        ast.token.pstring, args.data(), args.data() + num_args, types, false);

                    if(cast_result >= 0)
                    {
                        assert(cast_result < (int)s.fields().size());
                        compiler_error(
                            args[cast_result].pstring, fmt(
                            "Unable to convert type % to type % in cast to %.\n",
                            args[cast_result].type, types[cast_result], s.global.name));
                    }

                    if(!is_check(D))
                    {
                        // Create a new rval.
                        rval_t new_rval;
                        new_rval.reserve(num_members(type));

                        for(unsigned i = 0; i < num_args; ++i)
                            for(auto& v : args[i].rval())
                                new_rval.push_back(std::move(v));

                        assert(new_rval.size() == num_members(type));

                        result.val = std::move(new_rval);
                    }
                }
                else if(type.name() == TYPE_TEA)
                {
                    if(num_args == 1 && args[0].type.name() == TYPE_TEA)
                    {
                        if(type.size() == 0)
                            type.unsafe_set_size(args[0].type.size());

                        return throwing_cast<D>(std::move(args[0]), type, false);
                    }

                    if(type.size() == 0)
                    {
                        if(num_args == 0)
                            compiler_error(ast.token.pstring, "Invalid array length of 0.");
                        type.unsafe_set_size(num_args);
                    }
                    else 
                        check_argn(type.size());

                    for(unsigned i = 0; i < num_args; ++i)
                        args[i] = throwing_cast<D>(std::move(args[i]), type.elem_type(), false);

                    if(is_interpret(D))
                    {
                    interpret_cast_array:
                        // Create a new rval.
                        rval_t new_rval(num_members(type));

                        for(unsigned i = 0; i < new_rval.size(); ++i)
                        {
                            ct_array_t shared = make_ct_array(num_args);
                            for(unsigned j = 0; j < num_args; ++j)
                                shared[j] = std::get<ssa_value_t>(args[j].rval()[i]);
                            new_rval[i] = std::move(shared);
                        }

                        result.val = std::move(new_rval);
                    }
                    else if(D == COMPILE)
                    {
                        unsigned const num_mem = num_members(type);

                        for(unsigned i = 0; i < num_mem; ++i)
                        {
                            type_t const mt = member_type(type, i);
                            assert(mt.name() == TYPE_TEA);

                            for(unsigned j = 0; j < num_args; ++j)
                                if(!args[j].is_ct())
                                    goto isnt_ct;
                        }
                        goto interpret_cast_array;
                    isnt_ct:

                        // Create a new rval.
                        rval_t new_rval(num_mem);

                        for(unsigned i = 0; i < num_mem; ++i)
                        {
                            type_t const mt = member_type(type, i);
                            assert(mt.name() == TYPE_TEA);

                            ssa_ht h = builder.cfg->emplace_ssa(SSA_init_array, type);
                            h->alloc_input(num_args);
                            for(unsigned j = 0; j < num_args; ++j)
                                h->build_set_input(j, std::get<ssa_value_t>(args[j].rval()[i]));

                            new_rval[i] = h;
                        }

                        result.val = std::move(new_rval);
                    }
                }

                result.type = type;
                return result;
            }
            else if(is_scalar(type.name()))
            {
                check_argn(1);
                return throwing_cast<D>(args[0], type, false);
            }
            else
                compiler_error(ast.token.pstring, fmt("Unable to cast to %.", type));
        }

    case TOK_index8:
    case TOK_index16:
        {
            bool const is8 = ast.token.type == TOK_index8;

            // TOK_index is a psuedo token used to implement array indexing. 

            // TODO
            //if(handle_lt<D>(rpn_stack, 2, *token))
                //break;

            expr_value_t array_val = do_expr<D>(ast.children[0]);

            bool const is_ptr = ::is_ptr(array_val.type.name());
            //bool const is_mptr = ::is_mptr(array_val.type.name()); TODO

            if(is_ptr && is_interpret(D))
                compiler_error(ast.token.pstring, "Pointers cannot be dereferenced at compile-time.");

            if(is_ptr && ::is_aptr(array_val.type.name()))
            {
                compiler_error(ast.token.pstring, fmt(
                    "Unable to dereference type % using '%'.", array_val.type,
                    is8 ? "[]" : "{}"));
            }

            if(!is_tea(array_val.type.name()) && !is_ptr)
            {
                std::string note;
                if(is_paa(array_val.type.name()))
                    note = fmt_note("Did you forget to use operator '@'?");
                throw compiler_error_t(
                    fmt_error(array_val.pstring, fmt(
                        "Expecting array or pointer type to '%'. Got %.", 
                        is8 ? "[]" : "{}", array_val.type))
                    + note);
            }

            if(is_ptr && precheck_tracked)
            {
                // TODO: Update this when inlining is added.
                unsigned const size = array_val.type.group_tail_size();
                precheck_tracked->deref_types.insert(array_val.type);
                for(unsigned i = 0; i < size; ++i)
                    precheck_tracked->deref_groups.emplace(array_val.type.group(i), src_type_t{ array_val.pstring, array_val.type });
            }

            expr_value_t array_index = throwing_cast<D>(do_expr<D>(ast.children[1]), is8 ? TYPE_U : TYPE_U20, true);

            auto const compile_read_ptr = [&](ssa_value_t ptr, ssa_value_t bank)
            {
                if(is8)
                {
                    return builder.cfg->emplace_ssa(
                        SSA_read_ptr, TYPE_U, 
                        ptr, ssa_value_t(), bank,
                        std::get<ssa_value_t>(array_index.rval()[0]));
                }
                else
                {
                    ssa_ht h = builder.cfg->emplace_ssa(
                        SSA_add, TYPE_APTR, 
                        ptr,
                        std::get<ssa_value_t>(array_index.rval()[0]),
                        ssa_value_t(0u, TYPE_BOOL));

                    return builder.cfg->emplace_ssa(
                        SSA_read_ptr, TYPE_U, 
                        h, ssa_value_t(), bank,
                        ssa_value_t(0u, TYPE_U));
                }
            };

            if(!is_check(D) && array_val.is_lval())
            {
                if(is_ptr)
                {
                    assert(is_compile(D));

                    rval_t rval = to_rval<D>(array_val).rval();

                    bool const is_banked = is_banked_ptr(array_val.type.name());

                    deref_t deref =
                    {
                        .ptr = from_variant<D>(rval[0], array_val.type), 
                        .bank = is_banked ? from_variant<D>(rval[1], TYPE_U) : ssa_value_t(), 
                        .index = array_index.ssa()
                    };

                    if(!is8)
                    {
                         deref.ptr = builder.cfg->emplace_ssa(
                            SSA_add, TYPE_APTR, 
                            deref.ptr, deref.index,
                            ssa_value_t(0u, TYPE_BOOL));

                         deref.index = ssa_value_t(0u, TYPE_U);
                    }

                    array_val.val = std::move(deref);
                }
                else
                {
                    if(!is8)
                        array_val.lval().flags |= LVALF_INDEX_16;
                    assert(!array_val.lval().index);
                    array_val.lval().index = array_index.ssa();
                }
            }
            else if(rval_t* rval_ptr = array_val.is_rval())
            {
                rval_t& rval = *rval_ptr;

                if(is_interpret(D))
                {
                    assert(0); // TODO: implement
                    /*
                    unsigned const index = array_index.whole();
                    array_val.index.set(index, TYPE_U);
                    
                    if(index >= array_val.type.array_length())
                    {
                        compiler_error(array_index.pstring, 
                            fmt("Array index is out of bounds. (index: % >= size: %)", 
                                index, array_val.type.array_length()));
                    }

                    for(ct_variant_t& v : array_val.rval)
                        v = std::get<ct_array_t>(v)[index];
                        */
                }
                else if(is_compile(D))
                {
                    if(is_ptr)
                    {
                        bool const is_banked = is_banked_ptr(array_val.type.name());
                        assert(array_val.rval().size() == is_banked ? 2 : 1);

                        // TODO
                        //ssa_value_t prev_in_order = {};
                        //if(auto ptr_i = ir->gmanager.ptr_i(array_val.type))
                            //prev_in_order = var_lookup(builder.cfg, to_var_i(ptr_i), 0);

                        ssa_ht const h = compile_read_ptr(
                            from_variant<D>(rval[0], array_val.type),
                            is_banked ? from_variant<D>(rval[1], TYPE_U) : ssa_value_t());

                        if(ptr_to_vars(array_val.type))
                            h->append_daisy();

                        rval[0] = h;
                        rval.resize(1);
                    }
                    else
                    {
                        for(unsigned i = 0; i < rval.size(); ++i)
                        {
                            type_t const etype = ::member_type(array_val.type, i);
                            assert(etype.name() == TYPE_TEA);

                            rval[i] = builder.cfg->emplace_ssa(
                                is8 ? SSA_read_array8 : SSA_read_array16, etype.elem_type(), 
                                from_variant<D>(rval[i], etype), ssa_value_t(0u, TYPE_U20), 
                                std::get<ssa_value_t>(array_index.rval()[0]));
                        }
                    }
                }
            }
            else if(strval_t* strval = array_val.is_strval())
            {
                std::string const& str = strval->get_string();

                if(is_interpret(D) || (is_compile(D) && array_index.ssa().is_num()))
                {
                    unsigned const i = array_index.whole();

                    if(i >= str.size())
                    {
                        compiler_error(array_index.pstring, 
                            fmt("Array index is out of bounds. (index: % >= size: %)", 
                                i, str.size()));
                    }

                    array_val.val = rval_t{ ssa_value_t(std::uint8_t(str[i]), TYPE_U) };
                }
                else if(is_compile(D))
                {
                    // convert to rom data
                    rom_array_ht const rom_array = sl_manager.get_rom_array(&strval->charmap->global, strval->index, strval->compressed);
                    assert(rom_array);
                    assert(strval->charmap->group_data());
                    bool const is_banked = strval->charmap->group_data()->banked_ptrs();

                    locator_t const loc = locator_t::rom_array(rom_array);

                    ssa_ht const h = compile_read_ptr(
                        loc, is_banked ? loc.with_is(IS_BANK) : ssa_value_t());

                    array_val.val = rval_t { h };
                }
            }

            if(is_ptr)
                array_val.type = TYPE_U;
            else
                array_val.type = array_val.type.elem_type();

            array_val.pstring = concat(array_val.pstring, array_index.pstring);

            return array_val;
        }

        /* TODO: remove
    case TOK_dquote:
        {
            string_literal_t const* literal = token.ptr<string_literal_t>();
            assert(literal);

            global_t const* charmap = literal->charmap;
            if(!charmap)
                TODO;

            if(charmap->gclass() != GLOBAL_CHARMAP)
                compiler_error(literal->pstring, fmt("% is not a charmap.", charmap->name));

            assert(false);
        }
        break;
        */

    case TOK_character:
        {
            assert(ast.charmap);
            charmap_t const& charmap = get_charmap(ast.token.pstring, *ast.charmap);

            int const result = charmap.convert(ast.token.value);
            if(result < 0)
                compiler_error(ast.token.pstring, fmt("Character isn't in %.", charmap.global.name));

            common_value.set(result, TYPE_INT);
            goto push_int;
        }

    case TOK_string_compressed:
    case TOK_string_uncompressed:
        {
            assert(ast.charmap);
            charmap_t const& charmap = get_charmap(ast.token.pstring, *ast.charmap);

            bool const compressed = ast.token.type == TOK_string_compressed;
            unsigned const index = ast.token.value;
            unsigned const str_size = sl_manager.get_string(&charmap.global, index, compressed).size();

            expr_value_t v =
            { 
                .val = strval_t{ .charmap = &charmap, .compressed = compressed, .index = ast.token.value },
                .type = type_t::tea(TYPE_U, str_size), 
                .pstring = ast.token.pstring,
            };

            return v;
        }

    case TOK_sizeof_expr:
        common_type = do_expr<CHECK>(ast.children[0]).type;
        goto do_sizeof;

    case TOK_sizeof:
        {
            common_type = dethunkify({ ast.token.pstring, *ast.token.ptr<type_t const>() }, true, this);
        do_sizeof:
            unsigned const size = common_type.size_of();

            if(size == 0)
                compiler_error(ast.token.pstring, fmt("Type % has no size.", common_type));

            common_value.set(size, TYPE_INT);
            goto push_int;
        }

    case TOK_len_expr:
        common_type = do_expr<CHECK>(ast.children[0]).type;
        goto do_len;

    case TOK_len:
        {
            common_type = dethunkify({ ast.token.pstring, *ast.token.ptr<type_t const>() }, true, this);
        do_len:
            if(is_check(D))
                goto push_int;

            unsigned const size = common_type.array_length();

            if(size == 0)
                compiler_error(ast.token.pstring, fmt("Type % isn't an array.", common_type));

            common_value.set(size, TYPE_INT);
            goto push_int;
        }

    case TOK_assign:
        return infix(&eval_t::do_assign<D>, false, true);

    case TOK_logical_and:
    case TOK_logical_or:
        return do_logical<D>(ast);

    case TOK_eq:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct eq_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs == rhs; }
            static ssa_op_t op() { return SSA_eq; }
        };
        return infix(&eval_t::do_compare<eq_p>);
    case TOK_not_eq:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct not_eq_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs != rhs; }
            static ssa_op_t op() { return SSA_not_eq; }
        };
        return infix(&eval_t::do_compare<not_eq_p>);
    case TOK_lt:
    case TOK_gt:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct lt_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs < rhs; }
            static ssa_op_t op() { return SSA_lt; }
        };
        return infix(&eval_t::do_compare<lt_p>, ast.token.type == TOK_gt);
    case TOK_lte:
    case TOK_gte:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct lte_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs <= rhs; }
            static ssa_op_t op() { return SSA_lte; }
        };
        return infix(&eval_t::do_compare<lte_p>, ast.token.type == TOK_gte);

    case TOK_plus:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct plus_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs + rhs; }
            static ssa_op_t op() { return SSA_add; }
        };
        return infix(&eval_t::do_arith<plus_p>);

    case TOK_plus_assign:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        return infix(&eval_t::do_assign_arith<plus_p>, false, true);

    case TOK_minus: 
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct minus_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs - rhs; }
            static ssa_op_t op() { return SSA_sub; }
        };
        return infix(&eval_t::do_arith<minus_p>);
    case TOK_minus_assign:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        return infix(&eval_t::do_assign_arith<minus_p>, false, true);

    case TOK_bitwise_and: 
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct bitwise_and_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs & rhs; }
            static ssa_op_t op() { return SSA_and; }
        };
        return infix(&eval_t::do_arith<bitwise_and_p>);
    case TOK_bitwise_and_assign:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        return infix(&eval_t::do_assign_arith<bitwise_and_p>, false, true);

    case TOK_bitwise_or:  
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct bitwise_or_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs | rhs; }
            static ssa_op_t op() { return SSA_or; }
        };
        return infix(&eval_t::do_arith<bitwise_or_p>);
    case TOK_bitwise_or_assign:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        return infix(&eval_t::do_assign_arith<bitwise_or_p>, false, true);

    case TOK_bitwise_xor:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct bitwise_xor_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs ^ rhs; }
            static ssa_op_t op() { return SSA_xor; }
        };
        return infix(&eval_t::do_arith<bitwise_xor_p>);
    case TOK_bitwise_xor_assign:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        return infix(&eval_t::do_assign_arith<bitwise_xor_p>, false, true);

    case TOK_lshift:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct lshift_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, std::uint8_t shift) { return lhs << shift; }
            static ssa_op_t op() { return SSA_shl; }
        };
        return infix(&eval_t::do_shift<lshift_p>);
    case TOK_lshift_assign:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        return infix(&eval_t::do_assign_shift<lshift_p>, false, true);

    case TOK_rshift:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        struct rshift_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, std::uint8_t shift) { return lhs >> shift; }
            static ssa_op_t op() { return SSA_shr; }
        };
        return infix(&eval_t::do_shift<rshift_p>);
    case TOK_rshift_assign:
        // TODO
        //if(handle_lt<D>(rpn_stack, 2, *token))
            //break;
        return infix(&eval_t::do_assign_shift<rshift_p>, false, true);

    case TOK_unary_negate:
        // TODO
        //if(handle_lt<D>(rpn_stack, 1, *token))
            //break;
        {
            expr_value_t v = throwing_cast<D>(do_expr<D>(ast.children[0]), TYPE_BOOL, true);

            if(is_interpret(D))
                v.ssa().set(unsigned(!v.fixed()), TYPE_BOOL);
            else if(is_compile(D))
            {
                // Must be two lines; reference invalidation lurks.
                ssa_ht const ssa = builder.cfg->emplace_ssa(SSA_eq, TYPE_BOOL, v.ssa(), ssa_value_t(0u, TYPE_BOOL));
                v.ssa() = ssa;
            }

            return v;
        }

    case TOK_unary_plus:
        // TODO
        //if(handle_lt<D>(rpn_stack, 1, *token))
            //break;
        {
            expr_value_t v = to_rval<D>(do_expr<D>(ast.children[0]));
            req_quantity(ast.token, v);
            if(!is_arithmetic(v.type.name()) && !is_ptr(v.type.name()))
            {
                compiler_error(v.pstring, fmt("% expects arithmetic or pointer inputs. (Operand is %)", 
                                              token_string(ast.token.type), v.type));
            }
            return v;
        }

    case TOK_unary_minus:
        // TODO
        //if(handle_lt<D>(rpn_stack, 1, *token))
            //break;
        {
            expr_value_t v = to_rval<D>(do_expr<D>(ast.children[0]));
            req_quantity(ast.token, v);

            if(is_interpret(D))
                v.ssa().set(mask_numeric(fixed_t{ -v.fixed().value }, v.type.name()), v.type.name());
            else if(D == COMPILE)
            {
                // Must be two lines; reference invalidation lurks.
                ssa_ht const ssa = builder.cfg->emplace_ssa(SSA_sub, v.type, ssa_value_t(0u, v.type.name()), v.ssa());
                v.ssa() = ssa;
            }

            return v;
        }

    case TOK_unary_xor:
        // TODO
        //if(handle_lt<D>(rpn_stack, 1, *token))
            //break;
        {
            expr_value_t v = to_rval<D>(do_expr<D>(ast.children[0]));
            req_quantity(ast.token, v);

            if(is_interpret(D))
                v.ssa().set(mask_numeric(fixed_t{ ~v.fixed().value }, v.type.name()), v.type.name());
            else if(D == COMPILE)
            {
                // Must be two lines; reference invalidation lurks.
                ssa_ht const ssa = builder.cfg->emplace_ssa(
                    SSA_xor, v.type, ssa_value_t(numeric_bitmask(v.type.name()), v.type.name()), v.ssa());
                v.ssa() = ssa;
            }

            return v;
        }
    }
    assert(false);
    return {};
}

/* TODO
template<eval_t::do_t D>
void eval_t::do_expr(rpn_stack_t& rpn_stack, token_t const* expr)
{
    check_time();

    rpn_stack.clear(); // Reset the stack.

    assert(expr);
    for(token_t const* token = expr; token->type;)
        token = do_token<D>(rpn_stack, token);
}
*/

/* TODO
type_t lval_type(lval_t const& lval)
{
    type_t type = var_types[lval.var_i];
    
    for(unsigned field : lval.fields)
    {
        assert(type.name() == TYPE_STRUCT);
        struct_t const& s = *type.struct_();
        type = s.field(type).type();


    }

    unsigned member = lval.member;
    for(unsigned i = 0; i < lval.fields_accesed; ++i)
    {

        unsigned field = s.member_field(member);
        member -= 
        s.field(field).type();

        unsigned field = type.struct_()->member_field(member);
    }
}
*/

template<eval_t::do_t D>
expr_value_t eval_t::to_rval(expr_value_t v)
{
    if(v.is_rval())
        return v;

    if(lval_t* lval = v.is_lval())
    {
        unsigned const num_members = ::num_members(v.type);
        type_t type;
        rval_t rval;

        if(lval->is_global())
        {
            global_t const& global = lval->global();

            switch(global.gclass())
            {
            case GLOBAL_CONST:
                if(!is_check(D))
                {
                    const_t const& c = global.impl<const_t>();
                    rval = c.rval();
                    type = c.type();
                }
                break;

            case GLOBAL_VAR:
                if(is_compile(D))
                {
                    lval->set_var_i(to_var_i(global.handle<gvar_ht>()));
                    goto have_var_i;
                }
                else if(!is_check(D))
                    compiler_error(v.pstring, "Cannot use global variable in this context.");
                break;

            case GLOBAL_FN:
                if(lval->arg < 0)
                {
                    assert(v.type.name() == TYPE_FN);
                    assert(lval->member == 0);
                    assert(lval->atom < 0);
                    assert(!lval->index);

                    v.val = rval_t{ ssa_value_t(locator_t::fn(global.handle<fn_ht>(), lval->ulabel())) };
                    type = v.type;

                    assert(v.rval().size() == 1);
                }
                else
                    compiler_error(v.pstring, "Cannot use the value of a function parameter or return this way.");
                return v;
            default:
                assert(false);
            }
        }
        else
        {
            if(D == INTERPRET_CE)
                compiler_error(v.pstring, "Expression cannot be evaluated at compile-time.");
        have_var_i:
            rval.resize(num_members);
            type = var_types[lval->var_i()];
        }

        if(is_check(D))
        {
            v.val = std::move(rval);
            return v;
        }

        type = ::member_type(type, lval->member);

        if(is_interpret(D))
        {
            if(lval->is_var())
                for(unsigned i = 0; i < num_members; ++i)
                    rval[i] = interpret_locals[lval->var_i()][lval->member + i];

            if(lval->index)
            {
                assert(is_tea(type.name()));
                assert(lval->index.is_num());

                unsigned const index = lval->index.whole();
                assert(index < type.array_length());
                for(auto& v : rval) // TODO: handle link
                    v = std::get<ct_array_t>(v)[index];

                type = type.elem_type();
            }

            if(lval->atom >= 0)
            {
                assert(rval.size() == 1);

                if(is_tea(type.name()))
                {
                    assert(is_tea(v.type.name()));
                    assert(v.type.elem_type() == TYPE_U);
                    int const shift = lval->atom - frac_bytes(type.elem_type().name());

                    unsigned const tea_length = type.array_length();
                    ct_array_t const& from = std::get<ct_array_t>(rval[0]);
                    ct_array_t to = make_ct_array(tea_length);

                    for(unsigned i = 0; i < tea_length; ++i)
                        to[i] = _interpret_shift_atom(from[i], shift);

                    rval = { std::move(to) };
                }
                else
                {
                    assert(v.type == TYPE_U);
                    int const shift = lval->atom - frac_bytes(type.name());
                    rval = { _interpret_shift_atom(std::get<ssa_value_t>(rval[0]), shift) };
                }
            }
        }
        else if(is_compile(D))
        {
            if(lval->is_var())
                for(unsigned i = 0; i < num_members; ++i)
                    rval[i] = var_lookup(builder.cfg, lval->var_i(), lval->member + i);

            if(lval->index)
            {
                assert(is_tea(type.name()));
                type = type.elem_type();

                for(unsigned i = 0; i < num_members; ++i)
                {
                    // TODO
                    rval[i] = builder.cfg->emplace_ssa(
                        (lval->flags & LVALF_INDEX_16) ? SSA_read_array16 : SSA_read_array8, type, 
                        from_variant<D>(rval[i], type), ssa_value_t(0u, TYPE_U20), lval->index);
                }
            }

            if(lval->atom >= 0)
            {
                assert(rval.size() == 1);
                ssa_ht const h = builder.cfg->emplace_ssa(
                    is_tea(type.name()) ? SSA_array_get_byte : SSA_get_byte, 
                    is_tea(type.name()) ? type_t::tea(TYPE_U, type.size()) : TYPE_U, 
                    std::get<ssa_value_t>(rval[0]), 
                    ssa_value_t(lval->atom, TYPE_U));
                rval = { h };
            }
        }

        v.val = std::move(rval);
    }
    else if(deref_t const* deref = v.is_deref())
    {
        if(!is_compile(D))
            compiler_error(v.pstring, "Can only dereference at compile time.");

        ssa_ht const read = builder.cfg->emplace_ssa(
            SSA_read_ptr, TYPE_U, deref->ptr, ssa_value_t(), deref->bank, deref->index);

        v.val = rval_t{ read };
    }
    else if(strval_t const* strval = v.is_strval())
    {
        passert(v.type.name() == TYPE_TEA, v.type);
        assert(strval->charmap);

        std::string const& str = strval->get_string();
        assert(str.size() == v.type.size());
        ct_array_t ct_array = make_ct_array(str.size());

        for(unsigned i = 0; i < str.size(); ++i)
            ct_array[i] = ssa_value_t(std::uint8_t(str[i]), TYPE_U);

        v.val = rval_t{ std::move(ct_array) };
    }
    else
        std::runtime_error("Cannot convert to rvalue.");
    return v;
}

template<eval_t::do_t D>
expr_value_t eval_t::do_assign(expr_value_t lhs, expr_value_t rhs, token_t const& token)
{
    pstring_t const pstring = concat(lhs.pstring, lhs.pstring);

    if(is_check(D))
        return throwing_cast<D>(std::move(rhs), lhs.type, true);

    if(is_paa(lhs.type.name()))
        compiler_error(pstring, "Cannot assign pointer-addressible arrays.");

    if(lhs.is_lt())
        compiler_error(pstring, "Expression cannot be evaluated at link-time.");

    if(deref_t const* deref = lhs.is_deref())
    {
        if(is_interpret(D))
        {
            //assert(lhs.index);
            assert(false); // TODO: implement
        }
        else if(is_compile(D))
        {
            rhs = throwing_cast<D>(std::move(rhs), TYPE_U, true);

            ssa_ht const write = builder.cfg->emplace_ssa(
                SSA_write_ptr, TYPE_VOID,
                deref->ptr, ssa_value_t(), deref->bank, deref->index,
                std::get<ssa_value_t>(rhs.rval()[0]));
            write->append_daisy();

            return rhs;
        }
    }

    lval_t* const lval = lhs.is_lval();

    if(!lval)
        compiler_error(pstring, "Expecting lvalue on left side of rhs");

    rhs = throwing_cast<D>(std::move(rhs), lhs.type, true);

    if(lval->is_global())
    {
        global_t const& global = lval->global();

        if(global.gclass() == GLOBAL_VAR)
            lval->set_var_i(to_var_i(global.handle<gvar_ht>()));
        else
            compiler_error(pstring, fmt("Unable to modify %", global.name));
    }

    // de-atomize
    if(!is_check(D) && lval->atom >= 0)
    {
        expr_value_t without_atom = lhs;
        without_atom.lval().atom = -1;
        without_atom = to_rval<D>(without_atom);

        type_t new_type = member_type(var_types[lval->var_i()], lval->member);
        if(lval->index)
        {
            assert(is_tea(new_type.name()));
            new_type = new_type.elem_type();
        }
        assert(num_members(new_type) == 1);
        assert(num_members(rhs.type) == 1);

        if(is_interpret(D))
        {
            int const shift = lval->atom - frac_bytes(new_type.name());
            assert(shift >= 0);

            fixed_uint_t mask = numeric_bitmask(rhs.type.name());
            if(shift < 0)
                mask >>= (-shift * 8);
            else
                mask <<= (shift * 8);

            auto const convert = [&](ssa_value_t from, ssa_value_t to) -> ssa_value_t
            {
                fixed_uint_t replace = to.fixed().value;
                assert((replace & mask) == replace);

                if(shift < 0)
                    replace >>= (-shift * 8);
                else
                    replace <<= (shift * 8);

                fixed_uint_t u = from.fixed().value;
                u &= ~mask;
                u |= replace;
                return ssa_value_t(u, new_type.name());
            };

            if(is_tea(new_type.name()))
            {
                unsigned const tea_length = new_type.array_length();
                ct_array_t const& from = std::get<ct_array_t>(without_atom.rval()[0]);
                ct_array_t to = make_ct_array(tea_length);

                for(unsigned i = 0; i < tea_length; ++i)
                    to[i] = convert(from[i], to[i]);

                rhs.rval() = { std::move(to) };
            }
            else
                rhs.rval() = { convert(without_atom.ssa(), rhs.ssa()) };

        }
        else if(is_compile(D))
        {
            ssa_ht const h = builder.cfg->emplace_ssa(
                is_tea(new_type.name()) ? SSA_array_replace_byte : SSA_replace_byte, 
                new_type, 
                without_atom.ssa(), ssa_value_t(lval->atom, TYPE_U), rhs.ssa());
            rhs.rval() = { h };
        }

        rhs.type = new_type;
    }

    if(is_check(D))
        goto finish;

    assert(lval->var_i() < var_types.size());

    // Remap the identifier to point to the new value.
    if(is_interpret(D) && D != INTERPRET_CE)
    {
        assert(lval->var_i() < interpret_locals.size());
        rval_t& local = interpret_locals[lval->var_i()];
        rval_t& rval = rhs.rval();

        if(lval->index)
        {
            type_t const mt = member_type(var_types[lval->var_i()], lval->member);
            assert(mt.name() == TYPE_TEA);

            unsigned const array_size = mt.array_length();
            unsigned const index = lval->index.whole();
            assert(index <= array_size);

            for(unsigned i = 0; i < rval.size(); ++i)
            {
                // TODO: handle linked
                ct_array_t& shared = std::get<ct_array_t>(local[i + lval->member]);

                // If the array has multiple owners, copy it, creating a new one.
                if(shared.use_count() > 1)
                {
                    ct_array_t new_shared = make_ct_array(array_size);
                    std::copy(shared.get(), shared.get() + array_size, new_shared.get());
                    shared = std::move(new_shared);
                }

                // TODO: handle linked
                shared[index] = std::get<ssa_value_t>(rval[i]);
            }
        }
        else
        {
            assert(false);
            /* TODO
            if(lhs.atom)
            {
                assert(local.size() == 1);

                type_t const mt = member_type(var_types[lhs.var_i], lhs.member);
                int const shift = lhs.atom - frac_bytes(mt.name());
                assert(shift >= 0);

                fixed_uint_t mask = numeric_bitmask(rhs.type.name());
                fixed_uint_t replace = rhs.rval[i].u();
                assert((replace & mask) == replace);

                if(shift < 0)
                {
                    mask >>= (-shift * 8);
                    replace >>= (-shift * 8);
                }
                else
                {
                    mask <<= (shift * 8);
                    replace <<= (shift * 8);
                }

                fixed_uint_t u = local[lhs.member].u();
                u &= ~mask;
                u |= replace;
                local[lhs.member] = ssa_value_t(u, mt.name());
            }
            else
            */
            {
                for(unsigned i = 0; i < rval.size(); ++i)
                    local[i + lval->member] = rval[i];
            }
        }
    }
    else if(D == COMPILE)
    {
        ssa_value_array_t& local = builder.cfg.data<block_d>().vars[lval->var_i()];
        rval_t& rval = rhs.rval();

        if(lval->index)
        {
            type_t const mt = member_type(var_types[lval->var_i()], lval->member);
            assert(mt.name() == TYPE_TEA);

            for(unsigned i = 0; i < rval.size(); ++i)
            {
                //ssa_ht read = lhs.ssa(i).handle();
                //assert(read->op() == SSA_read_array);

                passert(rhs.type.name() != TYPE_TEA, rhs.type);
                type_t const type = type_t::tea(rhs.type, mt.size(), rhs.pstring);
                //assert(type.name() == TYPE_TEA);

                ssa_value_t const prev_array = var_lookup(builder.cfg, lval->var_i(), i);

                ssa_ht write = builder.cfg->emplace_ssa(
                    (lval->flags & LVALF_INDEX_16) ? SSA_write_array16 : SSA_write_array8, type,
                    prev_array, ssa_value_t(0u, TYPE_U20), lval->index, std::get<ssa_value_t>(rval[i]));

                local[i + lval->member] = write;
            }
        }
        else
        {
            for(unsigned i = 0; i < rval.size(); ++i)
                local[i + lval->member] = from_variant<D>(rval[i], member_type(rhs.type, i));
        }
    }

finish:
    return rhs;
}

/* TODO remove
template<eval_t::do_t D>
void eval_t::do_assign(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_value_t& assignee = rpn_stack.peek(1);
    rpn_value_t& assignment = rpn_stack.peek(0);

    pstring_t const pstring = concat(assignee.pstring, assignee.pstring);

    if(assignee.category == LVAL_PTR)
    {
        assert(assignee.atom <= 0); // anything else is unimplemented.

        if(D == INTERPRET)
        {
            assert(assignee.index);
            assert(false); // TODO: implement
        }
        else if(D == COMPILE)
        {
            assert(assignee.index);

            throwing_cast<D>(assignment, TYPE_U, true);

            ssa_ht const read = assignee.ssa(0).handle();
            assert(read->op() == SSA_read_ptr);

            // TODO
            //ssa_value_t prev_in_order = {};
            //if(auto ptr_i = ir->gmanager.ptr_i(assignee.derefed_from))
                //prev_in_order = var_lookup(builder.cfg, to_var_i(ptr_i), 0);

            ssa_ht const write = builder.cfg->emplace_ssa(
                SSA_write_ptr, TYPE_VOID,
                read->input(0), read->input(1), 
                read->input(2), read->input(3), 
                std::get<ssa_value_t>(assignment.rval[0]));
            write->append_daisy();

            assignee.rval = std::move(assignment.rval);

            goto finish;
        }
    }

    if(assignee.type.name() == TYPE_PAA)
        compiler_error(pstring, "Cannot assign pointer-addressible arrays.");

    if(assignee.is_lt())
        compiler_error(pstring, "Expression cannot be evaluated at link-time.");

    if(!is_lval(assignee.category))
        compiler_error(pstring, "Expecting lvalue on left side of assignment.");

    throwing_cast<D>(assignment, assignee.type, true);

    if(assignee.atom >= 0)
        deatomize();

    // deatomize
    {
        rpn_value_t& assignee = rpn_stack.peek(1);

        if(!is_link(D))
        {
            type_t const mt = member_type(var_types[assignee.var_i], assignee.member);

            // - get 'i' before the
            
            assignee.
        }

        if()
        {
            make_lt<Policy::D>(rpn_stack, 2, { .type = TOK_deatomize, .pstring = token.pstring });
        }
    }

    if(is_check(D))
        goto finish;

    assert(assignee.rval.size() == assignment.rval.size());
    assert(assignee.var_i < var_types.size());

    // Remap the identifier to point to the new value.
    if(is_interpret(D) && D != INTERPRET_CE)
    {
        assert(assignee.var_i < interpret_locals.size());
        rval_t& local = interpret_locals[assignee.var_i];

        if(assignee.category == LVAL_ARRAY)
        {
            assert(assignee.index);

            type_t const mt = member_type(var_types[assignee.var_i], assignee.member);
            assert(mt.name() == TYPE_TEA);
            assert(mt.elem_type() == assignee.type);

            unsigned const array_size = mt.array_length();
            unsigned const index = assignee.index.whole();
            assert(index <= array_size);

            for(unsigned i = 0; i < assignment.rval.size(); ++i)
            {
                ct_array_t& shared = std::get<ct_array_t>(local[i + assignee.member]);

                // If the array has multiple owners, copy it, creating a new one.
                if(shared.use_count() > 1)
                {
                    ct_array_t new_shared = make_ct_array(array_size);
                    std::copy(shared.get(), shared.get() + array_size, new_shared.get());
                    shared = std::move(new_shared);
                }

                // TODO
                //from_variant(assignment.rval[i], member_type(mt, i));

                shared[index] = std::get<ssa_value_t>(assignment.rval[i]);
                //else if(expr_vec_t const* vec = std::get_if<expr_vec_t>(&v))
                    //return locator_t::lt_expr(alloc_lt_value(type, *vec));
            }
        }
        else
        {
            assert(!assignee.index);
            if(assignee.atom)
            {
                assert(local.size() == 1);

                type_t const mt = member_type(var_types[assignee.var_i], assignee.member);
                int const shift = assignee.atom - frac_bytes(mt.name());
                assert(shift >= 0);

                fixed_uint_t mask = numeric_bitmask(assignment.type.name());
                fixed_uint_t replace = assignment.rval[i].u();
                assert((replace & mask) == replace);

                if(shift < 0)
                {
                    mask >>= (-shift * 8);
                    replace >>= (-shift * 8);
                }
                else
                {
                    mask <<= (shift * 8);
                    replace <<= (shift * 8);
                }

                fixed_uint_t u = local[assignee.member].u();
                u &= ~mask;
                u |= replace;
                local[assignee.member] = ssa_value_t(u, mt.name());
            }
            else
            {
                for(unsigned i = 0; i < assignment.rval.size(); ++i)
                    local[i + assignee.member] = assignment.rval[i];
            }
        }

        assignee.rval = std::move(assignment.rval);
    }
    else if(D == COMPILE)
    {
        ssa_value_array_t& local = builder.cfg.data<block_d>().vars[assignee.var_i];

        if(assignee.category == LVAL_ARRAY)
        {
            assert(assignee.index);

            type_t const mt = member_type(var_types[assignee.var_i], assignee.member);
            assert(mt.name() == TYPE_TEA);
            assert(mt.elem_type() == assignee.type);

            for(unsigned i = 0; i < assignment.rval.size(); ++i)
            {
                ssa_ht read = assignee.ssa(i).handle();
                assert(read->op() == SSA_read_array);

                assert(assignment.type.name() != TYPE_TEA);
                type_t const type = type_t::tea(member_type(assignment.type, i), mt.size(), assignment.pstring);
                assert(type.name() == TYPE_TEA);

                ssa_value_t const prev_array = var_lookup(builder.cfg, assignee.var_i, i);

                ssa_ht write = builder.cfg->emplace_ssa(
                    SSA_write_array, type,
                    prev_array, locator_t(), read->input(2), std::get<ssa_value_t>(assignment.rval[i]));

                local[i + assignee.member] = write;
            }
        }
        else
        {
            assert(!assignee.index);
            for(unsigned i = 0; i < assignment.rval.size(); ++i)
                local[i + assignee.member] = from_variant<D>(assignment.rval[i], member_type(assignment.type, i));
        }

        assignee.rval = std::move(assignment.rval);
    }

    // Leave the assignee on the stack, slightly modified.
finish:
    assignee.category = RVAL;
    rpn_stack.pop();
}
*/

void eval_t::req_quantity(token_t const& token, expr_value_t const& value)
{
    if(!is_quantity(value.type.name()))
    {
        compiler_error(value.pstring, fmt("% expects arithmetic quantity inputs. (Operand is %)", 
                                          token_string(token.type), value.type));
    }
}
    
void eval_t::req_quantity(token_t const& token, expr_value_t const& lhs, expr_value_t const& rhs)
{
    if(!is_quantity(lhs.type.name()) || !is_quantity(rhs.type.name()))
    {
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, fmt("% expects arithmetic quantity inputs. (Operands are % and %)", 
                                    token_string(token.type), lhs.type, rhs.type));
    }
}

expr_value_t eval_t::compile_binary_operator(
    expr_value_t const& lhs, expr_value_t const& rhs, 
    ssa_op_t op, type_t result_type, bool carry)
{
    ssa_value_t result;
    if(carry)
        result = builder.cfg->emplace_ssa(
            op, result_type, lhs.ssa(), rhs.ssa(), ssa_value_t(op == SSA_sub, TYPE_BOOL));
    else
        result = builder.cfg->emplace_ssa(op, result_type, lhs.ssa(), rhs.ssa());

    return
    {
        .val = rval_t{ result },
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };
}

template<typename Policy>
expr_value_t eval_t::do_compare(expr_value_t lhs, expr_value_t rhs, token_t const& token)
{
    req_quantity(token, lhs, rhs);

    expr_value_t result =
    {
        .type = TYPE_BOOL, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

    if(!is_scalar(lhs.type.name()) || !is_scalar(rhs.type.name()))
    {
        compiler_error(
            concat(lhs.pstring, rhs.pstring), 
            fmt("% isn't defined for this type combination. (% and %)",
                token_string(token.type), lhs.type, rhs.type));
    }

    if(lhs.type != rhs.type)
    {
        if(is_ct(lhs.type) && can_cast(lhs.type, rhs.type, true))
            lhs = throwing_cast<Policy::D>(std::move(lhs), rhs.type, true);
        else if(is_ct(rhs.type) && can_cast(rhs.type, lhs.type, true))
            rhs = throwing_cast<Policy::D>(std::move(rhs), lhs.type, true);
    }

    if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
        result.val = rval_t{ ssa_value_t(Policy::interpret(lhs.s(), rhs.s()), TYPE_BOOL) };
    else if(Policy::D == COMPILE)
    {
        // The implementation is kept simpler if both types being compared have the same size.
        if((Policy::op() == SSA_eq || Policy::op() == SSA_not_eq) 
           && !same_scalar_layout(lhs.type.name(), rhs.type.name()))
        {
            unsigned const w = std::max(whole_bytes(lhs.type.name()), whole_bytes(rhs.type.name()));
            unsigned const f = std::max(frac_bytes(lhs.type.name()), frac_bytes(rhs.type.name()));

            lhs = throwing_cast<Policy::D>(std::move(lhs), type_s_or_u(w, f, is_signed(lhs.type.name())), true, lhs.pstring);
            rhs = throwing_cast<Policy::D>(std::move(rhs), type_s_or_u(w, f, is_signed(rhs.type.name())), true, rhs.pstring);

            assert(same_scalar_layout(lhs.type.name(), rhs.type.name()));
        }

        return compile_binary_operator(lhs, rhs, Policy::op(), TYPE_BOOL);
    }

    return result;
}

static locator_t _loc_ptr(rval_t const& rval)
{
    if(rval.size() < 1)
        return {};
    if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&rval[0]))
        if(ssa->is_locator() && is_ptr(ssa->locator().type().name()))
            return ssa->locator();
    return {};
}

template<typename Policy>
expr_value_t eval_t::do_arith(expr_value_t lhs, expr_value_t rhs, token_t const& token)
{
    assert(lhs.is_rval() && rhs.is_rval());

    ssa_op_t const op = Policy::op();
    bool const summable = op == SSA_add || op == SSA_sub;

    if(summable)
    {
        if(!is_summable(lhs.type.name()) || !is_summable(rhs.type.name()))
        {
        invalid_input:
            pstring_t pstring = concat(lhs.pstring, rhs.pstring);
            compiler_error(pstring, fmt("Operator % is not defined for these types. (Operands are % and %)", 
                                        token_string(token.type), lhs.type, rhs.type));
        }
    }
    else
        req_quantity(token, lhs, rhs);

    expr_value_t result = { .pstring = concat(lhs.pstring, rhs.pstring) };

    if(summable)
    {
        bool const lptr = is_ptr(lhs.type.name());
        bool const rptr = is_ptr(rhs.type.name());
        
        if(lptr)
        {
            if(rptr)
            {
                if(op == SSA_sub)
                {
                    result.type = TYPE_U20;

                    if(!is_check(Policy::D))
                    {
                        // If both are locators with the same handle,
                        // we can calculate this at CT
                        locator_t const l = _loc_ptr(lhs.rval());
                        locator_t const r = _loc_ptr(rhs.rval());
                        // TODO: check for interpret
                        if(l && r && l.with_offset(0) == r.with_offset(0))
                        {
                            std::uint16_t const diff = l.offset() - r.offset();
                            result.val = rval_t{ ssa_value_t(diff, TYPE_U20) };
                        }
                        else
                        {
                            if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
                            {
                                // Create a link-time expression.
                                // TODO
                                assert(false);
                                //make_lt(rpn_stack, 2, { .type = TOK_minus, .pstring = token.pstring });
                                //return;
                            }
                            else if(Policy::D == COMPILE)
                            {
                                return compile_binary_operator(
                                    throwing_cast<Policy::D>(std::move(lhs), result.type, false),
                                    throwing_cast<Policy::D>(std::move(rhs), result.type, false),
                                    Policy::op(), result.type, ssa_argn(Policy::op()) > 2);
                            }
                        }
                    }
                }
                else
                    goto invalid_input;
            }
            else
            {
            ptr_int:
                result.type = lhs.type;
                assert(is_ptr(result.type.name()));
                assert(!is_ptr(rhs.type.name()));

                bool const banked = is_banked_ptr(result.type.name());

                rhs = throwing_cast<Policy::D>(std::move(rhs), TYPE_U20, true);

                if(!is_check(Policy::D))
                {
                    locator_t const l = _loc_ptr(lhs.rval());

                    if(l && rhs.is_ct())
                    {
                        locator_t const new_l = l.with_advance_offset(rhs.whole());
                        result.val = rval_t{ new_l };
                        if(banked)
                            result.rval().push_back(new_l.with_is(IS_BANK));
                    }
                    else
                    {
                        if(is_interpret(Policy::D))
                        {
                            // Create a link-time expression.
                            // TODO
                            assert(false);
                            //make_lt(rpn_stack, 2, { .type = op == SSA_add ? TOK_plus : TOK_minus, .pstring = token.pstring });
                            //return;
                        }
                        else if(Policy::D == COMPILE)
                        {
                            ct_variant_t bank;
                            if(banked)
                            {
                                assert(lhs.rval().size() == 2);
                                bank = lhs.rval()[1];
                            }

                            lhs = throwing_cast<Policy::D>(std::move(lhs), TYPE_U20, false);
                            rhs = throwing_cast<Policy::D>(std::move(rhs), TYPE_U20, false);

                            ssa_ht const sum = builder.cfg->emplace_ssa(
                                Policy::op(), TYPE_U20, 
                                lhs.ssa(0), rhs.ssa(0), 
                                ssa_value_t(Policy::op() == SSA_sub, TYPE_BOOL));

                            ssa_ht const cast = builder.cfg->emplace_ssa(
                                SSA_cast, result.type.with_banked(false), sum);

                            if(banked)
                                result.val = rval_t{ cast, bank };
                            else
                                result.val = rval_t{ cast };
                        }
                    }
                }
            }
        }
        else if(rptr)
        {
            if(op != SSA_add)
                goto invalid_input;

            std::swap(lhs, rhs);
            goto ptr_int;
        }
        else
            goto no_ptrs;
    }
    else
    {
    no_ptrs:
        if(lhs.type != rhs.type)
        {
            if(is_ct(lhs.type) && can_cast(lhs.type, rhs.type, true))
            {
                result.type = rhs.type;
                lhs = throwing_cast<Policy::D>(std::move(lhs), result.type, true);
            }
            else if(is_ct(rhs.type) && can_cast(rhs.type, lhs.type, true))
            {
                result.type = lhs.type;
                rhs = throwing_cast<Policy::D>(std::move(rhs), result.type, true);
            }
            else
            {
                compiler_error(result.pstring, 
                    fmt("% isn't defined for this type combination. (% and %)",
                        token_string(token.type), lhs.type, rhs.type));
            }
        }
        else
            result.type = lhs.type;

        assert(is_arithmetic(result.type.name()));
        assert(lhs.type == result.type);
        assert(rhs.type == result.type);

        if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
        {
            assert(is_masked(lhs.fixed(), lhs.type.name()));
            assert(is_masked(rhs.fixed(), rhs.type.name()));

            fixed_t f = { Policy::interpret(lhs.s(), rhs.s()) };
            f.value &= numeric_bitmask(result.type.name());
            result.val = rval_t{ ssa_value_t(f, result.type.name()) };
        }
        else if(Policy::D == COMPILE)
            return compile_binary_operator(lhs, rhs, Policy::op(), result.type, ssa_argn(Policy::op()) > 2);
    }

    return result;
}

template<typename Policy>
expr_value_t eval_t::do_shift(expr_value_t lhs, expr_value_t rhs, token_t const& token)
{
    req_quantity(token, lhs, rhs);

    if(rhs.type.name() == TYPE_INT)
        rhs = throwing_cast<Policy::D>(std::move(rhs), { TYPE_U }, true);
    else if(rhs.type.name() != TYPE_U)
        compiler_error(rhs.pstring, fmt("Ride-hand side of operator % must be type U or Int.", 
                                        token_string(token.type)));

    type_t const result_type = lhs.type;
    assert(is_arithmetic(result_type.name()));

    expr_value_t result =
    {
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

    if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
    {
        assert(is_masked(lhs.fixed(), lhs.type.name()));
        assert(is_masked(rhs.fixed(), rhs.type.name()));

        fixed_t f = { Policy::interpret(lhs.s(), rhs.whole()) };
        f.value &= numeric_bitmask(result_type.name());
        result.val = rval_t{ ssa_value_t(f, result_type.name()) };
    }
    else if(Policy::D == COMPILE)
        return compile_binary_operator(std::move(lhs), std::move(rhs), Policy::op(), result_type);

    return result;
}

template<typename Policy>
expr_value_t eval_t::do_assign_arith(expr_value_t lhs, expr_value_t rhs, token_t const& token)
{
    ssa_op_t const op = Policy::op();

    if((op == SSA_add || op == SSA_sub) && is_ptr(lhs.type.name()))
        rhs = throwing_cast<Policy::D>(std::move(rhs), TYPE_U20, true);
    else
        rhs = throwing_cast<Policy::D>(std::move(rhs), lhs.type, true);
    expr_value_t lhs_copy = to_rval<Policy::D>(lhs);
    return do_assign<Policy::D>(std::move(lhs), do_arith<Policy>(std::move(lhs_copy), rhs, token), token);
}

template<typename Policy>
expr_value_t eval_t::do_assign_shift(expr_value_t lhs, expr_value_t rhs, token_t const& token)
{
    return do_assign<Policy::D>(std::move(lhs), do_shift<Policy>(to_rval<Policy::D>(lhs), rhs, token), token);
}

template<eval_t::do_t D>
expr_value_t eval_t::do_logical(ast_node_t const& ast)
{
    assert(ast.token.type == TOK_logical_or || ast.token.type == TOK_logical_and);

    expr_value_t const lhs = throwing_cast<D>(do_expr<D>(ast.children[0]), TYPE_BOOL, true);
    bool const is_or = ast.token.type == TOK_logical_or;

    if(is_interpret(D))
    {
        if(bool(lhs.fixed()) == is_or)
            return lhs;
        return throwing_cast<D>(do_expr<D>(ast.children[1]), TYPE_BOOL, true);
    }
    else if(is_compile(D))
    {
        cfg_ht const branch_node = builder.cfg;
        cfg_exits_with_branch(lhs.ssa());

        cfg_ht const long_cut = builder.cfg = insert_cfg(true);
        branch_node->build_set_output(!is_or, long_cut);

        expr_value_t const rhs = throwing_cast<D>(do_expr<D>(ast.children[1]), TYPE_BOOL, true);

        cfg_ht const merge_node = insert_cfg(true);
        cfg_exits_with_jump();
        builder.cfg->build_set_output(0, merge_node);
        branch_node->build_set_output(is_or, merge_node);
        builder.cfg = merge_node;

        expr_value_t result =
        {
            .val = rval_t{ merge_node->emplace_ssa(
                SSA_phi, TYPE_BOOL, rhs.ssa(), ssa_value_t(is_or, TYPE_BOOL)) },
            .type = TYPE_BOOL,
            .pstring = concat(lhs.pstring, rhs.pstring),
        };

        return result;
    }

    expr_value_t const rhs = throwing_cast<D>(do_expr<D>(ast.children[1]), TYPE_BOOL, true);
    return { .type = TYPE_BOOL, .pstring = concat(lhs.pstring, rhs.pstring) };
}

template<eval_t::do_t D>
expr_value_t eval_t::force_truncate(expr_value_t value, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(value.type));
    assert(!is_ct(to_type));
    assert(is_arithmetic(to_type.name()) && is_arithmetic(value.type.name()));

    value = to_rval<D>(std::move(value));

    expr_value_t result =
    {
        .type = to_type, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
    };

    if(is_interpret(D) || (D == COMPILE && value.is_ct()))
        result.val = rval_t{ ssa_value_t(mask_numeric(value.fixed(), to_type.name()), to_type.name()) };
    else if(D == COMPILE)
        result.val = rval_t{ builder.cfg->emplace_ssa(SSA_cast, to_type, value.ssa()) };

    return result;
}

template<eval_t::do_t D>
expr_value_t eval_t::force_promote(expr_value_t value, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(value.type));
    assert(is_arithmetic(to_type.name()) && is_arithmetic(value.type.name()));

    value = to_rval<D>(std::move(value));

    expr_value_t result =
    {
        .type = to_type, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
    };

    if(is_interpret(D) || (D == COMPILE && value.is_ct()))
        result.val = rval_t{ ssa_value_t(mask_numeric({ value.s() }, to_type.name()), to_type.name()) };
    else if(D == COMPILE)
    {
        if(is_ct(to_type))
            compiler_error(value.pstring, fmt("Cannot promote type % to type % at runtime.", value.type, to_type));
        result.val = rval_t{ builder.cfg->emplace_ssa(SSA_cast, to_type, value.ssa()) };
    }

    return result;
}

template<eval_t::do_t D>
expr_value_t eval_t::force_intify_ptr(expr_value_t value, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(value.type));
    assert(!is_ct(to_type));
    assert(is_arithmetic(to_type.name()) && is_ptr(value.type.name()));

    value = to_rval<D>(std::move(value));

    expr_value_t result =
    {
        .type = to_type, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
    };

    if(is_interpret(D))
    {
        // TODO
        if(value.ssa().is_locator() && (to_type.name() == TYPE_U || to_type.name() == TYPE_S))
        {
            result.val = rval_t{ value.ssa().locator().with_is(IS_PTR) };
        }
        else
            assert(false);
        //passert(false, value.ssa());
        /* TODO LT
        std::array<token_t, 2> const tokens = 
        {{
             { .type = TOK_cast, .pstring = cast_pstring, .value = 1 },
             token_t::make_ptr(TOK_cast_type, cast_pstring, type_t::new_type(to_type))
        }};

        result.val = rval_t{ prep_lt<D>(rpn_value, &*tokens.begin(), &*tokens.end()) };
        */
    }
    else if(D == COMPILE)
    {
        ssa_value_t first_cast = value.ssa(0);

        if(to_type.name() != TYPE_U20)
            first_cast = builder.cfg->emplace_ssa(SSA_cast, TYPE_U20, first_cast);

        result.val = rval_t{ builder.cfg->emplace_ssa(SSA_cast, to_type, first_cast) };
    }

    return result;
}

template<eval_t::do_t D>
expr_value_t eval_t::force_ptrify_int(expr_value_t value, expr_value_t* bank, type_t to_type, pstring_t cast_pstring)
{
    //passert(!is_ct(value.type), value.type);
    passert(!is_ct(to_type), to_type);
    assert(is_ptr(to_type.name()) && is_arithmetic(value.type.name()));

    assert(is_banked_ptr(to_type.name()) == !!bank);

    value = throwing_cast<D>(std::move(value), TYPE_U20, true, pstring);
    if(bank)
        *bank = throwing_cast<D>(std::move(*bank), TYPE_U, true, pstring);

    expr_value_t result =
    {
        .type = to_type, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
    };

    if(is_interpret(D))
    {
        assert(value.rval().size() == 1);
        result.val = value.val;
    add_bank:
        if(bank)
        {
            assert(bank->rval().size() == 1);
            result.rval().push_back(bank->rval()[0]);
        }
    }
    else if(D == COMPILE)
    {
        ssa_value_t first_cast = value.ssa();

        type_t unbanked_type = to_type.with_banked(false);
        result.val = rval_t{ builder.cfg->emplace_ssa(SSA_cast, unbanked_type, first_cast) };
        goto add_bank;
    }

    return result;
}

template<eval_t::do_t D>
expr_value_t eval_t::force_convert_int(expr_value_t value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    assert(value.type.name() == TYPE_INT);
    assert(is_arithmetic(to_type.name()) && is_arithmetic(value.type.name()));

    value = to_rval<D>(std::move(value));

    expr_value_t result =
    {
        .type = to_type, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
    };

    if(!is_check(D))
    {
        fixed_t const masked = mask_numeric(value.fixed(), to_type.name());

        if(implicit && to_signed(masked.value, to_type.name()) != value.s())
        {
            throw compiler_error_t(
                fmt_error(result.pstring, fmt(
                    "Int value of % cannot be represented in type %. (Implicit type conversion)", 
                    to_double(fixed_t{ value.s() }), to_type))
                + fmt_note("Add an explicit cast operator to override.")
                );
        }

        result.val = rval_t{ ssa_value_t(masked, to_type.name()) };
    }

    return result;
}

template<eval_t::do_t D>
expr_value_t eval_t::force_round_real(expr_value_t value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    assert(value.type.name() == TYPE_REAL);
    assert(is_arithmetic(to_type.name()));

    value = to_rval<D>(std::move(value));

    expr_value_t result =
    {
        .type = to_type, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
    };

    if(!is_check(D))
    {
        fixed_sint_t const original = to_signed(value.u(), TYPE_REAL);
        fixed_uint_t u = value.u();
        fixed_uint_t const mask = numeric_bitmask(to_type.name());
        if(fixed_uint_t z = builtin::ctz(mask))
            u += (1ull << (z - 1)) & u;
        u &= mask;

        if(implicit)
        {
            fixed_uint_t const supermask = ::supermask(numeric_bitmask(to_type.name()));
            if(static_cast<fixed_sint_t>(original & supermask) != to_signed(original & mask, to_type.name()))
            {
                throw compiler_error_t(
                    fmt_error(value.pstring, fmt(
                        "Num value of % doesn't fit in type %. (Implicit type conversion)", 
                        to_double(fixed_t{original}), to_type))
                    + fmt_note("Add an explicit cast operator to override.")
                    );
            }
        }

        assert(is_masked({u}, to_type.name()));

        result.val = rval_t{ ssa_value_t(fixed_t{ u }, to_type.name()) };
        assert(result.u() == u);
    }

    return result;
}


// This is used to implement the other cast functions.
template<eval_t::do_t D>
expr_value_t eval_t::force_boolify(expr_value_t value, pstring_t cast_pstring)
{
    value = to_rval<D>(std::move(value));

    expr_value_t result =
    {
        .type = TYPE_BOOL, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
        .time = is_compile(D) ? RT : CT, // TODO: what is this for?
    };

    if(is_interpret(D) || (is_compile(D) && value.is_ct()))
    {
        if(is_arithmetic(value.type.name()))
            result.val = rval_t{ ssa_value_t(boolify(value.fixed()), TYPE_BOOL) };
    }
    else if(D == COMPILE)
    {
        result.val = rval_t{ builder.cfg->emplace_ssa(
            SSA_not_eq, TYPE_BOOL, value.ssa(), ssa_value_t(0u, value.type.name())) };
    }

    return result;
}

template<eval_t::do_t D>
expr_value_t eval_t::force_resize_tea(expr_value_t value, type_t to_type, pstring_t cast_pstring)
{
    assert(is_tea(value.type.name()));
    assert(is_tea(to_type.name()));
    assert(value.type.elem_type() == to_type.elem_type());

    value = to_rval<D>(std::move(value));
    rval_t& rval = value.rval();

    type_t const elem_type = to_type.elem_type();
    unsigned const from_size = value.type.size();
    unsigned const to_size = to_type.size();

    expr_value_t result =
    {
        .type = to_type, 
        .pstring = cast_pstring ? concat(value.pstring, cast_pstring) : value.pstring,
    };

    if(from_size == to_size)
        result.val = std::move(value.val);
    else if(is_interpret(D) || (is_compile(D) && value.is_ct()))
    {
        if(to_size > from_size)
        {
            for(unsigned m = 0; m < rval.size(); ++m)
            {
                ct_array_t const& from = std::get<ct_array_t>(rval[m]);
                ct_array_t to = make_ct_array(to_type.size());
                std::copy(from.get(), from.get() + from_size, to.get());

                // Zero-init the rest:
                ssa_value_t fill(0u, ::member_type(elem_type, m).name());
                for(unsigned i = from_size; i < to_size; ++i)
                    to[i] = fill;

                rval[m] = std::move(to);
            }
        }

        result.val = std::move(rval);
    }
    else if(is_compile(D))
    {
        for(unsigned m = 0; m < rval.size(); ++m)
        {
            type_t const mt = ::member_type(value.type, m);
            assert(is_tea(mt.name()));

            rval[m] = builder.cfg->emplace_ssa(
                SSA_resize_array, type_t::tea(mt.elem_type(), to_size),
                from_variant<D>(rval[m], mt));
        }

        result.val = std::move(rval);
    }

    return result;
}

template<eval_t::do_t D>
bool eval_t::cast(expr_value_t& value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    switch(can_cast(value.type, to_type, implicit))
    {
    default: assert(false);
    case CAST_FAIL: 
        return false;
    case CAST_NOP:
        value.type = to_type;
        value = to_rval<D>(std::move(value));
        return true;
    case CAST_PROMOTE:
        value = force_promote<D>(std::move(value), to_type, cast_pstring);
        return true;
    case CAST_TRUNCATE:
        value = force_truncate<D>(std::move(value), to_type, cast_pstring);
        return true;
    case CAST_BOOLIFY:
        value = force_boolify<D>(std::move(value), cast_pstring);
        return true;
    case CAST_CONVERT_INT:
        value = force_convert_int<D>(std::move(value), to_type, implicit, cast_pstring);
        return true;
    case CAST_ROUND_REAL:
        value = force_round_real<D>(std::move(value), to_type, implicit, cast_pstring);
        return true;
    case CAST_INTIFY_PTR:
        value = force_intify_ptr<D>(std::move(value), to_type, cast_pstring);
        return true;
    case CAST_PTRIFY_INT:
        assert(!is_banked_ptr(to_type.name()));
        value = force_ptrify_int<D>(std::move(value), nullptr, to_type, cast_pstring);
        return true;
    case CAST_RESIZE_TEA:
        value = force_resize_tea<D>(std::move(value), to_type, cast_pstring);
        return true;
    }
}

template<eval_t::do_t D>
expr_value_t eval_t::throwing_cast(expr_value_t value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    if(!cast<D>(value, to_type, implicit, cast_pstring))
    {
        compiler_error(value.pstring, fmt(
            "Unable to perform % type cast from % to %.", 
            (implicit ? "implicit": "explicit"), value.type, to_type));
    }

    return value;
}

// Converts multiple values at once, but only if all casts are valid.
// On success, -1 is returned and 'val_begin' to 'val_end' may be modified to their casted type.
// On failure, an andex into 'begin' is return, with the failed cast.
template<eval_t::do_t D>
int eval_t::cast_args(
    pstring_t pstring, expr_value_t* begin, expr_value_t* end, 
    type_t const* type_begin, bool implicit)
{
    assert(begin <= end);
    std::size_t const size = end - begin;

    cast_result_t* results = ALLOCA_T(cast_result_t, size);
    for(std::size_t i = 0; i != size; ++i)
        if(!(results[i] = can_cast(begin[i].type, type_begin[i], true)))
            return i;

    for(std::size_t i = 0; i != size; ++i)
        cast<D>(begin[i], type_begin[i], implicit, pstring);

    return -1; // means no errors!
}

cfg_ht eval_t::insert_cfg(bool seal, pstring_t label_name)
{
    cfg_ht const new_node = ir->emplace_cfg();
    cfg_data_pool::resize<block_d>(cfg_pool::array_size());
    block_d& block_data = new_node.data<block_d>();
    block_data.label_name = label_name;
    block_data.sealed = seal;

    auto const init_vector = [this](block_d::vector_t& vec)
    {
        assert(num_vars() == var_types.size());
        vec.resize(var_types.size());
        for(unsigned i = 0; i < var_types.size(); ++i)
            vec[i].resize(std::max<unsigned>(1, ::num_members(var_types[i])));
    };

    init_vector(block_data.vars);
    if(!seal)
    {
        init_vector(block_data.unsealed_phis);
        assert(block_data.unsealed_phis.size() == block_data.vars.size());
    }

    return new_node;
}

void eval_t::seal_block(block_d& block_data)
{
    assert(block_data.sealed == false);
    ssa_value_t v;
    for(unsigned i = 0; i < num_vars(); ++i)
        for(unsigned member = 0; member < block_data.unsealed_phis[i].size(); ++member)
            if((v = block_data.unsealed_phis[i][member]) && v.holds_ref())
                fill_phi_args(v->handle(), i, member);
    block_data.unsealed_phis.clear();
    block_data.sealed = true;
}

// Relevant paper:
//   Simple and Efficient Construction of Static Single Assignment Form
ssa_value_t eval_t::var_lookup(cfg_ht cfg_node, unsigned var_i, unsigned member)
{
    block_d& block_data = cfg_node.data<block_d>();

    assert(var_i < block_data.vars.size());
    assert(block_data.vars.size() == var_types.size());
    assert(member < block_data.vars[var_i].size());

    if(ssa_value_t lookup = from_variant<COMPILE>(block_data.vars[var_i][member], member_type(var_types[var_i], member)))
        return lookup;
    else if(block_data.sealed)
    {
        // If the block doesn't contain a definition for 'var_i',
        // recursively look up its definition in predecessor nodes.
        // If there are multiple predecessors, a phi node will be created.
        try
        {
            switch(cfg_node->input_size())
            {
            case 0:
                passert(false, to_string(stmt->name));
                throw var_lookup_error_t();
            case 1:
                return var_lookup(cfg_node->input(0), var_i, member);
            default:
                ssa_ht const phi = cfg_node->emplace_ssa(SSA_phi, ::member_type(var_types[var_i], member));
                block_data.vars[var_i][member] = phi;
                fill_phi_args(phi, var_i, member);
            #ifndef NDEBUG
                for(unsigned i = 0; i < phi->input_size(); ++i)
                    assert(phi->input(i));
                assert(phi);
            #endif
                return phi;
            }
        }
        catch(var_lookup_error_t&)
        {
            if(block_data.label_name.size && var_i < num_local_vars())
            {
                pstring_t var_name = fn->def().local_vars[var_i].decl.name;
                file_contents_t file(var_name.file_i);
                throw compiler_error_t(
                    fmt_error(block_data.label_name, fmt(
                        "Jump to label crosses initialization "
                        "of variable %.", var_name.view(file.source())), &file)
                    + fmt_note(var_name, "Variable is defined here:", &file));
            }
            throw;
        }
    }
    else 
    {
        // If the node is unsealed, the predecessors are not fully known,
        // and thus it's impossible to determine the var's definition.
        // To work around this, an incomplete phi node can be created, which
        // will then be filled when the node is sealed.
        assert(block_data.unsealed_phis.size() == block_data.vars.size());
        ssa_ht const phi = cfg_node->emplace_ssa(SSA_phi, ::member_type(var_types[var_i], member));
        block_data.vars[var_i][member] = phi;
        block_data.unsealed_phis[var_i][member] = phi;
        assert(phi);
        return phi;
    }
}

rval_t eval_t::var_lookup(cfg_ht cfg_node, unsigned var_i)
{
    block_d& block_data = cfg_node.data<block_d>();

    assert(var_i < block_data.vars.size());

    rval_t rval(block_data.vars[var_i].size());

    assert(rval.size() == num_members(var_types[var_i]));

    for(unsigned member = 0; member < rval.size(); ++member)
    {
        assert(member < block_data.vars[var_i].size());
        rval[member] = var_lookup(cfg_node, var_i, member);
    }

    return rval;
}

void eval_t::fill_phi_args(ssa_ht phi, unsigned var_i, unsigned member)
{
    // Input must be an empty phi node.
    assert(phi->op() == SSA_phi);
    assert(phi->input_size() == 0);

    // Fill the input array using local lookups.
    cfg_ht const cfg_node = phi->cfg_node();
    unsigned const input_size = cfg_node->input_size();
    phi->alloc_input(input_size);
    for(unsigned i = 0; i < input_size; ++i)
    {
        // This has to be on two lines, otherwise reference invalidation lurks.
        ssa_value_t v = var_lookup(cfg_node->input(i), var_i, member);
        phi->build_set_input(i, v);
    }
}


template<eval_t::do_t D>
ssa_value_t eval_t::from_variant(ct_variant_t const& v, type_t type)
{
    if(ssa_value_t const* value = std::get_if<ssa_value_t>(&v))
       return *value;
    else if(ct_array_t const* array = std::get_if<ct_array_t>(&v))
    {
        if(!is_compile(D))
            throw std::runtime_error("Cannot convert ct_array_t to ssa_value_t.");
        assert(num_members(type) == 1);

        ssa_ht h = builder.cfg->emplace_ssa(SSA_init_array, type);
        unsigned const length = type.array_length();
        h->alloc_input(length);
        for(unsigned i = 0; i < length; ++i)
        {
            ssa_value_t v = (*array)[i];
            if(!v)
                v = builder.cfg->emplace_ssa(SSA_uninitialized, type.elem_type());
            h->build_set_input(i, v);
        }

        return h;
    }
    else if(ast_node_t const* const* ast = std::get_if<ast_node_t const*>(&v))
    {
        // TODO
        assert(false);
        //return locator_t::lt_expr(alloc_lt_value(type, *vec));
    }
    return {};
}

ssa_value_array_t eval_t::from_rval(rval_t const& rval, type_t type)
{
    ssa_value_array_t array;
    array.reserve(rval.size());

    for(unsigned i = 0; i < rval.size(); ++i)
        array.push_back(from_variant<COMPILE>(rval[i], member_type(type, i)));

    return array;
}

ssa_value_array_t eval_t::from_rval(expr_value_t const& value)
{
    return from_rval(value.rval(), value.type);
}

void eval_t::cfg_exits_with_jump()
{
    builder.cfg->alloc_output(1);
}

void eval_t::cfg_exits_with_branch(ssa_value_t condition)
{
    ssa_ht const if_h = builder.cfg->emplace_ssa(SSA_if, TYPE_VOID, condition);
    if_h->append_daisy();
    builder.cfg->alloc_output(2);
    assert(builder.cfg->output_size() == 2);
    assert(builder.cfg->last_daisy() == if_h);
}

// Jumps are like 'break', 'continue', 'goto', etc.
cfg_ht eval_t::compile_goto()
{
    // The syntax allows code to exist following a jump statement.
    // Said code is unreachable, but gets compiled anyway.
    // Implement using a conditional that always takes the false branch.
    // (This will be optimized out later)

    cfg_exits_with_branch(ssa_value_t(0u, TYPE_BOOL));
    cfg_ht dead_branch = insert_cfg(true);
    builder.cfg->build_set_output(1, dead_branch);
    return dead_branch;
}

#if 0

static token_t _make_token(rpn_value_t const& rpn)
{
    if(rpn.is_ct())
    {
        if(rpn.type == TYPE_INT)
            return { .type = TOK_Int, .pstring = rpn.pstring, .value = rpn.s() };
        if(rpn.type == TYPE_REAL)
            return { .type = TOK_Real, .pstring = rpn.pstring, .value = rpn.s() };
    }

    token_t tok = { .type = TOK_rpair, .pstring = rpn.pstring };
    tok.set_ptr(eternal_emplace<rpair_t>(rpn.rval, rpn.type));
    return tok;
}

static void _expr_vec_append(expr_vec_t& vec, rpn_value_t const& rpn)
{
    expr_vec_t const* sub;
    if(rpn.rval.size() == 1 && (sub = std::get_if<expr_vec_t>(&rpn.rval[0])))
        vec.insert(vec.end(), sub->begin(), sub->end());
    else
        vec.push_back(_make_token(rpn));
}

static expr_vec_t _make_expr_vec(rpn_stack_t& rpn_stack, unsigned argn)
{
    expr_vec_t ret;
    ret.reserve(argn * 2 + 2);
    
    for(unsigned i = 0; i < argn; ++i)
        _expr_vec_append(ret, rpn_stack.peek(argn - i - 1));

    return ret;
}

void eval_t::make_lt(rpn_stack_t& rpn_stack, unsigned argn,
                     token_t const* op_begin, token_t const* op_end)
{
    expr_vec_t vec = _make_expr_vec(rpn_stack, argn);
    vec.insert(vec.end(), op_begin, op_end);

    // Use CHECK to update the stack:
    token_t const* token = do_token<CHECK>(rpn_stack, op_begin);
    assert(token == op_end);

    // Now add our expr_vec:
    rpn_stack.peek(0).rval = { vec };
}

void eval_t::make_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op)
{
    make_lt(rpn_stack, argn, &op, &op + 1);
}

template<eval_t::do_t D>
rval_t eval_t::prep_lt(rpn_value_t const& rpn_value, token_t const* op_begin, token_t const* op_end)
{
    expr_vec_t vec;
    _expr_vec_append(vec, rpn_value);
    vec.insert(vec.end(), op_begin, op_end);

    return { vec };
}

template<eval_t::do_t D>
rval_t eval_t::prep_lt(rpn_value_t const& rpn_value, token_t const& token)
{
    return prep_lt<D>(rpn_value, &token, &token + 1);
}

template<eval_t::do_t D>
bool eval_t::handle_lt(rpn_stack_t& rpn_stack, unsigned argn,
                       token_t const* op_begin, token_t const* op_end)
{
    if(is_check(D) || D == LINK)
        return false;

    bool has_lt_arg = false;

    // Every arg must either be a LT value, or a CT value,
    // and we need to find at least one LT value.
    assert(rpn_stack.size() >= argn);
    for(unsigned i = 0; i < argn; ++i)
    {
        if(rpn_stack.peek(i).is_lt())
            has_lt_arg = true;
        else if(!rpn_stack.peek(i).is_ct())
        {
            if(D == COMPILE)
            {
                // Convert the rval to SSA values:
                for(unsigned j = 0; j < argn; ++j)
                {
                    rpn_value_t& rpn = rpn_stack.peek(j);
                    for(unsigned k = 0; k < rpn.rval.size(); ++k)
                        rpn.rval[k] = from_variant<D>(rpn.rval[k], member_type(rpn.type, k));
                }
            }

            return false;
        }
    }

    if(has_lt_arg)
        make_lt(rpn_stack, argn, op_begin, op_end);

    return has_lt_arg;
}

template<eval_t::do_t D>
bool eval_t::handle_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op)
{
    return handle_lt<D>(rpn_stack, argn, &op, &op + 1);
}
#endif
