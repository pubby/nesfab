#include "eval.hpp"

#include <cassert>
#include <chrono>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "alloca.hpp"
#include "sval.hpp"
#include "decl.hpp"
#include "globals.hpp"
#include "file.hpp"
#include "options.hpp"
#include "ir.hpp"
#include "rpn.hpp"
#include "stmt.hpp"
#include "eternal_new.hpp"
#include "lt.hpp"
#include "group.hpp"

namespace sc = std::chrono;
namespace bc = boost::container;

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
    bc::small_vector<sval_t, 8> interpret_locals;
    bc::small_vector<sval_t, 8> interpret_local_defaults;
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
    spair_t final_result;
    std::vector<locator_t> paa; // Only used when defining PAA inits
    precheck_tracked_t* precheck_tracked = nullptr; // Various things callers of 'eval_t' may want.

    enum do_t
    {
        CHECK,        // Resolves types and syntax, but not values.
        INTERPRET_CE, // Like INTERPRET, but can't read/write locals.
        INTERPRET,    // Calculates values at compile-time.
        COMPILE,      // Generates the SSA IR.
        LINK
    };

    static constexpr bool is_check(do_t d) { return d == CHECK_SYNTAX || d == CHECK_TYPES_SYNTAX; }
    static constexpr bool is_interpret(do_t d) { return d == INTERPRET_CE || d == INTERPRET; }

    template<do_t Do>
    struct do_wrapper_t { static constexpr auto D = Do; };

    template<do_t D>
    eval_t(do_wrapper_t<D>, pstring_t pstring, token_t const* expr, type_t expected_type = TYPE_VOID);

    template<do_t D>
    eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const& fn_ref, 
           precheck_tracked_t* tracked, sval_t const* args, unsigned num_args);

    eval_t(ir_t& ir, fn_t const& fn);

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
    token_t const* do_token(rpn_stack_t& rpn_stack, token_t const* token);

    template<do_t D>
    void do_expr(rpn_stack_t& rpn_stack, token_t const* expr);

    template<do_t D>
    void do_expr_result(token_t const* expr, type_t expected_result);

    template<do_t D>
    void do_assign(rpn_stack_t& rpn_stack, token_t const& token);

    template<typename Policy>
    void do_compare(rpn_stack_t& rpn_stack, token_t const& token);

    template<typename Policy>
    void do_arith(rpn_stack_t& rpn_stack, token_t const& token);

    template<typename Policy>
    void do_shift(rpn_stack_t& rpn_stack, token_t const& token);

    template<typename Policy>
    void interpret_shift(rpn_stack_t& rpn_stack, token_t const& token);

    template<typename Policy>
    void do_assign_arith(rpn_stack_t& rpn_stack, token_t const& token);

    template<typename Policy>
    void do_assign_shift(rpn_stack_t& rpn_stack, token_t const& token);

    template<typename Policy>
    void do_logical_begin(rpn_stack_t& rpn_stack, token_t const*& token);

    template<typename Policy>
    void do_logical_end(rpn_stack_t& rpn_stack);

    void req_quantity(token_t const& token, rpn_value_t const& value);
    void req_quantity(token_t const& token, rpn_value_t const& lhs, rpn_value_t const& rhs);

    // Cast-related
    template<do_t D>
    void force_truncate(rpn_value_t& rpn_value, type_t to_type, pstring_t pstring = {});

    template<do_t D>
    void force_promote(rpn_value_t& rpn_value, type_t to_type, pstring_t pstring = {});

    template<do_t D>
    void force_intify_ptr(rpn_value_t& rpn_value, type_t to_type, pstring_t pstring = {});

    template<do_t D>
    void force_ptrify_int(rpn_value_t& rpn_value, rpn_value_t const* bank, type_t to_type, pstring_t pstring = {});

    template<do_t D>
    void force_convert_int(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t pstring = {});

    template<do_t D>
    void force_round_real(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t pstring = {});

    template<do_t D>
    void force_boolify(rpn_value_t& rpn_value, pstring_t pstring = {});

    template<do_t D>
    bool cast(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t pstring = {});

    template<do_t D>
    void throwing_cast(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t pstring = {});

    template<do_t D>
    int cast_args(pstring_t pstring, rpn_value_t* begin, rpn_value_t* end, type_t const* type_begin, bool implicit);

    std::size_t num_locals() const { assert(fn); return fn->def().local_vars.size(); }

    type_t var_i_type(unsigned var_i) const;
    void init_sval(access_t a, sval_t& sval);
    access_t access(rpn_value_t const& rpn_value) const;
    ssa_value_t const& get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index) const;
    ssa_value_t& get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index);

    ///////////////////////
    // compiler-specific //
    ///////////////////////

    std::size_t num_vars() const { assert(ir); return num_locals() + ir->gmanager.num_locators(); }

    unsigned to_var_i(gmanager_t::index_t index) const { assert(index); return index.id + num_locals(); }
    template<typename T>
    unsigned to_var_i(T gvar) const { return to_var_i(ir->gmanager.var_i(gvar)); }

    // Block and local variable functions
    void seal_block(block_d& block_data);
    void fill_phi_args(ssa_ht phi, unsigned var_i, unsigned member);
    ssa_value_t var_lookup(cfg_ht node, unsigned var_i, unsigned member);
    sval_t var_lookup(cfg_ht node, unsigned var_i);
    ssa_value_t from_variant(ct_variant_t const& v, type_t type);
    ssa_value_array_t from_sval(sval_t const& sval, type_t type);

    cfg_ht insert_cfg(bool seal, pstring_t label_name = {});
    void cfg_exits_with_jump();
    void cfg_exits_with_branch(ssa_value_t condition);
    cfg_ht compile_goto();

    void compile_binary_operator(rpn_stack_t&, ssa_op_t op, type_t result_type, bool carry = false);

    ///////////////////
    // link-specific //
    ///////////////////

    template<eval_t::do_t D>
    void make_lt(rpn_stack_t& rpn_stack, unsigned argn,
                 token_t const* op_begin, token_t const* op_end);

    template<eval_t::do_t D>
    void make_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op);

    template<eval_t::do_t D>
    sval_t prep_lt(rpn_value_t const& rpn_value, token_t const* op_begin, token_t const* op_end);

    template<eval_t::do_t D>
    sval_t prep_lt(rpn_value_t const& rpn_value, token_t const& op);

    template<eval_t::do_t D>
    bool handle_lt(rpn_stack_t& rpn_stack, unsigned argn,
                   token_t const* op_begin, token_t const* op_end);

    template<eval_t::do_t D>
    bool handle_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op);
};

thread_local eval_t::ir_builder_t eval_t::builder;

spair_t interpret_expr(pstring_t pstring, token_t const* expr, type_t expected_type, eval_t* env)
{
    if(env)
    {
        env->do_expr_result<eval_t::INTERPRET_CE>(expr, expected_type);
        return env->final_result;
    }
    else
    {
        eval_t i(eval_t::do_wrapper_t<eval_t::INTERPRET>{}, pstring, expr, expected_type);
        return i.final_result;
    }
}

std::vector<locator_t> interpret_paa(pstring_t pstring, token_t const* expr)
{
    eval_t i(eval_t::do_wrapper_t<eval_t::INTERPRET>{}, pstring, expr, TYPE_VOID);
    return std::move(i.paa);
}

precheck_tracked_t build_tracked(fn_t const& fn)
{
    precheck_tracked_t tracked;
    eval_t eval(eval_t::do_wrapper_t<eval_t::CHECK_SYNTAX>{}, {}, fn, &tracked, nullptr, 0);
    return tracked;
}

void build_ir(ir_t& ir, fn_t const& fn)
{
    assert(cfg_data_pool::array_size() == 0);
    assert(ssa_data_pool::array_size() == 0);
    cfg_data_pool::scope_guard_t<block_d> cg(0);

    eval_t eval(ir, fn);
}

template<eval_t::do_t D>
eval_t::eval_t(do_wrapper_t<D>, pstring_t pstring, token_t const* expr, type_t expected_type)
: pstring(pstring)
, start_time(clock::now())
{
    do_expr_result<D>(expr, expected_type);
}

template<eval_t::do_t D>
eval_t::eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const& fn_ref, 
               precheck_tracked_t* tracked, sval_t const* args, unsigned num_args)
: pstring(pstring)
, fn(&fn_ref)
, stmt(fn_ref.def().stmts.data())
, start_time(clock::now())
, precheck_tracked(tracked)
{
    unsigned const nlocals = num_locals();

    interpret_local_defaults.resize(nlocals);
    var_types.resize(nlocals);
        for(unsigned i = 0; i < nlocals; ++i)
            var_types[i] = ::dethunkify(fn->def().local_vars[i].src_type, true, this);

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
            interpret_local_defaults.resize(nlocals);

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

eval_t::eval_t(ir_t& ir_ref, fn_t const& fn_ref)
: fn(&fn_ref)
, stmt(fn_ref.def().stmts.data())
, ir(&ir_ref)
, start_time(clock::now())
{
    // Reset the static thread-local state:
    builder.clear(); // TODO: make sure this isn't called in recursion
    ir->gmanager.init(fn->handle());

    unsigned const nlocals = num_locals();

    interpret_local_defaults.resize(nlocals);
    var_types.resize(nlocals);
    for(unsigned i = 0; i < nlocals; ++i)
        var_types[i] = ::dethunkify(fn->def().local_vars[i].src_type, true, this);

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
void eval_t::do_expr_result(token_t const* expr, type_t expected_type)
{
    rpn_stack_t rpn_stack;
    do_expr<D>(rpn_stack, expr);

    if(expected_type.name() != TYPE_VOID)
    {
        if(!can_size_unsized_array(rpn_stack.only1().type, expected_type))
            throwing_cast<D>(rpn_stack.only1(), expected_type, true);

        if(is_interpret(D))
            final_result.value = rpn_stack.only1().sval;

        final_result.type = rpn_stack.only1().type;
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

template<eval_t::do_t D>
void eval_t::interpret_stmts()
{
    static_assert(D != COMPILE);

    rpn_stack_t rpn_stack;

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
                    var_types[var_i] = dethunkify(fn->def().local_vars[var_i].src_type, true, this);

                if(stmt->expr)
                {
                    do_expr<D>(rpn_stack, stmt->expr);

                    if(can_size_unsized_array(rpn_stack.peek(0).type, var_types[var_i]))
                        var_types[var_i].set_array_length(rpn_stack.peek(0).type.array_length(), rpn_stack.peek(0).pstring);

                    throwing_cast<D>(rpn_stack.peek(0), var_types[var_i], true);

                    if(D == INTERPRET)
                    {
                        assert(interpret_locals[var_i].empty());
                        interpret_local_defaults[var_i] = rpn_stack.only1().sval;
                        interpret_locals[var_i] = std::move(rpn_stack.only1().sval);
                        rpn_stack.pop(1);
                    }
                }
                else if(D == INTERPRET)
                {
                    type_t const type = var_types[var_i];
                    unsigned const num = num_members(type);
                    assert(num > 0);

                    sval_t sval;
                    sval.reserve(num);

                    for(unsigned i = 0; i < num; ++i)
                    {
                        type_t const mt = member_type(type, i);
                        if(mt.name() == TYPE_TEA)
                            sval.emplace_back(make_ct_array(mt.array_length()));
                        else
                            sval.emplace_back();
                    }

                    interpret_locals[var_i] = std::move(sval);
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
            do_expr<D>(rpn_stack, stmt->expr);
            ++stmt;
            break;

        case STMT_DO:
        case STMT_END_IF:
        case STMT_LABEL:
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
            do_expr<D>(rpn_stack, stmt->expr);
            throwing_cast<D>(rpn_stack.only1(), TYPE_BOOL, true);
            if(!is_interpret(D) || rpn_stack.only1().fixed())
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
            do_expr<D>(rpn_stack, stmt->expr);
            throwing_cast<D>(rpn_stack.only1(), TYPE_BOOL, true);
            if(!is_interpret(D) || rpn_stack.only1().fixed())
                ++stmt;
            else
                stmt = &fn->def()[stmt->link];
            break;

        case STMT_END_DO:
            do_expr<D>(rpn_stack, stmt->expr);
            throwing_cast<D>(rpn_stack.only1(), TYPE_BOOL, true);
            if(is_interpret(D) && rpn_stack.only1().fixed())
                stmt = &fn->def()[stmt->link];
            else
                ++stmt;
            break;

        case STMT_RETURN:
            {
                type_t return_type = fn->type().return_type();
                if(stmt->expr)
                {
                    do_expr<D>(rpn_stack, stmt->expr);
                    throwing_cast<D>(rpn_stack.only1(), return_type, true);
                    if(is_interpret(D))
                        final_result.value = std::move(rpn_stack.only1().sval);
                    final_result.type = std::move(rpn_stack.only1().type);
                    rpn_stack.pop(1);
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
                precheck_tracked->wait_nmis.push_back(stmt_ht{ stmt - fn->def().stmts.data() });
            ++stmt;
            break;

        case STMT_FENCE:
            if(precheck_tracked)
                precheck_tracked->fences.push_back(stmt_ht{ stmt - fn->def().stmts.data() });
            ++stmt;
            break;
        }
    }
    assert(false);
}

void eval_t::compile_block()
{
    rpn_stack_t rpn_stack;

    while(true)
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
            {
                do_expr<COMPILE>(rpn_stack, stmt->expr);

                if(can_size_unsized_array(rpn_stack.peek(0).type, var_types[var_i]))
                    var_types[var_i].set_array_length(rpn_stack.peek(0).type.array_length(), rpn_stack.peek(0).pstring);

                throwing_cast<COMPILE>(rpn_stack.peek(0), var_types[var_i], true);
                value = from_sval(rpn_stack.only1().sval, rpn_stack.only1().type);

                assert(var_i < interpret_local_defaults.size());
                interpret_local_defaults[var_i] = rpn_stack.only1().sval;
            }
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
        do_expr<COMPILE>(rpn_stack, stmt->expr);
        ++stmt;
        break;

    case STMT_IF:
        {
            // Branch the active node.
            do_expr<COMPILE>(rpn_stack, stmt->expr);
            ++stmt;
            cfg_ht branch = builder.cfg;
            throwing_cast<COMPILE>(rpn_stack.only1(), TYPE_BOOL, true);
            cfg_exits_with_branch(rpn_stack.only1().ssa());

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

            do_expr<COMPILE>(rpn_stack, stmt->expr);
            ++stmt;
            cfg_ht const end_branch = builder.cfg;
            throwing_cast<COMPILE>(rpn_stack.only1(), TYPE_BOOL, true);
            cfg_exits_with_branch(rpn_stack.only1().ssa());

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

                do_expr<COMPILE>(rpn_stack, stmt->expr);
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

            do_expr<COMPILE>(rpn_stack, stmt->expr);
            ++stmt;
            cfg_ht const end_branch = builder.cfg;
            throwing_cast<COMPILE>(rpn_stack.only1(), TYPE_BOOL, true);
            cfg_exits_with_branch(rpn_stack.only1().ssa());

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
                do_expr<COMPILE>(rpn_stack, stmt->expr);
                throwing_cast<COMPILE>(rpn_stack.only1(), return_type, true);

                auto const& rpn = rpn_stack.only1();

                ssa_value_array_t array;
                array.reserve(rpn.sval.size());

                for(unsigned i = 0; i < rpn.sval.size(); ++i)
                    array.push_back(from_variant(rpn.sval[i], member_type(rpn.type, i)));

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
            compiler_error(stmt->pstring, "break statement outside of loop.");
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
            do_expr<COMPILE>(rpn_stack, stmt->expr);
            ++stmt;
            builder.cfg = dead;
        }
        break;

    case STMT_NMI:
        if(fn->fclass == FN_NMI)
            compiler_error(stmt->pstring, "Cannot wait for nmi inside nmi.");
        if(precheck_tracked)
            precheck_tracked->wait_nmis.push_back(stmt_ht{ stmt - fn->def().stmts.data() });
        goto do_fence;
    case STMT_FENCE:
        if(precheck_tracked)
            precheck_tracked->fences.push_back(stmt_ht{ stmt - fn->def().stmts.data() });
        // fall-through
    do_fence:
        {
            bc::small_vector<ssa_value_t, 32> inputs;

            block_d& block_data = builder.cfg.data<block_d>();
            ssa_ht const fenced = builder.cfg->emplace_ssa(
                stmt->name == STMT_NMI ? SSA_wait_nmi : SSA_fence, TYPE_VOID);
            fenced->append_daisy();

            //assert(fn->precheck_wait_nmi());

            ir->gmanager.for_each_gvar([&](gvar_ht gvar, gmanager_t::index_t index)
            {
                for(gmember_ht m : gvar->handles())
                {
                    if(fn->fence_reads().test(m.id))
                    {
                        inputs.push_back(var_lookup(builder.cfg, to_var_i(index), m->member()));
                        inputs.push_back(locator_t::gmember(m, 0));
                    }

                    // Create writes after reads:
                    if(fn->fence_writes().test(m.id))
                    {
                        ssa_ht const read = builder.cfg->emplace_ssa(
                            SSA_read_global, m->type(), fenced, locator_t::gmember(m, 0));
                        block_data.vars[to_var_i(index)][m->member()] = read;
                    }
                }
            });

            xbitset_t<gmember_ht> reads(0);
            xbitset_t<gmember_ht> writes(0);

            ir->gmanager.for_each_gmember_set(fn->handle(),
            [&](bitset_uint_t const* gmember_set, gmanager_t::index_t index,locator_t locator)
            {
                reads = fn->fence_reads();
                writes = fn->fence_writes();

                bitset_and(reads.size(), reads.data(), gmember_set);
                bitset_and(writes.size(), writes.data(), gmember_set);

                if(!reads.all_clear())
                {
                    inputs.push_back(var_lookup(builder.cfg, to_var_i(index), 0));
                    inputs.push_back(locator);
                }

                // Create writes after reads:
                if(!writes.all_clear())
                {
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
    assert(false);
}

template<eval_t::do_t D>
token_t const* eval_t::do_token(rpn_stack_t& rpn_stack, token_t const* token)
{
    //using namespace std::literals::chrono_literals; TODO: remove?
    using S = fixed_sint_t;

    // Declare cross-label vars before switch.
    ssa_value_t common_value;
    type_t common_type;
    ssa_value_t hw_addr;

    switch(token->type)
    {
    default:
        throw std::runtime_error(fmt("Invalid token '%' in expression.", token_string(token->type)));

    case TOK_ident:
        assert(token->value < num_locals());

        if(D == INTERPRET_CE)
            compiler_error(token->pstring, "Expression cannot be evaluated at compile-time.");
        else if(D == LINK)
            compiler_error(token->pstring, "Expression cannot be evaluated at link-time.");
        else
        {
            assert(is_check(D) || D == INTERPRET || D == COMPILE);
            assert(token->value < var_types.size());

            rpn_value_t new_top =
            {
                .category = LVAL, 
                .type = var_types[token->value],
                .pstring = token->pstring,
                .var_i = token->value,
            };

            if(D == COMPILE) 
            {
                new_top.sval = var_lookup(builder.cfg, token->value);
                new_top.time = RT;
            }
            else if(D == INTERPRET)
            {
                if(interpret_locals[token->value].empty())
                {
                    compiler_error(token->pstring, 
                        "Variable is invalid because goto jumped past its initialization.");
                }

                new_top.sval = interpret_locals[token->value];
            }

            if(!is_check(D))
                assert(new_top.sval.size() == num_members(new_top.type));
            rpn_stack.push(std::move(new_top));
        }

        break;

    case TOK_global_ident:
        {
            if(D == LINK)
                compiler_error(token->pstring, "Expression cannot be evaluated at link-time.");

            global_t const* global = token->ptr<global_t>();
            switch(global->gclass())
            {
            default: 
                throw std::runtime_error(
                    "Unimplemented global in expression.");

            case GLOBAL_VAR:
                if(D == COMPILE || is_check(D))
                {
                    if(precheck_tracked)
                        precheck_tracked->gvars_used.emplace(global->handle<gvar_ht>(), token->pstring);

                    rpn_value_t new_top =
                    {
                        .category = LVAL, 
                        .type = global->impl<gvar_t>().type(),
                        .pstring = token->pstring,
                    };

                    if(D == COMPILE)
                    {
                        unsigned const var_i = to_var_i(global->handle<gvar_ht>());
                        new_top.var_i = var_i;
                        new_top.sval = var_lookup(builder.cfg, var_i);
                        new_top.time = RT;
                    }

                    rpn_stack.push(std::move(new_top));
                }
                else
                    compiler_error(token->pstring, "Cannot use global variable in this context.");
                break;

            case GLOBAL_CONST:
                {
                    const_t const& c = global->impl<const_t>();
                    assert(!is_thunk(c.type().name()));

                    /* TODO: remove
                    if(is_paa(c.type().name()))
                    {
                        // Convert to a pointer.
                        rpn_value_t new_top =
                        {
                            .sval = make_sval(locator_t::lt_const_ptr(c.handle())),
                            .category = RVAL, 
                            .type = type_t::ptr(c.group(), c.group_data->once),
                            .pstring = token->pstring,
                        };
                        rpn_stack.push(std::move(new_top));
                    }
                    else
                    */
                    {
                        //assert(c.sval().size());

                        rpn_value_t new_top =
                        {
                            .category = RVAL, 
                            .type = c.type(),
                            .pstring = token->pstring,
                        };

                        if(!is_check(D))
                            new_top.sval = c.sval();

                        rpn_stack.push(std::move(new_top));
                    }
                }
                break;

            case GLOBAL_FN:
                rpn_stack.push({ 
                    .sval = { ssa_value_t(locator_t::fn(global->handle<fn_ht>())) },
                    .category = RVAL, 
                    .type = global->impl<fn_t>().type(), 
                    .pstring = token->pstring });
                break;
            }
        }
        break;

    case TOK_default:
        {
            if(D == LINK)
                compiler_error(token->pstring, "Expression cannot be evaluated at link-time.");

            global_t const* global = token->ptr<global_t>();
            global_datum_t const* datum = global->datum();

            if(!datum || !datum->init_expr)
                compiler_error(token->pstring, fmt("% has no default value.", global->name));

            rpn_value_t new_top =
            {
                .category = RVAL, 
                .type = datum->type(),
                .pstring = token->pstring,
            };

            if(!is_check(D))
                new_top.sval = datum->sval();

            rpn_stack.push(std::move(new_top));
        }
        break;

    case TOK_local_default:
        {
            if(D == LINK)
                compiler_error(token->pstring, "Expression cannot be evaluated at link-time.");

            unsigned const var_i = token->value;
            assert(var_i < interpret_local_defaults.size());

            rpn_value_t new_top =
            {
                .category = RVAL, 
                .type = var_types[var_i],
                .pstring = token->pstring,
            };

            if(!is_check(D))
            {
                new_top.sval = interpret_local_defaults[var_i];
                if(new_top.sval.empty())
                    compiler_error(token->pstring, "Variable has no default value.");
            }

            rpn_stack.push(std::move(new_top));
        }
        break;

    case TOK_at:
        {
            global_t const* global = token->ptr<global_t>();

            if(global->gclass() == GLOBAL_CONST)
            {
                const_t const& c = global->impl<const_t>();
                if(!c.is_paa)
                    compiler_error(token->pstring, "Cannot get address.");

                bool const banked = c.group_data->once;

                sval_t sval = make_sval(locator_t::lt_const_ptr(c.handle()));
                if(banked)
                    sval.push_back(locator_t::lt_const_ptr(c.handle()).with_is(IS_BANK));

                // Convert to a pointer.
                rpn_value_t new_top =
                {
                    .sval = std::move(sval),
                    .category = RVAL, 
                    .type = type_t::ptr(c.group(), false, banked),
                    .pstring = token->pstring,
                };
                rpn_stack.push(std::move(new_top));
            }
            else if(global->gclass() == GLOBAL_VAR)
            {
                gvar_t const& v = global->impl<gvar_t>();
                if(!v.is_paa)
                    compiler_error(token->pstring, "Cannot get address.");

                assert(v.begin() != v.end());

                // Convert to a pointer.
                rpn_value_t new_top =
                {
                    .sval = make_sval(locator_t::lt_gmember_ptr(v.begin())),
                    .category = RVAL, 
                    .type = type_t::ptr(v.group(), true, false),
                    .pstring = token->pstring,
                };
                assert(is_mptr(new_top.type.name()));
                rpn_stack.push(std::move(new_top));

            }
            else
                compiler_error(token->pstring, "Cannot get address.");


        }
        break;

    case TOK_int:
        common_value.set(mask_numeric(fixed_t{ token->value }, TYPE_INT), TYPE_INT);
    push_int:
        {
            rpn_value_t new_top = 
            {
                .category = RVAL, 
                .type = { TYPE_INT }, 
                .pstring = token->pstring 
            };

            if(!is_check(D))
                new_top.sval = { common_value };

            rpn_stack.push(std::move(new_top));
        }
        break;

    case TOK_real:
        common_value.set(mask_numeric(fixed_t{ token->value }, TYPE_REAL), TYPE_REAL);
        {
            rpn_value_t new_top = 
            {
                .category = RVAL, 
                .type = { TYPE_REAL }, 
                .pstring = token->pstring 
            };

            if(!is_check(D))
                new_top.sval = { common_value };

            rpn_stack.push(std::move(new_top));
        }
        break;

    case TOK_period:
        {
            // Periods represent struct member access.

            rpn_value_t& struct_val = rpn_stack.peek(0);

            if(struct_val.type.name() != TYPE_STRUCT)
            {
                compiler_error(struct_val.pstring, fmt(
                    "Expecting struct type. Got %.", struct_val.type));
            }

            struct_t const& s = struct_val.type.struct_();

            std::uint64_t const hash = token->value;
            auto const ptr = s.fields().lookup(hash);

            if(!ptr)
            {
                file_contents_t file(token->pstring.file_i);
                compiler_error(token->pstring, fmt(
                    "% isn't a member of %.", 
                    token->pstring.view(file.source()), s.global.name), &file);
            }

            unsigned const field_i = ptr - s.fields().begin();
            unsigned const member_i = member_index(struct_val.type, field_i);
            
            if(!is_check(D))
            {
                // Shrink the sval to only contain the specified field.
                unsigned const size = num_members(ptr->second.type());
                assert(struct_val.sval.size() == num_members(struct_val.type));
                assert(size + member_i <= struct_val.sval.size());
                if(member_i != 0)
                    for(unsigned i = 0; i < size; ++i)
                        struct_val.sval[i] = std::move(struct_val.sval[i + member_i]);
                struct_val.sval.resize(size);
            }

            struct_val.member += member_i;
            struct_val.type = ptr->second.type();
            struct_val.pstring = concat(struct_val.pstring, token->pstring);

            break;
        }

    case TOK_apply:
        {
            // TOK_apply is a psuedo token used to represent application. 
            // The token's 'value' stores the application's arity:
            std::size_t const num_args = token->value;

            if(handle_lt<D>(rpn_stack, num_args+1, *token))
                break;

            // The eval stack contains the arguments to be applied.
            // Right beneath those it contains the fn value to be called.
            rpn_value_t& fn_rpn = rpn_stack.peek(num_args);

            if(fn_rpn.type.name() != TYPE_FN)
            {
                compiler_error(fn_rpn.pstring, fmt(
                    "Expecting function type. Got %.", fn_rpn.type));
            }

            assert(fn_rpn.sval.size() == 1);
            ssa_value_t const fn_value = fn_rpn.ssa();
            assert(fn_value.is_locator());
            fn_ht const call = fn_value.locator().fn();

            pstring_t const call_pstring = concat(fn_rpn.pstring, token->pstring);

            if(call->fclass == FN_NMI)
                compiler_error(call_pstring, "Cannot call nmi function.");

            if(call->fclass == FN_MODE && is_interpret(D))
                compiler_error(call_pstring, "Cannot goto mode at compile-time.");

            std::size_t const num_params = fn_rpn.type.num_params();
            type_t const* const params = fn_rpn.type.types();

            if(num_args != num_params)
            {
                compiler_error(
                    fn_rpn.pstring, fmt(
                    "Passed % arguments to a function of type %. "
                    "Expecting % arguments.",
                    num_args, fn_rpn.type, num_params));
            }

            // Now for the arguments.
            // Cast all arguments to match the fn signature.
            rpn_value_t* const args = &rpn_stack.peek(num_args - 1);

            int const cast_result = cast_args<D>(fn_rpn.pstring, args, rpn_stack.past_top(), params, true);

            if(cast_result >= 0)
            {
                compiler_error(
                    args[cast_result].pstring, fmt(
                    "Unable to convert type % "
                    "to type % in function application.\n"
                    "Expected signature: % ",
                    args[cast_result].type, params[cast_result], fn_rpn.type));
            }

            if(precheck_tracked)
            {
                assert(fn);
                if(call->fclass == FN_MODE)
                {
                    // Track that we're going to a mode here:
                    precheck_tracked->goto_modes.push_back(std::make_pair(
                        call, stmt_ht{ stmt - fn->def().stmts.data() }));
                }
                else
                    precheck_tracked->calls.emplace(call, call_pstring);
            }

            // Now do the call!

            if(is_check(D))
            {
                rpn_value_t new_top =
                {
                    .category = RVAL, 
                    .type = fn_rpn.type.return_type(), 
                    .pstring = call_pstring,
                };

                rpn_stack.pop(num_args + 1);
                rpn_stack.push(std::move(new_top));
            }
            else if(is_interpret(D))
            {
            interpret_fn:

                bc::small_vector<sval_t, 8> sval_args(num_args);
                for(unsigned i = 0; i < num_args; ++i)
                {
                    if(!args[i].is_ct())
                        compiler_error(args[i].pstring, "Expecting compile-time constant value.");
                    sval_args[i] = args[i].sval;
                }

                try
                {
                    // NOTE: call as INTERPRET, not D.
                    eval_t sub(do_wrapper_t<INTERPRET>{}, call_pstring, *call, nullptr, sval_args.data(), sval_args.size());

                    // Update the eval stack.
                    rpn_value_t new_top =
                    {
                        .sval = std::move(sub.final_result.value),
                        .category = RVAL, 
                        .type = fn_rpn.type.return_type(), 
                        .pstring = call_pstring,
                    };

                    rpn_stack.pop(num_args + 1);
                    rpn_stack.push(std::move(new_top));
                }
                catch(out_of_time_t& e)
                {
                    e.msg += fmt_note(this->pstring, "Backtrace:");
                    throw;
                }
            }
            else if(D == COMPILE)
            {
                if(call->fclass == FN_CT)
                    goto interpret_fn;
                // TODO: Interpret in other situations, too.

                bc::small_vector<ssa_value_t, 32> fn_inputs;

                // The [0] argument holds the fn_t ptr.
                fn_inputs.push_back(ssa_value_t(locator_t::fn(call)));
                
                // For modes, the [1] argument references the stmt:
                if(call->fclass == FN_MODE)
                    fn_inputs.push_back(locator_t::stmt(stmt_ht{ stmt - fn->def().stmts.data() }));

                // Prepare the input globals

                bool const is_idep = fn->global.ideps().count(&call->global) > 0;
                assert(is_idep || call->fclass == FN_MODE);

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

                        fn_inputs.push_back(from_variant(args[i].sval[j], member_type));
                        fn_inputs.push_back(loc);
                    }
                }

                // Create the dependent node.
                ssa_op_t const op = (call->fclass == FN_MODE) ? SSA_goto_mode : SSA_fn_call;
                ssa_ht const fn_node = builder.cfg->emplace_ssa(op, TYPE_VOID);
                fn_node->link_append_input(&*fn_inputs.begin(), &*fn_inputs.end());

                if(call->fclass == FN_MODE || !call->ir_io_pure() || ir->gmanager.num_locators() > 0)
                    fn_node->append_daisy();

                if(call->fclass != FN_MODE)
                {
                    assert(is_idep);

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

                    rpn_value_t new_top =
                    {
                        .category = RVAL, 
                        .type = fn_rpn.type.return_type(), 
                        .pstring = concat(fn_rpn.pstring, token->pstring)
                    };

                    type_t const return_type = fn_rpn.type.return_type();
                    unsigned const return_members = ::num_members(return_type);

                    for(unsigned m = 0; m < return_members; ++m)
                    {
                        ssa_ht ret = builder.cfg->emplace_ssa(
                            SSA_read_global, member_type(return_type, m), fn_node, locator_t::ret(call, m, 0));

                        new_top.sval.push_back(ret);
                    }

                    // Update the eval stack.

                    rpn_stack.pop(num_args + 1);
                    rpn_stack.push(std::move(new_top));
                }
                else
                {
                    rpn_stack.pop(num_args + 1);
                }
            }

            break;
        }

        // TODO
        /* TODO
    case TOK_byte_array_argn:
        assert(0);
        {
            // TOK_byte_array are pseudo tokens used to implement bulk data of bytes.

            // Extract how many args this cast parsed:
            unsigned const argn = token->value;

            // Advance the token to get the TOK_byte_array.
            ++token;
            assert(token->type == TOK_byte_array);
            locator_t const* byte_array = token->ptr<locator_t const>();

            ct_array_t ct_array = make_ct_array(argn);
            std::copy(byte_array, byte_array + argn, ct_array.get());

            rpn_value_t new_top = 
            {
                .category = RVAL, 
                .type = type, 
                .pstring = token->pstring,
            };
            assert(0); // TODO

            rpn_stack.push(std::move(new_top));
        }
        break;
        */

    case TOK_hw_expr:
        {
            if(is_interpret(D))
                compiler_error(stmt->pstring, "Hardware expression cannot be evaluated at compile-time.");
            spair_t result = interpret_expr(token->pstring, token->ptr<token_t const>(), TYPE_U20, this);
            assert(result.type == TYPE_U20);

            hw_addr = from_variant(result.value[0], TYPE_U20);
            if(hw_addr.is_num())
                hw_addr = locator_t::addr(hw_addr.whole());
            else if(hw_addr.is_locator())
                hw_addr = hw_addr.locator().with_is(IS_DEREF);

            goto rw_hw;
        }

    case TOK_hw_addr:
        hw_addr = locator_t::addr(token->value);
    rw_hw:
        ++token;
        if(token->type == TOK_read_hw)
        {
            rpn_value_t new_top = 
            {
                .category = RVAL, 
                .type = TYPE_U, 
                .pstring = token->pstring,
            };

            if(is_interpret(D))
                compiler_error(stmt->pstring, "Hardware-related expressions cannot be evaluated at compile-time.");
            else if(D == COMPILE)
            {
                ssa_ht h = builder.cfg->emplace_ssa(SSA_read_hw, TYPE_U, hw_addr);
                h->append_daisy();
                new_top.sval = { h };
            }

            rpn_stack.push(std::move(new_top));
        }
        else if(token->type == TOK_write_hw)
        {
            throwing_cast<D>(rpn_stack.peek(0), TYPE_U, true);

            // Modify the top RPN value instead of pushing/popping:
            auto& top = rpn_stack.peek(0);
            assert(top.type == TYPE_U);
            top.category = RVAL;
            top.pstring = concat(top.pstring, token->pstring);

            if(is_interpret(D))
                compiler_error(stmt->pstring, "Hardware-related expressions cannot be evaluated at compile-time.");
            else if(D == COMPILE)
            {
                ssa_ht h = builder.cfg->emplace_ssa(
                    SSA_write_hw, TYPE_VOID, 
                    hw_addr, top.ssa());
                h->append_daisy();
            }
        }
        else
            passert(false, token_string(token->type));
        break;

    case TOK_push_paa:
        if(!is_check(D))
        {
            rpn_value_t const& v = rpn_stack.only1();
            ::append_locator_bytes(paa, v.sval, v.type, v.pstring);
        }
        rpn_stack.pop(1);
        break;

    case TOK_push_paa_byte_array:
        if(!is_check(D))
        {
            locator_t const* data = token->ptr<locator_t>();
            ++token;
            assert(token->type == TOK_push_paa);
            std::size_t const size = token->value;
            paa.insert(paa.end(), data, data + size);
        }
        break;

    case TOK_cast_argn:
        {
            // TOK_cast are pseudo tokens used to implement type casts.

            // Extract how many args this cast parsed:
            unsigned const argn = token->value;
            rpn_value_t* const args = &rpn_stack.peek(argn - 1);
            assert(rpn_stack.past_top() - args == argn);

            // Advance the token to get the TOK_cast_type.
            ++token;
            assert(token->type == TOK_cast_type);
            type_t const type = dethunkify({ token->pstring, *token->ptr<type_t const>() }, true, this);

            // Only handle LT for non-aggregates.
            if(!is_aggregate(type.name()) && handle_lt<D>(rpn_stack, argn, token-1, token+1))
                break;

            auto const check_argn = [&](unsigned size)
            { 
                if(argn != size)
                    compiler_error(token->pstring, fmt(
                        "Too % arguments to %. Expecting %.", 
                        argn < size ? "few" : "many", type, size));
            };

            if(is_aggregate(type.name()))
            {
                rpn_value_t new_top = 
                {
                    .category = RVAL, 
                    .type = type, 
                    .pstring = token->pstring,
                };

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

                    int const cast_result = cast_args<D>(token->pstring, args, rpn_stack.past_top(), types, false);

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
                        // Create a new sval.
                        sval_t new_sval;
                        new_sval.reserve(num_members(type));

                        for(unsigned i = 0; i < argn; ++i)
                            for(auto& v : args[i].sval)
                                new_sval.push_back(std::move(v));

                        assert(new_sval.size() == num_members(type));

                        new_top.sval = std::move(new_sval);
                    }
                }
                else if(type.name() == TYPE_TEA)
                {
                    check_argn(type.array_length());

                    for(unsigned i = 0; i < argn; ++i)
                        throwing_cast<D>(args[i], type.elem_type(), false);

                    if(is_interpret(D))
                    {
                    interpret_cast_array:
                        // Create a new sval.
                        sval_t new_sval(num_members(type));

                        for(unsigned i = 0; i < new_sval.size(); ++i)
                        {
                            ct_array_t shared = make_ct_array(argn);
                            for(unsigned j = 0; j < argn; ++j)
                                shared[j] = std::get<ssa_value_t>(args[j].sval[i]);
                            new_sval[i] = std::move(shared);
                        }

                        new_top.sval = std::move(new_sval);
                    }
                    else if(D == COMPILE)
                    {
                        unsigned const num_mem = num_members(type);

                        for(unsigned i = 0; i < num_mem; ++i)
                        {
                            type_t const mt = member_type(type, i);
                            assert(mt.name() == TYPE_TEA);

                            for(unsigned j = 0; j < argn; ++j)
                                if(!args[j].is_ct())
                                    goto isnt_ct;
                        }
                        goto interpret_cast_array;
                    isnt_ct:

                        // Create a new sval.
                        sval_t new_sval(num_mem);

                        for(unsigned i = 0; i < num_mem; ++i)
                        {
                            type_t const mt = member_type(type, i);
                            assert(mt.name() == TYPE_TEA);

                            ssa_ht h = builder.cfg->emplace_ssa(SSA_init_array, type);
                            h->alloc_input(argn);
                            for(unsigned j = 0; j < argn; ++j)
                                h->build_set_input(j, std::get<ssa_value_t>(args[j].sval[i]));

                            new_sval[i] = h;
                        }

                        new_top.sval = std::move(new_sval);
                    }
                }

                // Update the stack.
                rpn_stack.pop(argn);
                rpn_stack.push(std::move(new_top));
            }
            else if(is_scalar(type.name()))
            {
                check_argn(1);
                throwing_cast<D>(rpn_stack.peek(0), type, false);
            }
            else
                compiler_error(token->pstring, fmt("Unable to cast to %.", type));

            rpn_stack.peek(0).pstring = concat(token->pstring, rpn_stack.peek(0).pstring);
        }
        break;

    case TOK_index:
        {
            // TOK_index is a psuedo token used to implement array indexing. 

            if(handle_lt<D>(rpn_stack, 2, *token))
                break;

            // The eval stack contains the index on top.
            // Right beneath it contains the array.
            rpn_value_t& array_val = rpn_stack.peek(1);

            bool const is_ptr = ::is_ptr(array_val.type.name());
            bool const is_mptr = ::is_mptr(array_val.type.name());

            if(!is_tea(array_val.type.name()) && !is_ptr)
            {
                compiler_error(array_val.pstring, fmt(
                    "Expecting array or pointer type. Got %.", array_val.type));
            }

            if(is_ptr && precheck_tracked)
            {
                // TODO: Update this when inlining is added.
                unsigned const size = array_val.type.group_tail_size();
                precheck_tracked->deref_types.insert(array_val.type);
                for(unsigned i = 0; i < size; ++i)
                    precheck_tracked->deref_groups.emplace(array_val.type.group(i), src_type_t{ array_val.pstring, array_val.type });
            }

            rpn_value_t& array_index = rpn_stack.peek(0);
            if(!is_check(D))
                assert(array_val.sval.size() > 0);

            // Array indexes are always bytes.
            throwing_cast<D>(array_index, TYPE_U, true);

            array_val.pstring = concat(array_val.pstring, token->pstring);

            if(is_interpret(D))
            {
                assert(0); // TODO: implement
                unsigned const index = array_index.whole();
                array_val.index.set(index, TYPE_U);
                
                if(index >= array_val.type.array_length())
                {
                    compiler_error(array_index.pstring, 
                        fmt("Array index is out of bounds. (index: % >= size: %)", 
                            index, array_val.type.array_length()));
                }

                for(ct_variant_t& v : array_val.sval)
                    v = std::get<ct_array_t>(v)[index];
            }
            else if(D == COMPILE)
            {
                array_val.index = array_index.ssa();

                if(is_ptr)
                {
                    bool const is_banked = array_val.type.name() == TYPE_BANKED_PTR;
                    assert(array_val.sval.size() == is_banked ? 2 : 1);

                    // TODO
                    //ssa_value_t prev_in_order = {};
                    //if(auto ptr_i = ir->gmanager.ptr_i(array_val.type))
                        //prev_in_order = var_lookup(builder.cfg, to_var_i(ptr_i), 0);

                    ssa_ht const h = builder.cfg->emplace_ssa(
                        SSA_read_ptr, TYPE_U, 
                        from_variant(array_val.sval[0], array_val.type), ssa_value_t(),
                        is_banked ? from_variant(array_val.sval[1], array_val.type)
                                  : ssa_value_t(), 
                        std::get<ssa_value_t>(array_index.sval[0]));

                    if(ptr_to_vars(array_val.type))
                        h->append_daisy();

                    array_val.sval[0] = h;
                    array_val.sval.resize(1);
                }
                else
                {
                    for(unsigned i = 0; i < array_val.sval.size(); ++i)
                    {
                        type_t const etype = ::member_type(array_val.type, i);
                        assert(etype.name() == TYPE_TEA);

                        array_val.sval[i] = builder.cfg->emplace_ssa(
                            SSA_read_array, etype.elem_type(), 
                            from_variant(array_val.sval[i], etype), locator_t(), 
                            std::get<ssa_value_t>(array_index.sval[0]));
                    }
                }
            }

            if(is_ptr)
            {
                //array_val.derefed_from = array_val.type;
                array_val.type = TYPE_U;
                if(array_val.category != RVAL)
                    array_val.category = is_mptr ? LVAL_PTR : RVAL;
            }
            else
            {
                array_val.type = array_val.type.elem_type();
                if(array_val.category != RVAL)
                    array_val.category = LVAL_ARRAY;
            }

            rpn_stack.pop(1);
            break;
        }

    case TOK_sizeof_expr:
        {
            rpn_stack_t sub_stack;
            do_expr<CHECK_TYPES_SYNTAX>(sub_stack, token->ptr<token_t const>());
            common_type = sub_stack.peek(0).type;
            goto do_sizeof;
        }

    case TOK_sizeof:
        {
            common_type = dethunkify({ token->pstring, *token->ptr<type_t const>() }, true, this);
        do_sizeof:
            unsigned const size = common_type.size_of();

            if(size == 0)
                compiler_error(token->pstring, fmt("Type % has no size.", common_type));

            common_value.set(size, TYPE_INT);
            goto push_int;
        }

    case TOK_len_expr:
        {
            rpn_stack_t sub_stack;
            do_expr<CHECK_TYPES_SYNTAX>(sub_stack, token->ptr<token_t const>());
            common_type = sub_stack.peek(0).type;
            goto do_len;
        }

    case TOK_len:
        {
            common_type = dethunkify({ token->pstring, *token->ptr<type_t const>() }, true, this);
        do_len:
            if(is_check(D))
                goto push_int;

            unsigned const size = common_type.array_length();

            if(size == 0)
                compiler_error(token->pstring, fmt("Type % isn't an array.", common_type));

            common_value.set(size, TYPE_INT);
            goto push_int;
        }

    case TOK_assign:
        do_assign<D>(rpn_stack, *token);
        break;

    case TOK_logical_and:
        // TODO: lt
        struct logical_and_p : do_wrapper_t<D>
        {
            static token_type_t logical_token() { return TOK_logical_and; }
            static token_type_t end_logical_token() { return TOK_end_logical_and; }
        };
        do_logical_begin<logical_and_p>(rpn_stack, token);
        break;
    case TOK_end_logical_and:
        do_logical_end<logical_and_p>(rpn_stack);
        break;
    case TOK_logical_or:
        struct logical_or_p : do_wrapper_t<D>
        {
            static token_type_t logical_token() { return TOK_logical_or; }
            static token_type_t end_logical_token() { return TOK_end_logical_or; }
        };
        do_logical_begin<logical_or_p>(rpn_stack, token);
        break;
    case TOK_end_logical_or:
        do_logical_end<logical_or_p>(rpn_stack);
        break;

    case TOK_eq:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct eq_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs == rhs; }
            static ssa_op_t op() { return SSA_eq; }
        };
        do_compare<eq_p>(rpn_stack, *token);
        break;
    case TOK_not_eq:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct not_eq_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs != rhs; }
            static ssa_op_t op() { return SSA_not_eq; }
        };
        do_compare<not_eq_p>(rpn_stack, *token);
        break;
    case TOK_gt:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        std::swap(rpn_stack.peek(0), rpn_stack.peek(1));
        goto do_lt;
    case TOK_lt:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
    do_lt:
        struct lt_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs < rhs; }
            static ssa_op_t op() { return SSA_lt; }
        };
        do_compare<lt_p>(rpn_stack, *token);
        break;
    case TOK_gte:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        std::swap(rpn_stack.peek(0), rpn_stack.peek(1));
        goto do_lte;
    case TOK_lte:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
    do_lte:
        struct lte_p : do_wrapper_t<D>
        {
            static bool interpret(S lhs, S rhs) { return lhs <= rhs; }
            static ssa_op_t op() { return SSA_lte; }
        };
        do_compare<lte_p>(rpn_stack, *token);
        break;

    case TOK_plus:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct plus_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs + rhs; }
            static ssa_op_t op() { return SSA_add; }
        };
        do_arith<plus_p>(rpn_stack, *token);
        break;
    case TOK_plus_assign:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        do_assign_arith<plus_p>(rpn_stack, *token);
        break;

    case TOK_minus: 
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct minus_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs - rhs; }
            static ssa_op_t op() { return SSA_sub; }
        };
        do_arith<minus_p>(rpn_stack, *token);
        break;
    case TOK_minus_assign:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        do_assign_arith<minus_p>(rpn_stack, *token);
        break;

    case TOK_bitwise_and: 
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct bitwise_and_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs & rhs; }
            static ssa_op_t op() { return SSA_and; }
        };
        do_arith<bitwise_and_p>(rpn_stack, *token);
        break;
    case TOK_bitwise_and_assign:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        do_assign_arith<bitwise_and_p>(rpn_stack, *token);
        break;

    case TOK_bitwise_or:  
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct bitwise_or_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs | rhs; }
            static ssa_op_t op() { return SSA_or; }
        };
        do_arith<bitwise_or_p>(rpn_stack, *token);
        break;
    case TOK_bitwise_or_assign:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        do_assign_arith<bitwise_or_p>(rpn_stack, *token);
        break;

    case TOK_bitwise_xor:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct bitwise_xor_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, S rhs) { return lhs ^ rhs; }
            static ssa_op_t op() { return SSA_xor; }
        };
        do_arith<bitwise_xor_p>(rpn_stack, *token);
        break;
    case TOK_bitwise_xor_assign:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        do_assign_arith<bitwise_xor_p>(rpn_stack, *token);
        break;

    case TOK_lshift:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct lshift_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, std::uint8_t shift) { return lhs << shift; }
            static ssa_op_t op() { return SSA_shl; }
        };
        do_shift<lshift_p>(rpn_stack, *token);
        break;
    case TOK_lshift_assign:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        do_assign_shift<lshift_p>(rpn_stack, *token);
        break;

    case TOK_rshift:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        struct rshift_p : do_wrapper_t<D>
        {
            static S interpret(S lhs, std::uint8_t shift) { return lhs >> shift; }
            static ssa_op_t op() { return SSA_shr; }
        };
        do_shift<rshift_p>(rpn_stack, *token);
        break;
    case TOK_rshift_assign:
        if(handle_lt<D>(rpn_stack, 2, *token))
            break;
        do_assign_shift<rshift_p>(rpn_stack, *token);
        break;

    case TOK_unary_negate:
        if(handle_lt<D>(rpn_stack, 1, *token))
            break;
        {
            rpn_value_t& top = rpn_stack.peek(0);
            throwing_cast<D>(top, { TYPE_BOOL }, true);

            if(is_interpret(D))
                top.ssa().set(unsigned(!top.whole()), top.type.name());
            else if(D == COMPILE)
            {
                // Must be two lines; reference invalidation lurks.
                ssa_ht ssa = builder.cfg->emplace_ssa(SSA_eq, TYPE_BOOL, top.ssa(), ssa_value_t(0u, TYPE_BOOL));
                top.ssa() = ssa;
            }

            break;
        }

    case TOK_unary_minus:
        if(handle_lt<D>(rpn_stack, 1, *token))
            break;
        {
            rpn_value_t& top = rpn_stack.peek(0);
            req_quantity(*token, top);

            if(is_interpret(D))
                top.ssa().set(mask_numeric(fixed_t{ -top.s() }, top.type.name()), top.type.name());
            else if(D == COMPILE)
            {
                // Must be two lines; reference invalidation lurks.
                ssa_ht ssa = builder.cfg->emplace_ssa(SSA_sub, top.type, ssa_value_t(0u, top.type.name()), top.ssa());
                top.ssa() = ssa;
            }

            break;
        }

    case TOK_unary_xor:
        if(handle_lt<D>(rpn_stack, 1, *token))
            break;
        {
            rpn_value_t& top = rpn_stack.peek(0);
            req_quantity(*token, top);

            if(is_interpret(D))
                top.ssa().set(mask_numeric(fixed_t{ ~top.u() }, top.type.name()), top.type.name());
            else if(D == COMPILE)
            {
                // Must be two lines; reference invalidation lurks.
                ssa_ht ssa = builder.cfg->emplace_ssa(SSA_xor, top.type, 
                                                      ssa_value_t(numeric_bitmask(top.type.name()), top.type.name()), 
                                                      top.ssa());
                top.ssa() = ssa;
            }

            break;
        }
    }

    return token + 1;
}

template<eval_t::do_t D>
void eval_t::do_expr(rpn_stack_t& rpn_stack, token_t const* expr)
{
    check_time();

    rpn_stack.clear(); // Reset the stack.

    assert(expr);
    for(token_t const* token = expr; token->type;)
        token = do_token<D>(rpn_stack, token);
}

template<eval_t::do_t D>
void eval_t::do_assign(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_value_t& assignee = rpn_stack.peek(1);
    rpn_value_t& assignment = rpn_stack.peek(0);

    pstring_t const pstring = concat(assignee.pstring, assignee.pstring);

    if(assignee.category == LVAL_PTR)
    {
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
                std::get<ssa_value_t>(assignment.sval[0]));
            write->append_daisy();

            assignee.sval = std::move(assignment.sval);

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

    if(is_check(D))
        goto finish;

    assert(assignee.sval.size() == assignment.sval.size());
    assert(assignee.var_i < var_types.size());

    // Remap the identifier to point to the new value.
    if(D == INTERPRET)
    {
        assert(assignee.var_i < interpret_locals.size());
        sval_t& local = interpret_locals[assignee.var_i];

        if(assignee.category == LVAL_ARRAY)
        {
            assert(assignee.index);

            type_t const mt = member_type(var_types[assignee.var_i], assignee.member);
            assert(mt.name() == TYPE_TEA);
            assert(mt.elem_type() == assignee.type);

            unsigned const array_size = mt.array_length();
            unsigned const index = assignee.index.whole();
            assert(index <= array_size);

            for(unsigned i = 0; i < assignment.sval.size(); ++i)
            {
                ct_array_t& shared = std::get<ct_array_t>(local[i + assignee.member]);

                // If the array has multiple owners, copy it, creating a new one.
                if(shared.use_count() > 1)
                {
                    ct_array_t new_shared = make_ct_array(array_size);
                    std::copy(shared.get(), shared.get() + array_size, new_shared.get());
                    shared = std::move(new_shared);
                }

                shared[index] = std::get<ssa_value_t>(assignment.sval[i]);
            }
        }
        else
        {
            assert(!assignee.index);
            for(unsigned i = 0; i < assignment.sval.size(); ++i)
                local[i + assignee.member] = assignment.sval[i];
        }

        assignee.sval = std::move(assignment.sval);
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

            for(unsigned i = 0; i < assignment.sval.size(); ++i)
            {
                ssa_ht read = assignee.ssa(i).handle();
                assert(read->op() == SSA_read_array);

                assert(assignment.type.name() != TYPE_TEA);
                type_t const type = type_t::tea(member_type(assignment.type, i), mt.size(), assignment.pstring);
                assert(type.name() == TYPE_TEA);

                ssa_value_t const prev_array = var_lookup(builder.cfg, assignee.var_i, i);

                ssa_ht write = builder.cfg->emplace_ssa(
                    SSA_write_array, type,
                    prev_array, locator_t(), read->input(2), std::get<ssa_value_t>(assignment.sval[i]));

                local[i + assignee.member] = write;
            }
        }
        else
        {
            assert(!assignee.index);
            for(unsigned i = 0; i < assignment.sval.size(); ++i)
                local[i + assignee.member] = from_variant(assignment.sval[i], member_type(assignment.type, i));
        }

        assignee.sval = std::move(assignment.sval);
    }

    // Leave the assignee on the stack, slightly modified.
finish:
    assignee.category = RVAL;
    rpn_stack.pop();
}

void eval_t::req_quantity(token_t const& token, rpn_value_t const& value)
{
    if(!is_quantity(value.type.name()))
    {
        compiler_error(value.pstring, fmt("% expects arithmetic quantity inputs. (Operand is %)", 
                                          token_string(token.type), value.type));
    }
}
    
void eval_t::req_quantity(token_t const& token, rpn_value_t const& lhs, rpn_value_t const& rhs)
{
    if(!is_quantity(lhs.type.name()) || !is_quantity(rhs.type.name()))
    {
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, fmt("% expects arithmetic quantity inputs. (Operands are % and %)", 
                                    token_string(token.type), lhs.type, rhs.type));
    }
}

// Applies an operator to the top two values on the eval stack,
// turning them into a single value.
void eval_t::compile_binary_operator(rpn_stack_t& rpn_stack, ssa_op_t op, type_t result_type, bool carry)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    // Result will remain in 'lhs'.
    ssa_value_t result;
    if(carry)
        result = builder.cfg->emplace_ssa(op, result_type, lhs.ssa(), rhs.ssa(), 
                                          ssa_value_t(op == SSA_sub, TYPE_BOOL));
    else
        result = builder.cfg->emplace_ssa(op, result_type, lhs.ssa(), rhs.ssa());

    rpn_value_t new_top =
    {
        .sval = { result },
        .category = RVAL, 
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };
    
    rpn_stack.pop(2);
    rpn_stack.push(std::move(new_top));
}


template<typename Policy>
void eval_t::do_compare(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);
    req_quantity(token, lhs, rhs);

    rpn_value_t new_top =
    {
        .category = RVAL, 
        .type = { TYPE_BOOL }, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

    if(!is_scalar(lhs.type.name()) || !is_scalar(rhs.type.name()))
    {
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, fmt("% isn't defined for this type combination. (% and %)",
                                    token_string(token.type), lhs.type, rhs.type));
    }

    if(lhs.type != rhs.type)
    {
        if(is_ct(lhs.type) && can_cast(lhs.type, rhs.type, true))
            throwing_cast<Policy::D>(lhs, rhs.type, true);
        else if(is_ct(rhs.type) && can_cast(rhs.type, lhs.type, true))
            throwing_cast<Policy::D>(rhs, lhs.type, true);
    }

    if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
    {
        bool const result = Policy::interpret(lhs.s(), rhs.s());
        new_top.sval = make_sval(ssa_value_t((unsigned)result, TYPE_BOOL));
    }
    else if(Policy::D == COMPILE)
    {
        // The implementation is kept simpler if both types being compared have the same size.
        if((Policy::op() == SSA_eq || Policy::op() == SSA_not_eq) 
           && !same_scalar_layout(lhs.type.name(), rhs.type.name()))
        {
            unsigned const w = std::max(whole_bytes(lhs.type.name()), whole_bytes(rhs.type.name()));
            unsigned const f = std::max(frac_bytes(lhs.type.name()), frac_bytes(rhs.type.name()));

            throwing_cast<Policy::D>(lhs, type_s_or_u(w, f, is_signed(lhs.type.name())), true, lhs.pstring);
            throwing_cast<Policy::D>(rhs, type_s_or_u(w, f, is_signed(rhs.type.name())), true, rhs.pstring);

            assert(same_scalar_layout(lhs.type.name(), rhs.type.name()));
        }

        return compile_binary_operator(rpn_stack, Policy::op(), TYPE_BOOL);
    }

    rpn_stack.pop(2);
    rpn_stack.push(std::move(new_top));
}

static locator_t _loc_const(sval_t const& sval)
{
    if(sval.size() < 1)
        return {};
    if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&sval[0]))
        if(ssa->is_locator() && ssa->locator().lclass() == LOC_LT_CONST_PTR)
            return ssa->locator();
    return {};
}

template<typename Policy>
void eval_t::do_arith(rpn_stack_t& rpn_stack, token_t const& token)
{
    ssa_op_t const op = Policy::op();
    bool const summable = op == SSA_add || op == SSA_sub;

    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);
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

    rpn_value_t new_top =
    {
        .category = RVAL, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

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
                    new_top.type = TYPE_S20;

                    if(!is_check(Policy::D))
                    {
                        // If both are locators with the same handle,
                        // we can calculate this at CT
                        locator_t const l = _loc_const(lhs.sval);
                        locator_t const r = _loc_const(rhs.sval);
                        // TODO: check for interpret
                        if(l && r && l.const_() && l.const_() == r.const_())
                        {
                            std::uint16_t const diff = l.offset() - r.offset();
                            new_top.sval = make_sval(ssa_value_t(diff, TYPE_S20));
                        }
                        else
                        {
                            if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
                            {
                                // Create a link-time expression.
                                make_lt<Policy::D>(rpn_stack, 2, { .type = TOK_minus, .pstring = token.pstring });
                                return;
                            }
                            else if(Policy::D == COMPILE)
                            {
                                throwing_cast<Policy::D>(lhs, new_top.type, false);
                                throwing_cast<Policy::D>(rhs, new_top.type, false);
                                return compile_binary_operator(rpn_stack, Policy::op(), new_top.type, ssa_argn(Policy::op()) > 2);
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
                new_top.type = lhs.type;
                assert(is_ptr(new_top.type.name()));
                assert(!is_ptr(rhs.type.name()));

                bool const banked = new_top.type.name() == TYPE_BANKED_PTR;

                throwing_cast<Policy::D>(rhs, TYPE_S20, true);

                if(!is_check(Policy::D))
                {
                    locator_t const l = _loc_const(lhs.sval);

                    if(l && l.const_() && rhs.is_ct())
                    {
                        std::uint16_t const sum = std::uint16_t(l.offset()) + rhs.whole();
                        new_top.sval = make_sval(locator_t::lt_const_ptr(l.const_(), sum));

                        if(banked)
                            new_top.sval.push_back(locator_t::lt_const_ptr(l.const_()).with_is(IS_BANK));
                    }
                    else
                    {
                        if(is_interpret(Policy::D))
                        {
                            // Create a link-time expression.
                            make_lt<Policy::D>(rpn_stack, 2, { .type = op == SSA_add ? TOK_plus : TOK_minus, .pstring = token.pstring });
                            return;
                        }
                        else if(Policy::D == COMPILE)
                        {
                            ct_variant_t bank;
                            if(banked)
                            {
                                assert(lhs.sval.size() == 2);
                                bank = lhs.sval[1];
                            }

                            throwing_cast<Policy::D>(lhs, TYPE_U20, false);
                            throwing_cast<Policy::D>(rhs, TYPE_U20, false);

                            ssa_ht const sum = builder.cfg->emplace_ssa(
                                Policy::op(), TYPE_U20, 
                                lhs.ssa(0), rhs.ssa(0), 
                                ssa_value_t(Policy::op() == SSA_sub, TYPE_BOOL));

                            ssa_ht const cast = builder.cfg->emplace_ssa(
                                SSA_cast, new_top.type.set_banked(false), sum);

                            new_top.sval.push_back(cast);

                            if(banked)
                                new_top.sval.push_back(bank);
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
                new_top.type = rhs.type;
                throwing_cast<Policy::D>(lhs, new_top.type, true);
            }
            else if(is_ct(rhs.type) && can_cast(rhs.type, lhs.type, true))
            {
                new_top.type = lhs.type;
                throwing_cast<Policy::D>(rhs, new_top.type, true);
            }
            else
            {
                compiler_error(new_top.pstring, fmt("% isn't defined for this type combination. (% and %)",
                                                    token_string(token.type), lhs.type, rhs.type));
            }
        }
        else
            new_top.type = lhs.type;

        assert(is_arithmetic(new_top.type.name()));
        assert(lhs.type == new_top.type);
        assert(rhs.type == new_top.type);

        if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
        {
            assert(is_masked(lhs.fixed(), lhs.type.name()));
            assert(is_masked(rhs.fixed(), rhs.type.name()));

            fixed_t result = { Policy::interpret(lhs.s(), rhs.s()) };
            result.value &= numeric_bitmask(new_top.type.name());
            new_top.sval = make_sval(ssa_value_t(result, new_top.type.name()));
        }
        else if(Policy::D == COMPILE)
            return compile_binary_operator(rpn_stack, Policy::op(), new_top.type, ssa_argn(Policy::op()) > 2);
    }

    rpn_stack.pop(2);
    rpn_stack.push(std::move(new_top));
}

template<typename Policy>
void eval_t::do_shift(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);
    req_quantity(token, lhs, rhs);

    if(rhs.type.name() == TYPE_INT)
        throwing_cast<Policy::D>(rhs, { TYPE_U }, true);
    else if(rhs.type.name() != TYPE_U)
        compiler_error(rhs.pstring, fmt("Ride-hand side of operator % must be type U or Int.", 
                                        token_string(token.type)));

    type_t const result_type = lhs.type;
    assert(is_arithmetic(result_type.name()));

    rpn_value_t new_top =
    {
        .category = RVAL, 
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

    if(is_interpret(Policy::D) || (Policy::D == COMPILE && lhs.is_ct() && rhs.is_ct()))
    {
        assert(is_masked(lhs.fixed(), lhs.type.name()));
        assert(is_masked(rhs.fixed(), rhs.type.name()));

        fixed_t result = { Policy::interpret(lhs.s(), rhs.whole()) };
        result.value &= numeric_bitmask(result_type.name());
        new_top.sval = make_sval(ssa_value_t(result, result_type.name()));
    }
    else if(Policy::D == COMPILE)
        return compile_binary_operator(rpn_stack, Policy::op(), result_type);

    rpn_stack.pop(2);
    rpn_stack.push(std::move(new_top));
}

template<typename Policy>
void eval_t::do_assign_arith(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_stack.tuck(rpn_stack.peek(1), 1);
    throwing_cast<Policy::D>(rpn_stack.peek(0), rpn_stack.peek(1).type, true);
    do_arith<Policy>(rpn_stack, token);
    do_assign<Policy::D>(rpn_stack, token);
}

template<typename Policy>
void eval_t::do_assign_shift(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_stack.tuck(rpn_stack.peek(1), 1);
    do_shift<Policy>(rpn_stack, token);
    do_assign<Policy::D>(rpn_stack, token);
}

template<typename Policy>
void eval_t::do_logical_begin(rpn_stack_t& rpn_stack, token_t const*& token)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast<Policy::D>(top, { TYPE_BOOL }, true);

    if(is_check(Policy::D))
        rpn_stack.pop(1);
    else if(is_interpret(Policy::D))
    {
        if(bool(top.fixed()) == (Policy::logical_token() == TOK_logical_or))
        {
            for(int left = 1; left; --left)
            while(token->type != Policy::end_logical_token())
            {
                assert(token->type);
                ++token;
                if(token->type == Policy::logical_token())
                    ++left;
            }
        }
        else
            rpn_stack.pop(1);
    }
    else if(Policy::D == COMPILE)
    {
        cfg_ht const branch_node = builder.cfg;
        cfg_exits_with_branch(top.ssa());
        builder.logical_stack.push_back({ branch_node, top.pstring });
        rpn_stack.pop();

        cfg_ht const long_cut = insert_cfg(true);
        branch_node->build_set_output(Policy::logical_token() != TOK_logical_or, long_cut);
        builder.cfg = long_cut;
    }
}

template<typename Policy>
void eval_t::do_logical_end(rpn_stack_t& rpn_stack)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast<Policy::D>(top, { TYPE_BOOL }, true);

    if(Policy::D == COMPILE)
    {
        logical_data_t logical = builder.logical_stack.back();
        builder.logical_stack.pop_back();

        cfg_ht const merge_node = insert_cfg(true);
        cfg_exits_with_jump();
        builder.cfg->build_set_output(0, merge_node);
        logical.branch_node->build_set_output(Policy::logical_token() == TOK_logical_or, merge_node);

        top.ssa() = merge_node->emplace_ssa(
            SSA_phi, TYPE_BOOL, top.ssa(), ssa_value_t(Policy::logical_token() == TOK_logical_or, TYPE_BOOL));
        top.pstring = concat(logical.lhs_pstring, top.pstring);
        top.category = RVAL;

        builder.cfg = merge_node;
    }
}

template<eval_t::do_t D>
void eval_t::force_truncate(rpn_value_t& rpn_value, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(rpn_value.type));
    assert(!is_ct(to_type));
    assert(is_arithmetic(to_type.name()) && is_arithmetic(rpn_value.type.name()));

    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .type = to_type, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(is_interpret(D) || (D == COMPILE && rpn_value.is_ct()))
        new_rpn.sval = make_sval(ssa_value_t(mask_numeric(rpn_value.fixed(), to_type.name()), to_type.name()));
    else if(D == COMPILE)
        new_rpn.sval = make_sval(builder.cfg->emplace_ssa(SSA_cast, to_type, rpn_value.ssa()));

    rpn_value = std::move(new_rpn);
}

template<eval_t::do_t D>
void eval_t::force_promote(rpn_value_t& rpn_value, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(rpn_value.type));
    assert(is_arithmetic(to_type.name()) && is_arithmetic(rpn_value.type.name()));

    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .type = to_type, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(is_interpret(D) || (D == COMPILE && rpn_value.is_ct()))
        new_rpn.sval = make_sval(ssa_value_t(mask_numeric({ rpn_value.s() }, to_type.name()), to_type.name()));
    else if(D == COMPILE)
    {
        if(is_ct(to_type))
            compiler_error(rpn_value.pstring, fmt("Cannot promote type % to type % at runtime.", rpn_value.type, to_type));
        new_rpn.sval = make_sval(builder.cfg->emplace_ssa(SSA_cast, to_type, rpn_value.ssa()));
    }

    rpn_value = std::move(new_rpn);
}

template<eval_t::do_t D>
void eval_t::force_intify_ptr(rpn_value_t& rpn_value, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(rpn_value.type));
    assert(!is_ct(to_type));
    assert(is_arithmetic(to_type.name()) && is_ptr(rpn_value.type.name()));

    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .type = to_type, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(is_interpret(D))
    {
        std::array<token_t, 2> const tokens = 
        {{
             { .type = TOK_cast_argn, .pstring = cast_pstring, .value = 1 },
             token_t::make_ptr(TOK_cast_type, cast_pstring, type_t::new_type(to_type))
        }};

        new_rpn.sval = prep_lt<D>(rpn_value, &*tokens.begin(), &*tokens.end());
    }
    else if(D == COMPILE)
    {
        ssa_value_t first_cast = rpn_value.ssa(0);

        if(to_type.name() != TYPE_U20)
            first_cast = builder.cfg->emplace_ssa(SSA_cast, TYPE_U20, first_cast);

        new_rpn.sval = make_sval(builder.cfg->emplace_ssa(SSA_cast, to_type, first_cast));
    }

    rpn_value = std::move(new_rpn);
}

template<eval_t::do_t D>
void eval_t::force_ptrify_int(rpn_value_t& rpn_value, rpn_value_t const* bank, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(rpn_value.type));
    assert(!is_ct(to_type));
    assert(is_ptr(to_type.name()) && is_arithmetic(rpn_value.type.name()));

    assert((to_type.name() == TYPE_BANKED_PTR) == !!bank);

    throwing_cast<D>(rpn_value, TYPE_U20, true, pstring);
    if(bank)
        throwing_cast<D>(rpn_value, TYPE_U, true, pstring);

    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .type = to_type, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(is_interpret(D))
    {
        assert(rpn_value.sval.size() == 1);
        new_rpn.sval.push_back(rpn_value.sval[0]);

        if(bank)
        {
            assert(bank->sval.size() == 1);
            new_rpn.sval.push_back(bank->sval[0]);
        }
    }
    else if(D == COMPILE)
    {
        ssa_value_t first_cast = rpn_value.ssa();

        type_t unbanked_type = to_type.set_banked(false);
        new_rpn.sval = make_sval(builder.cfg->emplace_ssa(SSA_cast, unbanked_type, first_cast));

        if(bank)
        {
            assert(bank->sval.size() == 1);
            new_rpn.sval.push_back(bank->sval[0]);
        }
    }

    rpn_value = std::move(new_rpn);
}

template<eval_t::do_t D>
void eval_t::force_convert_int(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    assert(rpn_value.type.name() == TYPE_INT);
    assert(is_arithmetic(to_type.name()) && is_arithmetic(rpn_value.type.name()));

    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .type = to_type, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(!is_check(D))
    {
        fixed_t const masked = mask_numeric(rpn_value.fixed(), to_type.name());

        if(implicit && to_signed(masked.value, to_type.name()) != rpn_value.s())
        {
            throw compiler_error_t(
                fmt_error(rpn_value.pstring, fmt(
                    "Int value of % cannot be represented in type %. (Implicit type conversion)", 
                    to_double(fixed_t{ rpn_value.s() }), to_type))
                + fmt_note("Add an explicit cast operator to override.")
                );
        }

        new_rpn.sval = make_sval(ssa_value_t(masked, to_type.name()));
    }

    rpn_value = std::move(new_rpn);
}

template<eval_t::do_t D>
void eval_t::force_round_real(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    assert(rpn_value.type.name() == TYPE_REAL);
    assert(is_arithmetic(to_type.name()));

    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .type = to_type, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(!is_check(D))
    {
        fixed_sint_t const original = to_signed(rpn_value.u(), TYPE_REAL);
        fixed_uint_t value = rpn_value.u();
        fixed_uint_t const mask = numeric_bitmask(to_type.name());
        if(fixed_uint_t z = builtin::ctz(mask))
            value += (1ull << (z - 1)) & value;
        value &= mask;

        if(implicit)
        {
            fixed_uint_t const supermask = ::supermask(numeric_bitmask(to_type.name()));
            if(static_cast<fixed_sint_t>(original & supermask) != to_signed(original & mask, to_type.name()))
            {
                throw compiler_error_t(
                    fmt_error(rpn_value.pstring, fmt(
                        "Num value of % doesn't fit in type %. (Implicit type conversion)", 
                        to_double(fixed_t{original}), to_type))
                    + fmt_note("Add an explicit cast operator to override.")
                    );
            }
        }

        assert(is_masked({value}, to_type.name()));

        new_rpn.sval = make_sval(ssa_value_t(fixed_t{ value }, to_type.name()));
        assert(new_rpn.u() == value);
    }

    rpn_value = std::move(new_rpn);
}


// This is used to implement the other cast functions.
template<eval_t::do_t D>
void eval_t::force_boolify(rpn_value_t& rpn_value, pstring_t cast_pstring)
{
    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .time = (D == COMPILE) ? RT : CT, // TODO: what is this for?
        .type = { TYPE_BOOL }, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(is_interpret(D) || (D == COMPILE && rpn_value.is_ct()))
    {
        if(is_arithmetic(rpn_value.type.name()))
            new_rpn.sval = make_sval(ssa_value_t(boolify(rpn_value.fixed()), TYPE_BOOL));
    }
    else if(D == COMPILE)
    {
        new_rpn.sval = make_sval(builder.cfg->emplace_ssa(
            SSA_not_eq, TYPE_BOOL, rpn_value.ssa(), ssa_value_t(0u, rpn_value.type.name())));
    }

    rpn_value = std::move(new_rpn);
}

template<eval_t::do_t D>
bool eval_t::cast(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    switch(can_cast(rpn_value.type, to_type, implicit))
    {
    default: assert(false);
    case CAST_FAIL: 
        return false;
    case CAST_NOP:
        rpn_value.type = to_type;
        rpn_value.category = RVAL;
        return true;
    case CAST_PROMOTE:
        force_promote<D>(rpn_value, to_type, cast_pstring);
        return true;
    case CAST_TRUNCATE:
        force_truncate<D>(rpn_value, to_type, cast_pstring);
        return true;
    case CAST_BOOLIFY:
        force_boolify<D>(rpn_value, cast_pstring);
        return true;
    case CAST_CONVERT_INT:
        force_convert_int<D>(rpn_value, to_type, implicit, cast_pstring);
        return true;
    case CAST_ROUND_REAL:
        force_round_real<D>(rpn_value, to_type, implicit, cast_pstring);
        return true;
    case CAST_INTIFY_PTR:
        force_intify_ptr<D>(rpn_value, to_type, cast_pstring);
        return true;
    }
}

template<eval_t::do_t D>
void eval_t::throwing_cast(rpn_value_t& rpn_value, type_t to_type, bool implicit, pstring_t cast_pstring)
{
    if(!cast<D>(rpn_value, to_type, implicit, cast_pstring))
    {
        compiler_error(rpn_value.pstring, fmt(
            "Unable to perform % type cast from % to %.", 
            (implicit ? "implicit": "explicit"), rpn_value.type, to_type));
    }
}
// Converts multiple values at once, but only if all casts are valid.
// On success, -1 is returned and 'val_begin' to 'val_end' may be modified to their casted type.
// On failure, an andex into 'begin' is return, with the failed cast.
template<eval_t::do_t D>
int eval_t::cast_args(pstring_t pstring, rpn_value_t* begin, 
                      rpn_value_t* end, type_t const* type_begin, bool implicit)
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

    if(ssa_value_t lookup = from_variant(block_data.vars[var_i][member], member_type(var_types[var_i], member)))
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
                throw var_lookup_error_t();
            case 1:
                return var_lookup(cfg_node->input(0), var_i, member);
            default:
                ssa_ht const phi = cfg_node->emplace_ssa(SSA_phi, var_types[var_i]);
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
            if(block_data.label_name.size && var_i < num_locals())
            {
                pstring_t var_name = fn->def().local_vars[var_i].name;
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
        ssa_ht const phi = cfg_node->emplace_ssa(SSA_phi, var_types[var_i]);
        block_data.vars[var_i][member] = phi;
        block_data.unsealed_phis[var_i][member] = phi;
        assert(phi);
        return phi;
    }
}

sval_t eval_t::var_lookup(cfg_ht cfg_node, unsigned var_i)
{
    block_d& block_data = cfg_node.data<block_d>();

    assert(var_i < block_data.vars.size());

    sval_t sval(block_data.vars[var_i].size());

    assert(sval.size() == num_members(var_types[var_i]));

    for(unsigned member = 0; member < sval.size(); ++member)
    {
        assert(member < block_data.vars[var_i].size());
        sval[member] = var_lookup(cfg_node, var_i, member);
    }

    return sval;
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

ssa_value_t eval_t::from_variant(ct_variant_t const& v, type_t type)
{
    if(ssa_value_t const* value = std::get_if<ssa_value_t>(&v))
       return *value;
    else if(ct_array_t const* array = std::get_if<ct_array_t>(&v))
    {
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
    else if(expr_vec_t const* vec = std::get_if<expr_vec_t>(&v))
        return locator_t::lt_expr(alloc_lt_value(type, *vec));
    return {};
}

ssa_value_array_t eval_t::from_sval(sval_t const& sval, type_t type)
{
    ssa_value_array_t array;
    array.reserve(sval.size());

    for(unsigned i = 0; i < sval.size(); ++i)
        array.push_back(from_variant(sval[i], member_type(type, i)));

    return array;
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

static token_t _make_token(rpn_value_t const& rpn)
{
    if(rpn.is_ct())
    {
        if(rpn.type == TYPE_INT)
            return { .type = TOK_Int, .pstring = rpn.pstring, .value = rpn.s() };
        if(rpn.type == TYPE_REAL)
            return { .type = TOK_Real, .pstring = rpn.pstring, .value = rpn.s() };
    }

    token_t tok = { .type = TOK_spair, .pstring = rpn.pstring };
    tok.set_ptr(eternal_emplace<spair_t>(rpn.sval, rpn.type));
    return tok;
}

static void _expr_vec_append(expr_vec_t& vec, rpn_value_t const& rpn)
{
    expr_vec_t const* sub;
    if(rpn.sval.size() == 1 && (sub = std::get_if<expr_vec_t>(&rpn.sval[0])))
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

template<eval_t::do_t D>
void eval_t::make_lt(rpn_stack_t& rpn_stack, unsigned argn,
                     token_t const* op_begin, token_t const* op_end)
{
    expr_vec_t vec = _make_expr_vec(rpn_stack, argn);
    vec.insert(vec.end(), op_begin, op_end);

    // Use CHECK_TYPES_SYNTAX to update the stack:
    token_t const* token = do_token<CHECK_TYPES_SYNTAX>(rpn_stack, op_begin);
    assert(token == op_end);

    // Now add our expr_vec:
    rpn_stack.peek(0).sval = { vec };
}

template<eval_t::do_t D>
void eval_t::make_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op)
{
    make_lt<D>(rpn_stack, argn, &op, &op + 1);
}

template<eval_t::do_t D>
sval_t eval_t::prep_lt(rpn_value_t const& rpn_value, token_t const* op_begin, token_t const* op_end)
{
    expr_vec_t vec;
    _expr_vec_append(vec, rpn_value);
    vec.insert(vec.end(), op_begin, op_end);

    return { vec };
}

template<eval_t::do_t D>
sval_t eval_t::prep_lt(rpn_value_t const& rpn_value, token_t const& token)
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
                // Convert the sval to SSA values:
                for(unsigned j = 0; j < argn; ++j)
                {
                    rpn_value_t& rpn = rpn_stack.peek(j);
                    for(unsigned k = 0; k < rpn.sval.size(); ++k)
                        rpn.sval[k] = from_variant(rpn.sval[k], member_type(rpn.type, k));
                }
            }

            return false;
        }
    }

    if(has_lt_arg)
        make_lt<D>(rpn_stack, argn, op_begin, op_end);

    return has_lt_arg;
}

template<eval_t::do_t D>
bool eval_t::handle_lt(rpn_stack_t& rpn_stack, unsigned argn, token_t const& op)
{
    return handle_lt<D>(rpn_stack, argn, &op, &op + 1);
}
