#include "eval.hpp"

#include <chrono>

#include <boost/container/small_vector.hpp>

#include "alloca.hpp"
#include "cval.hpp"
#include "decl.hpp"
#include "globals.hpp"
#include "file.hpp"
#include "options.hpp"
#include "ir.hpp"
#include "rpn.hpp"
#include "stmt.hpp"

namespace sc = std::chrono;
namespace bc = boost::container;

// Data associated with each block node, to be used when making IRs.
struct block_d
{
    // Variables are mapped to a range in this order:
    // 1) local variables
    // 2) global variables
    // 3) global variable sets
    // The following sets use this order.

    // An array of size 'num_vars()'
    // Keeps track of which ssa node a var refers to.
    // A handle of {0} means the local var isn't in the block.
    ssa_value_t* fn_vars = nullptr;

    // An array of size 'num_vars()'
    // Phi nodes in the block which have yet to be sealed.
    ssa_value_t* unsealed_phis = nullptr;

    // Only used for labels.
    pstring_t label_name = {};

    // A CFG node is sealed when all its predecessors are set.
    constexpr bool sealed() const { return unsealed_phis == nullptr; }
};

class eval_t
{
private:
    pstring_t pstring = {};
    fn_t const* fn = nullptr;
    stmt_t const* stmt = nullptr;
    ir_t* ir = nullptr;
    bc::small_vector<sval_t, 8> interpret_locals;
    bc::small_vector<type_t, 8> local_types;

    // TODO
    //bc::small_vector<sval_t, 8> compile_locals;

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

        bc::small_vector<ssa_value_t, 8> return_values;
        bc::small_vector<cfg_ht, 8> return_jumps;

        array_pool_t<ssa_value_t> pool;

        void clear()
        {
            cfg = {};

            logical_stack.clear();

            break_stack.clear();
            continue_stack.clear();
            
            return_values.clear();
            return_jumps.clear();

            pool.clear();
        }
    };

    static thread_local ir_builder_t builder;
public:
    spair_t final_result;

    enum do_t
    {
        CHECK,        // Resolves types, but not values.
        INTERPRET_CE, // Like INTERPRET, but can't read/write locals.
        INTERPRET,    // Calculates values at compile time.
        COMPILE       // Generates the SSA IR.
    };

    static constexpr bool is_interpret(do_t d) { return d == INTERPRET_CE || d == INTERPRET; }

    template<do_t Do>
    struct do_wrapper_t { static constexpr auto D = Do; };

    template<do_t D>
    eval_t(do_wrapper_t<D>, pstring_t pstring, token_t const* expr, type_t expected_type = TYPE_VOID);

    template<do_t D>
    eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const& fn, sval_t const* args);

    struct access_t
    {
        type_t type = {};
        unsigned member = 0;
        ssa_value_t index = {};
    };

    /* TODO: remove
    template<do_t D>
    /sval_t to_sval(rpn_value_t const& rpn_value) const;
    sval_t const& root_sval(rpn_value_t const& rpn_value) const;
    type_t root_type(rpn_value_t const& rpn_value) const;
    ssa_value_t local_leaf(rpn_value_t const& rpn_value, access_t a) const;
    */

    void check_time();

    template<do_t D>
    void interpret_stmts();

    cfg_ht compile_block(cfg_ht cfg_node);
    cfg_ht compile_expr(rpn_stack_t& rpn_stack, cfg_ht cfg_node, token_t const* expr);

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

    // Counts locals and gvars. Only usable during ir building.
    std::size_t num_vars() const { assert(ir); return num_locals() + ir->gvar_loc_manager.num_unique_locators(); }

    type_t var_i_type(unsigned var_i) const;
    void init_sval(access_t a, sval_t& sval);
    access_t access(rpn_value_t const& rpn_value) const;
    ssa_value_t const& get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index) const;
    ssa_value_t& get_local(pstring_t pstring, unsigned var_i, unsigned member, unsigned index);

    ///////////////////////
    // compiler-specific //
    ///////////////////////

    unsigned to_var_i(gvar_loc_manager_t::index_t index) const { return index.value + num_locals(); }

    // Block and local variable functions
    void seal_block(block_d& block_data);
    void fill_phi_args(ssa_ht phi, unsigned var_i);
    sval_t var_lookup(cfg_ht node, unsigned var_i);

    cfg_ht insert_cfg(bool seal, pstring_t label_name = {});
    void cfg_exits_with_jump(cfg_node_t& node);
    void cfg_exits_with_branch(cfg_node_t& node, ssa_value_t condition);
    cfg_ht compile_goto(cfg_ht branch_node);

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

template<eval_t::do_t D>
eval_t::eval_t(do_wrapper_t<D>, pstring_t pstring, token_t const* expr, type_t expected_type)
: pstring(pstring)
, start_time(clock::now())
{
    do_expr_result<D>(expr, expected_type);
}

template<eval_t::do_t D>
eval_t::eval_t(do_wrapper_t<D>, pstring_t pstring, fn_t const& fn_ref, sval_t const* args)
: pstring(pstring)
, fn(&fn_ref)
, stmt(fn_ref.def().stmts.data())
, start_time(clock::now())
{
    // TODO
    interpret_locals.resize(fn->def().local_vars.size());
    local_types.resize(fn->def().local_vars.size());

    unsigned const argn = fn->type().num_params();
    for(unsigned i = 0; i < argn; ++i)
    {
        local_types[i] = ::dethunkify(fn->def().local_vars[i].src_type, this);
        interpret_locals[i] = args[i];
    }

    if(D == COMPILE)
    {
        /*
        assert(false);
        assert(ir);

        builder.clear(); // TODO: make sure this isn't called in recursion
        ir->gvar_loc_manager.init(fn->handle());

        ir->root = insert_cfg(true);

        ssa_ht const entry = ir->root->emplace_ssa(SSA_entry, TYPE_VOID);
        entry->append_daisy();

        // Insert nodes for the arguments
        for(unsigned i = 0; i < fn->def().num_params; ++i)
        {
            ir->root.data<block_d>().fn_vars[i] = ir->root->emplace_ssa(
                SSA_read_global, fn->def().local_vars[i].type, entry, 
                locator_t::arg(fn->handle(), i, 0));
        }

        // Insert nodes for gvar reads
        ir->gvar_loc_manager.for_each_locator([&](locator_t loc, gvar_loc_manager_t::index_t i)
        {
            ir->root.data<block_d>().fn_vars[to_var_i(i)] = 
                ir->root->emplace_ssa(
                    SSA_read_global, ir->gvar_loc_manager.type(i), 
                    entry, loc);
        });

        // Create all of the SSA graph, minus the exit node:
        cfg_ht const end = compile_block(ir->root);
        cfg_exits_with_jump(*end);

        // Now create the exit block.
        // All return statements create a jump, which will jump to the exit node.
        type_t const return_type = fn->type().return_type();
        if(return_type != TYPE_VOID)
            builder.return_values.push_back(
                end->emplace_ssa(SSA_uninitialized, return_type));

        ir->exit = insert_cfg(true);

        for(cfg_ht node : builder.return_jumps)
            node->build_set_output(0, ir->exit);
        end->build_set_output(0, ir->exit);

        // Write all globals at the exit:
        std::vector<ssa_value_t> return_inputs;
        return_inputs.reserve(ir->gvar_loc_manager.num_unique_locators() * 2);

        ir->gvar_loc_manager.for_each_locator([&](locator_t loc, gvar_loc_manager_t::index_t i)
        {
            return_inputs.push_back(var_lookup(ir->exit, to_var_i(i)));
            return_inputs.push_back(loc);
        });

        ssa_ht ret = ir->exit->emplace_ssa(SSA_return, TYPE_VOID);

        // Append the return value, if it exists:
        if(return_type != TYPE_VOID)
        {
            ssa_ht phi = ir->exit->emplace_ssa(SSA_phi, return_type);
            phi->assign_input(&*builder.return_values.begin(), &*builder.return_values.end());
            return_inputs.push_back(phi);
            return_inputs.push_back(locator_t::ret(fn->handle()));
        }

        assert(return_inputs.size() % 2 == 0);
        ret->assign_input(&*return_inputs.begin(), &*return_inputs.end());
        ret->append_daisy();

#ifndef NDEBUG
        for(cfg_ht h = ir->cfg_begin(); h; ++h)
            assert(h.data<block_d>().sealed());
#endif
        */
    }
    else
        interpret_stmts<D>();
}

template<eval_t::do_t D>
void eval_t::do_expr_result(token_t const* expr, type_t expected_type)
{
    rpn_stack_t rpn_stack;
    do_expr<D>(rpn_stack, expr);

    if(expected_type.name() != TYPE_VOID)
        throwing_cast<D>(rpn_stack.only1(), expected_type, true);
    if(is_interpret(D))
        final_result.value = rpn_stack.only1().sval;

    final_result.type = rpn_stack.only1().type;
}

/* TODO: remove?
type_t eval_t::var_i_type(unsigned var_i) const
{
    assert(var_i < local_types.size());
    return local_types[var_i];
}
*/

/*
sval_t const& eval_t::root_sval(rpn_value_t const& rpn_value) const
{
    if(rpn_value.value.is_locator())
    {
        if(rpn_value.value.locator().lclass() == LOC_GLOBAL_CONST)
            return rpn_value.value.locator().const_()->sval();
        if(rpn_value.value.locator().lclass() == LOC_LOCAL_CONST)
            assert(false); // TODO!!
    }
    assert(rpn_value.var_i < interpret_locals.size());
    return interpret_locals[rpn_value.var_i];
}


type_t eval_t::root_type(rpn_value_t const& rpn_value) const
{
    if(rpn_value.value.is_locator())
    {
        if(rpn_value.value.locator().lclass() == LOC_GLOBAL_CONST)
            return rpn_value.value.locator().const_()->type();
        if(rpn_value.value.locator().lclass() == LOC_LOCAL_CONST)
            assert(false); // TODO!!
    }
    assert(rpn_value.var_i < local_types.size());
    return local_types[rpn_value.var_i];
}
*/

/*  TODO
ssa_value_t eval_t::local_leaf(rpn_value_t const& rpn_value, access_t a) const
{
    assert(rpn_value.var_i < local_types.size());
    assert(a.member < interpret_locals[rpn_value.var_i].size());

    sval_t const& sval = root_sval(rpn_value);

    if(a.member >= sval.size()) // Can happen during INTERPRET_CE
        compiler_error(rpn_value.pstring, "Unable to access during constant evaluation. ");

    ssa_value_t array = sval[a.member];

    if((unsigned)a.index >= array.size())
    {
        compiler_error(rpn_value.pstring, 
                       fmt("Array index is out of bounds. (index of % >= size of %)", 
                       a.index, array.size()));
    }

    return array[a.index];
}

auto eval_t::access(rpn_value_t const& rpn_value) const -> access_t
{
    access_t a = { .type = root_type(rpn_value) };
    assert(a.index < 0);
    assert(!is_thunk(a.type.name()));

    for(unsigned m = 0;;) // 'm' will track the member
    {
        if(a.type.name() == TYPE_ARRAY)
        {
            if(!rpn_value.index)
            {
                assert(m == rpn_value.members.size());
                return a;
            }

            assert(!a.index);

            a.index = rpn_value.index;
            a.type = a.type.elem_type();
        }
        else if(m == rpn_value.members.size())
            return a;
        else
        {
            assert(a.type.name() == TYPE_STRUCT);

            struct_t const& s = a.type.struct_();
            a.member += s.member(rpn_value.members[m]);
            a.type = s.field(rpn_value.members[m]).type;

            ++m;
        }
    }
}

template<eval_t::do_t D>
sval_t eval_t::to_sval(rpn_value_t const& rpn_value) const
{
    if(is_aggregate(rpn_value.type.name()))
    {
        auto a = access(rpn_value);

        sval_t sval(num_members(a.type));

        sval_t const& from = root_sval(rpn_value);

        if(!a.index)
            for(unsigned i = 0; i < sval.size(); ++i)
                sval[i] = from[a.member + i];
        else
        {
            if(is_interpret(D))
            {
                if(!a.index.is_num())
                    compiler_error(rpn_value.pstring, "Array index must be known at compile time.");

                unsigned const index = a.index.whole();

                for(unsigned i = 0; i < sval.size(); ++i)
                {
                    ct_array_t const* array = from[a.member + i].ct_array();
                    if(!array)
                        compiler_error(rpn_value.pstring, "Array must be known at compile time.");
                    sval[i] = array[index];
                }
            }
            else if(D == COMPILE)
            {
                type_t const rt = root_type(rpn_value);

                for(unsigned i = 0; i < sval.size(); ++i)
                {
                    type_t mt = member_type(rt, i);

                    if(mt.name() == TYPE_ARRAY)
                    {
                        assert(a.index);
                        sval[i] = builder.cfg->emplace_ssa(
                            SSA_read_array, mt.elem_type(), from[a.member + i], locator_t::none(), a.index);
                    }
                    else
                    {
                        assert(!a.index);
                        sval[i] = from[a.member + i];
                    }

                }
            }
        }

        return sval;
    }
    else
        return { rpn_value.value };
}
*/

// TODO
/*
void eval_t::init_sval(access_t a, sval_t& sval)
{
    if(a.type.name() == TYPE_STRUCT)
    {
        struct_t const& s = a.type.struct_();
        for(unsigned i = 0; i < s.fields().size(); ++i)
            init_sval({ s.field(i).type, a.member + s.member(i) , a.index }, sval);
    }
    else if(a.type.name() == TYPE_ARRAY)
        init_sval({ a.type.elem_type(), a.member, a.type.size() }, sval);
    else
    {
        assert(a.member < sval.size());
        a.index = std::max(a.index, 1);
        sval[a.member].resize(a.index);
    }
}
*/

void eval_t::check_time()
{
    auto elapsed = clock::now() - start_time;
    if(compiler_options().time_limit > 0)
    {
        if(elapsed > sc::milliseconds(compiler_options().time_limit))
        {
            file_contents_t file(this->pstring.file_i);
            throw out_of_time_t(
                fmt_error(file, this->pstring, "Ran out of time executing expression.")
                + fmt_note("Computation is likely divergent.\n")
                + fmt_note(fmt("Use compiler flag --timelimit 0 to ignore this error.\n", compiler_options().time_limit))
                );
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

        // Temporary locals can be allocated in 'do_expr'; this resets that.
        // TODO: remove
        //interpret_locals.resize(num_locals());
        //local_types.resize(num_locals());

        switch(stmt->name)
        {
        default: // Handles var inits
            if(is_var_init(stmt->name))
            {
                if(D == INTERPRET_CE)
                    compiler_error(stmt->pstring, "Expression cannot be evaluated at compile time.");

                unsigned const var_i = ::get_local_var_i(stmt->name);

                // Prepare the type.
                assert(var_i < local_types.size());
                if(local_types[var_i].name() == TYPE_VOID)
                    local_types[var_i] = dethunkify(fn->def().local_vars[var_i].src_type, this);

                if(stmt->expr)
                {
                    do_expr<D>(rpn_stack, stmt->expr);
                    throwing_cast<D>(rpn_stack.peek(0), local_types[var_i], true);

                    if(D == INTERPRET)
                    {
                        assert(interpret_locals[var_i].empty());
                        interpret_locals[var_i] = std::move(rpn_stack.only1().sval);
                        rpn_stack.pop(1);
                    }
                }
                else if(D == INTERPRET)
                {
                    unsigned const num = num_members(local_types[var_i]);
                    assert(num > 0);
                    interpret_locals[var_i].resize(num);

                    for(unsigned i = 0; i < num; ++i)
                    {
                        type_t const mt = member_type(local_types[var_i], i);
                        if(mt.name() != TYPE_ARRAY)
                            continue;
                        interpret_locals[var_i][i] = make_ct_array(mt.array_length());
                    }

                    /* TODO: remove
                    //type_t const t = local_types[var_i];
                    //unsigned const num = 

                    interpret_arrays[var_i] = std::make_new<ct_array_t>(num);

                    for(unsigned i = 0; i < num; ++i)
                    {
                        type_t const m = member_type(t, i);
                        if(m.name() != TYPE_ARRAY)
                            continue;
                        (*interpret_arrays[var_i])[i].resize(m.size());
                    }
                    */
                }

                ++stmt;
            }
            else
                compiler_error(stmt->pstring, "Statement cannot appear in constant evaluation.");
            break;

        case STMT_EXPR:
        case STMT_FOR_EFFECT:
            do_expr<D>(rpn_stack, stmt->expr);
            ++stmt;
            break;

        case STMT_DO:
        case STMT_END_BLOCK:
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
            type_t return_type = fn->type().return_type();
            if(return_type.name() != TYPE_VOID)
            {
                compiler_error(stmt->pstring, fmt(
                    "Interpreter reached end of function without returning %.", return_type));
            }
            return;
        }
    }
    assert(false);
}

cfg_ht eval_t::compile_block(cfg_ht cfg_node)
{
#if 0
    while(true)
    switch(stmt->name)
    {
    default: // Handles var inits
        if(is_var_init(stmt->name))
        {
            unsigned const var_i = get_local_var_i(stmt->name);

            ssa_value_t value;
            if(stmt->expr)
            {
                cfg_node = compile_expr(cfg_node, stmt->expr);
                throwing_cast(*cfg_node, rpn_stack.peek(0), 
                              fn.def().local_vars[var_i].type);
                value = rpn_stack.only1().value;
            }
            else
            {
                value = cfg_node->emplace_ssa(
                    SSA_uninitialized, fn.def().local_vars[var_i].type);
            }

            cfg_node.data<block_d>().fn_vars[var_i] = value;
            ++stmt;
        }
        else
            throw std::runtime_error("Unimplemented stmt.");
        break;
    case STMT_END_BLOCK:
        ++stmt;
        return cfg_node;

    case STMT_EXPR:
        cfg_node = compile_expr(cfg_node, stmt->expr);
        ++stmt;
        break;

    case STMT_IF:
        {
            // Branch the active node.
            cfg_ht branch = compile_expr(cfg_node, stmt->expr);
            ++stmt;
            throwing_cast(*branch, rpn_stack.only1(), {TYPE_BOOL});
            exits_with_branch(*branch, rpn_stack.only1().value);

            // Create new cfg_node for the 'true' branch.
            cfg_ht begin_true = insert_cfg(true);
            branch->build_set_output(1, begin_true);
            cfg_ht end_true = compile_block(begin_true);
            exits_with_jump(*end_true);

            if(stmt->name == STMT_ELSE)
            {
                // Create new block for the 'false' branch.
                ++stmt;
                cfg_ht begin_false = insert_cfg(true);
                branch->build_set_output(0, begin_false);
                // repurpose 'branch' to hold end of the 'false' branch.
                // Simplifies the assignment that follows.
                branch = compile_block(begin_false);
                exits_with_jump(*branch);
            }

            // Merge the two nodes.
            cfg_node = insert_cfg(true);
            end_true->build_set_output(0, cfg_node);
            branch->build_set_output(0, cfg_node);
            break;
        }

    case STMT_WHILE:
        {
            // The loop condition will go in its own block.
            exits_with_jump(*cfg_node);
            cfg_ht begin_branch = insert_cfg(false);
            cfg_node->build_set_output(0, begin_branch);

            cfg_ht end_branch = compile_expr(begin_branch, stmt->expr);
            ++stmt;
            throwing_cast(*end_branch, rpn_stack.only1(), {TYPE_BOOL});
            exits_with_branch(*end_branch, rpn_stack.only1().value);

            continue_stack.emplace_back();
            break_stack.emplace_back();

            // Compile the body.
            cfg_ht begin_body = insert_cfg(true);
            end_branch->build_set_output(1, begin_body);
            cfg_ht end_body = compile_block(begin_body);
            exits_with_jump(*end_body);
            end_body->build_set_output(0, begin_branch);

            // All continue statements jump to branch_node.
            // TODO: properly implement continue in 'for'
            assert(false);
            for(cfg_ht node : continue_stack.back())
                node->build_set_output(0, begin_branch);
            seal_block(begin_branch.data<block_d>());

            // Create the exit node.
            cfg_node = insert_cfg(true);
            end_branch->build_set_output(0, cfg_node);
            for(cfg_ht node : break_stack.back())
                node->build_set_output(0, cfg_node);

            continue_stack.pop_back();
            break_stack.pop_back();
            break;
        }

    case STMT_DO:
        {
            ++stmt;
            continue_stack.emplace_back();
            break_stack.emplace_back();

            // Compile the loop body
            exits_with_jump(*cfg_node);
            cfg_ht begin_body = insert_cfg(false);
            cfg_node->build_set_output(0, begin_body);
            cfg_ht end_body = compile_block(begin_body);

            assert(stmt->name == STMT_WHILE);

            // The loop condition can go in its own block, which is
            // necessary to implement 'continue'.
            exits_with_jump(*end_body);
            cfg_ht begin_branch = insert_cfg(true);
            end_body->build_set_output(0, begin_branch);

            cfg_ht end_branch = compile_expr(begin_branch, stmt->expr);
            ++stmt;
            throwing_cast(*end_branch, rpn_stack.only1(), {TYPE_BOOL});
            exits_with_branch(*end_branch, rpn_stack.only1().value);

            end_branch->build_set_output(1, begin_body);
            seal_block(begin_body.data<block_d>());

            // All continue statements jump to the branch node.
            for(cfg_ht node : continue_stack.back())
                node->build_set_output(0, begin_branch);

            // Create the exit cfg_node.
            cfg_node = insert_cfg(true);
            end_branch->build_set_output(0, cfg_node);
            for(cfg_ht node : break_stack.back())
                node->build_set_output(0, cfg_node);

            continue_stack.pop_back();
            break_stack.pop_back();
            break;
        }

    case STMT_RETURN:
        {
            type_t return_type = fn.type().return_type();
            if(stmt->expr)
            {
                cfg_node = compile_expr(cfg_node, stmt->expr);
                throwing_cast(*cfg_node, rpn_stack.only1(), return_type);
                builder.return_values.push_back(rpn_stack.only1().value);
            }
            else if(return_type.name() != TYPE_VOID)
                compiler_error(stmt->pstring, fmt(
                    "Expecting return expression of type %.", return_type));
            return_jumps.push_back(cfg_node);
            cfg_node = compile_goto(cfg_node);
            ++stmt;
            break;
        }

    case STMT_BREAK:
        if(break_stack.empty())
            compiler_error(stmt->pstring, "break statement outside of loop.");
        break_stack.back().push_back(cfg_node);
        cfg_node = compile_goto(cfg_node);
        ++stmt;
        break;

    case STMT_CONTINUE:
        if(continue_stack.empty())
            compiler_error(stmt->pstring, 
                           "continue statement outside of loop.");
        continue_stack.back().push_back(cfg_node);
        cfg_node = compile_goto(cfg_node);
        ++stmt;
        break;

    case STMT_LABEL:
        {
            label_t& label = label_map[stmt];
            unsigned const use_count = stmt->use_count;
            pstring_t label_name = stmt->pstring;
            ++stmt;

            // If there's no goto to this label, just ignore it.
            if(use_count == 0)
                break;

            exits_with_jump(*cfg_node);
            label.inputs.push_back(cfg_node);

            if(use_count + 1 == label.inputs.size())
            {
                // All the gotos to this label have been compiled,
                // that means this block can be sealed immediately!
                label.node = cfg_node = insert_cfg(true, label_name);
                for(cfg_ht node : label.inputs)
                    node->build_set_output(0, label.node);
            }
            else // Otherwise, seal the node at a later time.
                label.node = cfg_node = insert_cfg(false, label_name);

            break;
        }

    case STMT_GOTO:
        {
            label_t& label = label_map[&fn.def()[stmt->link]];
            unsigned const use_count = fn.def()[stmt->link].use_count;
            assert(use_count > 0);
            ++stmt;

            // Add the jump to the label.
            label.inputs.push_back(cfg_node);
            cfg_node = compile_goto(cfg_node);

            // If this is the last goto, finish and seal the node.
            if(use_count + 1 == label.inputs.size())
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

            cfg_ht branch = cfg_node;

            cfg_ht mode = insert_cfg(true);

            cfg_node = compile_goto(cfg_node);
            branch->build_set_output(0, mode);

            compile_expr(mode, stmt->expr);

            ++stmt;
            break;
        }
        ;
    }
    assert(false);
    assert(0);
#endif
}

cfg_ht eval_t::compile_expr(rpn_stack_t& rpn_stack, cfg_ht cfg_node, token_t const* expr)
{
    rpn_stack.clear();
    builder.logical_stack.clear();

    for(token_t const* token = expr; token->type; ++token)
    {
        switch(token->type)
        {
        default:
            throw std::runtime_error(fmt("Invalid token '%' in expression.", token_name(token->type)));

#if 0
        case TOK_ident:
            rpn_stack.push({ 
                .value = var_lookup(cfg_node, token->value), 
                .category = LVAL, 
                .type = fn->def().local_vars[token->value].type, 
                .pstring = token->pstring,
                .var_i = token->value });
            break;

        case TOK_global_ident:
            {
                global_t* global = token->ptr<global_t>();
                switch(global->gclass())
                {
                default:
                    throw std::runtime_error(
                        "Unimplemented global in expression.");
                case GLOBAL_VAR:
                    {
                        unsigned const var_i = to_var_i(ir.gvar_loc_manager.index(global->handle<gvar_ht>()));
                        rpn_stack.push({ 
                            .value = var_lookup(cfg_node, var_i), 
                            .category = LVAL, 
                            .type = global->impl<gvar_t>().type(), 
                            .pstring = token->pstring,
                            .var_i = var_i });
                    }
                    break;

                case GLOBAL_FN:
                    rpn_stack.push({ 
                        .value = ssa_value_t(locator_t::fn(global->handle<fn_ht>())),
                        .category = RVAL, 
                        .type = global->impl<fn_t>().type(), 
                        .pstring = token->pstring });
                    break;
                }
            }
            break;

        case TOK_assign:
            compile_assign(*cfg_node);
            break;

        case TOK_int: // TODO
            rpn_stack.push({
                .value = token->value, 
                .category = RVAL, 
                .type = { TYPE_INT },  // TODO
                .pstring = token->pstring });
            break;

        case TOK_apply:
            {
                // TOK_apply is a psuedo token used to represent application. 
                // The token's 'value' stores the application's arity:
                std::size_t const num_args = token->value;

                // The eval stack contains the arguments to be applied.
                // Right beneath those it contains the fn value to be called.
                rpn_value_t& fn_rpn = rpn_stack.peek(num_args);

                if(fn_rpn.type.name() != TYPE_FN)
                {
                    compiler_error(fn_rpn.pstring, fmt(
                        "Expecting function type. Got %.", fn_rpn.type));
                }

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

                for(std::uint64_t arg = 0, fail_bits = cast_args(*cfg_node, 
                    fn_rpn.pstring, args, rpn_stack.past_top(), params)
                   ; fail_bits; ++arg)
                {
                    if(fail_bits & 1)
                    {
                        compiler_error(
                            args[arg].pstring, fmt(
                            "Unable to convert type % "
                            "to type % in function application.\n"
                            "Expected signature: % ",
                            args[arg].type, params[arg], fn_rpn.type));
                    }
                    fail_bits >>= 1;
                }

                // For now, only const fns are allowed.
                // In the future, fn pointers may be supported.
                assert(fn_rpn.value.is_locator());
                fn_ht fn = fn_rpn.value.locator().fn();

                // Type checks are done. Now convert the call to SSA.
                ssa_ht fn_node = insert_fn_call(cfg_node, fn, args);

                // Update the eval stack.
                rpn_value_t new_top =
                {
                    .value = fn_node,
                    .category = RVAL, 
                    .type = fn_rpn.type.return_type(), 
                    .pstring = concat(fn_rpn.pstring, token->pstring)
                };

                rpn_stack.pop(num_args + 1);
                rpn_stack.push(std::move(new_top));

                break;
            }

            // inlining:
            // - create a new 'eval_t'.
            // - run the eval_t
            // -- different enter / returns behavior
            // how to handle globals?
            // how to handle locals?
            // how to handle structs?

            // STRUCTS:
            // - represent several SSA nodes
            // - lvals: work like interpreter



        case TOK_index:
            {
                // TOK_index is a psuedo token used to array indexing. 

                // The eval stack contains the index on top.
                // Right beneath it contains the array.
                rpn_value_t& array_val = rpn_stack.peek(1);

                if(array_val.type.name() != TYPE_ARRAY)
                {
                    compiler_error(array_val.pstring, fmt(
                        "Expecting array type. Got %.", array_val.type));
                }

                rpn_value_t& array_index = rpn_stack.peek(0);

                // Array indexes are always bytes.
                throwing_cast(*cfg_node, array_index, TYPE_U);

                // Type checks are done. Now convert the index to SSA.
                ssa_ht index_node = insert_array_index(cfg_node, array_val, array_index);

                // Update the eval stack.
                array_val.value = index_node;
                array_val.category = to_indexed(array_val.category);
                array_val.type = array_val.type.elem_type();
                array_val.pstring = token->pstring;
                rpn_stack.pop(1);

                // TODO
                break;
            }

        case TOK_logical_and:
            cfg_node = compile_logical_begin(cfg_node, false);
            break;
        case TOK_logical_or:
            cfg_node = compile_logical_begin(cfg_node, true);
            break;
        case TOK_end_logical_and:
            cfg_node = compile_logical_end(cfg_node, false);
            break;
        case TOK_end_logical_or:
            cfg_node = compile_logical_end(cfg_node, true);
            break;

        case TOK_eq:
            compile_compare(*cfg_node, SSA_eq);
            break;
        case TOK_not_eq:
            compile_compare(*cfg_node, SSA_not_eq);
            break;
        case TOK_gt:
            std::swap(rpn_stack.peek(0), rpn_stack.peek(1));
            // fall-through
        case TOK_lt:
            compile_compare(*cfg_node, SSA_lt);
            break;
        case TOK_gte:
            std::swap(rpn_stack.peek(0), rpn_stack.peek(1));
            // fall-through
        case TOK_lte:
            compile_compare(*cfg_node, SSA_lte);
            break;

        case TOK_asterisk: 
            compile_arith(*cfg_node, SSA_mul);
            break;
        case TOK_fslash: 
            compile_arith(*cfg_node, SSA_div);
            break;
        case TOK_plus:
            compile_arith(*cfg_node, SSA_add, true);  
            break;
        case TOK_minus: 
            compile_arith(*cfg_node, SSA_sub, true);
            break;
        case TOK_lshift:
            compile_shift(*cfg_node, SSA_shl);  
            break;
        case TOK_rshift: 
            compile_shift(*cfg_node, SSA_shr);
            break;
        case TOK_bitwise_and: 
            compile_arith(*cfg_node, SSA_and);
            break;
        case TOK_bitwise_or:  
            compile_arith(*cfg_node, SSA_or);
            break;
        case TOK_bitwise_xor:
            compile_arith(*cfg_node, SSA_xor);
            break;

        case TOK_plus_assign:
            compile_assign_arith(*cfg_node, SSA_add, true);
            break;
        case TOK_minus_assign:
            compile_assign_arith(*cfg_node, SSA_sub, true);
            break;
        case TOK_bitwise_and_assign:
            compile_assign_arith(*cfg_node, SSA_and);
            break;
        case TOK_bitwise_or_assign:
            compile_assign_arith(*cfg_node, SSA_or);
            break;
        case TOK_bitwise_xor_assign:
            compile_assign_arith(*cfg_node, SSA_xor);
            break;
#endif
        }
    }

    assert(rpn_stack.size() == 1);
    assert(builder.logical_stack.empty());
    return cfg_node;
}

template<eval_t::do_t D>
void eval_t::do_expr(rpn_stack_t& rpn_stack, token_t const* expr)
{
    using namespace std::literals::chrono_literals;
    using S = fixed_sint_t;

    check_time();

    rpn_stack.clear(); // Reset the stack.

    for(token_t const* token = expr; token->type; ++token)
    {
        // Declare cross-label vars before switch.
        ssa_value_t common_value;
        type_t common_type;

        switch(token->type)
        {
        default:
            throw std::runtime_error(fmt("Invalid token '%' in expression.", token_string(token->type)));

        case TOK_ident:
            assert(token->value < num_locals());

            if(D == INTERPRET_CE)
                compiler_error(token->pstring, "Expression cannot be evaluated at compile time.");
            else
            {
                assert(D == CHECK || D == INTERPRET || D == COMPILE);
                assert(token->value < local_types.size());

                rpn_value_t new_top =
                {
                    .category = LVAL, 
                    .type = local_types[token->value],
                    .pstring = token->pstring,
                    .var_i = token->value,
                };

                if(D == COMPILE) 
                    new_top.sval = var_lookup(builder.cfg, token->value);
                else if(D == INTERPRET)
                {
                    if(interpret_locals[token->value].empty())
                    {
                        compiler_error(token->pstring, 
                            "Variable is invalid because goto jumped past its initialization.");
                    }

                    new_top.sval = interpret_locals[token->value];
                }

                rpn_stack.push(std::move(new_top));
            }

            break;

        case TOK_global_ident:
            {
                global_t* global = token->ptr<global_t>();
                switch(global->gclass())
                {
                default: 
                    throw std::runtime_error(
                        "Unimplemented global in expression.");

                case GLOBAL_VAR:
                    if(D == COMPILE || D == CHECK)
                    {
                        rpn_value_t new_top =
                        {
                            .category = LVAL, 
                            .type = global->impl<gvar_t>().type(),
                            .pstring = token->pstring,
                        };

                        if(D == COMPILE)
                        {
                            unsigned const var_i = to_var_i(ir->gvar_loc_manager.index(global->handle<gvar_ht>()));
                            new_top.var_i = var_i;
                            new_top.sval = var_lookup(builder.cfg, var_i);
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
                        assert(c.sval().size());

                        rpn_value_t new_top =
                        {
                            .sval = c.sval(),
                            .category = RVAL, 
                            .type = c.type(),
                            .pstring = token->pstring,
                        };

                        rpn_stack.push(std::move(new_top));
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

        case TOK_int:
            common_value.set(mask_numeric(fixed_t{ token->value }, TYPE_INT));
        push_int:
            rpn_stack.push({
                .sval = { common_value },
                .category = RVAL, 
                .type = { TYPE_INT }, 
                .pstring = token->pstring });
            break;

        case TOK_real:
            common_value.set(mask_numeric(fixed_t{ token->value }, TYPE_REAL));
            rpn_stack.push({
                .sval = { common_value },
                .category = RVAL, 
                .type = { TYPE_REAL }, 
                .pstring = token->pstring });
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
                auto const it = s.fields().find(hash);

                if(!it)
                {
                    file_contents_t file(token->pstring.file_i);
                    compiler_error(file, token->pstring, fmt(
                        "% isn't a member of %.", 
                        token->pstring.view(file.source()), s.global.name));
                }

                unsigned const field_i = it - s.fields().begin();
                unsigned const member_i = member_index(struct_val.type, field_i);
                
                if(D != CHECK)
                {
                    // Shrink the sval to only contain the specified field.
                    unsigned const size = num_members(it->second.type());
                    for(unsigned i = 0; i < size; ++i)
                        struct_val.sval[i] = std::move(struct_val.sval[i + member_i]);
                    struct_val.sval.resize(size);
                }

                struct_val.member += member_i;
                struct_val.type = it->second.type();
                struct_val.pstring = concat(struct_val.pstring, token->pstring);

                break;
            }

        case TOK_apply:
            {
                // TOK_apply is a psuedo token used to represent application. 
                // The token's 'value' stores the application's arity:
                std::size_t const num_args = token->value;

                // The eval stack contains the arguments to be applied.
                // Right beneath those it contains the fn value to be called.
                rpn_value_t& fn_rpn = rpn_stack.peek(num_args);

                if(fn_rpn.type.name() != TYPE_FN)
                {
                    compiler_error(fn_rpn.pstring, fmt(
                        "Expecting function type. Got %.", fn_rpn.type));
                }

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

                // For now, only const fns are allowed.
                // In the future, fn pointers may be supported.
                assert(fn_rpn.sval.size() == 1);
                ssa_value_t const fn_value = fn_rpn.ssa();
                assert(fn_value.is_locator());
                fn_ht const call = fn_value.locator().fn();

                pstring_t const call_pstring = concat(fn_rpn.pstring, token->pstring);

                if(call->mode && D != COMPILE)
                    compiler_error(call_pstring, "Cannot goto mode at compile time.");

                // Now do the call!

                if(D == CHECK)
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
                        sval_args[i] = args[i].sval;

                    try
                    {
                        // NOTE: call as INTERPRET, not D.
                        eval_t sub(do_wrapper_t<INTERPRET>{}, call_pstring, *call, sval_args.data());

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
                        file_contents_t file(this->pstring.file_i);
                        e.msg += fmt_note(file, this->pstring, "Backtrace:");
                        throw;
                    }
                }
                else if(D == COMPILE)
                {
                    assert(0);
                    /* TODO
                    if(is_ct(call->type().return_type()))
                        goto interpret_fn;
                    // TODO: Interpret in other situations, too.

                    bc::small_vector<ssa_value_t, 32> fn_inputs;

                    // The [0] argument holds the fn_t ptr.
                    fn_inputs.push_back(ssa_value_t(locator_t::fn(call)));
                    
                    // Prepare the input globals

                    bool const is_idep = fn->global.ideps().count(&call->global) > 0;
                    assert(is_idep || fn->mode);

                    bitset_t const* reads_set;
                    if(!call->mode || is_idep)
                        reads_set = &call->ir_reads();
                    else
                        reads_set = &call->lang_gvars();

                    ir->gvar_loc_manager.for_each_singleton([&](gvar_ht gvar, gvar_loc_manager_t::index_t i)
                    {
                        if(reads_set->test(gvar.value))
                        {
                            fn_inputs.push_back(var_lookup(cfg_node, to_var_i(i)));
                            fn_inputs.push_back(ir->gvar_loc_manager.locator(i));
                        }
                    });

                    unsigned const bs_size = impl_bitset_size<gvar_t>();
                    assert(bs_size == reads_set->size());
                    assert(bs_size == gvar_loc_manager_t::bitset_size());
                    auto* const temp_set = ALLOCA_T(bitset_uint_t, bs_size);

                    ir->gvar_loc_manager.for_each_set([&](bitset_uint_t const* gvar_set, gvar_loc_manager_t::index_t i)
                    {
                        bitset_copy(bs_size, temp_set, gvar_set);
                        bitset_and(bs_size, temp_set, reads_set->data());
                        if(!bitset_all_clear(bs_size, temp_set))
                        {
                            fn_inputs.push_back(var_lookup(cfg_node, to_var_i(i)));
                            fn_inputs.push_back(ir->gvar_loc_manager.locator(i));
                        }

                    });

                    // Prepare the arguments
                    unsigned const num_args = call->type().num_params();
                    for(unsigned i = 0; i < num_args; ++i)
                    {
                        locator_t const loc = locator_t::arg(call, i, 0);

                        // If the arg isn't used in the function, ignore it.
                        if(is_idep && call->lvars().index(loc) < 0)
                            continue;

                        fn_inputs.push_back(args[i].value);
                        fn_inputs.push_back(loc);
                    }

                    // Create the dependent node.
                    ssa_op_t const op = call->mode ? SSA_goto_mode : SSA_fn_call;
                    ssa_ht const fn_node = cfg_node->emplace_ssa(op, call->type().return_type());
                    assert(fn_inputs.size() % 2 == 1);
                    fn_node->link_append_input(&*fn_inputs.begin(), &*fn_inputs.end());
                    fn_node->append_daisy();

                    if(!call->mode)
                    {
                        // After the fn is called, read all the globals it has written to:
                        bitset_t const& writes_set = call->ir_writes();

                        ir->gvar_loc_manager.for_each_singleton([&](gvar_ht gvar, gvar_loc_manager_t::index_t i)
                        {
                            if(writes_set.test(gvar.value))
                            {
                                ssa_ht read = cfg_node->emplace_ssa(
                                    SSA_read_global, gvar->type(), fn_node, ir->gvar_loc_manager.locator(i));
                                block_d& block_data = cfg_node.data<block_d>();
                                block_data.fn_vars[to_var_i(i)] = read;
                            }
                        });

                        ir->gvar_loc_manager.for_each_set([&](bitset_uint_t const* gvar_set, gvar_loc_manager_t::index_t i)
                        {
                            bitset_copy(bs_size, temp_set, gvar_set);
                            bitset_and(bs_size, temp_set, writes_set.data());
                            if(!bitset_all_clear(bs_size, temp_set))
                            {
                                ssa_ht read = cfg_node->emplace_ssa(
                                    SSA_read_global, TYPE_VOID, fn_node, ir->gvar_loc_manager.locator(i));
                                block_d& block_data = cfg_node.data<block_d>();
                                block_data.fn_vars[to_var_i(i)] = read;
                            }
                        });
                    }

                    // Update the eval stack.
                    rpn_value_t new_top =
                    {
                        .value = fn_node,
                        .category = RVAL, 
                        .type = fn_rpn.type.return_type(), 
                        .pstring = concat(fn_rpn.pstring, token->pstring)
                    };

                    rpn_stack.pop(num_args + 1);
                    rpn_stack.push(std::move(new_top));
                */
                }

                break;
            }

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
                type_t const type = dethunkify({ *token->ptr<type_t const>(), token->pstring }, this);

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

                        if(is_interpret(D))
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
                        else if(D == COMPILE)
                        {
                            assert(false);
                        }
                    }
                    else if(type.name() == TYPE_ARRAY)
                    {
                        check_argn(type.array_length());

                        for(unsigned i = 0; i < type.array_length(); ++i)
                            throwing_cast<D>(args[i], type.elem_type(), false);

                        if(is_interpret(D))
                        {
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
                            assert(false);
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

                // The eval stack contains the index on top.
                // Right beneath it contains the array.
                rpn_value_t& array_val = rpn_stack.peek(1);

                if(array_val.type.name() != TYPE_ARRAY)
                {
                    compiler_error(array_val.pstring, fmt(
                        "Expecting array type. Got %.", array_val.type));
                }

                rpn_value_t& array_index = rpn_stack.peek(0);
                assert(array_val.sval.size() > 0);

                // Array indexes are always bytes.
                throwing_cast<D>(array_index, TYPE_U, true);

                array_val.pstring = concat(array_val.pstring, token->pstring);

                if(is_interpret(D))
                {
                    unsigned const index = array_index.whole();
                    array_val.index.set(index);
                    
                    if(index >= array_val.type.array_length())
                    {
                        compiler_error(array_index.pstring, 
                            fmt("Array index is out of bounds. (index: % >= size: %)", 
                                index, array_val.type.array_length()));
                    }

                    for(auto& v : array_val.sval)
                        v = std::get<ct_array_t>(v)[index];
                }
                else if(D == COMPILE)
                {
                    assert(false);
                }

                // Set after calling 'access':
                array_val.type = array_val.type.elem_type();
                rpn_stack.pop(1);
                break;
            }

        case TOK_sizeof_expr:
            {
                rpn_stack_t sub_stack;
                do_expr<CHECK>(sub_stack, token->ptr<token_t const>());
                common_type = sub_stack.peek(0).type;
                goto do_sizeof;
            }

        case TOK_sizeof:
            {
                common_type = dethunkify({ *token->ptr<type_t const>(), token->pstring }, this);
            do_sizeof:
                unsigned const size = common_type.size_of();

                if(size == 0)
                    compiler_error(token->pstring, fmt("Type % has no size.", common_type));

                common_value.set(size);
                goto push_int;
            }

        case TOK_len_expr:
            {
                rpn_stack_t sub_stack;
                do_expr<CHECK>(sub_stack, token->ptr<token_t const>());
                common_type = sub_stack.peek(0).type;
                goto do_len;
            }

        case TOK_len:
            {
                common_type = dethunkify({ *token->ptr<type_t const>(), token->pstring }, this);
            do_len:
                unsigned const size = common_type.array_length();

                if(size == 0)
                    compiler_error(token->pstring, fmt("Type % isn't an array.", common_type));

                common_value.set(size);
                goto push_int;
            }

        case TOK_assign:
            do_assign<D>(rpn_stack, *token);
            break;

        case TOK_logical_and:
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
            struct eq_p : do_wrapper_t<D>
            {
                static bool interpret(S lhs, S rhs) { return lhs == rhs; }
            };
            do_compare<eq_p>(rpn_stack, *token);
            break;
        case TOK_not_eq:
            struct not_eq_p : do_wrapper_t<D>
            {
                static bool interpret(S lhs, S rhs) { return lhs != rhs; }
            };
            do_compare<not_eq_p>(rpn_stack, *token);
            break;
        case TOK_gt:
            struct gt_p : do_wrapper_t<D>
            {
                static bool interpret(S lhs, S rhs) { return lhs > rhs; }
            };
            do_compare<gt_p>(rpn_stack, *token);
            break;
        case TOK_lt:
            struct lt_p : do_wrapper_t<D>
            {
                static bool interpret(S lhs, S rhs) { return lhs < rhs; }
            };
            do_compare<lt_p>(rpn_stack, *token);
            break;
        case TOK_gte:
            struct gte_p : do_wrapper_t<D>
            {
                static bool interpret(S lhs, S rhs) { return lhs >= rhs; }
            };
            do_compare<gte_p>(rpn_stack, *token);
            break;
        case TOK_lte:
            struct lte_p : do_wrapper_t<D>
            {
                static bool interpret(S lhs, S rhs) { return lhs <= rhs; }
            };
            do_compare<lte_p>(rpn_stack, *token);
            break;

        case TOK_plus:
            struct plus_p : do_wrapper_t<D>
            {
                static S interpret(S lhs, S rhs) { return lhs + rhs; }
            };
            do_arith<plus_p>(rpn_stack, *token);
            break;
        case TOK_plus_assign:
            do_assign_arith<plus_p>(rpn_stack, *token);
            break;

        case TOK_minus: 
            struct minus_p : do_wrapper_t<D>
            {
                static S interpret(S lhs, S rhs) { return lhs - rhs; }
            };
            do_arith<minus_p>(rpn_stack, *token);
            break;
        case TOK_minus_assign:
            do_assign_arith<minus_p>(rpn_stack, *token);
            break;

        case TOK_bitwise_and: 
            struct bitwise_and_p : do_wrapper_t<D>
            {
                static S interpret(S lhs, S rhs) { return lhs & rhs; }
            };
            do_arith<bitwise_and_p>(rpn_stack, *token);
            break;
        case TOK_bitwise_and_assign:
            do_assign_arith<bitwise_and_p>(rpn_stack, *token);
            break;

        case TOK_bitwise_or:  
            struct bitwise_or_p : do_wrapper_t<D>
            {
                static S interpret(S lhs, S rhs) { return lhs | rhs; }
            };
            do_arith<bitwise_or_p>(rpn_stack, *token);
            break;
        case TOK_bitwise_or_assign:
            do_assign_arith<bitwise_or_p>(rpn_stack, *token);
            break;

        case TOK_bitwise_xor:
            struct bitwise_xor_p : do_wrapper_t<D>
            {
                static S interpret(S lhs, S rhs) { return lhs ^ rhs; }
            };
            do_arith<bitwise_xor_p>(rpn_stack, *token);
            break;
        case TOK_bitwise_xor_assign:
            do_assign_arith<bitwise_xor_p>(rpn_stack, *token);
            break;

        case TOK_lshift:
            struct lshift_p : do_wrapper_t<D>
            {
                static S interpret(S lhs, std::uint8_t shift) { return lhs << shift; }
            };
            do_shift<lshift_p>(rpn_stack, *token);
            break;
        case TOK_lshift_assign:
            do_assign_shift<lshift_p>(rpn_stack, *token);
            break;

        case TOK_rshift:
            struct rshift_p : do_wrapper_t<D>
            {
                static S interpret(S lhs, std::uint8_t shift) { return lhs >> shift; }
            };
            do_shift<rshift_p>(rpn_stack, *token);
            break;
        case TOK_rshift_assign:
            do_assign_shift<rshift_p>(rpn_stack, *token);
            break;

        case TOK_unary_negate:
            {
                rpn_value_t& top = rpn_stack.peek(0);
                throwing_cast<D>(top, { TYPE_BOOL }, true);

                if(is_interpret(D))
                    top.ssa().set(unsigned(!top.whole()));
                else if(D == COMPILE)
                    assert(false);

                break;
            }

        case TOK_unary_minus:
            {
                rpn_value_t& top = rpn_stack.peek(0);
                req_quantity(*token, top);

                if(is_interpret(D))
                    top.ssa().set(mask_numeric(fixed_t{ -top.s() }, top.type.name()));
                else if(D == COMPILE)
                    assert(false);

                break;
            }

        case TOK_unary_xor:
            {
                rpn_value_t& top = rpn_stack.peek(0);
                req_quantity(*token, top);

                std::printf("top = %i\n", (int)top.s());
                std::printf("top = %i\n", (int)~top.s());

                if(is_interpret(D))
                    top.ssa().set(mask_numeric(fixed_t{ ~top.u() }, top.type.name()));
                else if(D == COMPILE)
                    assert(false);

                break;
            }
        }
    }
}

template<eval_t::do_t D>
void eval_t::do_assign(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_value_t& assignee = rpn_stack.peek(1);
    rpn_value_t& assignment = rpn_stack.peek(0);

    pstring_t const pstring = concat(assignee.pstring, assignee.pstring);

    if(assignee.category == RVAL)
        compiler_error(pstring, "Expecting lvalue on left side of assignment.");

    throwing_cast<D>(assignment, assignee.type, true);

    assert(assignee.var_i < interpret_locals.size());

    // Remap the identifier to point to the new value.
    if(D == INTERPRET)
    {
        sval_t& local = interpret_locals[assignee.var_i];

        if(assignee.index)
        {
            type_t const mt = member_type(local_types[assignee.var_i], assignee.member);
            assert(mt.name() == TYPE_ARRAY);
            assert(mt.elem_type() == assignee.type);

            unsigned const array_size = mt.array_length();
            unsigned const index = assignee.index.whole();

            for(unsigned i = 0; i < assignment.sval.size(); ++i)
            {
                ct_array_t& shared = std::get<ct_array_t>(local[i + assignee.member]);

                // If the array has multiple owners, copy it, creating a new one.
                if(shared.use_count() > 1)
                {
                    std::puts("copying array");
                    ct_array_t new_shared = make_ct_array(array_size);
                    std::copy(shared.get(), shared.get() + array_size, new_shared.get());
                    shared = std::move(new_shared);
                }
                else
                    std::puts("NOT copying array");

                shared[index] = assignment.ssa(i);
            }
        }
        else
        {
            for(unsigned i = 0; i < assignment.sval.size(); ++i)
                local[i + assignee.member] = assignment.sval[i];
        }

        assignee.sval = std::move(assignment.sval);
        assignee.category = RVAL;
    }
    else if(D == COMPILE)
    {
        assert(false);
    }

    // Leave the assignee on the stack, slightly modified.
    assignee.category = RVAL;
    rpn_stack.pop();
}

void eval_t::req_quantity(token_t const& token, rpn_value_t const& value)
{
    if(!is_quantity(value.type.name()))
    {
        compiler_error(value.pstring, fmt("% expects arithmetic quantity inputs. (Input is %)", 
                                          token_string(token.type), value.type));
    }
}
    
void eval_t::req_quantity(token_t const& token, rpn_value_t const& lhs, rpn_value_t const& rhs)
{
    if(!is_quantity(lhs.type.name()) || !is_arithmetic(rhs.type.name()))
    {
        pstring_t pstring = concat(lhs.pstring, rhs.pstring);
        compiler_error(pstring, fmt("% expects arithmetic quantity inputs. (Inputs are % and %)", 
                                    token_string(token.type), lhs.type, rhs.type));
    }
}


// Applies an operator to the top two values on the eval stack,
// turning them into a single value.
/* TODO
void eval_t::compile_binary_operator(cfg_node_t& cfg_node, ssa_op_t op, type_t result_type, bool carry)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);

    // Result will remain in 'lhs'.
    ssa_value_t result;
    if(carry)
        result = cfg_node.emplace_ssa(op, result_type, lhs.value, rhs.value, 0);
    else
        result = cfg_node.emplace_ssa(op, result_type, lhs.value, rhs.value);

    rpn_value_t new_top =
    {
        .value = result,
        .category = RVAL, 
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };
    
    rpn_stack.pop(2);
    rpn_stack.push(std::move(new_top));
}
*/


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

    if(is_interpret(Policy::D))
    {
        bool const result = Policy::interpret(lhs.s(), rhs.s());
        new_top.sval = make_sval((unsigned)result);
    }
    else if(Policy::D == COMPILE)
    {
        assert(false);
    }

    rpn_stack.pop(2);
    rpn_stack.push(std::move(new_top));
}

template<typename Policy>
void eval_t::do_arith(rpn_stack_t& rpn_stack, token_t const& token)
{
    rpn_value_t& lhs = rpn_stack.peek(1);
    rpn_value_t& rhs = rpn_stack.peek(0);
    req_quantity(token, lhs, rhs);

    type_t result_type;

    if(lhs.type != rhs.type)
    {
        if(is_ct(lhs.type) && can_cast(lhs.type, rhs.type, true))
        {
            result_type = rhs.type;
            throwing_cast<Policy::D>(lhs, result_type, true);
        }
        else if(is_ct(rhs.type) && can_cast(rhs.type, lhs.type, true))
        {
            result_type = lhs.type;
            throwing_cast<Policy::D>(rhs, result_type, true);
        }
        else
        {
        //bad_types: TODO
            pstring_t pstring = concat(lhs.pstring, rhs.pstring);
            compiler_error(pstring, fmt("% isn't defined for this type combination. (% and %)",
                                        token_string(token.type), lhs.type, rhs.type));
        }
    }
    else
        result_type = lhs.type;

    assert(is_arithmetic(result_type.name()));
    assert(lhs.type == result_type);
    assert(rhs.type == result_type);

    rpn_value_t new_top =
    {
        .category = RVAL, 
        .type = result_type, 
        .pstring = concat(lhs.pstring, rhs.pstring)
    };

    if(is_interpret(Policy::D))
    {
        assert(is_masked(lhs.fixed(), lhs.type.name()));
        assert(is_masked(rhs.fixed(), rhs.type.name()));

        fixed_t result = { Policy::interpret(lhs.s(), rhs.s()) };
        result.value &= numeric_bitmask(result_type.name());
        new_top.sval = make_sval(result);
    }
    else if(Policy::D == COMPILE)
    {
        assert(false);
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

    if(is_interpret(Policy::D))
    {
        assert(is_masked(lhs.fixed(), lhs.type.name()));
        assert(is_masked(rhs.fixed(), rhs.type.name()));

        fixed_t result = { Policy::interpret(lhs.s(), rhs.whole()) };
        result.value &= numeric_bitmask(result_type.name());
        new_top.sval = make_sval(result);
    }
    else if(Policy::D == COMPILE)
    {
        assert(false);
    }

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

    if(Policy::D == CHECK)
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
        assert(false);
    }
}

template<typename Policy>
void eval_t::do_logical_end(rpn_stack_t& rpn_stack)
{
    rpn_value_t& top = rpn_stack.peek(0);
    throwing_cast<Policy::D>(top, { TYPE_BOOL }, true);

    if(Policy::D == COMPILE)
    {
        assert(false);
    }
}

template<eval_t::do_t D>
void eval_t::force_truncate(rpn_value_t& rpn_value, type_t to_type, pstring_t cast_pstring)
{
    assert(!is_ct(rpn_value.type));
    assert(is_arithmetic(to_type.name()) && is_arithmetic(rpn_value.type.name()));

    rpn_value_t new_rpn =
    {
        .category = RVAL, 
        .type = to_type, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };

    if(is_interpret(D))
        new_rpn.sval = make_sval(mask_numeric(rpn_value.fixed(), to_type.name()));
    else if(D == COMPILE)
    {
        assert(false);
    }

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

    if(is_interpret(D))
        new_rpn.sval = make_sval(mask_numeric({ rpn_value.s() }, to_type.name()));
    else if(D == COMPILE)
    {
        assert(false);
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

    if(D != CHECK)
    {
        fixed_t const masked = mask_numeric(rpn_value.fixed(), to_type.name());

        if(implicit && to_signed(masked.value, to_type.name()) != rpn_value.s())
        {
            file_contents_t file(rpn_value.pstring.file_i);
            throw compiler_error_t(
                fmt_error(file, rpn_value.pstring, fmt(
                    "Int value of % can't be represented in type %. (Implicit type conversion)", 
                    to_double(fixed_t{ rpn_value.s() }), to_type))
                + fmt_note("Add an explicit cast operator to override.")
                );
        }

        new_rpn.sval = make_sval(masked);
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

    if(D != CHECK)
    {
        fixed_sint_t const original = to_signed(rpn_value.u(), { TYPE_REAL });
        fixed_uint_t value = rpn_value.u();
        fixed_uint_t const mask = numeric_bitmask(to_type.name());
        if(fixed_uint_t z = builtin::ctz(mask))
            value += (1ull << (z - 1)) & value;
        value &= mask;

        if(implicit)
        {
            fixed_uint_t const supermask = numeric_supermask(to_type.name());
            if((to_signed(value, to_type.name()) & supermask) != (rpn_value.s() & supermask))
            {
                file_contents_t file(rpn_value.pstring.file_i);
                throw compiler_error_t(
                    fmt_error(file, rpn_value.pstring, fmt(
                        "Num value of % doesn't fit in type %. (Implicit type conversion)", 
                        to_double(fixed_t{original}), to_type))
                    + fmt_note("Add an explicit cast operator to override.")
                    );
            }
        }

        assert(is_masked({value}, to_type.name()));

        new_rpn.sval = make_sval(fixed_t{ value });
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
        .type = { TYPE_BOOL }, 
        .pstring = cast_pstring ? concat(rpn_value.pstring, cast_pstring) : rpn_value.pstring,
    };


    if(is_interpret(D))
    {
        if(is_arithmetic(rpn_value.type.name()))
            new_rpn.sval = make_sval(boolify(rpn_value.fixed()));
    }
    else if(D == COMPILE)
    {
        assert(false);
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
    block_data.fn_vars = builder.pool.alloc(num_vars());
    block_data.unsealed_phis = seal ? nullptr : builder.pool.alloc(num_vars());
    block_data.label_name = label_name;
    return new_node;
}

void eval_t::cfg_exits_with_jump(cfg_node_t& node)
{
    node.alloc_output(1);
}

void eval_t::cfg_exits_with_branch(cfg_node_t& node, ssa_value_t condition)
{
    ssa_ht if_h = node.emplace_ssa(SSA_if, TYPE_VOID, condition);
    if_h->append_daisy();
    node.alloc_output(2);
    assert(node.output_size() == 2);
    assert(node.last_daisy() == if_h);
}

// Jumps are like 'break', 'continue', 'goto', etc.
cfg_ht eval_t::compile_goto(cfg_ht branch_node)
{
    // The syntax allows code to exist following a jump statement.
    // Said code is unreachable, but gets compiled anyway.
    // Implement using a conditional that always takes the false branch.
    // (This will be optimized out later)

    cfg_exits_with_branch(*branch_node, 0u);
    cfg_ht dead_branch = insert_cfg(true);
    branch_node->build_set_output(1, dead_branch);
    return dead_branch;
}


