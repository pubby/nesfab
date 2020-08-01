#include "globals.hpp"

#include <iostream>
#include <fstream>

#include "alloca.hpp"
#include "compiler_error.hpp"
#include "fnv1a.hpp"
#include "ir_builder.hpp"
#include "o.hpp"
#include "cg_schedule.hpp" // TODO
#include "graphviz.hpp"

std::string to_string(stmt_name_t stmt_name)
{

    switch(stmt_name)
    {
    default:
        if(is_var_init(stmt_name))
            return ("STMT_VAR_INIT " 
                    + std::to_string(get_local_var_i(stmt_name)));
        else
            return "bad stmt_name_t";

#define X(x) case x: return #x;
    STMT_ENUM
#undef X
    }
}

std::string to_string(global_class_t gclass)
{
    switch(gclass)
    {
    default: return "bad global class";
#define X(x) case x: return #x;
    GLOBAL_CLASS_ENUM
#undef X
    }
}

global_t const* global_manager_t::global(ssa_node_t& ssa_node) const
{
    switch(ssa_node.op())
    {
    case SSA_fn_call:
        assert(ssa_node.input_size() >= 2);
        assert(ssa_node.input(1).is_const());
        return ssa_node.input(1).ptr<global_t const>();
    default:
        return nullptr;
    }
}

unsigned global_manager_t::get_index(pstring_t name)
{
    std::string_view view = name.view();
    rh::apair<unsigned*, bool> result = global_map.emplace(
        fnv1a<std::uint64_t>::hash(view.data(), view.size()),
        [this, view](unsigned i) -> bool
        {
            return view == globals[i].name.view();
        },
        [&]() -> unsigned
        { 
            globals.push_back({ name });
            return globals.size() - 1;
        });
    return *result.first;
}

void global_manager_t::verify_undefined(global_t& global)
{
    if(global.gclass != GLOBAL_UNDEFINED)
    {
        compiler_error(global.name, fmt(
            "Global identifier already in use. Previous definition at %.",
            fmt_source_pos(global.name)));
    }
}

global_t& global_manager_t::new_fn(
    pstring_t name, 
    var_decl_t const* params_begin, 
    var_decl_t const* params_end, 
    type_t return_type)
{
    // Setup the type vector (needed for creating the fn type)
    unsigned num_params = params_end - params_begin;
    type_t* types = ALLOCA_T(type_t, num_params + 1);
    for(unsigned i = 0; i != num_params; ++i)
        types[i] = params_begin[i].type;
    types[num_params] = return_type;

    global_t& global = lookup_name(name);
    verify_undefined(global);
    global.type = type_t::fn(types, types + num_params + 1);
    global.gclass = GLOBAL_FN;
    global.fn = &fns.emplace_back(params_begin, params_end);
    return global;
}

global_t& global_manager_t::new_const(pstring_t name, type_t type)
{
    global_t& global = lookup_name(name);
    verify_undefined(global);
    global.gclass = GLOBAL_CONST;
    global.type = type;
    return global;
}

global_t& global_manager_t::new_var(pstring_t name, type_t type)
{
    global_t& global = lookup_name(name);
    verify_undefined(global);
    global.gclass = GLOBAL_VAR;
    global.type = type;
    global.var = &m_vars.emplace_back(global, m_vars.size(), nullptr);
    return global;
}

void global_manager_t::finish()
{
    // Check to make sure every global was defined:
    for(global_t& global : globals)
        if(global.gclass == GLOBAL_UNDEFINED)
            compiler_error(global.name, "Name not in scope.");

    auto toposorted = toposort_deps();

    /* TODO: remove
    // Setup the 'modifies' bitsets.
    // Also setup 'gvar_map'.
    bitset_pool.clear();
    m_modifies_bitset_size = bitset_size<bitset_uint_t>(m_vars.size());
    for(global_t* global : toposorted)
    {
        if(global->gclass != GLOBAL_FN)
            continue;

        global->fn->reads = bitset_pool.alloc(m_modifies_bitset_size);
        global->fn->writes = bitset_pool.alloc(m_modifies_bitset_size);
        assert(bitset_all_clear(m_modifies_bitset_size, global->fn->reads));
        assert(bitset_all_clear(m_modifies_bitset_size, global->fn->writes));

        for(global_t* dep : global->deps)
        {
            if(dep->gclass == GLOBAL_VAR)
            {
                bitset_set(global->fn->modifies, dep->var->gvar_i);

                // Add to 'gvar_map' here:
                if(gvar_map.emplace(dep, fn_vars.size()).second)
                    fn_vars.push_back({ dep->type, dep->name });
            }
            else if(dep->gclass == GLOBAL_FN)
            {
                assert(dep->fn->modifies);
                bitset_or(m_modifies_bitset_size,
                          global->fn->modifies, 
                          dep->fn->modifies);
            }
        }
    }
    */

    // TODO: These are some flags
    constexpr bool optimize = false;
    constexpr bool graph = true;
    constexpr bool compile = false;

    for(global_t* global : toposorted)
    {
        // Compile it!
        switch(global->gclass)
        {
        default:
            throw std::runtime_error("Invalid global.");

        case GLOBAL_FN:
            {
                // Compile the FN.
                ir_t ir = build_ir(*this, *global);
                assert(ir.valid());

                o_phis(ir);

                if(optimize)
                {
                    bool changed;
                    do
                    {
                        changed = false;
                        changed |= o_phis(ir);
                        changed |= o_abstract_interpret(ir);
                    }
                    while(changed);
                }

                // Set the global's 'read' and 'write' bitsets:
                calc_reads_writes(*global, ir);

                //byteify(ir, *this, *global);
                //make_conventional(ir);

                if(graph)
                {
                    std::ofstream dg("graphs/deps.gv");
                    gv_deps(dg);

                    std::ofstream ocfg(fmt("graphs/%_cfg.gv", 
                                           global->name.view()));
                    if(ocfg.is_open())
                        graphviz_cfg(ocfg, ir);

                    std::ofstream ossa(fmt("graphs/%_ssa.gv", 
                                           global->name.view()));
                    if(ossa.is_open())
                        graphviz_ssa(ossa, ir);
                }

                //TODO
                if(compile)
                {
                    // Test scheduling
                    cfg_data_pool::scope_guard_t<cfg_cg_d> c(cfg_pool::array_size());
                    ssa_data_pool::scope_guard_t<ssa_cg_d> s(ssa_pool::array_size());
                    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
                    {
                        std::cout << " - \n";
                        for(ssa_ht h : schedule_cfg_node(cfg_it))
                            std::cout << h.index << ' ' << h->op() << '\n';
                    }
                }

                break;
            }

        case GLOBAL_CONST:
            // Evaluate at runtime.
            // TODO
            break;

        case GLOBAL_VAR:
            // TODO
            break;
        }
    }
}

void global_manager_t::calc_reads_writes(global_t& global, ir_t const& ir)
{
    assert(global.gclass == GLOBAL_FN);

    unsigned const bitset_size = gvar_bitset_size<bitset_uint_t>();
    global.fn->reads  = bitset_pool.alloc(bitset_size);
    global.fn->writes = bitset_pool.alloc(bitset_size);

    assert(bitset_all_clear(bitset_size, global.fn->reads));
    assert(bitset_all_clear(bitset_size, global.fn->writes));

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() == SSA_write_global)
        {
            assert(ssa_it->input_size() == 2);
            if(global_t* written = ssa_it->input(1).ptr<global_t>())
            {
                assert(written->gclass == GLOBAL_VAR);

                // Writes only have effect if they're not writing back a
                // previously read value.
                ssa_value_t value = ssa_it->input(0);
                if(!value.holds_ref()
                   || value->op() != SSA_read_global
                   || value->input(1).ptr<global_t>() != written)
                {
                    bitset_set(global.fn->writes, written->var->id);
                }
            }
        }
        else if(ssa_it->op() == SSA_read_global)
        {
            assert(ssa_it->input_size() == 2);
            if(global_t* read = ssa_it->input(1).ptr<global_t>())
            {
                assert(read->gclass == GLOBAL_VAR);

                // Reads only have effect if something actually uses them:
                for(unsigned i = 0; i < ssa_it->output_size(); ++i)
                {
                    ssa_ht output = ssa_it->output(i);
                    if(output->op() != SSA_fence
                        && (output->op() != SSA_write_global
                            || output->input(1).ptr<global_t>() != read))
                    {
                        bitset_set(global.fn->reads, read->var->id);
                        break;
                    }
                }
            }
        }
        else if(ssa_it->op() == SSA_fn_call)
        {
            global_t* callee = ssa_it->input(1).ptr<global_t>();

            assert(callee);
            assert(callee->gclass == GLOBAL_FN);

            bitset_or(bitset_size, global.fn->writes, callee->fn->writes);
            bitset_or(bitset_size, global.fn->reads,  callee->fn->reads);
        }
    }
}

std::vector<global_t*> global_manager_t::toposort_deps()
{
    std::vector<global_t*> topo;
    topo.reserve(globals.size());

    for(global_t& global : globals)
        global.mark = MARK_NONE;

    for(global_t& global : globals)
        if(global.mark == MARK_NONE)
            toposort_visit(topo, global);

    return topo;
}

void global_manager_t::toposort_visit(std::vector<global_t*>& topo,
                                      global_t& global)
{
    if(global.mark == MARK_PERMANENT)
        return;
    else if(global.mark == MARK_TEMPORARY)
        compiler_error(global.name, 
                       "Global contains recursive or circular dependency.");
    global.mark = MARK_TEMPORARY;
    for(global_t* idep : global.ideps)
        toposort_visit(topo, *idep);
    global.mark = MARK_PERMANENT;
    topo.push_back(&global);
}

void global_manager_t::debug_print()
{
#ifndef NDEBUG
    for(global_t const& global : globals)
    {
        std::cout << "GLOBAL " << global.name.view() << '\n';
        std::cout << "gclass = " << to_string(global.gclass) << '\n';
        std::cout << "type = " << global.type << '\n';
        if(global.gclass == GLOBAL_FN)
        {
            std::cout << "vars:" << '\n';
            for(unsigned i = 0; i < global.fn->local_vars.size(); ++i)
            {
                var_decl_t var = global.fn->local_vars[i];
                if(i < global.fn->num_params)
                    std::cout << "PARAM: ";
                else
                    std::cout << "LOCAL: ";
                std::cout << var.type << ' ' << var.name.view() << '\n';
            }
            for(stmt_t const& stmt : global.fn->stmts)
            {
                std::cout << to_string(stmt.name) << '\n';
                if(stmt.expr)
                {
                    for(token_t* ptr = stmt.expr; ptr->type; ++ptr)
                        std::cout << "  " << ptr->to_string() << '\n';
                }
            }
        }
        std::cout << '\n';
    }

    std::cout << "\nTOPO:\n";
    auto sorted = toposort_deps();
    for(global_t* global : sorted)
        std::cout << "GLOBAL " << global->name.view() << '\n';
#endif
}

std::ostream& global_manager_t::gv_deps(std::ostream& o)
{
    o << "digraph {\n";
    for(global_t& global : globals)
        for(global_t* idep : global.ideps)
            o << global.name.view() << " -> " << idep->name.view() << '\n';
    o << "}\n";
    return o;
}
