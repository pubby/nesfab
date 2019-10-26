#include "globals.hpp"

#include <iostream>
#include <fstream>

#include "alloca.hpp"
#include "compiler_error.hpp"
#include "fnv1a.hpp"
#include "ir_builder.hpp"
#include "o.hpp"

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

    global_t& global = get(name);
    verify_undefined(global);
    global.type = type_t::fn(types, types + num_params + 1);
    global.gclass = GLOBAL_FN;
    global.fn = &fns.emplace_back(params_begin, params_end);
    return global;
}

global_t& global_manager_t::new_const(pstring_t name, type_t type)
{
    global_t& global = get(name);
    verify_undefined(global);
    global.gclass = GLOBAL_CONST;
    global.type = type;
    return global;
}

global_t& global_manager_t::new_var(pstring_t name, type_t type)
{
    global_t& global = get(name);
    verify_undefined(global);
    global.gclass = GLOBAL_VAR;
    global.type = type;
    return global;
}

void global_manager_t::finish()
{
    for(global_t& global : globals)
    {
        for(global_t* dep : global.deps)
            if(dep->gclass == GLOBAL_UNDEFINED)
                compiler_error(dep->name, "Name not in scope.");
    }

    for(global_t* global : toposort_deps())
    {
        // Compile it!
        switch(global->gclass)
        {
        default:
            throw std::runtime_error("Invalid global.");

        case GLOBAL_FN:
            {
                // Compile the FN.
                ir_builder_t ir_builder(*this, *global);
                ir_builder.compile();

                //assert(ir_builder.ir.valid());
                //o_remove_trivial_phis(ir_builder.ir);
                //assert(ir_builder.ir.valid());
                for(unsigned i = 0; i < 1; ++i)
                {
                    o_remove_redundant_phis(ir_builder.ir);
                    o_remove_trivial_phis(ir_builder.ir);
                    o_abstract_interpret(ir_builder.ir);
                }

                std::puts("DONE");
                //o_abstract_interpret(ir_builder.ir);

                {
                    std::ofstream o(fmt("graphs/%_cfg.gv", 
                                        global->name.view()));
                    if(o.is_open())
                        ir_builder.ir.gv_cfg(o);
                    std::puts("done cfg");
                }

                {
                    std::ofstream o(fmt("graphs/%_ssa.gv", 
                                        global->name.view()));
                    if(o.is_open())
                        ir_builder.ir.gv_ssa(o);
                    std::puts("done ssa");
                }

                /*
                {
                    scheduler_t scheduler(ir_builder.ir);
                    std::ofstream o(fmt("graphs/%_cfg.gv", 
                                        global->name.view()));
                    if(o.is_open())
                        scheduler.gv(o);
                }
                */
                break;
            }

        case GLOBAL_CONST:
            // Evaluate at runtime.
            // TODO
            break;

        case GLOBAL_VAR:
            // Allocate memory
            global->ds_region = ds_manager.alloc(global->type.sizeof_());
            
            // Sets 'modifies'.
            for(std::uint16_t i = 0; i < global->ds_region.size; ++i)
                global->modifies.set(global->ds_region.offset + i);

            break;
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
    for(global_t* dep : global.deps)
        toposort_visit(topo, *dep);
    global.mark = MARK_PERMANENT;
    topo.push_back(&global);
}

void global_manager_t::debug_print()
{
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
}

std::ostream& global_manager_t::gv_deps(std::ostream& o)
{
    o << "digraph {\n";
    for(global_t& global : globals)
        for(global_t* dep : global.deps)
            o << global.name.view() << " -> " << dep->name.view() << '\n';
    o << "}\n";
    return o;
}
