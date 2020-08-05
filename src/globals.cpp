#include "globals.hpp"

#include <iostream>
#include <fstream>

#include "alloca.hpp"
#include "bitset.hpp"
#include "compiler_error.hpp"
#include "fnv1a.hpp"
#include "ir_builder.hpp"
#include "o.hpp"
#include "options.hpp"
#include "cg_schedule.hpp" // TODO
#include "graphviz.hpp"
#include "thread.hpp"

// global_t statics:
std::deque<global_t> global_t::global_pool;
std::deque<fn_t> global_t::fn_pool;
std::vector<global_t*> global_t::var_vec;
std::vector<global_t*> global_t::ready;

label_t* global_t::new_label()
{
    std::lock_guard<std::mutex> lock(label_pool_mutex);
    return &label_pool.emplace();
}

token_t const* global_t::new_expr(token_t const* begin, token_t const* end)
{
    std::lock_guard<std::mutex> lock(expr_pool_mutex);
    return expr_pool.insert(begin, end);
}

global_t& global_t::lookup(pstring_t name)
{
    std::string_view view = name.view();
    auto hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    std::lock_guard<std::mutex> lock(global_pool_mutex);
    rh::apair<global_t**, bool> result = global_pool_map.emplace(hash,
        [view](global_t* ptr) -> bool
        {
            return view == ptr->name.view();
        },
        [&]() -> global_t*
        { 
            return &global_pool.emplace_back(name);
        });

    return **result.first;
}

// Changes a global from UNDEFINED to some specified 'gclass'.
// This gets called whenever a global is parsed.
void global_t::define(global_class_t gclass, type_t type, impl_t impl,
                      global_t::ideps_set_t&& ideps)
{
    assert(compiler_phase() == PHASE_PARSE);
    {
        std::lock_guard<std::mutex> global_lock(m_define_mutex);
        if(m_gclass != GLOBAL_UNDEFINED)
        {
            compiler_error(name, fmt(
                "Global identifier already in use. Previous definition at %.",
                fmt_source_pos(name)));
        }
        m_type = type;
        m_gclass = gclass;
        m_impl = impl;
        m_ideps = std::move(ideps);
    }
    ideps.clear();
}

fn_t& global_t::define_fn(type_t type, global_t::ideps_set_t&& ideps, 
                          fn_def_t&& fn_def)
{
    fn_t* new_fn;
    {
        std::lock_guard<std::mutex> fns_lock(fn_pool_mutex);
        new_fn = &fn_pool.emplace_back(std::move(fn_def));
    }
    define(GLOBAL_FN, type, { .fn = new_fn }, std::move(ideps));
    return *new_fn;
}

gvar_ht global_t::define_var(type_t type, global_t::ideps_set_t&& ideps)
{
    unsigned index;
    {
        std::lock_guard<std::mutex> fns_lock(var_vec_mutex);
        index = var_vec.size();
        var_vec.push_back(this);
    }
    define(GLOBAL_VAR, type, { .index = index }, std::move(ideps));
    return { index };
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::build_order()
{
    assert(compiler_phase() == PHASE_ORDER_GLOBALS);

    for(global_t& global : global_pool)
    {
        // Check to make sure every global was defined:
        if(global.gclass() == GLOBAL_UNDEFINED)
            compiler_error(global.name, "Name not in scope.");

        // Build 'm_iuse' and 'm_ideps_left':
        global.m_ideps_left.store(global.m_ideps.size(), 
                                  std::memory_order_relaxed);
        for(global_t* idep : global.m_ideps)
            idep->m_iuses.insert(&global);

        if(global.m_ideps.empty())
            ready.push_back(&global);
    }
}

void global_t::compile()
{
    constexpr bool optimize = false;
    constexpr bool compile = false;

    // Compile it!
    switch(gclass())
    {
    default:
        throw std::runtime_error("Invalid global.");
    case GLOBAL_FN:
        {
            // Compile the FN.
            ir_t ir = build_ir(*this);
            assert(ir.valid());

            o_phis(ir); // TODO: remove

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
            m_impl.fn->calc_reads_writes(ir);

            //byteify(ir, *this, *global);
            //make_conventional(ir);

            if(compiler_options().graphviz)
            {
                std::ofstream ocfg(fmt("graphs/%_cfg.gv", name.view()));
                if(ocfg.is_open())
                    graphviz_cfg(ocfg, ir);

                std::ofstream ossa(fmt("graphs/%_ssa.gv", name.view()));
                if(ossa.is_open())
                    graphviz_ssa(ossa, ir);
            }

            //TODO
            /*
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
            */

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

    // OK! The global is now compiled.
    // Now add all its dependents onto the ready list:

    global_t** newly_ready = ALLOCA_T(global_t*, m_iuses.size());
    global_t** newly_ready_end = newly_ready;

    for(global_t* iuse : m_iuses)
        if(--iuse->m_ideps_left == 0)
            *(newly_ready_end++) = iuse;

    unsigned new_globals_left;

    {
        std::lock_guard lock(ready_mutex);
        ready.insert(ready.end(), newly_ready, newly_ready_end);
        new_globals_left = --globals_left;
    }

    if(newly_ready_end != newly_ready || new_globals_left == 0)
        ready_cv.notify_all();
}

global_t* global_t::await_ready_global()
{
    std::unique_lock<std::mutex> lock(ready_mutex);
    ready_cv.wait(lock, []{ return globals_left == 0 || !ready.empty(); });

    if(globals_left == 0)
        return nullptr;
    
    global_t* ret = ready.back();
    ready.pop_back();
    return ret;
}

void global_t::compile_all()
{
    globals_left = global_pool.size();

    // Spawn threads to compile in parallel:
    parallelize(compiler_options().num_threads,
    [](std::atomic<bool>& exception_thrown)
    {
        while(!exception_thrown)
        {
            global_t* global = await_ready_global();
            std::printf("compiling %p\n", global);
            if(!global)
                return;
            global->compile();
        }
    });
}

void fn_t::calc_reads_writes(ir_t const& ir)
{
    unsigned const set_size = bitset_size<>(global_t::num_vars());

    {
        std::lock_guard<std::mutex> lock(bitset_pool_mutex);
        m_writes = bitset_pool.alloc(set_size);
        m_reads  = bitset_pool.alloc(set_size);
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() == SSA_write_global)
        {
            assert(ssa_it->input_size() == 2);
            locator_ht loc = ssa_it->input(1).locator();

            if(ir.locators.is_single(loc))
            {
                global_t const& written = ir.locators.get_single(loc);
                assert(written.gclass() == GLOBAL_VAR);

                // Writes only have effect if they're not writing back a
                // previously read value.
                ssa_value_t value = ssa_it->input(0);
                if(!value.holds_ref()
                   || value->op() != SSA_read_global
                   || value->input(1).locator() != loc)
                {
                    bitset_set(m_writes, written.var().value);
                }
            }
        }
        else if(ssa_it->op() == SSA_read_global)
        {
            assert(ssa_it->input_size() == 2);
            locator_ht loc = ssa_it->input(1).locator();

            if(ir.locators.is_single(loc))
            {
                global_t const& read = ir.locators.get_single(loc);
                assert(read.gclass() == GLOBAL_VAR);

                // Reads only have effect if something actually uses them:
                for(unsigned i = 0; i < ssa_it->output_size(); ++i)
                {
                    ssa_ht output = ssa_it->output(i);
                    if(output->op() != SSA_fence
                        && (output->op() != SSA_write_global
                            || output->input(1).locator() != loc))
                    {
                        bitset_set(m_reads, read.var().value);
                        break;
                    }
                }
            }
        }
        else if(ssa_it->op() == SSA_fn_call)
        {
            global_t const* callee = ssa_it->input(1).ptr<global_t>();

            assert(callee);
            assert(callee->gclass() == GLOBAL_FN);

            bitset_or(set_size, m_writes, callee->fn().writes());
            bitset_or(set_size, m_reads,  callee->fn().reads());
        }
    }
}

std::string to_string(global_class_t gclass)
{
    switch(gclass)
    {
    default: return "bad global class";
#define X(x) case x: return #x;
    GLOBAL_CLASS_XENUM
#undef X
    }
}

