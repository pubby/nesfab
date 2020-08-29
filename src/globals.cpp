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
#include "cg_byteify.hpp" // TODO
#include "cg_isel.hpp" // TODO
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

global_t& global_t::lookup(pstring_t name, char const* source)
{
    std::string_view view = name.view(source);
    auto hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    std::lock_guard<std::mutex> lock(global_pool_mutex);
    rh::apair<global_t**, bool> result = global_pool_map.emplace(hash,
        [view](global_t* ptr) -> bool
        {
            return std::equal(view.begin(), view.end(), ptr->name.begin());
        },
        [name, source]() -> global_t*
        { 
            return &global_pool.emplace_back(name, source);
        });

    return **result.first;
}

// Changes a global from UNDEFINED to some specified 'gclass'.
// This gets called whenever a global is parsed.
void global_t::define(pstring_t pstring, global_class_t gclass, type_t type,
                      impl_t impl, global_t::ideps_set_t&& ideps)
{
    assert(compiler_phase() == PHASE_PARSE);
    {
        std::lock_guard<std::mutex> global_lock(m_define_mutex);
        if(m_gclass != GLOBAL_UNDEFINED)
        {
            file_contents_t file1(pstring.file_i);
            file_contents_t file2(m_pstring.file_i);
            throw compiler_error_t(
                fmt_error(file1, pstring, 
                          fmt("Global identifier % already in use.", 
                              pstring.view(file1.source())))
                + fmt_error(file2, m_pstring, 
                            "Previous definition here:"));
        }
        m_type = type;
        m_gclass = gclass;
        m_pstring = pstring;
        m_impl = impl;
        m_ideps = std::move(ideps);
    }
    ideps.clear();
}

fn_t& global_t::define_fn(pstring_t pstring, type_t type,
                          global_t::ideps_set_t&& ideps, fn_def_t&& fn_def)
{
    fn_t* new_fn;
    {
        std::lock_guard<std::mutex> fns_lock(fn_pool_mutex);
        new_fn = &fn_pool.emplace_back(std::move(fn_def));
    }
    define(pstring, GLOBAL_FN, type, { .fn = new_fn }, std::move(ideps));
    return *new_fn;
}

gvar_ht global_t::define_var(pstring_t pstring, type_t type,
                             global_t::ideps_set_t&& ideps)
{
    unsigned index;
    {
        std::lock_guard<std::mutex> fns_lock(var_vec_mutex);
        index = var_vec.size();
        var_vec.push_back(this);
    }
    define(pstring, GLOBAL_VAR, type, { .index = index }, std::move(ideps));
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
        {
            file_contents_t file(global.m_pstring.file_i);
            compiler_error(file, global.m_pstring, "Name not in scope.");
        }

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
    // Compile it!
    switch(gclass())
    {
    default:
        throw std::runtime_error("Invalid global.");
    case GLOBAL_FN:
        {
            // Compile the FN.
            ssa_pool::clear();
            cfg_pool::clear();
            ir_t ir;
            build_ir(ir, *this);
            ir.assert_valid();

            if(compiler_options().optimize)
            {
                bool changed;
                do
                {
                    changed = false;
                    changed |= o_phis(ir);
                    changed |= o_abstract_interpret(ir);
                    changed |= o_remove_unused_ssa(ir);
                }
                while(changed);
            }

            // Set the global's 'read' and 'write' bitsets:
            m_impl.fn->calc_reads_writes_purity(ir);

            byteify(ir, *this);
            //make_conventional(ir);

            if(compiler_options().optimize)
            {
                bool changed;
                do
                {
                    changed = false;
                    changed |= o_phis(ir);
                    changed |= o_abstract_interpret(ir);
                    changed |= o_remove_unused_ssa(ir);
                }
                while(changed);
            }

            // TODO: scheduling
            schedule_ir(ir);

            for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
            {
                std::cout << "\n\n";
                auto schedule = get_schedule(cfg_it);

                for(ssa_ht h : schedule)
                    std::cout << h->op() << '\n';

                sel_t const* sel = select_instructions(&*schedule.begin(), 
                                                       &*schedule.end());
                while(sel)
                {
                    std::cout << "SEL " << to_string(sel->op) << ' ' << sel->cost << '\n';
                    sel = sel->prev;
                }
            }

            if(compiler_options().graphviz)
            {
                std::ofstream ocfg(fmt("graphs/%_cfg.gv", name));
                if(ocfg.is_open())
                    graphviz_cfg(ocfg, ir);

                std::ofstream ossa(fmt("graphs/%_ssa.gv", name));
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
            if(!global)
                return;
            global->compile();
        }
    });
}

void fn_t::calc_reads_writes_purity(ir_t const& ir)
{
    unsigned const set_size = bitset_size<>(global_t::num_vars());

    bitset_uint_t* writes = bitset_pool.alloc(set_size);
    bitset_uint_t* reads  = bitset_pool.alloc(set_size);
    bool io_pure = true;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_flags(ssa_it->op()) & SSAF_IMPURE)
            io_pure = false;

        if(ssa_flags(ssa_it->op()) & SSAF_WRITE_GLOBALS)
        {
            for_each_written_global(ssa_it,
            [this, writes](ssa_value_t def, locator_t loc)
            {
                if(loc.lclass() == LCLASS_GLOBAL)
                {
                    global_t const& written = global_t::lookup(loc.gvar());
                    assert(written.gclass() == GLOBAL_VAR);

                    // Writes only have effect if they're not writing back a
                    // previously read value.
                    if(!def.holds_ref()
                       || def->op() != SSA_read_global
                       || def->input(1).locator() != loc)
                    {
                        bitset_set(writes, written.var().value);
                    }
                }
            });
        }
        else if(ssa_it->op() == SSA_read_global)
        {
            assert(ssa_it->input_size() == 2);
            locator_t loc = ssa_it->input(1).locator();

            if(loc.lclass() == LCLASS_GLOBAL)
            {
                global_t const& read = global_t::lookup(loc.gvar());
                assert(read.gclass() == GLOBAL_VAR);

                // Reads only have effect if something actually uses them:
                for(unsigned i = 0; i < ssa_it->output_size(); ++i)
                {
                    auto oe = ssa_it->output_edge(i);
                    // TODO: removed fence check here.
                    if(!is_locator_write(oe)
                       || oe.handle->input(oe.index + 1) != loc)
                    {
                        bitset_set(reads, read.var().value);
                        break;
                    }
                }
            }
        }
        else if(ssa_it->op() == SSA_fn_call)
        {
            global_t const& callee = get_fn(*ssa_it);

            assert(callee.gclass() == GLOBAL_FN);

            bitset_or(set_size, writes, callee.fn().writes());
            bitset_or(set_size, reads,  callee.fn().reads());
            io_pure &= callee.fn().io_pure();
        }
    }

    {
        std::lock_guard<std::mutex> lock(bitset_pool_mutex);
        m_writes = writes;
        m_reads  = reads;
        m_io_pure = io_pure;
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

