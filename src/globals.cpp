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
#include "byteify.hpp"
#include "cg.hpp"
#include "graphviz.hpp"
#include "thread.hpp"

// global_t statics:
std::deque<global_t> global_t::global_pool;
std::vector<global_t*> global_t::ready;

global_t& global_t::lookup(pstring_t name, char const* source)
{
    assert(compiler_phase() <= PHASE_PARSE);

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
unsigned global_t::define(pstring_t pstring, global_class_t gclass, ideps_set_t&& ideps, 
                          std::function<unsigned(global_t&)> create_impl)
{
    assert(compiler_phase() == PHASE_PARSE);
    unsigned ret;
    {
        std::lock_guard<std::mutex> global_lock(m_define_mutex);
        if((gclass == GLOBAL_GROUP || gclass == GLOBAL_VBANK) && m_gclass == gclass)
            return m_impl_index;
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
        m_gclass = gclass;
        m_pstring = pstring; // Not necessary but useful for error reporting.
        m_impl_index = ret = create_impl(*this);
        m_ideps = std::move(ideps);
    }
    ideps.clear();
    return ret;
}

template<typename T, typename... Args>
static unsigned _append_to_vec(Args&&... args)
{
    std::lock_guard<std::mutex> lock(global_impl_vec_mutex<T>);
    global_impl_vec<T>.emplace_back(std::forward<Args>(args)...);
    return global_impl_vec<T>.size() - 1;
}

vbank_ht global_t::lookup_vbank(pstring_t name, char const* source)
{
    return 
    { 
        lookup(name, source).define(name, GLOBAL_VBANK, {}, [](global_t& g)
            { return _append_to_vec<vbank_t>(g); })
    };
}

group_ht global_t::lookup_group(pstring_t name, char const* source)
{
    return 
    { 
        lookup(name, source).define(name, GLOBAL_GROUP, {}, [](global_t& g)
            { return _append_to_vec<group_t>(g); })
    };
}

fn_ht global_t::define_fn(pstring_t pstring,  global_t::ideps_set_t&& ideps, 
                          type_t type, fn_def_t&& fn_def)
{
    // Create the fn
    fn_ht const fn = 
    {
        define(pstring, GLOBAL_FN, std::move(ideps), [&](global_t& g)
            { return _append_to_vec<fn_t>(g, type, std::move(fn_def)); })
    };

    return fn;
}

gvar_ht global_t::define_var(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                            type_t type, group_ht group)
{
    // Create the var
    gvar_ht const var = 
    {
        define(pstring, GLOBAL_VAR, std::move(ideps), [&](global_t& g)
            { return _append_to_vec<gvar_t>(g, type, group); })
    };

    // Add it to the group
    {
        std::lock_guard<std::mutex> lock(global_impl_vec_mutex<group_t>);
        global_impl_vec<group_t>[group.value].add_gvar(var);
    }
    
    return var;
}

const_ht global_t::define_const(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                               type_t type, vbank_ht vbank)
{
    // Create the const
    const_ht const cnst = 
    {
        define(pstring, GLOBAL_CONST, std::move(ideps), [&](global_t& g)
            { return _append_to_vec<const_t>(g, type, vbank); })
    };

    // Add it to the vbank
    {
        std::lock_guard<std::mutex> lock(global_impl_vec_mutex<vbank_t>);
        global_impl_vec<vbank_t>[vbank.value].add_const(cnst);
    }
    
    return cnst;
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
    auto const save_graph = [&](ir_t& ir, char const* suffix)
    {
        std::ofstream ocfg(fmt("graphs/%_cfg_%.gv", name, suffix));
        if(ocfg.is_open())
            graphviz_cfg(ocfg, ir);

        std::ofstream ossa(fmt("graphs/%_ssa_%.gv", name, suffix));
        if(ossa.is_open())
            graphviz_ssa(ossa, ir);
    };

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
            build_ir(ir, this->impl<fn_t>());
            ir.assert_valid();

            if(compiler_options().graphviz)
                save_graph(ir, "initial");

            if(compiler_options().optimize)
            {
                bool changed;
                do
                {
                    changed = false;
                    changed |= o_phis(ir);
                    changed |= o_merge_basic_blocks(ir);
                    changed |= o_abstract_interpret(ir);
                    changed |= o_remove_unused_ssa(ir);
                }
                while(changed);
            }

            if(compiler_options().graphviz)
                save_graph(ir, "o1");

            // Set the global's 'read' and 'write' bitsets:
            impl<fn_t>().calc_reads_writes_purity(ir);

            byteify(ir, *this);
            //make_conventional(ir);

            if(compiler_options().graphviz)
                save_graph(ir, "byteify");

            if(compiler_options().optimize)
            {
                bool changed;
                do
                {
                    changed = false;
                    changed |= o_phis(ir);
                    changed |= o_merge_basic_blocks(ir);
                    changed |= o_abstract_interpret(ir);
                    changed |= o_remove_unused_ssa(ir);

                }
                while(changed);
            }

            if(compiler_options().graphviz)
                save_graph(ir, "o2");

            code_gen(ir);

            if(compiler_options().graphviz)
                save_graph(ir, "cg");

            /*
            for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
            {
                std::cout << "\n\nCFG:";

                for(ssa_node_t& n : *cfg_it)
                    std::cout << n.op() << '\n';
            }
            */

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

    case GLOBAL_VBANK:
        // TODO
        break;

    case GLOBAL_GROUP:
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

    /* TODO
    // Now allocate memory.
    for(gvar_ht i = {0}; i.value < global_impl_vec<gvar_t>.size(); ++i.value)
    {
        gvar_t& gvar = global_impl_vec<gvar_t>[i];

        // TODO
        throw 0;

    }

    for(fn_t& fn : global_impl_vec<fn_t>)
    {

    }
    */
}

std::size_t fn_t::rw_bitset_size() 
{
    return bitset_size<>(global_impl_vec<fn_t>.size());
}

void fn_t::calc_reads_writes_purity(ir_t const& ir)
{
    unsigned const set_size = rw_bitset_size();

    bitset_uint_t* writes = bitset_pool.alloc(set_size);
    bitset_uint_t* reads  = bitset_pool.alloc(set_size);
    group_bitset_t groups = 0;
    bool io_pure = true;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_flags(ssa_it->op()) & SSAF_IMPURE)
            io_pure = false;

        if(ssa_flags(ssa_it->op()) & SSAF_WRITE_GLOBALS)
        {
            for_each_written_global(ssa_it,
            [this, writes, &groups](ssa_value_t def, locator_t loc)
            {
                assert(loc.lclass() != LCLASS_GVAR_SET);

                if(loc.lclass() == LCLASS_GVAR)
                {
                    gvar_t const& written = *loc.gvar();
                    assert(written.global.gclass() == GLOBAL_VAR);

                    // Writes only have effect if they're not writing back a
                    // previously read value.
                    // TODO: verify this is correct
                    if(!def.holds_ref()
                       || def->op() != SSA_read_global
                       || def->input(1).locator() != loc)
                    {
                        bitset_set(writes, loc.gvar().value);
                        groups |= written.group_bitset();
                    }
                }
            });
        }
        else if(ssa_it->op() == SSA_read_global)
        {
            assert(ssa_it->input_size() == 2);
            locator_t loc = ssa_it->input(1).locator();

            assert(loc.lclass() != LCLASS_GVAR_SET);

            if(loc.lclass() == LCLASS_GVAR)
            {
                gvar_t const& read = *loc.gvar();
                assert(read.global.gclass() == GLOBAL_VAR);

                // Reads only have effect if something actually uses them:
                for(unsigned i = 0; i < ssa_it->output_size(); ++i)
                {
                    auto oe = ssa_it->output_edge(i);
                    // TODO: removed fence check here.
                    if(!is_locator_write(oe)
                       || oe.handle->input(oe.index + 1) != loc)
                    {
                        bitset_set(reads, loc.gvar().value);
                        groups |= read.group_bitset();
                        break;
                    }
                }
            }
        }
        else if(ssa_it->op() == SSA_fn_call)
        {
            fn_t const& callee = get_fn(*ssa_it);

            bitset_or(set_size, writes, callee.writes());
            bitset_or(set_size, reads,  callee.reads());
            groups  |= callee.groups();
            io_pure &= callee.io_pure();
        }
    }

    {
        std::lock_guard<std::mutex> lock(bitset_pool_mutex);
        m_writes = writes;
        m_reads  = reads;
        m_groups = groups;
        m_io_pure = io_pure;
    }
}

/*
void alloc_args(ir_t const& ir)
{
    // Calculate which arg RAM is used by called fns:
    m_recursive_arg_ram = {};
    for(global_t* idep : global.ideps())
        if(idep->gclass() == GLOBAL_FN)
            m_recursive_arg_ram |= idep->impl<fn_t>().m_recursive_arg_ram;

    // Now allocate RAM for our args:

    // QUESTION: how do we determine what to allocate in ZP?
    // - ptrs should always go in ZP
    // - arrays should never go in ZP, I guess?
}
*/

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

