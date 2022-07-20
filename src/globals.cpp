#include "globals.hpp"

#include <iostream> // TODO remove?
#include <fstream> // TODO remove?

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
#include "guard.hpp"
#include "group.hpp"
#include "ram_alloc.hpp"
#include "eval.hpp"
#include "rom_array.hpp"

// global_t statics:
std::deque<global_t> global_t::global_pool;
std::vector<global_t*> global_t::ready;

///////////////////////////////////////

///////////////////////////////////////

global_t& global_t::lookup(char const* source, pstring_t name)
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

/* TODO
global_t& global_t::lookup_sourceless(std::string_view view)
{
    auto hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    std::lock_guard<std::mutex> lock(global_pool_mutex);
    rh::apair<global_t**, bool> result = global_pool_map.emplace(hash,
        [view](global_t* ptr) -> bool
        {
            return std::equal(view.begin(), view.end(), ptr->name.begin());
        },
        [view]() -> global_t*
        { 
            return &global_pool.emplace_back(view);
        });

    return **result.first;
}
*/

// Changes a global from UNDEFINED to some specified 'gclass'.
// This gets called whenever a global is parsed.
unsigned global_t::define(pstring_t pstring, global_class_t gclass, 
                          ideps_set_t&& ideps, ideps_set_t&& weak_ideps,
                          std::function<unsigned(global_t&)> create_impl)
{
    assert(compiler_phase() <= PHASE_PARSE);
    unsigned ret;
    {
        std::lock_guard<std::mutex> global_lock(m_define_mutex);
        if(m_gclass != GLOBAL_UNDEFINED)
        {
            if(pstring && m_pstring)
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
            else
                throw compiler_error_t(fmt("Global identifier % already in use.", name));
        }

        m_gclass = gclass;
        m_pstring = pstring; // Not necessary but useful for error reporting.
        m_impl_index = ret = create_impl(*this);
        m_ideps = std::move(ideps);
        m_weak_ideps = std::move(weak_ideps);
    }
    ideps.clear();
    return ret;
}

/* TODO
template<typename T, typename... Args>
static unsigned _append_to_vec(Args&&... args)
{
    std::lock_guard<std::mutex> lock(global_impl_vec_mutex<T>);
    global_impl_vec<T>.emplace_back(std::forward<Args>(args)...);
    return global_impl_vec<T>.size() - 1;
}
*/

fn_t& global_t::define_fn(pstring_t pstring,
                          global_t::ideps_set_t&& ideps, global_t::ideps_set_t&& weak_ideps, 
                          type_t type, fn_def_t&& fn_def, fclass_t fclass)
{
    fn_t* ret;

    // Create the fn
    define(pstring, GLOBAL_FN, std::move(ideps), std::move(weak_ideps), [&](global_t& g)
    { 
        return impl_deque_alloc<fn_t>(ret, g, type, std::move(fn_def), fclass); 
    });

    if(fclass == FN_MODE)
    {
        std::lock_guard<std::mutex> lock(modes_vec_mutex);
        modes_vec.push_back(ret);
    }

    return *ret;
}

gvar_t& global_t::define_var(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                             src_type_t src_type, std::pair<group_vars_t*, group_vars_ht> group,
                             token_t const* expr)
{
    gvar_t* ret;

    // Create the var
    gvar_ht h = { define(pstring, GLOBAL_VAR, std::move(ideps), {}, [&](global_t& g)
    { 
        return impl_deque_alloc<gvar_t>(ret, g, src_type, group.second, expr);
    })};

    // Add it to the group
    assert(group.first);
    group.first->add_gvar(h);
    
    return *ret;
}

const_t& global_t::define_const(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                                src_type_t src_type, std::pair<group_data_t*, group_data_ht> group,
                                token_t const* expr)
{
    const_t* ret;

    // Create the const
    const_ht h = { define(pstring, GLOBAL_CONST, std::move(ideps), {}, [&](global_t& g)
    { 
        return impl_deque_alloc<const_t>(ret, g, src_type, group.second, expr);
    })};

    // Add it to the group
    if(group.first)
        group.first->add_const(h);
    
    return *ret;
}

struct_t& global_t::define_struct(pstring_t pstring, global_t::ideps_set_t&& ideps,
                                  field_map_t&& fields)
                                
{
    struct_t* ret;

    // Create the struct
    struct_ht h = { define(pstring, GLOBAL_STRUCT, std::move(ideps), {}, [&](global_t& g)
    { 
        return impl_deque_alloc<struct_t>(ret, g, std::move(fields));
    })};
    
    return *ret;
}

void global_t::init()
{
    assert(compiler_phase() == PHASE_INIT);

    /* TODO
    using namespace std::literals;
    lookup_sourceless("(universal group)"sv).define({}, GLOBAL_GROUP, {}, {}, [](global_t& g)
    {
        assert(global_impl_vec<group_t>.empty());
        return _append_to_vec<group_t>(g); 
    });

    lookup_sourceless("(universal vbank)"sv).define({}, GLOBAL_VBANK, {}, {}, [](global_t& g)
    {
        assert(global_impl_vec<vbank_t>.empty());
        return _append_to_vec<vbank_t>(g); 
    });
    */
}

bool global_t::has_dep(global_t& other)
{
    assert(compiler_phase() > PHASE_PARSE);

    // Every global depends on itself.
    if(this == &other)
        return true;
    
    if(ideps().count(&other))
        return true;

    for(global_t* idep : ideps())
        if(idep->has_dep(other))
            return true;

    return false;
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::parse_cleanup()
{
    assert(compiler_phase() == PHASE_PARSE_CLEANUP);

    // Handle weak ideps:
    for(global_t& global : global_pool)
    {
        for(global_t* idep : global.m_weak_ideps)
        {
            // No point if we already have the idep.
            if(global.ideps().count(idep))
                continue;

            // Avoid loops.
            if(idep->has_dep(global))
                continue;

            global.m_ideps.insert(idep);
        }

        global.m_weak_ideps.clear();
        global.m_weak_ideps.container.shrink_to_fit();
    }

    // Calculate language gvars and groups:
    for(fn_t& fn : impl_deque<fn_t>)
        fn.calc_lang_gvars_groups();
}

global_t* global_t::detect_cycle(global_t& global, std::vector<std::string>& error_msgs)
{
    if(global.m_ideps_left == 2) // Re-use 'm_ideps_left' to track the DFS.
        return nullptr;

    if(global.m_ideps_left == 1)
        return &global;

    global.m_ideps_left = 1;

    for(global_t* idep : global.m_ideps)
    {
        if(global_t* error = detect_cycle(*idep, error_msgs))
        {
            if(error != &global)
            {
                file_contents_t file(global.m_pstring.file_i);
                error_msgs.push_back(fmt_error(file, global.m_pstring, 
                    "Mutually recursive with:"));
                return error;
            }

            file_contents_t file(global.m_pstring.file_i);
            std::string msg = fmt_error(file, global.m_pstring,
                fmt("% has a recursive definition.", global.name));
            for(std::string const& str : error_msgs)
                msg += str;

            throw compiler_error_t(std::move(msg));
        }
    }

    global.m_ideps_left = 2;

    return nullptr;
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::count_members()
{
    unsigned total_members = 0;
    for(struct_t& s : impl_deque<struct_t>)
        total_members += s.count_members();

    impl_vector<gmember_t>.reserve(total_members);

    for(gvar_t& gvar : impl_deque<gvar_t>)
    {
        gvar.dethunkify(false);

        unsigned const begin = impl_vector<gmember_t>.size();

        unsigned const num = ::num_members(gvar.type());
        for(unsigned i = 0; i < num; ++i)
            impl_vector<gmember_t>.emplace_back(gvar, impl_vector<gmember_t>.size());

        gvar.set_gmember_range({begin}, {impl_vector<gmember_t>.size()});
    }
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::build_order()
{
    assert(compiler_phase() == PHASE_ORDER_GLOBALS);

    // Detect cycles
    for(global_t& global : global_pool)
    {
        std::vector<std::string> error_msgs;
        detect_cycle(global, error_msgs);
    }

    for(global_t& global : global_pool)
    {
        // Check to make sure every global was defined:
        if(global.gclass() == GLOBAL_UNDEFINED)
        {
            if(global.m_pstring)
                compiler_error(global.m_pstring, "Name not in scope.");
            else
                throw compiler_error_t(fmt("Name not in scope: %.", global.name));
        }

        global.m_ideps_left.store(global.m_ideps.size(), std::memory_order_relaxed);
        for(global_t* idep : global.m_ideps)
            idep->m_iuses.insert(&global);

        if(global.m_ideps.empty())
            ready.push_back(&global);
    }
}

void global_t::compile()
{
//#ifdef DEBUG_PRINT
    std::cout << "COMPILING " << name << std::endl;
//#endif

    // Compile it!
    switch(gclass())
    {
    default:
        throw std::runtime_error("Invalid global.");
    case GLOBAL_FN:
        this->impl<fn_t>().compile();
        break;

    case GLOBAL_CONST:
        this->impl<const_t>().compile();
        break;

    case GLOBAL_VAR:
        this->impl<gvar_t>().compile();
        break;

    case GLOBAL_STRUCT:
        this->impl<struct_t>().compile();
        break;
    }

    m_compiled = true;

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
    assert(compiler_phase() == PHASE_COMPILE);

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

    // 1. allocate gvar memory
    // 2. allocate local memory
    // 3. layout vbanks in rom 
    // 4. layout fns in rom 


    // LAYOUT:
    // - vbank is broken into segments
    // - 

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

void global_t::alloc_ram()
{
    ::alloc_ram(ram_bitset_t::filled());

    // TODO: remove
    for(fn_t const& fn : impl_deque<fn_t>)
        fn.proc().write_assembly(std::cout, fn);
}

//////////
// fn_t //
///////////

void fn_t::calc_lang_gvars_groups()
{
    if(m_lang_gvars)
        return;

    m_lang_gvars.reset(impl_bitset_size<gvar_t>());
    m_lang_group_vars.reset(impl_bitset_size<group_vars_t>());

    for(global_t* idep : global.ideps())
    {
        if(idep->gclass() == GLOBAL_VAR)
        {
            m_lang_gvars.set(idep->index());
            m_lang_group_vars.set(idep->impl<gvar_t>().group_vars.value);
        }
        else if(idep->gclass() == GLOBAL_FN)
        {
            fn_t& fn = idep->impl<fn_t>();

            if(fn.fclass == FN_MODE)
                continue;

            fn.calc_lang_gvars_groups();

            assert(fn.m_lang_gvars);
            assert(fn.m_lang_group_vars);

            m_lang_gvars |= fn.m_lang_gvars;
            m_lang_group_vars |= fn.m_lang_group_vars;
        }
    }

    // TODO: add pointers to group_vars?
}

void fn_t::calc_ir_bitsets(ir_t const& ir)
{
    bitset_t  reads(impl_bitset_size<gmember_t>());
    bitset_t writes(impl_bitset_size<gmember_t>());
    bitset_t group_vars(impl_bitset_size<group_vars_t>());
    bitset_t immediate_groups(impl_bitset_size<group_t>());
    bitset_t calls(impl_bitset_size<fn_t>());
    bool io_pure = true;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_flags(ssa_it->op()) & SSAF_IO_IMPURE)
            io_pure = false;

        if(ssa_it->op() == SSA_fn_call)
        {
            fn_ht const callee_h = get_fn(*ssa_it);
            fn_t const& callee = *callee_h;

            writes     |= callee.ir_writes();
            reads      |= callee.ir_reads();
            group_vars |= callee.ir_group_vars();
            calls      |= callee.ir_calls();
            calls.set(callee_h.value);
            io_pure &= callee.ir_io_pure();
        }

        if(ssa_flags(ssa_it->op()) & SSAF_WRITE_GLOBALS)
        {
            for_each_written_global(ssa_it,
            [&](ssa_value_t def, locator_t loc)
            {
                if(loc.lclass() == LOC_GMEMBER)
                {
                    gmember_t const& written = *loc.gmember();
                    assert(written.gvar.global.gclass() == GLOBAL_VAR);

                    // Writes only have effect if they're not writing back a
                    // previously read value.
                    // TODO: verify this is correct
                    if(!def.holds_ref()
                       || def->op() != SSA_read_global
                       || def->input(1).locator() != loc)
                    {
                        writes.set(written.index);
                        group_vars.set(written.gvar.group_vars.value);
                        immediate_groups.set(written.gvar.group_vars->group.handle().value);
                    }
                }
            });
        }
        else if(ssa_it->op() == SSA_read_global)
        {
            assert(ssa_it->input_size() == 2);
            locator_t loc = ssa_it->input(1).locator();

            if(loc.lclass() == LOC_GMEMBER)
            {
                gmember_t const& read = *loc.gmember();
                assert(read.gvar.global.gclass() == GLOBAL_VAR);

                // Reads only have effect if something actually uses them:
                for(unsigned i = 0; i < ssa_it->output_size(); ++i)
                {
                    auto oe = ssa_it->output_edge(i);
                    // TODO: verify this is correct
                    if(!is_locator_write(oe) || oe.handle->input(oe.index + 1) != loc)
                    {
                        reads.set(read.index);
                        group_vars.set(read.gvar.group_vars.value);
                        immediate_groups.set(read.gvar.group_vars->group.handle().value);
                        break;
                    }
                }
            }
        }

        // TODO: add pointers to ir_group_vars and ir_immediate_groups?
    }

    m_ir_writes = std::move(writes);
    m_ir_reads  = std::move(reads);
    m_ir_group_vars = std::move(group_vars);
    m_ir_calls = std::move(calls);
    m_ir_immediate_groups = std::move(immediate_groups);
    m_ir_io_pure = io_pure;
}

void fn_t::assign_lvars(lvars_manager_t&& lvars)
{
    assert(compiler_phase() == PHASE_COMPILE);
    m_lvars = std::move(lvars);
    m_lvar_spans.clear();
    m_lvar_spans.resize(m_lvars.num_this_lvars());
}

void fn_t::mask_usable_ram(ram_bitset_t const& mask)
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);
    m_usable_ram &= mask;
}

void fn_t::assign_lvar_span(unsigned lvar_i, span_t span)
{
    assert(lvar_i < m_lvar_spans.size()); 
    assert(!m_lvar_spans[lvar_i]);

    m_lvar_spans[lvar_i] = span;
    m_lvar_ram |= ram_bitset_t::filled(span.size, span.addr);
    m_usable_ram -= m_lvar_ram;
}

span_t fn_t::lvar_span(unsigned lvar_i) const
{
    assert(lvar_i < m_lvars.num_all_lvars());

    if(lvar_i < m_lvars.num_this_lvars())
        return m_lvar_spans[lvar_i];

    locator_t const loc = m_lvars.locator(lvar_i);
    if(lvars_manager_t::is_call_lvar(handle(), loc))
    {
        int index = loc.fn()->m_lvars.index(loc);

        if(!loc.fn()->m_lvars.is_lvar(index))
            return {};

        return loc.fn()->lvar_span(index);
    }

    throw std::runtime_error("Unknown lvar span");
}

/* TODO: remove
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

void fn_t::compile()
{
    assert(compiler_phase() == PHASE_COMPILE);

    // Dethunkify the fn type:
    {
        type_t* types = ALLOCA_T(type_t, def().num_params + 1);
        for(unsigned i = 0; i != def().num_params; ++i)
            types[i] = ::dethunkify(def().local_vars[i].src_type, true);
        types[def().num_params] = ::dethunkify(def().return_type, true);
        m_type = type_t::fn(types, types + def().num_params + 1);
    }

    if(fclass == FN_CT)
        return; // Nothing else to do!

    for(unsigned i = 0; i < def().num_params; ++i)
    {
        auto const& decl = def().local_vars[i];
        if(is_ct(decl.src_type.type))
            compiler_error(decl.src_type.pstring, fmt("Function must be declared as ct to use type %.", decl.src_type.type));
    }
    if(is_ct(def().return_type.type))
        compiler_error(def().return_type.pstring, fmt("Function must be declared as ct to use type %.", def().return_type.type));

    // Compile the FN.
    ssa_pool::clear();
    cfg_pool::clear();
    ir_t ir;
    build_ir(ir, *this);

    auto const save_graph = [&](ir_t& ir, char const* suffix)
    {
        std::ofstream ocfg(fmt("graphs/cfg/%_cfg_%.gv", global.name, suffix));
        if(ocfg.is_open())
            graphviz_cfg(ocfg, ir);

        std::ofstream ossa(fmt("graphs/%_ssa_%.gv", global.name, suffix));
        if(ossa.is_open())
            graphviz_ssa(ossa, ir);
    };


    if(compiler_options().graphviz)
        save_graph(ir, "initial");

    ir.assert_valid();

    {
        bool changed;
        do
        {
            changed = false;
            changed |= o_phis(ir);
            changed |= o_merge_basic_blocks(ir);
            changed |= o_remove_unused_arguments(ir, *this, false);
            changed |= o_identities(ir);
            changed |= o_abstract_interpret(ir);
            changed |= o_remove_unused_ssa(ir);
            changed |= o_global_value_numbering(ir);
        }
        while(changed);
    }

    // Set the global's 'read' and 'write' bitsets:
    calc_ir_bitsets(ir);

    if(compiler_options().graphviz)
        save_graph(ir, "o1");

    byteify(ir, *this);
    //make_conventional(ir);

    if(compiler_options().graphviz)
        save_graph(ir, "byteify");

    {
        bool changed;
        do
        {
            changed = false;
            changed |= o_phis(ir);
            changed |= o_merge_basic_blocks(ir);
            changed |= o_remove_unused_arguments(ir, *this, true);
            changed |= o_identities(ir);
            changed |= o_abstract_interpret(ir);
            changed |= o_remove_unused_ssa(ir);
            changed |= o_global_value_numbering(ir);

        }
        while(changed);
    }

    if(compiler_options().graphviz)
        save_graph(ir, "o2");

    code_gen(ir, *this);

    if(compiler_options().graphviz)
        save_graph(ir, "cg");

    return;

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
         tinglerz bar   for(ssa_ht h : schedule_cfg_node(cfg_it))
                std::cout << h.index << ' ' << h->op() << '\n';
        }
    }
    */
}

/* TODO: remove?
void fn_t::for_each_param_member(bool atoms, std::function<void(type_t, locator_t)> const& fn) const
{
    assert(global.compiled());

    fn_ht const h = handle();

    unsigned const num_params = type().num_params();
    for(unsigned arg = 0; arg < num_params; ++arg)
    {
        type_t const param_type = fn.type().type(argn);
        unsigned const num_param_members = ::num_members(param_type);

        for(unsigned m = 0; m < num_param_members; ++m)
        {
            type_t const mt = ::member_type(param_type, m);

            if(atoms)
            {
                unsigned const num_atoms = ::num_atoms(mt);
                for(unsigned atom = 0; atom < num_atoms; ++atom)
                    fn(mt, locator_t::arg(h, m, atom));
            }
            else
                fn(mt, locator_t::arg(h, m));
        }
    }
}
*/

////////////
// gvar_t //
////////////

void gvar_t::set_gmember_range(gmember_ht begin, gmember_ht end)
{
    assert(compiler_phase() == PHASE_COUNT_MEMBERS);
    m_begin_gmember = begin;
    m_end_gmember = end;
}

void gvar_t::dethunkify(bool full)
{
    assert(compiler_phase() == PHASE_COMPILE || compiler_phase() == PHASE_COUNT_MEMBERS);
    m_src_type.type = ::dethunkify(m_src_type, full);
}

void gvar_t::compile()
{
    assert(compiler_phase() == PHASE_COMPILE);

    dethunkify(true);

    if(init_expr)
    {
        assert(!is_paa(m_src_type.type.name()));
        spair_t spair = interpret_expr(global.pstring(), init_expr, m_src_type.type);
        m_sval = std::move(spair.value);
        m_src_type.type = std::move(spair.type); // Handles unsized arrays
    }
}

void gvar_t::for_each_locator(std::function<void(locator_t)> const& fn) const
{
    assert(compiler_phase() > PHASE_COMPILE);

    for(gmember_ht h = begin_gmember(); h != end_gmember(); ++h)
    {
        unsigned const num = num_atoms(h->type());
        for(unsigned atom = 0; atom < num; ++atom)
            fn(locator_t::gmember(h, atom));
    }
}

///////////////
// gmember_t //
///////////////

void gmember_t::alloc_spans()
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);
    assert(m_spans.empty());
    m_spans.resize(num_atoms(type()));
}

/////////////
// const_t //
/////////////

group_ht const_t::group() const { return group_data->group.handle(); }

void const_t::compile()
{
    std::puts("start");
    m_src_type.type = ::dethunkify(m_src_type, true);
    std::puts("ok");
    assert(init_expr);

    if(m_src_type.type.name() == TYPE_PAA)
    {
        rom_array_t paa = { interpret_paa(global.pstring(), init_expr) };

        unsigned const def_length = m_src_type.type.array_length();
        if(def_length && def_length != paa.data.size())
             compiler_error(m_src_type.pstring, fmt("Length of data (%) does not match its type %.", paa.data.size(), m_src_type.type));

        m_src_type.type.set_array_length(paa.data.size());
        // TODO : remove?
        //m_sval = { lookup_rom_array({}, group_data, std::move(paa)) };
    }
    else
    {
        spair_t spair = interpret_expr(global.pstring(), init_expr, m_src_type.type);
        m_sval = std::move(spair.value);
        m_src_type.type = std::move(spair.type); // Handles unsized arrays
    }

    // TODO: remove all this
    /*
    if(ssa_value_t const* v = std::get_if<ssa_value_t>(&m_sval[0]))
        std::printf("%s = %i\n", global.name.data(), v->whole());
    else if(ct_array_t const* a = std::get_if<ct_array_t>(&m_sval[0]))
    {
        unsigned tea_size = m_src_type.type.array_length();
        for(unsigned i = 0; i < tea_size; ++i)
            std::printf("%s[%u] = %i\n", global.name.data(), i, (*a)[i].whole());

    }
    */
}

//////////////
// struct_t //
//////////////

unsigned struct_t::count_members()
{
    assert(compiler_phase() == PHASE_COUNT_MEMBERS);

    if(m_num_members != UNCOUNTED)
        return m_num_members;

    unsigned count = 0;

    for(unsigned i = 0; i < fields().size(); ++i)
    {
        type_t type = const_cast<type_t&>(field(i).type()) = dethunkify(field(i).decl.src_type, false);

        if(is_tea(type.name()))
            type = type.elem_type();

        if(type.name() == TYPE_STRUCT)
        {
            struct_t& s = const_cast<struct_t&>(type.struct_());
            s.count_members();
            assert(s.m_num_members != UNCOUNTED);
            count += s.m_num_members;
        }
        else
        {
            assert(!is_aggregate(type.name()));
            ++count;
        }
    }

    return m_num_members = count;
}

void struct_t::compile()
{
    assert(compiler_phase() == PHASE_COMPILE);

    // Dethunkify
    for(unsigned i = 0; i < fields().size(); ++i)
        const_cast<type_t&>(field(i).type()) = dethunkify(field(i).decl.src_type, true);

    gen_member_types(*this, 0);
}

// Builds 'm_member_types', sets 'm_has_array_member', and dethunkifies the struct.
void struct_t::gen_member_types(struct_t const& s, unsigned tea_size)
{
    for(unsigned i = 0; i < fields().size(); ++i)
    {
        type_t type = s.field(i).type();

        if(type.name() == TYPE_TEA)
        {
            tea_size = type.size();
            type = type.elem_type();
        }

        assert(!is_thunk(type.name()));

        if(type.name() == TYPE_STRUCT)
        {
            assert(&type.struct_() != &s);
            gen_member_types(type.struct_(), tea_size);
        }
        else
        {
            assert(!is_aggregate(type.name()));
            assert(tea_size <= 256);

            if(tea_size)
            {
                m_member_types.push_back(type_t::tea(type, tea_size));
                m_has_tea_member = true;
            }
            else
                m_member_types.push_back(type);
        }
    }
}

////////////////////
// free functions //
////////////////////

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
