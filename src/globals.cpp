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
#include "rom.hpp"

global_t& global_t::lookup(char const* source, pstring_t name)
{
    std::string_view view = name.view(source);

    std::uint64_t const hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    return *global_ht::with_pool([&, hash, view, source](auto& pool)
    {
        rh::apair<global_t**, bool> result = global_pool_map.emplace(hash,
            [view](global_t* ptr) -> bool
            {
                return std::equal(view.begin(), view.end(), 
                                  ptr->name.begin(), ptr->name.end());
            },
            [&pool, name, source]() -> global_t*
            { 
                return &pool.emplace_back(name, source);
            });

        return *result.first;
    });
}

global_t const* global_t::lookup_sourceless(std::string_view view)
{
    std::uint64_t const hash = fnv1a<std::uint64_t>::hash(view.data(), view.size());

    return global_ht::with_const_pool([&, hash, view](auto const&)
    {
        auto result = global_pool_map.lookup(hash,
            [view](global_t* ptr) -> bool
            {
                return std::equal(view.begin(), view.end(), 
                                  ptr->name.begin(), ptr->name.end());
            });

        return result.second ? *result.second : nullptr;
    });
}

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
                file_contents_t file(pstring.file_i);
                throw compiler_error_t(
                    fmt_error(pstring, fmt("Global identifier % already in use.", 
                                           pstring.view(file.source())), &file)
                    + fmt_error(m_pstring, "Previous definition here:"));
            }
            else
                throw compiler_error_t(fmt("Global identifier % already in use.", name));
        }

        m_gclass = gclass;
        assert(pstring);
        m_pstring = pstring; // Not necessary but useful for error reporting.
        m_impl_id = ret = create_impl(*this);
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
                          type_t type, fn_def_t&& fn_def, mods_t&& mods, fn_class_t fclass)
{
    fn_t* ret;

    // Create the fn
    define(pstring, GLOBAL_FN, std::move(ideps), std::move(weak_ideps), [&](global_t& g)
    { 
        return fn_ht::pool_emplace(
            ret, g, type, std::move(fn_def), std::move(mods), fclass).id; 
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
        return gvar_ht::pool_emplace(ret, g, src_type, group.second, expr).id;
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
        return const_ht::pool_emplace(ret, g, src_type, group.second, expr).id;
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
    define(pstring, GLOBAL_STRUCT, std::move(ideps), {}, [&](global_t& g)
    { 
        return struct_ht::pool_emplace(ret, g, std::move(fields)).id;
    });
    
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

    // Verify groups are created:
    for(group_t const& group : group_ht::values())
        if(group.gclass() == GROUP_UNDEFINED)
            compiler_error(group.pstring(), "Group name not in scope.");

    // Handle weak ideps:
    for(global_t& global : global_ht::values())
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

    // Validate groups:
    for(fn_t const& fn : fn_ht::values())
    {
        if(fn.fclass == FN_MODE && !fn.mods.explicit_group_vars)
            compiler_error(fn.global.pstring(), "Missing vars modifier.");

        fn.mods.validate_groups();

        for(mods_t const& mods : fn.def().mods)
            mods.validate_groups();
    }
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::precheck()
{
    assert(compiler_phase() == PHASE_PRECHECK);

    for(fn_t& fn : fn_ht::values())
        fn.precheck_eval();
    for(fn_t& fn : fn_ht::values())
        fn.precheck_propagate();
    for(fn_t const* fn : modes())
        fn->precheck_verify();
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
                error_msgs.push_back(fmt_error(global.m_pstring, "Mutually recursive with:"));
                return error;
            }

            std::string msg = fmt_error(global.m_pstring,
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
    for(struct_t& s : struct_ht::values())
        total_members += s.count_members();

    gmember_ht::with_pool([total_members](auto& pool){ pool.reserve(total_members); });

    for(gvar_t& gvar : gvar_ht::values())
    {
        gvar.dethunkify(false);

        gmember_ht const begin = { gmember_ht::with_pool([](auto& pool){ return pool.size(); }) };

        unsigned const num = ::num_members(gvar.type());
        for(unsigned i = 0; i < num; ++i)
            gmember_ht::with_pool([&](auto& pool){ pool.emplace_back(gvar, pool.size()); });

        gmember_ht const end = { gmember_ht::with_pool([](auto& pool){ return pool.size(); }) };

        gvar.set_gmember_range(begin, end);
    }
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::build_order()
{
    assert(compiler_phase() == PHASE_ORDER_GLOBALS);

    // Detect cycles
    for(global_t& global : global_ht::values())
    {
        std::vector<std::string> error_msgs;
        detect_cycle(global, error_msgs);
    }

    for(global_t& global : global_ht::values())
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

    globals_left = global_ht::pool().size();

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

//////////
// fn_t //
///////////

fn_t::fn_t(global_t& global, type_t type, fn_def_t&& fn_def, mods_t&& mods, fn_class_t fclass) 
: global(global)
, fclass(fclass)
, mods(std::move(mods))
, m_type(std::move(type))
, m_def(std::move(fn_def)) 
, m_rom_proc(rom_proc_ht::pool_make())
{
    switch(fclass)
    {
    default:
        break;
    case FN_MODE:
        m_pimpl.reset(new mode_impl_t());
        break;
    }
}

void fn_t::calc_ir_bitsets(ir_t const& ir)
{
    bitset_t  reads(gmember_ht::bitset_size());
    bitset_t writes(gmember_ht::bitset_size());
    bitset_t group_vars(group_vars_ht::bitset_size());
    bitset_t deref_groups(group_ht::bitset_size());
    bitset_t calls(fn_ht::bitset_size());
    bool io_pure = true;

    // Handle preserved groups
    for(auto const& fn_stmt : m_eval_tracked->goto_modes)
    {
        if(mods_t const* goto_mods = def().mods_of(fn_stmt.second))
        {
            goto_mods->for_each_group_vars([&](group_vars_ht gv)
            {
                group_vars.set(gv.id);
                reads |= gv->gmembers();
            });
        }
    }

    // Iterate the IR looking for reads and writes
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
            calls.set(callee_h.id);
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
                        group_vars.set(written.gvar.group_vars.id);
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
                        group_vars.set(read.gvar.group_vars.id);
                        break;
                    }
                }
            }
        }

        if(ssa_flags(ssa_it->op()) & SSAF_INDEXES_PTR)
        {
            io_pure = false;

            type_t const ptr_type = ssa_it->input(0).type();
            assert(is_ptr(ptr_type.name()));

            unsigned const size = ptr_type.group_tail_size();
            for(unsigned i = 0; i < size; ++i)
            {
                // Set 'deref_groups':
                group_ht const h = ptr_type.group(i);
                deref_groups.set(h.id);

                // Also update 'ir_group_vars':
                group_t const& group = *h;
                if(group.gclass() == GROUP_VARS)
                    group_vars.set(group.handle<group_vars_ht>().id);
            }
        }
    }

    m_ir_writes = std::move(writes);
    m_ir_reads  = std::move(reads);
    m_ir_group_vars = std::move(group_vars);
    m_ir_calls = std::move(calls);
    m_ir_deref_groups = std::move(deref_groups);
    m_ir_io_pure = io_pure;
}

void fn_t::assign_lvars(lvars_manager_t&& lvars)
{
    assert(compiler_phase() == PHASE_COMPILE);
    m_lvars = std::move(lvars);
    m_lvar_spans.clear();
    m_lvar_spans.resize(m_lvars.num_this_lvars());
}

/*
void fn_t::mask_usable_ram(ram_bitset_t const& mask)
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);
    m_usable_ram &= mask;
}
*/

void fn_t::assign_lvar_span(unsigned lvar_i, span_t span)
{
    assert(lvar_i < m_lvar_spans.size()); 
    assert(!m_lvar_spans[lvar_i]);

    m_lvar_spans[lvar_i] = span;
}

span_t fn_t::lvar_span(int lvar_i) const
{
    assert(lvar_i < int(m_lvars.num_all_lvars()));

    if(lvar_i < int(m_lvars.num_this_lvars()))
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

span_t fn_t::lvar_span(locator_t loc) const
{
    return lvar_span(m_lvars.index(loc));
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
        if(!compiler_options().graphviz)
            return;

        std::ofstream ocfg(fmt("graphs/cfg__%__%.gv", global.name, suffix));
        if(ocfg.is_open())
            graphviz_cfg(ocfg, ir);

        std::ofstream ossa(fmt("graphs/ssa__%__%.gv", global.name, suffix));
        if(ossa.is_open())
            graphviz_ssa(ossa, ir);
    };

    auto const optimize_suite = [&](bool post_byteified)
    {
        unsigned iter = 0;
        bool changed;
        do
        {
            changed = false;
            changed |= o_phis(ir);
            changed |= o_merge_basic_blocks(ir);
            changed |= o_remove_unused_arguments(ir, *this, post_byteified);
            changed |= o_identities(ir, nullptr);
            changed |= o_abstract_interpret(ir, nullptr);
            changed |= o_remove_unused_ssa(ir);
            changed |= o_global_value_numbering(ir, nullptr);

            if(post_byteified)
            {
                // Once byteified, keep shifts out of the IR and only use rotates.
                changed |= shifts_to_rotates(ir);
            }

            // Enable this to debug:
            //save_graph(ir, fmt("during_o_%", iter).c_str());
            ++iter;
        }
        while(changed);
    };

    save_graph(ir, "1_initial");
    ir.assert_valid();

    optimize_suite(false);
    save_graph(ir, "2_o1");

    // Set the global's 'read' and 'write' bitsets:
    calc_ir_bitsets(ir);

    byteify(ir, *this);
    save_graph(ir, "3_byteify");

    optimize_suite(true);
    save_graph(ir, "4_o2");

    code_gen(ir, *this);
    save_graph(ir, "5_cg");
}

void fn_t::precheck_verify() const
{
    assert(fclass == FN_MODE);

    auto& pimpl = this->pimpl<mode_impl_t>();

    for(auto const& pair : pimpl.incoming_preserved_groups)
    {
        if(pair.first->gclass() != GROUP_VARS)
            continue;

        if(!precheck_group_vars().test(pair.first->impl_id()))
        {
            std::string groups = "";
            precheck_group_vars().for_each<group_vars_ht>([&groups](auto gv)
            {
                groups += gv->group.name;
            });
            if(groups.empty())
                groups = "(no groups)";
            else
                groups = "vars " + groups;

            throw compiler_error_t(
                fmt_error(pair.second, 
                    fmt("Unable to preserve % as mode % is % excluding it.", 
                        pair.first->name, global.name,
                        mods.explicit_group_vars ? "explictly" : "implicitly"))
                + fmt_note(global.pstring(), fmt("% includes: %", 
                                                 global.name, groups)));
        }
    }
}

void fn_t::precheck_eval()
{
    assert(compiler_phase() == PHASE_PRECHECK);
    assert(!m_eval_tracked);

    // Run the evaluator to generate 'm_eval_tracked':
    m_eval_tracked.reset(new eval_tracked_t(build_tracked(*this)));

    std::cout << "PRECHECK\n";
    for(auto const& pair : m_eval_tracked->gvars_used)
        std::cout << "GVAR USED " << pair.first->global.name << std::endl;
}

void fn_t::precheck_propagate()
{
    assert(compiler_phase() == PHASE_PRECHECK);

    if(m_precheck_group_vars)
        return;

    unsigned const bs_size = group_vars_ht::bitset_size();

    m_precheck_group_vars.reset(bs_size);

    // For efficiency, we'll convert the mod groups into a bitset.
    bitset_uint_t* temp_bs = ALLOCA_T(bitset_uint_t, bs_size);
    bitset_uint_t* mod_group_vars = ALLOCA_T(bitset_uint_t, bs_size);

    bitset_clear_all(bs_size, mod_group_vars);
    if(mods.explicit_group_vars)
    {
        for(auto const& pair : mods.group_vars)
            bitset_set(mod_group_vars, pair.first->impl_id());
        bitset_or(bs_size, m_precheck_group_vars.data(), mod_group_vars);
    }

    auto const group_vars_to_string = [bs_size](bitset_uint_t const* bs)
    {
        std::string str;
        bitset_for_each<group_vars_ht>(bs_size, bs, [&str](auto gv)
            { str += gv->group.name; });
        return str;
    };

    auto const group_map_to_string = [](auto const& map)
    {
        std::string str;
        for(auto const& pair : map)
            str += pair.first->name;
        return str;
    };

    // Handle accesses through pointers:
    for(auto const& pair : m_eval_tracked->deref_groups)
    {
        auto const error = [&](group_class_t gclass, auto& groups)
        {
            type_t const type = pair.second.type;

            char const* const keyword = group_class_keyword(gclass);

            std::string const msg = fmt(
                "% access requires groups that are excluded from % (% %).",
                type, global.name, keyword, group_map_to_string(groups));

            std::string missing = "";
            for(unsigned i = 0; i < type.group_tail_size(); ++i)
            {
                group_ht const group = type.group(i);
                if(group->gclass() == gclass && !groups.count(group))
                    missing += group->name;
            }

            throw compiler_error_t(
                fmt_error(pair.second.pstring, msg)
                + fmt_note(fmt("Excluded groups: % %", keyword, missing)));
        };

        if(pair.first->gclass() == GROUP_VARS)
        {
            if(mods.explicit_group_vars && !mods.group_vars.count(pair.first))
                error(GROUP_VARS, mods.group_vars);

            m_precheck_group_vars.set(pair.first->impl_id());
        }
        else if(pair.first->gclass() == GROUP_DATA)
        {
            if(mods.explicit_group_data && !mods.group_data.count(pair.first))
                error(GROUP_DATA, mods.group_data);
        }
    }

    // Handle accesses through goto modes:
    for(auto const& fn_stmt : m_eval_tracked->goto_modes)
    {
        stmt_t const& stmt = def()[fn_stmt.second];
        mods_t const* goto_mods = def().mods_of(fn_stmt.second);

        if(!goto_mods || !goto_mods->explicit_group_vars)
            compiler_error(stmt.pstring, "Missing vars modifier.");

        if(!goto_mods)
            continue;

        // Track incoming, for the called mode: 
        auto& call_pimpl = fn_stmt.first->pimpl<mode_impl_t>();
        {
            std::lock_guard<std::mutex> lock(call_pimpl.incoming_preserved_groups_mutex);
            call_pimpl.incoming_preserved_groups.insert(
                goto_mods->group_vars.begin(), goto_mods->group_vars.end());
        }

        // Handle our own groups:
        for(auto const& pair : goto_mods->group_vars)
        {
            if(pair.first->gclass() == GROUP_VARS)
            {
                if(mods.explicit_group_vars && !mods.group_vars.count(pair.first))
                {
                    std::string const msg = fmt(
                        "Preserved groups are excluded from % (vars %).",
                        global.name, group_map_to_string(mods.group_vars));

                    std::string missing = "";
                    for(auto const& pair : goto_mods->group_vars)
                        if(pair.first->gclass() == GROUP_VARS && !mods.group_vars.count(pair.first))
                            missing += pair.first->name;

                    throw compiler_error_t(
                        fmt_error(stmt.pstring, msg)
                        + fmt_note(fmt("Excluded groups: vars %", missing)));
                }

                m_precheck_group_vars.set(pair.first->impl_id());
            }
        }
    }

    // Handle direct dependencies.

    {
        auto const error = [&](global_t const& idep, 
                               std::string const& dep_group_string, 
                               std::string const& missing)
        {
            std::string const msg = fmt(
                "% (vars %) requires groups that are excluded from % (vars %).",
                idep.name, dep_group_string, global.name, 
                group_vars_to_string(mod_group_vars));

            if(pstring_t const pstring = def().find_global(&idep))
            {
                throw compiler_error_t(
                    fmt_error(pstring, msg)
                    + fmt_note(fmt("Excluded groups: vars %", missing)));
            }
            else // This shouldn't occur, but just in case...
                compiler_error(global.pstring(), msg);
        };

        for(auto const& pair : m_eval_tracked->gvars_used)
        {
            group_ht const group = pair.first->group();

            if(mods.explicit_group_vars && !mods.group_vars.count(group))
                error(pair.first->global, group->name, group->name);

            m_precheck_group_vars.set(group.id);
        }

        for(auto const& pair : m_eval_tracked->calls)
        {
            fn_t& call = *pair.first;

            call.precheck_propagate(); // Recurse

            assert(call.fclass != FN_MODE);
            assert(call.m_precheck_group_vars);

            bitset_copy(bs_size, temp_bs, call.m_precheck_group_vars.data());
            bitset_difference(bs_size, temp_bs, mod_group_vars);

            if(mods.explicit_group_vars && !bitset_all_clear(bs_size, temp_bs))
            {
                error(call.global,
                      group_vars_to_string(call.precheck_group_vars().data()),
                      group_vars_to_string(temp_bs));
            }

            m_precheck_group_vars |= call.m_precheck_group_vars;
        }
    }
}

/*
void fn_t::calc_lang_gvars()
{
    assert(0); // TODO: handle global_modes
    assert(compiler_phase() < PHASE_COMPILE);

    if(m_lang_gvars)
        return;

    m_lang_gvars.reset(gvar_ht::bitset_size());

    for(global_t* idep : global.ideps())
    {
        if(idep->gclass() == GLOBAL_VAR)
            m_lang_gvars.set(idep->impl_id());
        else if(idep->gclass() == GLOBAL_FN)
        {
            fn_t& fn = idep->impl<fn_t>();

            if(fn.fclass == FN_MODE)
                continue;

            // Recurse to ensure the called fn is calculated:
            fn.calc_lang_gvars();
            assert(fn.m_lang_gvars);

            m_lang_gvars |= fn.m_lang_gvars;
        }
    }

    // For every 'goto mode' statement, track preserved groups.
    for(stmt_t const& stmt : def().stmts)
    {
        if(stmt.name != STMT_GOTO_MODE)
            continue;

        if(mods_t const* mods = def()[stmt.mods])
        {
            if(!mods->explicit_group_vars)
                continue;

            // Lazily allocate.
            if(!m_lang_preserves_group_vars)
                m_lang_preserves_group_vars.reset(group_vars_ht::bitset_size());
            m_lang_preserves_group_vars.set(pair.first->impl_id());
        }
    }
}
*/


////////////////////
// global_datum_t //
////////////////////

void global_datum_t::dethunkify(bool full)
{
    assert(compiler_phase() == PHASE_COMPILE || compiler_phase() == PHASE_COUNT_MEMBERS);
    m_src_type.type = ::dethunkify(m_src_type, full);
}

void global_datum_t::compile()
{
    assert(compiler_phase() == PHASE_COMPILE);

    dethunkify(true);

    if(!init_expr)
        return;

    if(m_src_type.type.name() == TYPE_PAA)
    {
        loc_vec_t paa = interpret_paa(global.pstring(), init_expr);

        unsigned const def_length = m_src_type.type.array_length();
        if(def_length && def_length != paa.size())
             compiler_error(m_src_type.pstring, fmt("Length of data (%) does not match its type %.", paa.size(), m_src_type.type));

        m_src_type.type.set_array_length(paa.size());
        paa_init(std::move(paa));
        //m_rom_array = lookup_rom_array({}, group(), std::move(paa));

        // TODO : remove?
        //m_sval = { m_paa };
    }
    else
    {
        spair_t spair = interpret_expr(global.pstring(), init_expr, m_src_type.type);
        m_src_type.type = std::move(spair.type); // Handles unsized arrays
        sval_init(std::move(spair.value));
    }
}

////////////
// gvar_t //
////////////

group_ht gvar_t::group() const { return group_vars->group.handle(); }

void gvar_t::paa_init(loc_vec_t&& paa)
{
    m_init_data = std::move(paa);
}

void gvar_t::sval_init(sval_t&& sval)
{
    m_sval = std::move(sval);

    m_init_data.clear();
    append_locator_bytes(m_init_data, m_sval, m_src_type.type, global.pstring());
}

void gvar_t::set_gmember_range(gmember_ht begin, gmember_ht end)
{
    assert(compiler_phase() == PHASE_COUNT_MEMBERS);
    m_begin_gmember = begin;
    m_end_gmember = end;
}

void gvar_t::for_each_locator(std::function<void(locator_t)> const& fn) const
{
    assert(compiler_phase() > PHASE_COMPILE);

    for(gmember_ht h = begin(); h != end(); ++h)
    {
        unsigned const num = num_atoms(h->type(), 0);
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
    m_spans.resize(num_atoms(type(), 0));
}

locator_t const* gmember_t::init_data(unsigned atom) const
{
    unsigned const size = init_size();

    if(is_ptr(type().name()))
    {
        assert(atom <= 1);
        assert(size == 2);
        atom = 0;
    }

    unsigned const offset = ::member_offset(gvar.type(), member());
    return gvar.init_data().data() + offset + (atom * size);
}

std::size_t gmember_t::init_size() const
{
    if(is_ptr(type().name()))
    {
        assert(!is_banked_ptr(type().name()));
        return 2;
    }
    return ::num_offsets(type());
}

bool gmember_t::zero_init(unsigned atom) const
{
    if(!gvar.init_expr)
        return false;

    std::size_t const size = init_size();
    locator_t const* data = init_data(atom);

    for(unsigned i = 0; i < size; ++i)
        if(!data[i].eq_const(0))
            return false;

    return true;

}

/////////////
// const_t //
/////////////

group_ht const_t::group() const { return group_data->group.handle(); }

void const_t::paa_init(loc_vec_t&& paa)
{
    m_rom_array = rom_array_t::make(std::move(paa), group_data);
    assert(m_rom_array);
}

void const_t::sval_init(sval_t&& sval)
{
    m_sval = std::move(sval);
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
    std::uint16_t offset = 0;
    
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
                type = type_t::tea(type, tea_size);
                m_has_tea_member = true;
            }

            m_member_types.push_back(type);
            m_member_offsets.push_back(offset);

            offset += type.size_of();
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

fn_t const& get_main_entry()
{
    assert(compiler_phase() > PHASE_PARSE);

    using namespace std::literals;
    global_t const* main_entry = global_t::lookup_sourceless("main"sv);

    if(!main_entry || main_entry->gclass() != GLOBAL_FN || main_entry->impl<fn_t>().fclass != FN_MODE)
        throw compiler_error_t("Missing definition of mode main. Program has no entry point.");

    fn_t const& main_fn = main_entry->impl<fn_t>();
    if(main_fn.def().num_params > 0)
        compiler_error(main_fn.def().local_vars[0].name, "Mode main cannot have parameters.");

    return main_fn;
}
