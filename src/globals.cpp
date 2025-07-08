#include "globals.hpp"

#include <ostream>
#include <fstream>
#ifndef NDEBUG
#include <iostream>
#endif

#include "alloca.hpp"
#include "bitset.hpp"
#include "compiler_error.hpp"
#include "fnv1a.hpp"
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
#include "ir_util.hpp"
#include "ir_algo.hpp"
#include "debug_print.hpp"
#include "text.hpp"
#include "switch.hpp"

//////////////
// global_t //
//////////////

// Changes a global from UNDEFINED to some specified 'gclass'.
// This gets called whenever a global is parsed.
unsigned global_t::define(lpstring_t lpstring, global_class_t gclass, 
                          ideps_map_t&& ideps, std::function<unsigned(global_t&)> create_impl)
{
    assert(compiler_phase() <= PHASE_PARSE);
    unsigned ret;
    {
        std::lock_guard<std::mutex> global_lock(m_define_mutex);
        if(m_gclass != GLOBAL_UNDEFINED)
        {
            if(gclass == GLOBAL_FN_SET && m_gclass == GLOBAL_FN_SET)
            {
                m_ideps.insert(ideps.begin(), ideps.end());
                return m_impl_id;
            }

            if(lpstring && m_lpstring)
            {
                file_contents_t file(lpstring.file_i);
                throw compiler_error_t(
                    fmt_error(lpstring, fmt("Global identifier % already in use.", name), &file)
                    + fmt_note(m_lpstring, "Previous definition here:"));
            }
            else
                throw compiler_error_t(fmt("Global identifier % already in use.", name));
        }

        m_gclass = gclass;
        assert(lpstring);
        m_lpstring = lpstring; // Not necessary but useful for error reporting.
        m_impl_id = ret = create_impl(*this);
        m_ideps = std::move(ideps);
    }
    ideps.clear();
    return ret;
}

fn_ht global_t::define_fn(lpstring_t lpstring, ideps_map_t&& ideps,
                          type_t type, fn_def_t&& fn_def, std::unique_ptr<mods_t> mods, 
                          fn_class_t fclass, bool iasm, fn_set_t* fn_set)
{
    fn_t* ret;

    // Create the fn
    fn_ht h = { define(lpstring, GLOBAL_FN, std::move(ideps), [&](global_t& g)
    { 
        return fn_ht::pool_emplace(
            ret, g, type, std::move(fn_def), std::move(mods), fclass, iasm, fn_set).id; 
    }) };

    if(fclass == FN_MODE)
    {
        std::lock_guard<std::mutex> lock(modes_vec_mutex);
        modes_vec.push_back(ret);
    }
    else if(fclass == FN_NMI)
    {
        std::lock_guard<std::mutex> lock(nmi_vec_mutex);
        nmi_vec.push_back(ret);
    }
    else if(fclass == FN_IRQ)
    {
        std::lock_guard<std::mutex> lock(irq_vec_mutex);
        irq_vec.push_back(ret);
    }

    return h;
}

gvar_ht global_t::define_var(lpstring_t lpstring, ideps_map_t&& ideps, 
                             src_type_t src_type, defined_group_vars_t d,
                             ast_node_t const* expr, std::unique_ptr<paa_def_t> paa_def, std::unique_ptr<mods_t> mods)
{
    gvar_t* ret;

    // Create the var
    gvar_ht h = { define(lpstring, GLOBAL_VAR, std::move(ideps), [&](global_t& g)
    { 
        return gvar_ht::pool_emplace(ret, g, src_type, d.vars_handle, expr, std::move(paa_def), std::move(mods)).id;
    })};

    // Add it to the group
    if(d.vars)
        d.vars->add_gvar(h);
    else
    {
        std::lock_guard lock(gvar_t::m_groupless_gvars_lock);
        gvar_t::m_groupless_gvars.push_back(h);
    }
    
    return h;
}

const_ht global_t::define_const(lpstring_t lpstring, ideps_map_t&& ideps, 
                                src_type_t src_type, defined_group_data_t d, bool omni, ast_node_t const* chrrom,
                                ast_node_t const* expr, std::unique_ptr<paa_def_t> paa_def,
                                std::unique_ptr<mods_t> mods)
{
    const_t* ret;

    // Create the const
    const_ht h = { define(lpstring, GLOBAL_CONST, std::move(ideps), [&](global_t& g)
    { 
        return const_ht::pool_emplace(ret, g, src_type, d.data_handle, !omni, chrrom, expr, std::move(paa_def), std::move(mods)).id;
    })};

    // Add it to the group
    if(d.data)
        d.data->add_const(h);
    
    return h;
}

struct_ht global_t::define_struct(lpstring_t lpstring, ideps_map_t&& ideps,
                                  field_map_t&& fields)
{
    struct_t* ret;

    // Create the struct
    struct_ht h = { define(lpstring, GLOBAL_STRUCT, std::move(ideps), [&](global_t& g)
    { 
        return struct_ht::pool_emplace(ret, g, std::move(fields)).id;
    }) };
    
    return h;
}

charmap_ht global_t::define_charmap(
        lpstring_t lpstring, bool is_default, 
        string_literal_t const& characters, 
        string_literal_t const& sentinel,
        unsigned offset,
        std::unique_ptr<mods_t> mods)
{
    charmap_t* ret;

    // Create the charmap
    charmap_ht h = { define(lpstring, GLOBAL_CHARMAP, {}, [&](global_t& g)
    { 
        return charmap_ht::pool_emplace(ret, g, is_default, characters, sentinel, offset, std::move(mods)).id;
    }) };

    return h;
}

fn_set_t& global_t::define_fn_set(lpstring_t lpstring)
{
    fn_set_t* ret = nullptr;

    // Create the fn set
    fn_set_ht h = { define(lpstring, GLOBAL_FN_SET, {}, [&](global_t& g)
    { 
        return fn_set_ht::pool_emplace(ret, g).id;
    }) };

    return ret ? *ret : h.safe();
}

global_t& global_t::default_charmap(pstring_t at)
{
    using namespace std::literals;
    static TLS global_t* result = nullptr;
    if(!result)
        result = &lookup_sourceless(extend(at), "charmap"sv);
    return *result;
}

global_t*& global_t::new_chrrom(lpstring_t at, global_t* g)
{
    using namespace std::literals;

    if(!g) // If 'g' is nullptr, we're creating an unnamed chrrom.
    {
        g = global_ht::with_pool([&](auto& pool)
        {
            return &pool.emplace_back(at, "chrrom", pool.size());
        });
    }

    std::lock_guard lock(chrrom_deque_mutex);
    return chrrom_deque.emplace_back(g);
}

bool global_t::has_chrrom()
{
    std::lock_guard lock(chrrom_deque_mutex);
    return !chrrom_deque.empty();
}

void global_t::for_each_chrrom(std::function<void(global_t*)> const& fn)
{
    std::lock_guard lock(chrrom_deque_mutex);
    for(auto const& ptr : chrrom_deque)
        fn(ptr);
}

void global_t::init()
{
    assert(compiler_phase() == PHASE_INIT);
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::parse_cleanup()
{
    assert(compiler_phase() == PHASE_PARSE_CLEANUP);

    // Handle solo_interrupts: 
    if(fn_t* irq = fn_t::solo_irq())
    {
        for(fn_t* mode : modes())
        {
            if(mode->mods() && mode->mods()->irq)
            {
                throw compiler_error_t(
                    fmt_error(mode->global.pstring(), "Mode using irq modifier when +solo_interrupt irq exists.")
                    + fmt_note(irq->global.pstring(), "+solo_interrupt irq located here."));
            }

            mode->ensure_mods();
            mode->m_mods->irq = &irq->global;
            mode->global.m_ideps.emplace(&irq->global, idep_pair_t{ .calc = IDEP_VALUE, .depends_on = IDEP_VALUE });
        }
    }

    // Verify globals are created:
    for(global_t& global : global_ht::values())
    {
        if(global.gclass() == GLOBAL_UNDEFINED)
        {
            if(global.m_lpstring)
                compiler_error(global.pstring(), fmt("Name not in scope: %", global.name));
            else
                throw compiler_error_t(fmt("Name not in scope: %", global.name));
        }
    }

    // Handle charmaps
    for(charmap_t& charmap : charmap_ht::values())
        charmap.create_group_data();

    // Validate groups and mods:
    for(fn_t const& fn : fn_ht::values())
    {
        for(mods_t const& mods : fn.def().mods)
            mods.validate_groups();

        if(fn.mods())
        {
            fn.mods()->validate_groups();

            if(global_t const* nmi = fn.mods()->nmi)
            {
                if(nmi->gclass() != GLOBAL_FN || nmi->impl<fn_t>().fclass != FN_NMI)
                {
                    throw compiler_error_t(
                        fmt_error(fn.global.pstring(), fmt("% is not a nmi function.", nmi->name))
                        + fmt_note(nmi->pstring(), "Declared here."));
                }
            }

            if(global_t const* irq = fn.mods()->irq)
            {
                if(irq->gclass() != GLOBAL_FN || irq->impl<fn_t>().fclass != FN_IRQ)
                {
                    throw compiler_error_t(
                        fmt_error(fn.global.pstring(), fmt("% is not an irq function.", irq->name))
                        + fmt_note(irq->pstring(), "Declared here."));
                }
            }
        }
    }

    // Determine group vars inits:
    for(group_t* gv : group_vars_ht::values())
        gv->vars()->determine_has_init();

    // Setup NMI indexes:
    for(unsigned i = 0; i < nmis().size(); ++i)
        nmis()[i]->pimpl<nmi_impl_t>().index = i;

    // Setup IRQ indexes:
    for(unsigned i = 0; i < irqs().size(); ++i)
        irqs()[i]->pimpl<irq_impl_t>().index = i;
}

template<typename Fn>
void global_t::do_all(Fn const& fn, bool parallel)
{
    {
        std::lock_guard lock(ready_mutex);
        globals_left = global_ht::pool().size();
    }

    // Spawn threads to compile in parallel:
    parallelize(parallel ? compiler_options().num_threads : 1,
    [&fn](std::atomic<bool>& exception_thrown)
    {
        ssa_pool::init();
        cfg_pool::init();

        while(!exception_thrown)
        {
            global_t* global = await_ready_global();

            if(!global)
                return;

            do global = fn(*global);
            while(global);
        }
    },
    []
    {
        {
            std::lock_guard lock(ready_mutex);
            globals_left = 0;
        }
        ready_cv.notify_all();
    });

    ready_cv.notify_all();
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::resolve_all()
{
    assert(compiler_phase() == PHASE_RESOLVE);

    do_all([&](global_t& g){ return g.resolve(nullptr); });
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::precheck_all()
{
    assert(compiler_phase() == PHASE_PRECHECK);

    do_all([&](global_t& g){ return g.precheck(nullptr); });

    for(fn_t const* fn : modes())
        fn->precheck_finish_mode();
    for(fn_t const* fn : nmis())
        fn->precheck_finish_nmi_irq();
    for(fn_t const* fn : irqs())
        fn->precheck_finish_nmi_irq();

    // Verify fences:
    for(fn_t const* nmi : nmis())
        if(nmi->m_precheck_wait_nmi)
            compiler_error(nmi->global.pstring(), "Waiting for nmi inside nmi handler.");
    for(fn_t const* irq : irqs())
        if(irq->m_precheck_wait_nmi)
            compiler_error(irq->global.pstring(), "Waiting for nmi inside irq handler.");

    // Define 'm_precheck_parent_modes'
    for(fn_t* mode : modes())
    {
        fn_ht const mode_h = mode->handle();

        mode->m_precheck_calls.for_each([&](fn_ht call)
        {
            call->m_precheck_parent_modes.insert(mode_h);
        });

        mode->m_precheck_parent_modes.insert(mode_h);
    }

    // Allocate 'used_in_modes' for NMIs and IRQs:
    for(fn_t* nmi : nmis())
        nmi->pimpl<nmi_impl_t>().used_in_modes.alloc();
    for(fn_t* irq : irqs())
        irq->pimpl<irq_impl_t>().used_in_modes.alloc();

    // Then populate 'used_in_modes':
    for(fn_t* mode : modes())
    {
        if(fn_ht nmi = mode->mode_nmi())
            nmi->pimpl<nmi_impl_t>().used_in_modes.set(mode->handle().id);
        if(fn_ht irq = mode->mode_irq())
            irq->pimpl<irq_impl_t>().used_in_modes.set(mode->handle().id);
    }

    // Set 'precheck_mode_group_vars':
    for(fn_t* mode : modes())
    {
        auto& bs = mode->pimpl<mode_impl_t>().m_precheck_mode_group_vars;
        bs = mode->m_precheck_group_vars;
        if(fn_ht nmi = mode->mode_nmi())
            bs |= nmi->m_precheck_group_vars;
        if(fn_ht irq = mode->mode_irq())
            bs |= irq->m_precheck_group_vars;
    }

    // Determine which rom procs each fn should have:

    for(fn_t* mode : modes())
    {
        assert(mode->fclass == FN_MODE);

        mode->m_precheck_calls.for_each([&](fn_ht call)
        {
            call->m_precheck_romv |= ROMVF_IN_MODE;
        });

        mode->m_precheck_romv |= ROMVF_IN_MODE;
    }

    for(fn_t* nmi : nmis())
    {
        assert(nmi->fclass == FN_NMI);

        nmi->m_precheck_calls.for_each([&](fn_ht call)
        {
            call->m_precheck_romv |= ROMVF_IN_NMI;
        });

        nmi->m_precheck_romv |= ROMVF_IN_NMI;
    }

    for(fn_t* irq : irqs())
    {
        assert(irq->fclass == FN_IRQ);

        irq->m_precheck_calls.for_each([&](fn_ht call)
        {
            call->m_precheck_romv |= ROMVF_IN_IRQ;
        });

        irq->m_precheck_romv |= ROMVF_IN_IRQ;
    }

    // Set ROMV for fn_sets:
    for(fn_set_t const& set : fn_set_ht::values())
        assert(set.m_precheck_romv == 0);
    rh::robin_map<fn_set_ht, pstring_t> prev_set_pstrings;
    for(fn_t const& fn : fn_ht::values())
    {
        if(!fn.m_precheck_tracked)
            continue;

        // TODO: handle iasm fn ptr calls

        for(auto const& pair : fn.m_precheck_tracked->calls_ptrs)
        {
            assert(pair.second);

            if(builtin::popcount(fn.m_precheck_romv) > 1)
                compiler_error(pair.second, "Fn pointer call site can be reached from multiple threads.");

            pair.first->m_precheck_romv |= fn.m_precheck_romv;

            if(builtin::popcount(pair.first->m_precheck_romv) > 1)
            {
                assert(prev_set_pstrings[pair.first]);
                throw compiler_error_t(
                    fmt_error(pair.second, fmt("Fn pointer call site for Fn.% can be reached from multiple threads.", pair.first->global.name))
                    + fmt_note(prev_set_pstrings[pair.first], "Conflicting call located here."));
            }

            if(fn.m_precheck_romv)
                prev_set_pstrings[pair.first] = pair.second;
        }
    }
    for(fn_set_t& set : fn_set_ht::values())
        if(!set.m_precheck_romv)
            set.m_precheck_romv |= ROMVF_IN_MODE;

    // Extend fn set ROMV to their fns:
    for(fn_set_t& set : fn_set_ht::values())
    {
        for(fn_ht fn : set)
        {
            fn->m_precheck_romv |= set.m_precheck_romv;
            fn->m_precheck_calls.for_each([&](fn_ht call)
            {
                call->m_precheck_romv |= set.m_precheck_romv;
            });
        }
    }

    for(fn_t& fn : fn_ht::values())
    {
        // Allocate rom procs:
        assert(!fn.m_rom_proc);
        fn.m_rom_proc = rom_proc_ht::pool_make(romv_allocs_t{}, fn.m_precheck_romv, mod_test(fn.mods(), MOD_align));

        if(mod_test(fn.mods(), MOD_static))
            fn.m_rom_proc.safe().mark_rule(ROMR_STATIC);

        // Determine each 'm_precheck_called':
        if(fn.m_precheck_calls)
        {
            fn.m_precheck_calls.for_each([&](fn_ht call)
            {
                call->m_precheck_called += 1;

                if(fn.iasm)
                    call->mark_referenced_return(); // Used to disable inlining.
            });
        }
    }
}

// Not thread safe!
global_t* global_t::detect_cycle(global_t& global, idep_class_t pass, idep_class_t calc)
{
    if(global.m_ideps_left >= calc) // Re-use 'm_ideps_left' to track the DFS.
        return nullptr;

    if(global.m_ideps_left == -1)
        return &global;

    global.m_ideps_left = -1;

    for(auto const& pair : global.m_ideps)
    {
        assert(pair.second.calc);
        assert(pair.second.depends_on);

        // If we can calculate:
        if(pair.second.calc > calc)
            continue;

        // If the the idep was computed in a previous pass:
        if(pair.first->gclass() != GLOBAL_FN_SET && pair.first->gclass() != GLOBAL_STRUCT)
            if(pair.second.calc < pass || pair.second.depends_on < pass)
                continue;

        if(global_t* error = detect_cycle(*pair.first, pass, pair.second.depends_on))
        {
            if(error != &global)
            {
                detect_cycle_error_msgs.push_back(fmt_error(global.m_lpstring, "Mutually recursive with:"));
                return error;
            }

            std::string msg = fmt_error(global.m_lpstring,
                fmt("% has a recursive definition.", global.name));
            for(std::string const& str : detect_cycle_error_msgs)
                msg += str;

            detect_cycle_error_msgs.clear();
            throw compiler_error_t(std::move(msg));
        }
    }

    global.m_ideps_left = calc;

    return nullptr;
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::count_members()
{
    for(fn_set_t& s : fn_set_ht::values())
        s.count_members();

    unsigned total_members = 0;
    for(struct_t& s : struct_ht::values())
        total_members += s.count_members();

    gmember_ht::with_pool([total_members](auto& pool){ pool.reserve(total_members); });

    for(gvar_t& gvar : gvar_ht::values())
    {
        gvar.dethunkify(false);

        gmember_ht const begin = { gmember_ht::with_pool([](auto& pool){ return pool.size(); }) };

        unsigned const num = ::num_members(gvar.type(), true);
        for(unsigned i = 0; i < num; ++i)
            gmember_ht::with_pool([&](auto& pool){ pool.emplace_back(gvar, pool.size()); });

        gmember_ht const end = { gmember_ht::with_pool([](auto& pool){ return pool.size(); }) };

        gvar.set_gmember_range(begin, end);
    }
}

void global_t::group_members()
{
    gvar_t::m_groupless_gmembers.alloc();
    for(gvar_ht gvar : gvar_t::groupless_gvars())
        for(auto m = gvar->begin(); m != gvar->end(); ++m)
            gvar_t::m_groupless_gmembers.set(m.id);
}

// This function isn't thread-safe.
// Call from a single thread only.
void global_t::build_order()
{
    idep_class_t pass = {};
    switch(compiler_phase())
    {
    case PHASE_COUNT_MEMBERS:
    case PHASE_ORDER_RESOLVE:
        pass = IDEP_TYPE;
        break;
    case PHASE_ORDER_PRECHECK:
    case PHASE_ORDER_COMPILE:
        pass = IDEP_VALUE;
        break;
    default:
        assert(false);
    }

    // Detect cycles and clear 'm_iuses':
    for(global_t& global : global_ht::values())
    {
        detect_cycle(global, pass, pass);
        global.m_iuses.clear();
    }

    // Build the order:
    for(global_t& global : global_ht::values())
    {
        // 'm_ideps_left' was set by 'detect_cycle',
        // and holds the level of calculation required:
        idep_class_t const calc = idep_class_t(global.m_ideps_left.load());

        unsigned ideps_left = 0;
        for(auto const& pair : global.m_ideps)
        {
            // If we can calculate:
            if(pair.second.calc > calc)
                continue;

            // If the the idep was computed in a previous pass:
            if(pair.first->gclass() != GLOBAL_FN_SET && pair.first->gclass() != GLOBAL_STRUCT)
                if(pair.second.calc < pass || pair.second.depends_on < pass)
                    continue;

            ++ideps_left;
            assert(pair.first != &global);
            pair.first->m_iuses.insert(&global);
        }

        global.m_ideps_left.store(ideps_left);

        if(ideps_left == 0)
            ready.push_back(&global);
    }

    assert(ready.size());
}

global_t* global_t::resolve(log_t* log)
{
    assert(compiler_phase() == PHASE_RESOLVE);

    dprint(log, "RESOLVING", name);
    delegate([](auto& g){ g.resolve(); });

#ifndef NDEBUG
    m_resolved = true;
#endif
    return completed();
}

global_t* global_t::precheck(log_t* log)
{
    assert(compiler_phase() == PHASE_PRECHECK);

    dprint(log, "PRECHECKING", name);
    delegate([](auto& g){ g.precheck(); });

#ifndef NDEBUG
    m_prechecked = true;
#endif
    return completed();
}

global_t* global_t::compile(log_t* log)
{
    assert(compiler_phase() == PHASE_COMPILE);

    dprint(log, "COMPILING", name, m_ideps.size());
    delegate([](auto& g){ g.compile(); });

#ifndef NDEBUG
    m_compiled = true;
#endif
    return completed();
}

global_t* global_t::completed()
{
    // OK! The global is done.
    // Now add all its dependents onto the ready list:

    global_t** newly_ready = ALLOCA_T(global_t*, m_iuses.size());
    global_t** newly_ready_end = newly_ready;

    for(global_t* iuse : m_iuses)
        if(--iuse->m_ideps_left == 0)
            *(newly_ready_end++) = iuse;

    std::size_t const newly_ready_size = newly_ready_end - newly_ready;
    global_t* ret = nullptr;

    if(newly_ready_size == 1)
    {
        ret = newly_ready[0];
        std::lock_guard lock(ready_mutex);
        if(globals_left)
            --globals_left;
    }
    else if(newly_ready_size == 0)
    {
        std::lock_guard lock(ready_mutex);
        if(globals_left)
        {
            if(!ready.empty())
            {
                ret = ready.back();
                ready.pop_back();
            }
            --globals_left;
        }
    }
    else
    {
        assert(newly_ready_size > 1);
        ret = newly_ready[0];
        ++newly_ready;

        std::lock_guard lock(ready_mutex);
        if(globals_left)
        {
            ready.insert(ready.end(), newly_ready, newly_ready_end);
            --globals_left;
        }
    }

    if(newly_ready_size == 2)
        ready_cv.notify_one();
    else if (newly_ready_size > 2 || globals_left == 0)
        ready_cv.notify_all();

    return ret;
}

global_t* global_t::await_ready_global()
{
    std::unique_lock<std::mutex> lock(ready_mutex);
    ready_cv.wait(lock, []{ return !ready.empty() || globals_left == 0; });

    if(globals_left == 0)
        return nullptr;
    
    global_t* ret = ready.back();
    ready.pop_back();
    return ret;
}

void global_t::compile_all()
{
    assert(compiler_phase() == PHASE_COMPILE);

    do_all([&](global_t& g){ return g.compile(nullptr); });
}

global_datum_t* global_t::datum() const
{
    switch(gclass())
    {
    case GLOBAL_VAR: return &impl<gvar_t>();
    case GLOBAL_CONST: return &impl<const_t>();
    default: return nullptr;
    }
}

std::vector<local_const_t> const* global_t::local_consts() const
{
    if(gclass() == GLOBAL_FN)
        return &impl<fn_t>().def().local_consts;
    if(auto* dat = datum())
        if(auto* def = dat->paa_def())
            return &def->local_consts;
    return nullptr;
}

///////////
// fn_t  //
///////////

fn_t::fn_t(global_t& global, type_t type, fn_def_t&& fn_def, std::unique_ptr<mods_t> mods, 
           fn_class_t fclass, bool iasm, fn_set_t* fn_set) 
: modded_t(std::move(mods))
, global(global)
, fclass(fclass)
, iasm(iasm)
, m_type(std::move(type))
, m_def(std::move(fn_def)) 
, m_fn_set(fn_set)
{
    if(compiler_options().ir_info || mod_test(this->mods(), MOD_info))
        m_info_stream.reset(new std::stringstream());

    switch(fclass)
    {
    default:
        break;
    case FN_MODE: m_pimpl.reset(new mode_impl_t()); break;
    case FN_NMI:  m_pimpl.reset(new nmi_impl_t());  break;
    case FN_IRQ:  m_pimpl.reset(new irq_impl_t());  break;
    }

    m_sloppy = compiler_options().sloppy || mod_test(this->mods(), MOD_sloppy);
    m_sloppy &= !mod_test(this->mods(), MOD_sloppy, false);

    if(mod_test(this->mods(), MOD_solo_interrupt))
    {
        if(!mod_test(this->mods(), MOD_static))
            compiler_error(global.pstring(), "+solo_interrupt requires +static.");

        if(fclass == FN_IRQ)
        {
            std::lock_guard<std::mutex> lock(m_solo_irq_mutex);
            if(m_solo_irq)
            {
                throw compiler_error_t(
                    fmt_error(global.pstring(), "Multiple copies of +solo_interrupt.")
                    + fmt_note(m_solo_irq->global.pstring(), "Previously declared here."));
            }
            m_solo_irq = this;
        }
    }
}

fn_ht fn_t::mode_nmi() const
{ 
    assert(fclass == FN_MODE); 
    assert(compiler_phase() > PHASE_PARSE_CLEANUP);
    return (mods() && mods()->nmi) ? mods()->nmi->handle<fn_ht>() : fn_ht{};
}

fn_ht fn_t::mode_irq() const
{ 
    assert(fclass == FN_MODE); 
    assert(compiler_phase() >= PHASE_PARSE_CLEANUP);
    return (mods() && mods()->irq) ? mods()->irq->handle<fn_ht>() : fn_ht{};
}

unsigned fn_t::nmi_index() const
{
    assert(fclass == FN_NMI);
    assert(compiler_phase() > PHASE_PARSE_CLEANUP);
    return pimpl<nmi_impl_t>().index;
}

unsigned fn_t::irq_index() const
{
    assert(fclass == FN_IRQ);
    assert(compiler_phase() > PHASE_PARSE_CLEANUP);
    return pimpl<irq_impl_t>().index;
}

xbitset_t<fn_ht> const& fn_t::nmi_used_in_modes() const
{
    assert(fclass == FN_NMI);
    assert(compiler_phase() > PHASE_PRECHECK);
    return pimpl<nmi_impl_t>().used_in_modes;
}

xbitset_t<fn_ht> const& fn_t::irq_used_in_modes() const
{
    assert(fclass == FN_IRQ);
    assert(compiler_phase() > PHASE_PRECHECK);
    return pimpl<irq_impl_t>().used_in_modes;
}

bool fn_t::ct_pure() const
{
    switch(fclass)
    {
    case FN_CT:
        return true;
    case FN_FN:
        assert(global.compiled());
        return (ir_io_pure() 
                && precheck_rw().all_clear()
                && ir_deref_groups().all_clear()
                && !ir_tests_ready());
    default:
        return false;
    }

}

void fn_t::calc_ir_bitsets(ir_t const* ir_ptr)
{
    xbitset_t<gmember_ht>  reads(0);
    xbitset_t<gmember_ht> writes(0);
    xbitset_t<group_vars_ht> group_vars(0);
    xbitset_t<group_ht> deref_groups(0);
    xbitset_t<fn_ht> calls(0);
    bool tests_ready = false;
    bool io_pure = true;
    bool fences = false;
    bool runtime = false;

    bool const is_static = mod_test(mods(), MOD_static);

    // Handle preserved groups
    if(!m_precheck_tracked->goto_modes.empty())
        reads |= gvar_t::groupless_gmembers();
    for(auto const& fn_stmt : m_precheck_tracked->goto_modes)
    {
        if(mods_t const* goto_mods = fn_stmt.second.mods)
        {
            goto_mods->for_each_list_vars(MODL_PRESERVES, [&](group_vars_ht gv, pstring_t)
            {
                group_vars.set(gv.id);
                reads |= (*gv)->vars()->gmembers();
            });
        }
    }

    // Handle 'employs':
    if(mods())
    {
        auto const for_vars = [&](group_ht g, pstring_t)
        {
            if(g->using_vars())
            {
                deref_groups.set(g.id);
                auto const& gv = *g->vars();
                reads  |= gv.gmembers();
                writes |= gv.gmembers();
            }
        };

        auto const for_data = [&](group_ht g, pstring_t)
        {
            if(g->using_any_data())
                deref_groups.set(g.id);
        };

        mods()->for_each_list(MODL_EMPLOYS, [&](group_ht g, pstring_t p){ for_vars(g, p); for_data(g, p); });
        mods()->for_each_list(MODL_EMPLOYS_VARS, for_vars);
        mods()->for_each_list(MODL_EMPLOYS_DATA, for_data);
    }

    if(iasm)
    {
        assert(!ir_ptr);
        assert(mods());
        assert(mods()->explicit_lists & MODL_EMPLOYS_ANY);

        io_pure = false;
        fences = true;
        runtime = true;
        m_bank_switches = true;
        group_vars = m_precheck_group_vars;

        // iasm always employs groupless vars:
        reads  |= gvar_t::groupless_gmembers();
        writes |= gvar_t::groupless_gmembers();

        for(auto const& pair : precheck_tracked().calls)
        {
            fn_t const& callee = *pair.first;
            reads   |= callee.ir_reads();
            writes  |= callee.ir_writes();
            calls   |= callee.ir_calls();
            calls.set(pair.first.id);
        }
    }
    else
    {
        assert(ir_ptr);
        ir_t const& ir = *ir_ptr;

        // Iterate the IR looking for reads and writes
        for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            if(ssa_flags(ssa_it->op()) & SSAF_IO_IMPURE)
                io_pure = false;

            if(ssa_it->op() == SSA_ready)
                tests_ready = true;

            if(callable_t const* callee = get_callable(*ssa_it))
            {
                reads      |= callee->ir_reads();
                writes     |= callee->ir_writes();
                group_vars |= callee->ir_group_vars();
                calls      |= callee->ir_calls();
                fences     |= callee->ir_fences();
                runtime    |= callee->ir_runtime();
                io_pure    &= callee->ir_io_pure();
                callee->for_each_fn([&](fn_ht callee_h){ calls.set(callee_h.id); });

                if(fclass != FN_MODE && is_static && mapper().bankswitches())
                    m_returns_in_different_bank |= callee->returns_in_different_bank();

                m_bank_switches |= callee->bank_switches();
            }

            if(ssa_flags(ssa_it->op()) & SSAF_RUNTIME)
                runtime = true;

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
                            if(written.gvar.group_vars)
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
                            if(read.gvar.group_vars)
                                group_vars.set(read.gvar.group_vars.id);
                            break;
                        }
                    }
                }
            }

            if(ssa_flags(ssa_it->op()) & SSAF_INDEXES_PTR)
            {
                using namespace ssai::rw_ptr;

                io_pure = false;

                type_t const ptr_type = ssa_it->input(PTR).type();
                passert(is_ptr(ptr_type.name()), ssa_it->input(PTR));

                unsigned const size = ptr_type.group_tail_size();
                for(unsigned i = 0; i < size; ++i)
                {
                    // Set 'deref_groups':
                    group_ht const h = ptr_type.group(i);
                    deref_groups.set(h.id);

                    // Also update 'ir_group_vars':
                    group_t const& group = *h;
                    if(group.using_vars())
                        group_vars.set(group.vars_handle().id);
                }
            }

            if(ssa_flags(ssa_it->op()) & SSAF_FENCE)
                fences = true;

            if(ssa_flags(ssa_it->op()) & SSAF_BANK_INPUT)
            {
                m_bank_switches = true;

                using namespace ssai::rw_ptr;
                ssa_value_t const bank = ssa_it->input(ssa_bank_input(ssa_it->op()));

                if(fclass != FN_MODE && bank && is_static && mapper().bankswitches())
                    m_returns_in_different_bank = true;
            }
        }
    }

    m_ir_writes = std::move(writes);
    m_ir_reads  = std::move(reads);
    m_ir_group_vars = std::move(group_vars);
    m_ir_calls = std::move(calls);
    m_ir_deref_groups = std::move(deref_groups);
    m_ir_tests_ready = tests_ready;
    m_ir_io_pure = io_pure;
    m_ir_fences = fences;
    m_ir_runtime = runtime;
}

void fn_t::assign_direct_rom_arrays(fc::vector_set<rom_array_ht>&& set)
{
    assert(compiler_phase() == PHASE_COMPILE);
    m_direct_rom_arrays = std::move(set);
}

void fn_t::assign_lvars(lvars_manager_t&& lvars)
{
    assert(compiler_phase() == PHASE_COMPILE);
    m_lvars = std::move(lvars);
    for(auto& vec : m_lvar_spans)
    {
        vec.clear();
        vec.resize(m_lvars.num_this_lvars());
    }
}

void fn_t::assign_lvar_span(romv_t romv, unsigned lvar_i, span_t span)
{
    passert(lvar_i < m_lvar_spans[romv].size(), lvar_i, m_lvar_spans[romv].size()); 
    assert(!m_lvar_spans[romv][lvar_i]);
    passert(precheck_romv() & (1 << romv), global.name, (int)precheck_romv(), (int)romv);

    m_lvar_spans[romv][lvar_i] = span;
    assert(lvar_span(romv, lvar_i) == span);
}

span_t fn_t::lvar_span(romv_t romv, int lvar_i) const
{
    assert(lvar_i < int(m_lvars.num_all_lvars()));

    if(lvar_i < 0)
        return {};

    if(lvar_i < int(m_lvars.num_this_lvars()))
        return m_lvar_spans[romv][lvar_i];

    locator_t const loc = m_lvars.locator(lvar_i);
    if(lvars_manager_t::is_call_lvar(handle(), loc))
    {
        int index = loc.fn()->m_lvars.index(loc);

        if(!loc.fn()->m_lvars.is_lvar(index))
            return {};

        return loc.fn()->lvar_span(romv, index);
    }

    throw std::runtime_error("Unknown lvar span");
}

span_t fn_t::lvar_span(romv_t romv, locator_t loc) const
{
    return lvar_span(romv, m_lvars.index(loc));
}

static void _resolve_local_consts(global_ht global, std::vector<local_const_t>& local_consts, fn_t* fn = nullptr)
{
    for(unsigned i = 0; i < local_consts.size(); ++i)
    {
        auto& c = local_consts[i];

        if(c.expr)
        {
            c.decl.src_type.type = ::dethunkify(c.decl.src_type, true, nullptr, local_consts.data());

            c.value = interpret_local_const(
                c.decl.name, fn, *c.expr, 
                c.decl.src_type.type, local_consts.data()).value;
        }
        else
        {
            locator_t label = locator_t::named_label(global, i);

            if(is_banked_ptr(c.type().name()))
                c.value = { label, label.with_is(IS_BANK) };
            else
                c.value = { label };

            assert(c.value.size() == num_members(c.type()));
        }
    }
}

void fn_t::resolve()
{
    // Dethunkify the fn type:
    {
        type_t* types = ALLOCA_T(type_t, def().num_params + 1);
        for(unsigned i = 0; i != def().num_params; ++i)
            types[i] = ::dethunkify(def().local_vars[i].decl.src_type, true);
        types[def().num_params] = ::dethunkify(def().return_type, true);
        m_type = type_t::fn(types, types + def().num_params + 1);
    }

    if(fclass == FN_CT)
        goto resolve_ct;

    // Finish up types:
    for(unsigned i = 0; i < def().num_params; ++i)
    {
        auto const& decl = def().local_vars[i].decl;
        if(is_ct(decl.src_type.type))
            compiler_error(decl.src_type.pstring, fmt("Function must be declared as ct to use type %.", decl.src_type.type));
    }

    if(is_ct(def().return_type.type))
        compiler_error(def().return_type.pstring, fmt("Function must be declared as ct to use type %.", def().return_type.type));

    if(iasm)
    {
        if(!mods() || !(mods()->explicit_lists & MODL_EMPLOYS_ANY))
            compiler_error(global.pstring(), "Missing employs modifier.");
    }

    // Resolve local constants:
resolve_ct:
    _resolve_local_consts(global.handle(), m_def.local_consts, this);
}

void fn_t::precheck()
{
    if(fclass == FN_CT)
        return; // Nothing to do!

    if(iasm)
    {
        assert(def().stmts.size() == 2);
        assert(def().stmts[0].name == STMT_EXPR);
        assert(!m_precheck_tracked);
        m_precheck_tracked.reset(new precheck_tracked_t(build_tracked(*this, def().local_consts.data())));
    }
    else
    {
        // Run the evaluator to generate 'm_precheck_tracked':
        assert(!m_precheck_tracked);
        m_precheck_tracked.reset(new precheck_tracked_t(build_tracked(*this, def().local_consts.data())));
    }

    assert(m_precheck_tracked);
    calc_precheck_bitsets();
}

void fn_t::compile_iasm()
{
    assert(iasm);

    calc_ir_bitsets(nullptr);

    assert(def().stmts.size() == 2);
    assert(def().stmts[0].name == STMT_EXPR);
    assert(def().stmts[0].expr[0].token.type == lex::TOK_byte_block_proc);

    asm_proc_t proc = std::get<asm_proc_t>(interpret_byte_block(def().stmts[0].pstring, def().stmts[0].expr[0], 
                                                                this, def().local_consts.data()));
    proc.fn = handle();

    assign_lvars(lvars_manager_t(*this));

    proc.build_label_offsets();
    rom_proc().safe().assign(std::move(proc));
}

void fn_t::compile()
{
    log_t* log = nullptr;
    assert(compiler_phase() == PHASE_COMPILE);

    if(fclass == FN_CT)
        return; // Nothing to do!

    if(iasm)
        return compile_iasm();

    // Compile the FN.
    ssa_pool::clear();
    cfg_pool::clear();
    ir_t ir;
    build_ir(ir, *this);

    auto const save_graph = [&](ir_t& ir, char const* suffix)
    {
        if(!compiler_options().graphviz && !mod_test(mods(), MOD_graphviz))
            return;

        std::filesystem::create_directory("graphs/");
        std::filesystem::create_directory("graphs/cfg/");
        std::filesystem::create_directory("graphs/ssa/");

        std::ofstream ocfg(fmt("graphs/cfg/%__%.gv", global.name, suffix));
        if(ocfg.is_open())
            graphviz_cfg(ocfg, ir);

        std::ofstream ossa(fmt("graphs/ssa/%__%.gv", global.name, suffix));
        if(ossa.is_open())
            graphviz_ssa(ossa, ir);
    };

    auto const optimize_suite = [&](bool post_byteified)
    {
#define RUN_O(o, ...) do { if(o(__VA_ARGS__)) { \
    changed = true; \
    /*assert((std::printf("DID_O %s %s %i\n", global.name.c_str(), #o, iter), true));*/  } \
    ir.assert_valid(); \
    } while(false)

        unsigned iter = 0;
        unsigned const MAX_ITER = sloppy() ? 10 : 100;
        bool changed;

        // Simplify locators:
        o_optimize_locators(log, ir);

        // Do this now, to reduce the size of the IR:
        o_remove_unused_ssa(log, ir);

        do
        {
            changed = false;

            dprint(log, "OPTIMIZATION_PASS", post_byteified, iter);

            save_graph(ir, fmt("A_pre_fork_%_%", post_byteified, iter).c_str());

            RUN_O(o_defork, log, ir);
            RUN_O(o_fork, log, ir);
            save_graph(ir, fmt("B_post_fork_%_%", post_byteified, iter).c_str());

            RUN_O(o_phis, log, ir);

            RUN_O(o_merge_basic_blocks, log, ir);

            RUN_O(o_remove_unused_arguments, log, ir, *this, post_byteified);

            save_graph(ir, fmt("C_pre_id_%_%", post_byteified, iter).c_str());
            RUN_O(o_identities, log, ir, post_byteified);
            save_graph(ir, fmt("D_post_id_%_%", post_byteified, iter).c_str());

            // 'o_loop' populates 'ai_prep', which feeds into 'o_abstract_interpret'.
            // Thus, they must occur sequentially.
            reset_ai_prep();
            save_graph(ir, fmt("E_pre_loop_%_%", post_byteified, iter).c_str());
            RUN_O(o_loop, log, ir, post_byteified, sloppy());
            save_graph(ir, fmt("F_pre_ai_%_%", post_byteified, iter).c_str());
            RUN_O(o_abstract_interpret, log, ir, post_byteified);
            save_graph(ir, fmt("G_post_ai_%_%", post_byteified, iter).c_str());

            RUN_O(o_remove_unused_ssa, log, ir);

            save_graph(ir, fmt("H_pre_motion_%_%", post_byteified, iter).c_str());
            RUN_O(o_motion, log, ir);
            save_graph(ir, fmt("I_post_motion_%_%", post_byteified, iter).c_str());

            if(post_byteified)
            {
                // Once byteified, keep shifts out of the IR and only use rotates.
                RUN_O(o_shl_tables, log, ir);
                changed |= shifts_to_rotates(ir, true);
            }

            // Enable this to debug:
            save_graph(ir, fmt("Z_during_o_%_%", post_byteified, iter).c_str());
            ++iter;

            if(iter >= MAX_ITER)
                break;
        }
        while(changed);
    };

    save_graph(ir, "1_initial");
    ir.assert_valid();

    optimize_suite(false);
    save_graph(ir, "2_o1");

    // Set the global's 'read' and 'write' bitsets:
    calc_ir_bitsets(&ir);
    assert(ir_reads());

    // Convert shifts and switches:
    // NOTE: Do NOT use operator || here.
    if(o_shl_tables(log, ir) | switch_partial_to_full(ir))
        optimize_suite(false);
    save_graph(ir, "3_transform");

    byteify(ir, *this);
    o_optimize_locators(log, ir);
    save_graph(ir, "4_byteify");
    ir.assert_valid();

    optimize_suite(true);
    save_graph(ir, "5_o2");

    std::size_t const proc_size = code_gen(log, ir, *this);
    save_graph(ir, "6_cg");

    // Calculate inline-ability
    assert(m_always_inline == false);
    if(fclass == FN_FN && !mod_test(mods(), MOD_inline, false))
    {
        if(referenced())
        {
            m_always_inline = false;
            if(mod_test(mods(), MOD_inline, true))
                compiler_warning(global.pstring(), fmt("Unable to inline % as its being addressed.", global.name));
        }
        else if(mod_test(mods(), MOD_inline, true))
            m_always_inline = true;
        else if(precheck_called() == 1)
        {
            if(proc_size < INLINE_SIZE_ONCE)
                m_always_inline = true;
        }
        else if(proc_size < INLINE_SIZE_LIMIT)
        {
            bool const no_banks = ir_deref_groups().for_each_test([&](group_ht group) -> bool
            {
                return !group->using_data();
            });

            if(no_banks)
            {
                unsigned call_cost = 0;

                m_lvars.for_each_lvar(true, [&](locator_t loc, unsigned)
                {
                    if(loc.lclass() == LOC_ARG || loc.lclass() == LOC_RETURN 
                       || loc.lclass() == LOC_PTR_ARG || loc.lclass() == LOC_PTR_RETURN)
                    {
                        assert(loc.fn() == handle());
                        ++call_cost;
                    }
                });

                constexpr unsigned CALL_PENALTY = 3;

                if(proc_size < INLINE_SIZE_GOAL + (call_cost * CALL_PENALTY))
                    m_always_inline = true;
            }
        }
    }
}

void fn_t::precheck_finish_mode() const
{
    assert(fclass == FN_MODE);

    auto& pimpl = this->pimpl<mode_impl_t>();

    for(auto const& pair : pimpl.incoming_preserved_groups)
    {
        if(!pair.first->using_vars())
            continue;

        if(!precheck_group_vars().test(pair.first->vars_handle().id))
        {
            std::string groups;
            precheck_group_vars().for_each([&groups](group_vars_ht gv)
            {
                groups += (*gv)->name;
            });
            if(groups.empty())
                groups = "(no groups)";
            else
                groups = "vars " + groups;

            compiler_warning(
                fmt_warning(pair.second, 
                    fmt("Preserving % has no effect as mode % is excluding it.", 
                        pair.first->name, global.name))
                + fmt_note(global.pstring(), fmt("% includes: %", 
                                                 global.name, groups)), false);
        }
    }
}

void fn_t::precheck_finish_nmi_irq() const
{
    assert(fclass == FN_NMI || fclass == FN_IRQ);

    auto const first_goto_mode = [](fn_t const& fn) -> pstring_t
    {
        if(fn.precheck_tracked().goto_modes.empty())
            return {};
        return fn.precheck_tracked().goto_modes.begin()->second.pstring;
    };

    if(pstring_t pstring = first_goto_mode(*this))
        compiler_error(pstring, fmt("goto mode inside %.", fn_class_keyword(fclass)));

    m_precheck_calls.for_each([&](fn_ht call)
    {
        if(pstring_t pstring = first_goto_mode(*call))
        {
            throw compiler_error_t(
                fmt_error(pstring, fmt("goto mode reachable from % %", fn_class_keyword(fclass), global.name))
                + fmt_note(global.pstring(), fmt("% declared here.", global.name)));
        }
    });
}

void fn_t::calc_precheck_bitsets()
{
    assert(compiler_phase() == PHASE_PRECHECK);

    // Set 'wait_nmi' and 'fences':
    m_precheck_wait_nmi |= m_precheck_tracked->wait_nmis.size() > 0;

    if(iasm)
        m_precheck_fences = true;
    else
    {
        m_precheck_fences |= m_precheck_tracked->fences.size() > 0;
        m_precheck_fences |= m_precheck_wait_nmi;
    }

    unsigned const gv_bs_size = group_vars_ht::bitset_size();

    assert(!m_precheck_group_vars);
    assert(!m_precheck_rw);
    assert(!m_precheck_calls);

    m_precheck_group_vars.alloc();
    m_precheck_rw.alloc();
    m_precheck_calls.alloc();

    // For efficiency, we'll convert the mod groups into a bitset.
    bitset_uint_t* temp_bs = ALLOCA_T(bitset_uint_t, gv_bs_size);
    bitset_uint_t* mod_group_vars = ALLOCA_T(bitset_uint_t, gv_bs_size);

    bitset_clear_all(gv_bs_size, mod_group_vars);
    if(mods() && (mods()->explicit_lists & MODL_VARS))
    {
        mods()->for_each_list_vars(MODL_VARS, [&](group_vars_ht gv, pstring_t)
        {
            bitset_set(mod_group_vars, gv.id);
        });

        bitset_or(gv_bs_size, m_precheck_group_vars.data(), mod_group_vars);
    }

    if(mods() && (mods()->explicit_lists & MODL_EMPLOYS))
    {
        mods()->for_each_employs_vars([&](group_vars_ht gv, pstring_t)
        {
            bitset_set(m_precheck_group_vars.data(), gv.id);
        });
    }

    auto const group_vars_to_string = [gv_bs_size](bitset_uint_t const* bs)
    {
        std::string str;
        bitset_for_each<group_vars_ht>(gv_bs_size, bs, [&str](auto gv){ str += (*gv)->name; });
        return str;
    };

    auto const group_map_to_string = [this](mod_list_t lists)
    {
        std::string str;
        for(auto const& pair : mods()->lists)
            if(pair.second.lists & lists)
                str += pair.first->name;
        return str;
    };

    // Handle accesses through pointers:
    for(auto const& pair : m_precheck_tracked->deref_groups)
    {
        auto const error = [&](bool vars, mod_list_t lists)
        {
            char const* keyword = vars ? "vars" : "data";
            type_t const type = pair.second.type;

            std::string const msg = fmt(
                "% access requires groups that are excluded from % (% %).",
                type, global.name, keyword, group_map_to_string(lists));

            std::string missing = "";
            for(unsigned i = 0; i < type.group_tail_size(); ++i)
            {
                group_ht const group = type.group(i);
                if(((vars && group->using_vars()) || (!vars && group->using_any_data()))
                   && !mods()->in_lists(lists, group))
                {
                    missing += group->name;
                }
            }

            throw compiler_error_t(
                fmt_error(pair.second.pstring, msg)
                + fmt_note(fmt("Excluded groups: % %", keyword, missing)));
        };

        if(pair.first->using_vars())
        {
            if(mods() && (mods()->explicit_lists & MODL_VARS) && !mods()->in_lists(MODL_VARS, pair.first))
                error(true, MODL_VARS);

            m_precheck_group_vars.set(pair.first->vars_handle().id);
        }

        if(pair.first->using_any_data())
        {
            if(mods() && (mods()->explicit_lists & MODL_DATA) && !mods()->in_lists(MODL_DATA, pair.first))
                error(false, MODL_DATA);
        }
    }

    // Handle accesses through goto modes:
    for(auto const& fn_stmt : m_precheck_tracked->goto_modes)
    {
        pstring_t pstring = fn_stmt.second.pstring;
        mods_t const* goto_mods = fn_stmt.second.mods;

        if(!goto_mods || !(goto_mods->explicit_lists & MODL_PRESERVES))
            compiler_error(pstring ? pstring : global.pstring(), 
                           "Missing preserves modifier.");

        if(!goto_mods)
            continue;

        // Track incoming, for the called mode: 
        auto& call_pimpl = fn_stmt.first->pimpl<mode_impl_t>();
        {
            std::lock_guard<std::mutex> lock(call_pimpl.incoming_preserved_groups_mutex);
            goto_mods->for_each_list(MODL_PRESERVES, [&](group_ht g, pstring_t pstring)
            {
                call_pimpl.incoming_preserved_groups.emplace(g, pstring);
            });
        }

        // Handle our own groups:
        goto_mods->for_each_list(MODL_PRESERVES, [&](group_ht g, pstring_t)
        {
            if(g->using_vars())
            {
                if(mods() && (mods()->explicit_lists & MODL_PRESERVES) && !mods()->in_lists(MODL_PRESERVES, g))
                {
                    std::string const msg = fmt(
                        "Preserved groups are excluded from % (vars %).",
                        global.name, group_map_to_string(MODL_VARS));

                    std::string missing = "";
                    goto_mods->for_each_list(MODL_PRESERVES, [&](group_ht g, pstring_t)
                    {
                        if(g->using_vars() && !mods()->in_lists(MODL_PRESERVES, g))
                            missing += g->name;
                    });

                    throw compiler_error_t(
                        fmt_error(pstring, msg)
                        + fmt_note(fmt("Excluded groups: vars %", missing)));
                }

                m_precheck_group_vars.set(g->vars_handle().id);
            }
        });
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

        for(auto const& pair : m_precheck_tracked->gvars_used)
        {
            gvar_ht const gvar = pair.first;
            m_precheck_rw.set_n(gvar->begin().id, gvar->num_members());

            if(group_ht const group = gvar->group())
            {
                if(mods() && (mods()->explicit_lists & MODL_VARS) && !mods()->in_lists(MODL_VARS, group))
                    error(pair.first->global, group->name, group->name);

                m_precheck_group_vars.set(group->vars_handle().id);
            }
        }

        for(auto const& pair : m_precheck_tracked->calls)
        {
            fn_t& call = *pair.first;

            passert(call.fclass != FN_CT, global.name, call.global.name);
            passert(call.fclass != FN_MODE, global.name, call.global.name);
            passert(call.m_precheck_group_vars, global.name, call.global.name);
            passert(call.m_precheck_rw, global.name, call.global.name);

            bitset_copy(gv_bs_size, temp_bs, call.m_precheck_group_vars.data());
            bitset_difference(gv_bs_size, temp_bs, mod_group_vars);

            if(mods() && (mods()->explicit_lists & MODL_VARS) && !bitset_all_clear(gv_bs_size, temp_bs))
            {
                error(call.global,
                      group_vars_to_string(call.precheck_group_vars().data()),
                      group_vars_to_string(temp_bs));
            }

            m_precheck_group_vars |= call.m_precheck_group_vars;
            m_precheck_rw |= call.m_precheck_rw;

            // Calls
            m_precheck_calls.set(pair.first.id);
            m_precheck_calls |= call.m_precheck_calls;

            // 'wait_nmi' and 'fences'
            m_precheck_fences |= call.m_precheck_fences;
            m_precheck_wait_nmi |= call.m_precheck_wait_nmi;
        }
    }
}

void fn_t::mark_referenced_return()
{
    std::uint64_t expected = m_referenced.load();
    while(!m_referenced.compare_exchange_weak(expected, expected | 1));
    assert(referenced_return());
}

void fn_t::mark_referenced_param(unsigned param)
{
    assert(param < 63);
    std::uint64_t const mask = 0b10ull << param;
    assert(mask);
    std::uint64_t expected = m_referenced.load();
    while(!m_referenced.compare_exchange_weak(expected, expected | mask));
    assert(m_referenced.load() & mask);
}

void fn_t::for_each_referenced_locator(std::function<void(locator_t)> const& fn) const
{
    type_t const return_type = type().return_type();

    if(return_type.name() != TYPE_VOID && referenced_return())
    {
        unsigned const num_members = ::num_members(return_type);

        for(unsigned j = 0; j < num_members; ++j)
        {
            type_t const member_type = ::member_type(return_type, j);
            unsigned const num_atoms = ::num_atoms(member_type, 0);

            for(unsigned k = 0; k < num_atoms; ++k)
                fn(locator_t::ret(handle(), j, k));
        }
    }

    for_each_referenced_param_locator(fn);
}

void fn_t::for_each_referenced_param_locator(std::function<void(locator_t)> const& fn) const
{
    bitset_for_each(referenced_params(), [&](unsigned param)
    {
        var_decl_t const& decl = def().local_vars[param].decl;
        type_t const param_type = decl.src_type.type;
        unsigned const num_members = ::num_members(param_type);

        for(unsigned j = 0; j < num_members; ++j)
        {
            type_t const member_type = ::member_type(param_type, j);
            unsigned const num_atoms = ::num_atoms(member_type, 0);

            for(unsigned k = 0; k < num_atoms; ++k)
                fn(locator_t::arg(handle(), param, j, k));
        }
    });
}

locator_t fn_t::new_asm_goto_mode(fn_ht fn, unsigned label, pstring_t pstring, mods_t const* mods)
{
    if(!m_asm_goto_modes)
        m_asm_goto_modes.reset(new std::vector<asm_goto_mode_t>());
    auto& vec = *m_asm_goto_modes;

    locator_t const loc = locator_t::asm_goto_mode(handle(), vec.size());

    vec.push_back(asm_goto_mode_t{ 
        .fn = fn, 
        .label = label, 
        .pstring = pstring,
        .rom_proc = rom_proc_ht::pool_make(romv_allocs_t{}, m_precheck_romv, false)
    });

    if(mods)
    {
        mods->for_each_list_vars(MODL_PRESERVES, [&](group_vars_ht gv, pstring_t)
        {
            vec.back().preserves.insert(gv);
        });
    }

    return loc;
}

rom_proc_ht fn_t::asm_goto_mode_rom_proc(unsigned i) const
{
    assert(compiler_phase() > PHASE_COMPILE);
    assert(m_asm_goto_modes);
    assert(i < m_asm_goto_modes->size());

    return (*m_asm_goto_modes)[i].rom_proc;
}

// Not thread safe.
void fn_t::implement_asm_goto_modes()
{
    for(fn_t& fn : fn_ht::values())
    {
        if(auto* vec = fn.m_asm_goto_modes.get())
        for(auto& d : *vec)
        {
            asm_proc_t proc;
            int const iasm_child = proc.add_pstring(d.pstring);

            // Reset global variables:
            bool did_reset_nmi = false;
            bool did_reset_irq = false;
            d.fn->precheck_group_vars().for_each([&](group_vars_ht gv)
            {
                if(!(*gv)->vars()->has_init())
                    return;

                if(!d.preserves.count(gv))
                {
                    if(!did_reset_nmi && global_t::has_nmi())
                    {
                        // Reset the nmi handler until we've reset all group vars.
                        proc.push_inst({ .op = LDY_IMMEDIATE, .iasm_child = iasm_child, .arg = locator_t::const_byte(0) });
                        proc.push_inst({ .op = STY_ABSOLUTE, .iasm_child = iasm_child, .arg = locator_t::runtime_ram(RTRAM_nmi_index) });
                        did_reset_nmi = true;
                    }

                    if(!did_reset_irq && global_t::has_irq())
                    {
                        // Reset the irq handler until we've reset all group vars.
                        proc.push_inst({ .op = LDY_IMMEDIATE, .iasm_child = iasm_child, .arg = locator_t::const_byte(0) });
                        proc.push_inst({ .op = STY_ABSOLUTE, .iasm_child = iasm_child, .arg = locator_t::runtime_ram(RTRAM_irq_index) });
                        did_reset_nmi = true;
                    }

                    locator_t const loc = locator_t::reset_group_vars(gv);
                    proc.push_inst({ .op = LDY_IMMEDIATE, .iasm_child = iasm_child, .arg = loc.with_is(IS_BANK) });
                    if(bankswitch_use_x())
                        proc.push_inst({ .op = mapper().bankswitches() ? BANKED_X_JSR : JSR_ABSOLUTE, .iasm_child = iasm_child, .arg = loc });
                    else
                        proc.push_inst({ .op = mapper().bankswitches() ? BANKED_Y_JSR : JSR_ABSOLUTE, .iasm_child = iasm_child, .arg = loc });
                }
            });

            bool same_nmi = true;
            bool same_irq = true;
            for(fn_ht mode : fn.precheck_parent_modes())
            {
                same_nmi &= mode->mode_nmi() == d.fn->mode_nmi();
                same_irq &= mode->mode_irq() == d.fn->mode_irq();
            }

            // Set the NMI
            if(global_t::has_nmi() && (did_reset_nmi || !same_nmi))
            {
                proc.push_inst({ .op = LDY_IMMEDIATE, .iasm_child = iasm_child, .arg = locator_t::nmi_index(d.fn->mode_nmi()) });
                proc.push_inst({ .op = STY_ABSOLUTE, .iasm_child = iasm_child, .arg = locator_t::runtime_ram(RTRAM_nmi_index) });
            }

            // Set the IRQ
            if(global_t::has_irq() && (did_reset_irq || !same_irq))
            {
                proc.push_inst({ .op = LDY_IMMEDIATE, .iasm_child = iasm_child, .arg = locator_t::irq_index(d.fn->mode_irq()) });
                proc.push_inst({ .op = STY_ABSOLUTE, .iasm_child = iasm_child, .arg = locator_t::runtime_ram(RTRAM_irq_index) });
            }

            // Do the jump
            locator_t const loc = locator_t::fn(d.fn, d.label);
            proc.push_inst({ .op = LDY_IMMEDIATE, .iasm_child = iasm_child, .arg = loc.with_is(IS_BANK) });
            if(bankswitch_use_x())
                proc.push_inst({ .op = mapper().bankswitches() ? BANKED_X_JMP : JMP_ABSOLUTE, .iasm_child = iasm_child, .arg = loc });
            else
                proc.push_inst({ .op = mapper().bankswitches() ? BANKED_Y_JMP : JMP_ABSOLUTE, .iasm_child = iasm_child, .arg = loc });

            // Assign the proc:
            d.rom_proc.safe().assign(std::move(proc));
        }
    }
}

void fn_t::for_each_fn(std::function<void(fn_ht)> const& callback) const
{
    callback(handle());
}

////////////////////
// global_datum_t //
////////////////////

void global_datum_t::dethunkify(bool full)
{
    assert(compiler_phase() == PHASE_RESOLVE || compiler_phase() == PHASE_COUNT_MEMBERS);
    m_src_type.type = ::dethunkify(m_src_type, full);
}

void global_datum_t::resolve()
{
    assert(compiler_phase() == PHASE_RESOLVE);
    dethunkify(true);

    if(!init_expr)
    {
        if(::is_paa(m_src_type.type.name()) &&  m_src_type.type.size_of() == 0)
            compiler_error(global.pstring(), "Invalid size of 0.");
        return;
    }

    if(m_src_type.type.name() == TYPE_PAA)
    {
        passert(paa_def(), global.name);
        _resolve_local_consts(global.handle(), m_def->local_consts);

        auto data = interpret_byte_block(global.pstring(), *init_expr, nullptr, paa_def()->local_consts.data());
        std::size_t data_size = 0;

        if(auto const* proc = std::get_if<asm_proc_t>(&data))
            data_size = proc->size();
        else if(auto const* vec = std::get_if<loc_vec_t>(&data))
            data_size = vec->size();
        else
            assert(false);

        unsigned const def_length = m_src_type.type.array_length();
        if(def_length && def_length != data_size)
             compiler_error(m_src_type.pstring, fmt("Length of data (%) does not match its type %.", data_size, m_src_type.type));

        m_src_type.type.set_array_length(data_size);
        std::visit([this](auto&& v){ paa_init(std::move(v)); }, data);
    }
    else
    {
        rpair_t rpair = interpret_expr(global.pstring(), *init_expr, m_src_type.type);
        m_src_type.type = std::move(rpair.type); // Handles unsized arrays
        if(::is_paa(m_src_type.type.name()) &&  m_src_type.type.size_of() == 0)
            compiler_error(global.pstring(), "Invalid size of 0.");
        rval_init(std::move(rpair.value));
    }
}

void global_datum_t::precheck()
{
    assert(compiler_phase() == PHASE_PRECHECK);
}

void global_datum_t::compile()
{
    assert(compiler_phase() == PHASE_COMPILE);
}

////////////
// gvar_t //
////////////

group_ht gvar_t::group() const { return group_vars ? (*group_vars)->handle() : group_ht{}; }

void gvar_t::paa_init(loc_vec_t&& paa) 
{ 
    m_init_data = std::move(paa); 
}
void gvar_t::paa_init(asm_proc_t&& proc) 
{ 
    proc.build_label_offsets();
    m_init_data = std::move(proc); 
}

void gvar_t::rval_init(rval_t&& rval)
{
    m_rval = std::move(rval);

    loc_vec_t vec;
    append_locator_bytes(false, vec, m_rval, m_src_type.type, global.pstring());
    m_init_data = std::move(vec);
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

void gvar_t::link_init()
{
    if(asm_proc_t* proc = std::get_if<asm_proc_t>(&m_init_data))
    {
        proc->build_label_offsets();
        proc->link(ROMV_MODE, -1);
        proc->relocate(locator_t::addr(begin()->span(0).addr));
        m_linked_init = proc->loc_vec();
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

locator_t const* gmember_t::init_data(unsigned atom, loc_vec_t const& vec) const
{
    unsigned const offset = ::member_offset(gvar.type(), member());
    return vec.data() + offset + atom;
}

std::size_t gmember_t::init_size() const
{
    return ::num_offsets(type());
}

std::size_t gmember_t::init_span() const
{
    return ::num_atoms(type(), 0);
}

bool gmember_t::zero_init(unsigned atom) const
{
    if(!gvar.init_expr)
        return false;

    if(loc_vec_t const* vec = std::get_if<loc_vec_t>(&gvar.init_data()))
    {
        std::size_t const size = init_size();
        std::size_t const span = init_span();
        locator_t const* data = init_data(atom, *vec);

        for(unsigned i = 0; i < size; ++i)
            if(!data[i * span].eq_const(0))
                return false;

        return true;
    }

    return false;
}

/////////////
// const_t //
/////////////

group_ht const_t::group() const { return group_data ? (*group_data)->handle() : group_ht{}; }

void const_t::paa_init(loc_vec_t&& vec)
{
    rom_rule_t rule = ROMR_NORMAL;
    if(mod_test(mods(), MOD_sector))
        rule = ROMR_SECTOR;
    else if(mod_test(mods(), MOD_dpcm))
        rule = ROMR_DPCM;
    else if(!group_data || mod_test(mods(), MOD_static))
        rule = ROMR_STATIC;

    m_rom_array = rom_array_t::make(std::move(vec), mod_test(mods(), MOD_align), !banked, rule, group_data);

    assert(m_rom_array);
}

void const_t::paa_init(asm_proc_t&& proc)
{
    try
    {
        proc.absolute_to_zp();
        proc.build_label_offsets();
        proc.relocate(locator_t::gconst(handle()));
    }
    catch(relocate_error_t const& e)
    {
        compiler_error(global.pstring(), e.what());
    }

    assert(m_def);
    auto& def = *m_def;

    // Copy the offsets from 'proc' into 'def.offsets',
    // as proc won't stick around.
    def.offsets.resize(def.local_consts.size(), 0);
    for(auto const& pair : proc.labels)
        if(pair.first.lclass() == LOC_NAMED_LABEL && pair.first.global() == global.handle())
            def.offsets[pair.first.data()] = pair.second.offset;

    paa_init(proc.loc_vec());
}

void const_t::rval_init(rval_t&& rval)
{
    m_rval = std::move(rval);
}

std::int64_t const_t::eval_chrrom_offset() const
{
    assert(chrrom);
    assert(compiler_phase() >= PHASE_COMPILE);

    rpair_t const result = interpret_expr(global.pstring(), *chrrom, TYPE_INT);
    if(calc_time(result.type, result.value) >= LT)
        compiler_error(global.pstring(), "Unable to determine chrrom offset at compile-time.");

    std::int64_t offset = std::get<ssa_value_t>(result.value[0]).signed_whole();

    if(offset < 0)
        compiler_error(global.pstring(), fmt("Offset of % is not positive.", offset));

    return offset;
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
        {
            m_has_tea_member = true;
            type = dethunkify({ field(i).decl.src_type.pstring, type.elem_type() }, false);
        }

        if(type.name() == TYPE_STRUCT)
        {
            struct_t& s = const_cast<struct_t&>(type.struct_());
            if(&s == this)
                compiler_error(global.pstring(), fmt("% has a recursive definition.", global.name));
            s.count_members();
            assert(s.m_num_members != UNCOUNTED);
            count += s.m_num_members;
            m_has_tea_member |= s.m_has_tea_member;
        }
        else
        {
            passert(is_vec(type.name()) || !is_aggregate(type.name()), type);
            count += ::num_members(type);
        }
    }

    return m_num_members = count;
}

void struct_t::resolve()
{
    assert(compiler_phase() == PHASE_RESOLVE);

    // Dethunkify
    for(unsigned i = 0; i < fields().size(); ++i)
        const_cast<type_t&>(field(i).type()) = dethunkify(field(i).decl.src_type, true);

    gen_member_types(*this);
}

void struct_t::precheck() { assert(compiler_phase() == PHASE_PRECHECK); }
void struct_t::compile() { assert(compiler_phase() == PHASE_COMPILE); }

// Builds 'm_member_types' and dethunkifies the struct.
void struct_t::gen_member_types(struct_t const& s, int tea_size)
{
    std::uint16_t offset = 0;
    
    for(unsigned i = 0; i < s.fields().size(); ++i)
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
            assert(is_vec(type.name()) || !is_aggregate(type.name()));
            assert(tea_size <= 256);

            if(tea_size >= 0)
                type = type_t::tea(type, tea_size, global.pstring());

            unsigned const num = ::num_members(type);
            passert(num > 0, type, s.field(i).type(), global.name, i , fields().size());

            for(unsigned i = 0; i < num; ++i)
            {
                m_member_types.push_back(::member_type(type, i));
                m_member_offsets.push_back(offset);
                offset += ::member_type(type, i).size_of();
            }
        }
    }
}

///////////////
// charmap_t //
///////////////

charmap_t::charmap_t(global_t& global, bool is_default, 
                     string_literal_t const& characters, 
                     string_literal_t const& sentinel,
                     unsigned offset,
                     std::unique_ptr<mods_t> new_mods)
: modded_t(std::move(new_mods))
, global(global)
, is_default(is_default)
, m_offset(offset)
{
    if(offset >= 256)
        compiler_error(global.pstring(), fmt("charmap offset of % is too large.", offset));

    auto const add_literal = [&](string_literal_t const& lit)
    {
        if(lit.string.empty())
            return;

        char const* ptr = lit.string.data();
        char const* char_begin = ptr;
        char32_t utf32 = escaped_utf8_to_utf32(lit.pstring, ptr);
        assert(ptr != char_begin);

        char const* end = lit.string.data() + lit.string.size();
        while(true)
        {
            if(!ptr)
                compiler_error(lit.pstring, "Invalid character in charmap.");

            if(utf32 == SPECIAL_SLASH)
                compiler_error(lit.pstring, "Invalid '\\/' operator.");

            auto result = m_map.insert({ utf32, m_num_unique });

            if(!result.second)
                compiler_error(lit.pstring, fmt("Duplicate character: '%'", std::string_view(char_begin, ptr)));

            if(ptr == end)
            {
                ++m_num_unique;
                break;
            }

            char_begin = ptr;
            utf32 = escaped_utf8_to_utf32(ptr);

            if(utf32 == SPECIAL_SLASH)
            {
                char_begin = ptr;
                utf32 = escaped_utf8_to_utf32(ptr);
                if(!ptr)
                    compiler_error(lit.pstring, "Invalid '\\/' operator.");
            }
            else
                ++m_num_unique;
        }
    };

    add_literal(characters);

    if(m_num_unique > 256)
        compiler_error(global.pstring(), fmt("Too many characters (%) in charmap. Max: 256.", m_num_unique));

    if(m_num_unique == 0)
        compiler_error(global.pstring(), "Empty charmap");

    if(!sentinel.string.empty())
    {
        char const* ptr = sentinel.string.data();
        char32_t utf32 = escaped_utf8_to_utf32(ptr);

        if(ptr != sentinel.string.data() + sentinel.string.size())
            compiler_error(sentinel.pstring, "Invalid sentinel character.");

        int const converted = convert(utf32);
        if(converted < 0)
            compiler_error(sentinel.pstring, "Sentinel character must appear in charmap.");

        m_sentinel = converted;
    }
}

int charmap_t::convert(char32_t ch) const
{
    if(auto* result = m_map.lookup(ch))
        return (result->second + offset()) & 0xFF;
    return -1;
}

void charmap_t::create_group_data()
{
    assert(compiler_phase() == PHASE_PARSE_CLEANUP);

    if(mods() && (mods()->explicit_lists & MODL_STOWS))
    {
        m_stows_omni = mods()->details & MODD_STOWS_OMNI;

        mods()->for_each_list(MODL_STOWS, [&](group_ht g, pstring_t pstring)
        {
            g->define_data(global.pstring(), m_stows_omni);
        });
    }
}

void charmap_t::set_group_data()
{
    assert(compiler_phase() == PHASE_CHARMAP_GROUPS);
    assert(!m_group_data);

    if(mods() && (mods()->explicit_lists & MODL_STOWS))
    {
        mods()->for_each_list(MODL_STOWS, [&](group_ht g, pstring_t pstring)
        {
            if(m_group_data)
                compiler_error(pstring, "Too many 'stows' mods. Expecting one or zero.");

            m_group_data = g->data_handle();

            if(!m_group_data)
                compiler_error(global.pstring(), "Group argument of 'stows' modifier was not defined.");
        });

        if(!m_group_data)
            compiler_error(global.pstring(), "Invalid 'stows' modifier.");
    }
}

void charmap_t::set_all_group_data()
{
    assert(compiler_phase() == PHASE_CHARMAP_GROUPS);

    for(charmap_t& charmap : charmap_ht::values())
        charmap.set_group_data();
}

//////////////
// fn_set_t //
//////////////

global_t& fn_set_t::lookup(char const* source, pstring_t pstring)
{
    assert(compiler_phase() <= PHASE_PARSE);

    global_t* g = nullptr;

    std::uint64_t hash = fnv1a<std::uint64_t>::hash(pstring.view(source));

    {
        std::lock_guard<std::mutex> lock(m_fns_mutex);
        g = &m_fns_map.lookup(extend(pstring), pstring.view(source));
        auto result = m_fn_hashes.insert({ hash, g });
        if(!result.second && result.first->first != hash)
            compiler_error(pstring, "Hash collision in names.");
    }

    assert(g);
    global.add_idep(*g, { IDEP_VALUE, IDEP_VALUE });
    return *g;
}

global_t* fn_set_t::lookup_hash(std::uint64_t hash) const
{
    assert(compiler_phase() > PHASE_PARSE);
    if(global_t* const* g = m_fn_hashes.mapped(hash))
        return *g;
    return nullptr;
}

void fn_set_t::count_members()
{
    m_all_static = true;

    for(auto const& pair : m_fn_hashes)
    {
        fn_t const& fn = pair.second->impl<fn_t>();
        m_all_static &= mod_test(fn.mods(), MOD_static);
    }

    m_banked_ptrs = mapper().num_banks > 1 && !m_all_static;
}

void fn_set_t::resolve()
{
    for(auto const& pair : m_fn_hashes)
        m_fns.push_back(pair.second->handle<fn_ht>());

    pstring_t type_pstring = {};

    for(fn_ht fn : m_fns)
    {
        assert(fn->global.resolved());

        if(m_type.name() == TYPE_VOID)
        {
            m_type = fn->type();
            type_pstring = fn->global.pstring();
        }
        else if(m_type != fn->type())
        {
            throw compiler_error_t(
                fmt_error(fn->global.pstring(), 
                          fmt("Type of % (%) does not match type of fn set % (%).",
                              fn->global.name, fn->type(), global.name, m_type))
                + fmt_note(type_pstring, "fn set had its type defined here."));
        }
    }
}

void fn_set_t::precheck()
{
    xbitset_t<gmember_ht>  rw(0);
    xbitset_t<group_vars_ht> group_vars(0);
    xbitset_t<fn_ht> calls(0);

    if(m_fn_hashes.empty())
        compiler_error(global.pstring(), fmt("fn set % has no members.", global.name));

    for(fn_ht fn : *this)
    {
        rw         |= fn->m_precheck_rw;
        group_vars |= fn->m_precheck_group_vars;
        calls      |= fn->m_precheck_calls;

        m_precheck_fences   |= fn->m_precheck_fences;
        m_precheck_wait_nmi |= fn->m_precheck_wait_nmi;
    }

    m_precheck_rw = std::move(rw);
    m_precheck_group_vars = std::move(group_vars);
    m_precheck_calls = std::move(calls);
}

void fn_set_t::compile()
{
    xbitset_t<gmember_ht>  reads(0);
    xbitset_t<gmember_ht> writes(0);
    xbitset_t<group_vars_ht> group_vars(0);
    xbitset_t<group_ht> deref_groups(0);
    xbitset_t<fn_ht> calls(0);

    for(fn_ht fn : *this)
    {
        reads        |= fn->ir_reads();
        writes       |= fn->ir_writes();
        group_vars   |= fn->ir_group_vars();
        deref_groups |= fn->ir_deref_groups();
        calls        |= fn->ir_calls();

        m_ir_tests_ready |= fn->ir_tests_ready();
        m_ir_tests_ready |= fn->ir_io_pure();
        m_ir_tests_ready |= fn->ir_fences();
        m_returns_in_different_bank |= fn->returns_in_different_bank();
    }

     m_ir_reads = std::move(reads);
     m_ir_writes = std::move(writes);
     m_ir_group_vars = std::move(group_vars);
     m_ir_deref_groups = std::move(deref_groups);
     m_ir_calls = std::move(calls);
}

void fn_set_t::for_each_fn(std::function<void(fn_ht)> const& callback) const
{
    for(fn_ht fn : *this)
        callback(fn);
}

span_t fn_set_t::lvar_span(romv_t romv, locator_t loc) const
{
    if(begin() == end())
        return {};

    fn_ht const first = *begin();
    loc.set_handle(first.id);

    if(loc.lclass() == LOC_PTR_ARG)
        loc.set_lclass(LOC_ARG);
    else if(loc.lclass() == LOC_PTR_RETURN)
        loc.set_lclass(LOC_RETURN);
    else
        assert(false);

    return first->lvar_span(romv, loc);

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

fn_t const& get_main_mode()
{
    assert(compiler_phase() > PHASE_PARSE);

    using namespace std::literals;
    global_t const* main_mode = global_t::lookup_sourceless("main"sv);

    if(!main_mode || main_mode->gclass() != GLOBAL_FN || main_mode->impl<fn_t>().fclass != FN_MODE)
        throw compiler_error_t(fmt_error("Missing definition of mode main. Program has no entry point."));

    fn_t const& main_fn = main_mode->impl<fn_t>();
    if(main_fn.def().num_params > 0)
        compiler_error(main_fn.def().local_vars[0].decl.name, "Mode main cannot have parameters.");

    return main_fn;
}

void add_idep(ideps_map_t& map, global_t* global, idep_pair_t pair)
{
    auto result = map.emplace(global, pair);
    if(!result.second)
    {
        idep_pair_t& prev = result.first.underlying->second;
        prev.calc = std::min(prev.calc, pair.calc);
        prev.depends_on = std::max(prev.depends_on, pair.depends_on);
    }
}

charmap_t const& get_charmap(pstring_t from, global_t const& global)
{
    if(global.gclass() != GLOBAL_CHARMAP)
        compiler_error(from, fmt("% is not a charmap.", global.name));
   return global.impl<charmap_t>();
}
