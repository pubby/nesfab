#include "globals.hpp"

#include <iostream>
#include <fstream>

#include "alloca.hpp"
#include "bitset.hpp"
#include "compiler_error.hpp"
#include "compiler_limits.hpp"
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
                          type_t type, fn_def_t&& fn_def, bool mode)
{
    fn_t* ret;

    // Create the fn
    define(pstring, GLOBAL_FN, std::move(ideps), std::move(weak_ideps), [&](global_t& g)
    { 
        return impl_deque_alloc<fn_t>(ret, g, type, std::move(fn_def), mode); 
    });

    if(mode)
    {
        std::lock_guard<std::mutex> lock(modes_vec_mutex);
        modes_vec.push_back(ret);
    }

    return *ret;
}

gvar_t& global_t::define_var(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                             type_t type, std::pair<group_vars_t*, group_vars_ht> group)
{
    gvar_t* ret;

    // TODO: sleep

    // Create the var
    gvar_ht h = { define(pstring, GLOBAL_VAR, std::move(ideps), {}, [&](global_t& g)
    { 
        return impl_deque_alloc<gvar_t>(ret, g, type, group.second);
    })};

    // Add it to the group
    assert(group.first);
    group.first->add_gvar(h);
    
    return *ret;
}

const_t& global_t::define_const(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                                type_t type, std::pair<group_data_t*, group_data_ht> group)
{
    const_t* ret;

    // Create the const
    const_ht h = { define(pstring, GLOBAL_CONST, std::move(ideps), {}, [&](global_t& g)
    { 
        return impl_deque_alloc<const_t>(ret, g, type, group.second);
    })};

    // Add it to the group
    assert(group.first);
    group.first->add_const(h);
    
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
    assert(this != &other);
    
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
            if(global.m_pstring)
            {
                file_contents_t file(global.m_pstring.file_i);
                compiler_error(file, global.m_pstring, "Name not in scope.");
            }
            else
                throw compiler_error_t(fmt("Name not in scope: %.", global.name));
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

#ifdef DEBUG_PRINT
    std::cout << "COMPILING " << name << std::endl;
#endif

    // Compile it!
    switch(gclass())
    {
    default:
        throw std::runtime_error("Invalid global.");
    case GLOBAL_FN:
        {
            fn_t& fn = this->impl<fn_t>();

            // Compile the FN.
            ssa_pool::clear();
            cfg_pool::clear();
            ir_t ir;
            build_ir(ir, fn);

            if(compiler_options().graphviz)
                save_graph(ir, "initial");

            ir.assert_valid();

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
            fn.calc_ir_bitsets(ir);

            byteify(ir, fn);
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

            code_gen(ir, fn);

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

    for(fn_t const& fn : impl_deque<fn_t>)
        fn.proc().write_assembly(std::cout, fn);
#if 0
    assert(compiler_phase() == PHASE_ALLOC_RAM);

    for(fn_t* mode : modes_vec)
    {
        assert(mode);
        assert(mode->mode);

        // Build interference graph among 'group vars':
        assert(mode->ir_group_vars());
        mode->ir_group_vars().for_each([&](unsigned i)
        {
            group_vars_ht{i}->add_interferences(mode->ir_group_vars().data());
        });

        /*
        std::cout << "MODE = " << mode.global.name << std::endl;
        bitset_for_each(mode.ir_groups(), [](unsigned bit)
        {
            group_ht group = {bit};
            std::cout << "group = " << group->global.name << std::endl;
        });
        */
    }

    // First we'll allocate gvars:

    rh::batman_map<locator_t, unsigned> gvar_count;
    for(gvar_t const& gvar : impl_deque<gvar_t>)
        gvar.for_each_locator([&](locator_t loc){ gvar_count.insert({ loc, 0 }); });

    for(fn_t const& fn : impl_deque<fn_t>)
    {
        if(!fn.emits_code())
            continue;

        for(asm_inst_t const& inst : fn.proc().code)
            if(inst.arg.lclass() == LOC_GVAR)
                if(unsigned* count = gvar_count.mapped(inst.arg.mem_head()))
                    *count += 1;
    }

    struct gvar_rank_t
    {
        float score;
        locator_t loc;
    };

    std::vector<gvar_rank_t> ordered_gvars_zp;
    std::vector<gvar_rank_t> ordered_gvars;

    // Track which gvars are unused and use them to generate warning messages.
    fc::vector_set<gvar_ht> unused_gvars;

    for(auto const& pair : gvar_count)
    {
        if(pair.second == 0)
            unused_gvars.insert(pair.first.gvar());

        if(pair.first.mem_zp_only())
            ordered_gvars_zp.push_back({ pair.first.mem_size(), pair.first });
        else
            ordered_gvars.push_back({ (pair.first.mem_size() * 1000.0f) + pair.second, pair.first });
    }

    for(gvar_ht gvar : unused_gvars)
        compiler_warning(gvar->global.pstring(), "Global variable wastes RAM (not every byte is used).");

    std::sort(ordered_gvars_zp.begin(), ordered_gvars_zp.end(), 
              [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });
    std::sort(ordered_gvars.begin(), ordered_gvars.end(), 
              [](auto const& lhs, auto const& rhs) { return lhs.score > rhs.score; });

    for(auto const& rank : ordered_gvars)
    {
        std::cout << rank.loc.gvar().value << ' ' << rank.score << '\n';
    }

    std::vector<ram_bitset_t> group_vars_usable_ram;
    group_vars_usable_ram.resize(impl_deque<group_vars_t>.size(), ram_bitset_t::filled());

    auto const alloc_gvar_loc = [&](locator_t loc)
    {
        std::cout << loc.gvar().value << '\n';
        // - lookup the group
        gvar_t& gvar = *loc.gvar();
        group_vars_t& group_vars = gvar.group_vars;

        span_t const span = ::alloc_ram(group_vars_usable_ram[group_vars.handle().value],
                                        loc.mem_size(), loc.mem_zp_only());

        if(!span)
            throw std::runtime_error("Unable to allocate global variable (out of RAM).");

        ram_bitset_t const mask = ~ram_bitset_t::filled(span.size, span.addr);

        std::cout << "group = " << group_vars.handle().value << std::endl;
        group_vars_usable_ram.at(group_vars.handle().value) &= mask;
        //group_vars_usable_ram.at(group_vars.handle().value).clear(span.addr);
        // TODO: change how bitset is allocated
        group_vars.interfering_group_vars().for_each([&](unsigned i)
        {
            std::cout << "groups = " << i << std::endl;
            group_vars_usable_ram[i] &= mask;
        });

        std::cout << loc << " = " << span << std::endl;
    };

    for(gvar_rank_t const& rank : ordered_gvars_zp)
        alloc_gvar_loc(rank.loc);

    for(gvar_rank_t const& rank : ordered_gvars)
        alloc_gvar_loc(rank.loc);


    {
        // Now for locals

        std::vector<ram_bitset_t> usable_ram;
        usable_ram.resize(fn.lvars.num_lvars(), starting_ram);

        for(unsigned i = 0; i < fn.lvars.num_lvars(); ++i)
        {
            usable_ram[i] &= fn.usable_ram;
        }



    }

    // What do gvars interfere with?
    // - gvars in their group
    // - gvars in groups 

    // 1. order gvars by usage
    // 2. 

    /*
    struct weighted_gvar_t
    {
        unsigned weight;
        gvar_ht gvar;
        bool operator<=>(gvar_usage const& o) const = default;
    };

    std::vector<weighted_gvar_t> order;

    /*
    for(gvar_ht gvar : TODO)
    {

    }
    */
#endif
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

            if(fn.mode)
                continue;

            fn.calc_lang_gvars_groups();

            assert(fn.m_lang_gvars);
            assert(fn.m_lang_group_vars);

            m_lang_gvars |= fn.m_lang_gvars;
            m_lang_group_vars |= fn.m_lang_group_vars;
        }
    }
}

void fn_t::calc_ir_bitsets(ir_t const& ir)
{
    bitset_t  reads(impl_bitset_size<gvar_t>());
    bitset_t writes(impl_bitset_size<gvar_t>());
    bitset_t group_vars(impl_bitset_size<group_vars_t>());
    bitset_t immediate_group_data(impl_bitset_size<group_data_t>());
    bitset_t calls(impl_bitset_size<fn_t>());
    bool io_pure = true;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_flags(ssa_it->op()) & SSAF_IMPURE)
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
            [this, &writes, &group_vars](ssa_value_t def, locator_t loc)
            {
                if(loc.lclass() == LOC_GVAR)
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
                        writes.set(loc.gvar().value);
                        group_vars.set(written.group_vars.value);
                    }
                }
            });
        }
        else if(ssa_it->op() == SSA_read_global)
        {
            assert(ssa_it->input_size() == 2);
            locator_t loc = ssa_it->input(1).locator();

            if(loc.lclass() == LOC_GVAR)
            {
                gvar_t const& read = *loc.gvar();
                assert(read.global.gclass() == GLOBAL_VAR);

                // Reads only have effect if something actually uses them:
                for(unsigned i = 0; i < ssa_it->output_size(); ++i)
                {
                    auto oe = ssa_it->output_edge(i);
                    // TODO: verify this is correct
                    if(!is_locator_write(oe)
                       || oe.handle->input(oe.index + 1) != loc)
                    {
                        reads.set(loc.gvar().value);
                        group_vars.set(read.group_vars.value);
                        break;
                    }
                }
            }
        }
    }

    m_ir_writes = std::move(writes);
    m_ir_reads  = std::move(reads);
    m_ir_group_vars = std::move(group_vars);
    m_ir_immediate_group_data = std::move(immediate_group_data);
    m_ir_calls = std::move(calls);
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
    if(lvar_i < m_lvars.num_this_lvars())
        return m_lvar_spans[lvar_i];

    locator_t loc = m_lvars.locator(lvar_i);
    if(loc.lclass() == LOC_CALL_ARG)
    {
        loc.set_lclass(LOC_THIS_ARG);
        int index = loc.fn()->m_lvars.index(loc);

        if(index < 0)
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

////////////
// gvar_t //
////////////

void gvar_t::alloc_spans()
{
    assert(compiler_phase() == PHASE_ALLOC_RAM);
    assert(m_spans.empty());
    m_spans.resize(num_fields(type));
}

void gvar_t::for_each_locator(std::function<void(locator_t)> const& fn) const
{
    assert(compiler_phase() > PHASE_COMPILE);
    unsigned const num = num_fields(type);
    for(unsigned field = 0; field < num; ++field)
        fn(locator_t::gvar(handle(), field));
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

