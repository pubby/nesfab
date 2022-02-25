#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <cassert>
#include <ostream>

#include "robin/collection.hpp"
#include "robin/set.hpp"

#include "flat/flat_set.hpp"

#include "asm_proc.hpp"
#include "array_pool.hpp"
#include "bitset.hpp"
#include "file.hpp"
#include "decl.hpp"
#include "parser_types.hpp"
#include "phase.hpp"
#include "ram.hpp"
#include "stmt.hpp"
#include "types.hpp"
#include "lvar.hpp"

namespace bc = boost::container;

std::string to_string(global_class_t gclass);

/* TODO
class group_t
{
public:
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_GROUP;

    group_ht handle() const;

    explicit group_t(global_t& global)
    : global(global)
    {}

    global_t& global;

    void add_gvar(gvar_ht gvar)
    {
        assert(compiler_phase() <= PHASE_PARSE);
        std::lock_guard<std::mutex> lock(m_gvars_mutex);
        m_gvars.push_back(gvar);
    }

    void add_interference(group_ht group);
    void add_interferences(bitset_uint_t const* other_groups);

    void mark_ram_unavailable(ram_bitset_t ram)
    {
        m_usable_ram &= ~ram;
    }

private:
    std::mutex m_gvars_mutex; // Used during parsing only.
    std::vector<gvar_ht> m_gvars;

    std::mutex m_interfering_groups_mutex;
    bitset_t m_interfering_groups;

    ram_bitset_t m_usable_ram = {};
};
*/

class global_t
{
public:
    using ideps_set_t = fc::vector_set<global_t*>;
    std::string const name;
private:
    // These variables are set only by 'define', as soon
    // as the global's definition is parsed.
    std::mutex m_define_mutex;
    global_class_t m_gclass = GLOBAL_UNDEFINED;
    pstring_t m_pstring = {};

    // An index into some storage that holds the global's implementation data
    unsigned m_impl_index = ~0;

    // 'ideps' means "immediate dependencies".
    // AKA any global name that appears in the definition of this global.
    // This is set by 'define'
    ideps_set_t m_ideps;

    // 'weak_ideps' exist to handle recursive mode gotos.
    // These can be converted into regular ideps, if no cycles will be created.
    // Otherwise, they're simply discarded.
    ideps_set_t m_weak_ideps;

    // Likewise, 'iuses' holds the immediate users of this global.
    // This is built after all globals have been created, after parsing.
    ideps_set_t m_iuses;
    std::atomic<unsigned> m_ideps_left = 0;
public:
    explicit global_t(std::string_view name)
    : name(name)
    {}

    global_t(pstring_t pstring, char const* source)
    : name(pstring.view(source))
    , m_pstring(pstring)
    {}

    global_class_t gclass() const 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_gclass;
    }

    ideps_set_t const& ideps() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        return m_ideps;
    }

    pstring_t pstring() const { return m_pstring; }

    unsigned index() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_impl_index; 
    }

    template<typename T>
    T handle() const
    {
        static_assert(is_global_handle<T>::value);
        assert(gclass() == T::gclass);
        assert(compiler_phase() > PHASE_PARSE);
        return { m_impl_index };
    }

    template<typename T>
    T& impl() const
    {
        static_assert(is_global_impl<T>::value);
        assert(gclass() == T::gclass);
        assert(compiler_phase() > PHASE_PARSE);
        return impl_deque<T>[m_impl_index];
    }

    // If this global has a dependency to 'other'
    bool has_dep(global_t& other);

    // Helpers that delegate to 'define':
    fn_t& define_fn(pstring_t pstring, 
                    global_t::ideps_set_t&& ideps, global_t::ideps_set_t&& weak_ideps, 
                    type_t type, fn_def_t&& fn_def, bool mode);
    gvar_t& define_var(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                      type_t type, std::pair<group_vars_t*, group_vars_ht> group);
    const_t& define_const(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                          type_t type, std::pair<group_data_t*, group_data_ht> group);

    static void init();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(char const* source, pstring_t name);

    // TODO
    //static global_t& lookup_sourceless(std::string_view view);
    //static group_ht universal_group() { return {0}; }

    // Call after parsing
    static void parse_cleanup();

    // Call after parse_cleanup to build 'm_iuses' and 'm_ideps_left',
    // among other things.
    // This function isn't thread-safe.
    // Call from a single thread only.
    static void build_order();

    // Call after 'build_order' to well... compile everything!
    static void compile_all();

    // Call after 'compile_all'.
    // Assigns variables to ram addresses.
    static void alloc_ram();

    static std::vector<fn_t*> modes() { assert(compiler_phase() > PHASE_PARSE); return modes_vec; }
private:
    // Sets the variables of the global:
    unsigned define(pstring_t pstring, global_class_t gclass, 
                    ideps_set_t&& ideps, ideps_set_t&& weak_ideps,
                    std::function<unsigned(global_t&)> create_impl);

    void compile();

    // Returns and pops the next ready global from the ready list.
    static global_t* await_ready_global();

private:
    // Globals get allocated in these:
    inline static std::mutex global_pool_mutex;
    inline static rh::robin_auto_table<global_t*> global_pool_map;
    static std::deque<global_t> global_pool;

    // Tracks modes: 
    inline static std::mutex modes_vec_mutex;
    inline static std::vector<fn_t*> modes_vec;

    // These represent a queue of globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    static std::vector<global_t*> ready;
    inline static unsigned globals_left;
};

class fn_t
{
public:
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_FN;

    fn_t(global_t& global, type_t type, fn_def_t fn_def, bool mode) 
    : global(global)
    , type(std::move(type))
    , def(std::move(fn_def)) 
    , mode(mode)
    {}

    fn_ht handle() const;

    void calc_lang_gvars_groups();
    void calc_ir_bitsets(ir_t const& ir);

    bitset_t const& lang_gvars()  const { assert(m_lang_gvars);  return m_lang_gvars; }
    bitset_t const& lang_group_vars() const { assert(m_lang_group_vars); return m_lang_group_vars; }

    // These are only valid after 'calc_ir_reads_writes_purity' has ran.
    bitset_t const& ir_reads()  const { assert(m_ir_reads);  return m_ir_reads; }
    bitset_t const& ir_writes() const { assert(m_ir_writes); return m_ir_writes; }
    bitset_t const& ir_group_vars() const { assert(m_ir_group_vars); return m_ir_group_vars; }
    bitset_t const& ir_immediate_group_data() const { assert(m_ir_immediate_group_data); return m_ir_immediate_group_data; }
    bitset_t const& ir_calls() const { assert(m_ir_calls); return m_ir_calls; }
    bool ir_io_pure() const { assert(m_ir_writes); return m_ir_io_pure; }

    bool ir_reads(gvar_ht gvar)  const { return ir_reads().test(gvar.value); }
    bool ir_writes(gvar_ht gvar) const { return ir_writes().test(gvar.value); }

    // Be careful to call this from a single thread only.
    void assign_proc(asm_proc_t&& proc)
    {
        assert(compiler_phase() == PHASE_COMPILE);
        m_proc = std::move(proc);
    }

    asm_proc_t const& proc() const { assert(compiler_phase() > PHASE_COMPILE); return m_proc; }

    void assign_lvars(lvars_manager_t&& lvars);
    lvars_manager_t const& lvars() const { assert(compiler_phase() >= PHASE_COMPILE); return m_lvars; }
    
    void mask_usable_ram(ram_bitset_t const& mask);
    ram_bitset_t const& usable_ram() const { return m_usable_ram; }

    ram_bitset_t const& lvar_ram() const { return m_lvar_ram; }
    void assign_lvar_span(unsigned lvar_i, span_t span);
    span_t lvar_span(unsigned lvar_i) const;

    bool emits_code() const { return true; } // TODO: implement

public:
    global_t& global;
    type_t const type;
    fn_def_t const def;
    bool const mode;

    //rom_alloc_ht rom_alloc; TODO
private:
    bitset_t m_lang_gvars;
    bitset_t m_lang_group_vars;

    // Bitsets of all global vars read/written in fn (deep)
    // These get assigned by 'calc_reads_writes_purity'.
    // The thread synchronization is implicit in the order of compilation.
    bitset_t m_ir_reads;
    bitset_t m_ir_writes;
    bitset_t m_ir_group_vars;
    bitset_t m_ir_immediate_group_data;
    bitset_t m_ir_calls;

    // If the function doesn't do I/O.
    // (Using mutable memory state is OK.)
    // Gets set by 'calc_reads_writes_purity'.
    bool m_ir_io_pure = false;

    // Holds the assembly code generated.
    asm_proc_t m_proc;

    ram_bitset_t m_usable_ram = ram_bitset_t::filled(); // Tracks unallocated RAM addresses this fn can use
    ram_bitset_t m_lvar_ram = ram_bitset_t::filled();// Tracks which addresses are used by this fn's lvars.

    // Aids in allocating RAM for local variables:
    lvars_manager_t m_lvars;
    std::vector<span_t> m_lvar_spans;
};
 
class gvar_t
{
public:
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_VAR;

    inline gvar_ht handle() const { return global.handle<gvar_ht>(); }

    gvar_t(global_t& global, type_t type, group_vars_ht group_vars)
    : global(global)
    , type(type)
    , group_vars(group_vars)
    {}

    global_t& global;
    type_t const type;
    group_vars_ht group_vars;

    //group_bitset_t group_bitset() const { return 1ull << group.value; }

    void for_each_locator(std::function<void(locator_t)> const& fn) const;

    void alloc_spans();
    span_t span(unsigned field) const { assert(compiler_phase() >= PHASE_ALLOC_RAM); return m_spans[field]; }
    void assign_span(unsigned field, span_t span) { assert(compiler_phase() == PHASE_ALLOC_RAM); m_spans[field] = span; }

private:
    std::vector<span_t> m_spans = {};
};

class const_t
{
public:
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_CONST;

    const_t(global_t& global, type_t type, group_data_ht group)
    : global(global)
    , type(type)
    , group(group)
    {}

    global_t& global;
    type_t const type;
    group_data_ht const group;
};

inline fn_ht fn_t::handle() const { return global.handle<fn_ht>(); }

#endif
