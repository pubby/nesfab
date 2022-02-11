#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <cassert>
#include <condition_variable>
#include <mutex>
#include <ostream>
#include <deque>

#include "robin/collection.hpp"
#include "robin/set.hpp"

#include "flat/flat_set.hpp"

#include "array_pool.hpp"
#include "bitset.hpp"
#include "file.hpp"
#include "globals_types.hpp"
#include "handle.hpp"
//#include "ir.hpp"
#include "parser_types.hpp"
#include "phase.hpp"
#include "ram.hpp"
#include "stmt.hpp"
//#include "symbol_table.hpp"
#include "types.hpp"

namespace bc = boost::container;
class global_t;

#define GLOBAL_CLASS_XENUM \
    X(GLOBAL_UNDEFINED) \
    X(GLOBAL_FN) \
    X(GLOBAL_VAR) \
    X(GLOBAL_CONST) \
    X(GLOBAL_VBANK) \
    X(GLOBAL_GROUP)

enum global_class_t : std::uint8_t
{
#define X(x) x,
    GLOBAL_CLASS_XENUM
#undef X
};

std::string to_string(global_class_t gclass);

// These vectors hold global implementation data, 
// which varies depending on the global's type.
// The mutex is only used during the parsing phase.
template<typename T> inline std::mutex global_impl_vec_mutex;
template<typename T> inline std::deque<T> global_impl_vec;

// Handles reference globals, with their '.value' indexing into 
// the corresponding 'global_impl_vec'.
template<typename T>
struct global_impl_ht : handle_t<unsigned, T, ~0>
{
    global_t& global() const { return operator*().global; }
    T* operator->() const { return &operator*(); }
    T& operator*() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return global_impl_vec<T>[this->value];
    }
};

struct fn_ht : global_impl_ht<class fn_t> {};
struct gvar_ht : global_impl_ht<class gvar_t> {};
struct const_ht : global_impl_ht<class const_t> {};
struct group_ht : global_impl_ht<class group_t> {};
struct vbank_ht : global_impl_ht<class vbank_t> {};

class fn_t
{
public:
    fn_t(global_t& global, type_t type, fn_def_t fn_def, bool mode) 
    : global(global)
    , type(std::move(type))
    , def(std::move(fn_def)) 
    , mode(mode)
    {}

    fn_ht handle() const;

    // TODO
    //std::vector<type_t> arg_bytes_types;
    //std::vector<addr16_t> arg_bytes;
    //std::vector<addr16_t> return_bytes;

    void calc_lang_gvars_groups();
    void calc_ir_reads_writes_purity(ir_t const& ir);
    //void alloc_args(ir_t const& ir); TODO

    bitset_uint_t const* lang_gvars() const { assert(m_lang_gvars); return m_lang_gvars; }
    group_bitset_t lang_groups() const { assert(m_lang_groups); return m_lang_groups; }

    // These are only valid after 'calc_ir_reads_writes_purity' has ran.
    bitset_uint_t const* ir_reads() const  { assert(m_ir_reads);  return m_ir_reads; }
    bitset_uint_t const* ir_writes() const { assert(m_ir_writes); return m_ir_writes; }
    group_bitset_t ir_groups() const  { assert(m_ir_writes);  return m_ir_groups; }
    bool ir_io_pure() const { assert(m_ir_writes); return m_ir_io_pure; }

    bool ir_reads(gvar_ht gvar) const { return bitset_test(ir_reads(), gvar.value); }
    bool ir_writes(gvar_ht gvar) const { return bitset_test(ir_writes(), gvar.value); }

    static std::size_t rw_bitset_size();

    // TODO: remove?
    bool compiled() const { return m_compiled; }
    void mark_compiled() { assert(!m_compiled); m_compiled = true; }

    // TODO: remove?
    //ram_bitset_t const& locals_ram() const { return m_locals_ram; }

    //rh::batman_set<fn_ht> const& asm_calls() const { assert(m_did_cg); return m_asm_calls; }

public:
    global_t& global;
    type_t const type;
    fn_def_t const def;
    bool const mode;
private:
    bitset_uint_t* m_lang_gvars = nullptr;
    group_bitset_t m_lang_groups = 0;

    // Bitsets of all global vars read/written in fn (deep)
    // These get assigned by 'calc_reads_writes_purity'.
    // The thread synchronization is implicit in the order of compilation.
    bitset_uint_t* m_ir_reads = nullptr;
    bitset_uint_t* m_ir_writes = nullptr;
    group_bitset_t m_ir_groups = 0;

    // If the function doesn't do I/O.
    // (Using mutable memory state is OK.)
    // Gets set by 'calc_reads_writes_purity'.
    bool m_ir_io_pure = false;

    // Tracks which RAM addresses are used by the fn.
    //ram_bitset_t m_immediate_locals_ram = {}; // Ignores called fns.
    //ram_bitset_t m_locals_ram = {}; // Includes ram used by called fns 

    //std::vector<addr16_t> m_arg_addrs;
    //ram_bitset_t m_recursive_arg_ram;

    std::atomic<bool> m_compiled = false;

    // Every fn called by this fn, unioned recursively
    // This is created after code generation for this fn.
    //rh::batman_set<fn_ht> m_asm_calls;

private:
    // Holds bitsets of 'm_reads' and 'm_writes'
    inline static std::mutex bitset_pool_mutex;
    inline static array_pool_t<bitset_uint_t> bitset_pool;
};
 
class gvar_t
{
public:
    gvar_t(global_t& global, type_t type, group_ht group)
    : global(global)
    , type(type)
    , group(group)
    {}

    global_t& global;
    type_t const type;
    group_ht const group;

    group_bitset_t group_bitset() const { return 1ull << group.value; }
};

class const_t
{
public:
    const_t(global_t& global, type_t type, vbank_ht vbank)
    : global(global)
    , type(type)
    , vbank(vbank)
    {}

    global_t& global;
    type_t const type;
    vbank_ht const vbank;
};

class group_t
{
public:
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

    void add_interferences(group_bitset_t other_groups);

    void mark_ram_unavailable(ram_bitset_t ram)
    {
        m_usable_ram &= ~ram;
    }

private:
    std::mutex m_gvars_mutex; // Used during parsing only.
    std::vector<gvar_ht> m_gvars;

    std::mutex m_interfering_groups_mutex;
    group_bitset_t m_interfering_groups = 0;

    ram_bitset_t m_usable_ram = {};
};

class vbank_t
{
public:
    explicit vbank_t(global_t& global)
    : global(global)
    {}

    global_t& global;

    void add_const(const_ht c)
    {
        assert(compiler_phase() <= PHASE_PARSE);
        std::lock_guard<std::mutex> lock(consts_mutex);
        consts.push_back(c);
    }

private:
    std::mutex consts_mutex; // Used during parsing only.
    std::vector<const_ht> consts;
};


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

    unsigned index() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_impl_index; 
    }

    template<typename T>
    T handle() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        return { m_impl_index };
    }

    template<typename T>
    T& impl() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        return global_impl_vec<T>[m_impl_index];
    }

    // If this global has a dependency to 'other'
    bool has_dep(global_t& other);

    // Helpers that delegate to 'define':
    fn_ht define_fn(pstring_t pstring, 
                    global_t::ideps_set_t&& ideps, global_t::ideps_set_t&& weak_ideps, 
                    type_t type, fn_def_t&& fn_def, bool mode);
    gvar_ht define_var(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                      type_t type, group_ht group);
    const_ht define_const(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                          type_t type, vbank_ht vbank);

    static void init();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(char const* source, pstring_t name);
    static global_t& lookup_sourceless(std::string_view view);

    // Creates a vbank / group if it doesn't exist:
    static vbank_ht lookup_vbank(file_contents_t const& file, pstring_t name);
    static group_ht lookup_group(file_contents_t const& file, pstring_t name);

    static group_ht universal_group() { return {0}; }

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

    static global_t* current() { return currently_compiling; }
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
    inline static std::vector<fn_ht> modes_vec;

    // These represent a queue of globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    static std::vector<global_t*> ready;
    inline static unsigned globals_left;

    inline static thread_local global_t* currently_compiling = nullptr;
};

inline fn_ht fn_t::handle() const { return global.handle<fn_ht>(); }

#endif
