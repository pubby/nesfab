#ifndef GLOBALS_HPP
#define GLOBALS_HPP

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
    fn_t(global_t& global, type_t type, fn_def_t fn_def) 
    : global(global)
    , type(std::move(type))
    , def(std::move(fn_def)) 
    {}

    // TODO
    //std::vector<type_t> arg_bytes_types;
    //std::vector<addr16_t> arg_bytes;
    //std::vector<addr16_t> return_bytes;

    void calc_reads_writes_purity(ir_t const& ir);
    void alloc_args(ir_t const& ir);

    // These are only valid after 'calc_reads_writes_purity' has ran.
    bitset_uint_t const* reads() const  { assert(m_reads);  return m_reads; }
    bitset_uint_t const* writes() const { assert(m_writes); return m_writes; }
    group_bitset_t groups() const  { assert(m_writes);  return m_groups; }
    bool io_pure() const { assert(m_writes); return m_io_pure; }

    static std::size_t rw_bitset_size();

    // TODO: remove?
    //ram_bitset_t const& locals_ram() const { return m_locals_ram; }

public:
    global_t& global;
    type_t const type;
    fn_def_t const def;
private:
    // Bitsets of all global vars read/written in fn (deep)
    // These get assigned by 'calc_reads_writes_purity'.
    // The thread synchronization is implicit in the order of compilation.
    bitset_uint_t* m_reads = nullptr;
    bitset_uint_t* m_writes = nullptr;
    group_bitset_t m_groups = 0;

    // If the function doesn't do I/O.
    // (Using mutable memory state is OK.)
    // Gets set by 'calc_reads_writes_purity'.
    bool m_io_pure = false;

    // Tracks which RAM addresses are used by the fn.
    //ram_bitset_t m_immediate_locals_ram = {}; // Ignores called fns.
    //ram_bitset_t m_locals_ram = {}; // Includes ram used by called fns 

    std::vector<addr16_t> m_arg_addrs;
    ram_bitset_t m_recursive_arg_ram;

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
        std::lock_guard<std::mutex> lock(gvars_mutex);
        gvars.push_back(gvar);
    }

private:
    std::mutex gvars_mutex; // Used during parsing only.
    std::vector<gvar_ht> gvars;
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
    pstring_t m_pstring;

    // An index into some storage that holds the global's implementation data
    unsigned m_impl_index;

    // 'ideps' means "immediate dependencies".
    // AKA any global name that appears in the definition of this global.
    // This is set by 'define'
    ideps_set_t m_ideps;

    // Likewise, 'iuses' holds the immediate users of this global.
    // This is built after all globals have been created, after parsing.
    ideps_set_t m_iuses;
    std::atomic<unsigned> m_ideps_left = 0;
public:
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

    // Helpers that delegate to 'define':
    fn_ht define_fn(pstring_t pstring,  global_t::ideps_set_t&& ideps, 
                    type_t type, fn_def_t&& fn_def);
    gvar_ht define_var(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                      type_t type, group_ht group);
    const_ht define_const(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                          type_t type, vbank_ht vbank);

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(pstring_t name, char const* source);

    // Creates a vbank / group if it doesn't exist:
    static vbank_ht lookup_vbank(pstring_t name, char const* source);
    static group_ht lookup_group(pstring_t name, char const* source);

    // Call after parsing to build 'm_iuses' and 'm_ideps_left',
    // among other things.
    // This function isn't thread-safe.
    // Call from a single thread only.
    static void build_order();

    // Call after 'build_order' to well... compile everything!
    static void compile_all();
private:
    // Sets the variables of the global:
    unsigned define(pstring_t pstring, global_class_t gclass, ideps_set_t&& ideps, 
                    std::function<unsigned(global_t&)> create_impl);

    void compile();

    // Returns and pops the next ready global from the ready list.
    static global_t* await_ready_global();

private:
    // Globals get allocated in these:
    inline static std::mutex global_pool_mutex;
    inline static rh::robin_auto_table<global_t*> global_pool_map;
    static std::deque<global_t> global_pool;

    // These represent a queue of globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    static std::vector<global_t*> ready;
    inline static unsigned globals_left;
};

#endif
