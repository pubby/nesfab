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

    // These are only valid after 'calc_reads_writes_purity' has ran.
    bitset_uint_t const* reads() const  { assert(m_reads);  return m_reads; }
    bitset_uint_t const* writes() const { assert(m_writes); return m_writes; }
    group_bitset_t groups() const  { assert(m_writes);  return m_groups; }
    bool io_pure() const { assert(m_writes); return m_io_pure; }

    static std::size_t rw_bitset_size();

    ram_bitset_t const& locals_ram() const { return m_locals_ram; }

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
    ram_bitset_t m_immediate_locals_ram = {}; // Ignores called fns.
    ram_bitset_t m_locals_ram = {}; // Includes ram used by called fns 

private:
    // Holds bitsets of 'm_reads' and 'm_writes'
    inline static std::mutex bitset_pool_mutex;
    inline static array_pool_t<bitset_uint_t> bitset_pool;
};
 
class gvar_t
{
public:
    global_t& global;
    type_t const type;
    group_ht const group;

    group_bitset_t group_bitset() const { return 1ull << group.value; }
};

class const_t
{
public:
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


/*

using ggvar_ht = handle_t<unsigned, struct ggvar_ht_tag, ~0>;  // GVAR = global variable (mutable variables)
using ramb_ht = handle_t<int, struct ramb_ht_tag, -1, true>; // RAMB = ram block
using cnst_ht = handle_t<unsigned, struct romd_ht_tag, ~0>;  // constant
using bank_ht = handle_t<unsigned, struct bank_ht_tag, ~0>;
using ramb_bitset_t = std::uint64_t;

class fn_t;
class fn_def_t;
class var_t;
class ram_block_t;
class constant_t;
class bank_t;

struct global_t
{
public:
    using ideps_set_t = fc::vector_set<global_t*>;
    std::string const name;
private:
    // These variables are set only by 'define', as soon
    // as the global's definition is parsed.
    std::mutex m_define_mutex;
    global_class_t m_gclass = GLOBAL_UNDEFINED;
    type_t m_type = TYPE_VOID;
    pstring_t m_pstring;
    union impl_t
    {
        unsigned index;
        fn_t* Vincefn;
        ram_block_t* ram_block;
        bank_t* bank;
    } m_impl;

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

    type_t type() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_type; 
    }

    fn_t const& fn() const 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        assert(gclass() == GLOBAL_FN); 
        return *m_impl.fn;
    }

    var_t const& var() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        assert(gclass() == GLOBAL_VAR); 
        return lookup(gvar_ht());
    }

    ggvar_ht var_h() const 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        assert(gclass() == GLOBAL_VAR); 
        return { m_impl.index };
    }

    ramb_ht ramb_h() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        assert(gclass() == GLOBAL_RAM_BLOCK); 
        return { m_impl.index };
    }

    // TODO
    bank_ht bank_h() const
    { 
        assert(compiler_phase() >= PHASE_PARSE);
#ifndef NDEBUG
        std::lock_guard<std::mutex> global_lock(m_define_mutex);
        assert(gclass() == GLOBAL_BANK); 
#endif
        return { m_impl.index };
    }

    ideps_set_t const& ideps() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        return m_ideps;
    }

    fn_t& define_fn(pstring_t pstring, type_t type, 
                    global_t::ideps_set_t&& ideps, fn_def_t&& fn_def);
    ggvar_ht define_var(ramb_ht ramb, pstring_t pstring, type_t type, global_t::ideps_set_t&& ideps);
    gtab_ht define_table(global_t* var_block, pstring_t pstring, type_t type,
                         bank_ht bank, global_t::ideps_set_t&& ideps);

private:
    void define(pstring_t pstring, global_class_t gclass, type_t type, 
                impl_t impl, ideps_set_t&& ideps, 
                bool permit_redefine = false);

    void compile();
public:
    // Allocates an expression.
    static token_t const* new_expr(token_t const* begin, token_t const* end);
    static label_t* new_label();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(pstring_t name, char const* source);

    // Creates a bank if it doesn't exist.
    static global_t& lookup_bank(pstring_t name, char const* source);

    // Creates a ram block if it doesn't exist.
    static global_t& lookup_ram_block(pstring_t name, char const* source);

    // Looks up a global variable given a ggvar_ht index.
    // This function can only be called after 'var_vec' is 100% built.
    // TODO: rename? remove?
    inline static var_t& get(ggvar_ht h)
    {
        assert(compiler_phase() > PHASE_PARSE);
        assert(h.value < var_vec.size());
        return var_vec[h.value];
    }

    inline static table_t& get(gtab_ht h)
    {
        assert(compiler_phase() > PHASE_PARSE);
        assert(h.value < table_vec.size());
        return table_vec[h.value];
    }

    inline static std::size_t num_vars() 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return var_vec.size();
    }

    // Call after parsing to build 'm_iuses' and 'm_ideps_left',
    // among other things.
    // This function isn't thread-safe.
    // Call from a single thread only.
    static void build_order();

    // Call after 'build_order' to well... compile everything!
    static void compile_all();

private:
    // Returns and pops the next ready global from the ready list.
    static global_t* await_ready_global();
private:
    inline static std::mutex label_pool_mutex;
    inline static array_pool_t<label_t> label_pool;

    inline static std::mutex expr_pool_mutex;
    inline static array_pool_t<token_t> expr_pool;

    inline static std::mutex global_pool_mutex;
    inline static rh::robin_auto_table<global_t*> global_pool_map;
    static std::deque<global_t> global_pool;

    inline static std::mutex fn_pool_mutex;
    static std::deque<fn_t> fn_pool;

    inline static std::mutex var_vec_mutex;
    static std::vector<var_t> var_vec;

    inline static std::mutex ram_block_pool_mutex;
    static std::deque<ram_block_t> ram_block_pool;

    inline static std::mutex table_vec_mutex;
    static std::vector<table_t> table_vec;

    inline static std::mutex bank_pool_mutex;
    static std::deque<bank_t> bank_pool;

    // These represent a queue of globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    static std::vector<global_t*> ready;
    inline static unsigned globals_left;
};


class fn_t
{
public:
    explicit fn_t(fn_def_t fn_def) : def(std::move(fn_def)) {}

    // TODO
    //std::vector<type_t> arg_bytes_types;
    //std::vector<addr16_t> arg_bytes;
    //std::vector<addr16_t> return_bytes;

    void calc_reads_writes_purity(ir_t const& ir);

    // These are only valid after 'calc_reads_writes_purity' has ran.
    bitset_uint_t const* reads() const  { assert(m_reads);  return m_reads; }
    bitset_uint_t const* writes() const { assert(m_writes); return m_writes; }
    ramb_bitset_t read_blocks() const  { assert(m_reads);  return m_read_blocks; }
    ramb_bitset_t write_blocks() const { assert(m_writes); return m_write_blocks; }
    bool io_pure() const { assert(m_writes); return m_io_pure; }

public:
    fn_def_t const def;
private:
    // Bitsets of all global vars read/written in fn (deep)
    // These get assigned by 'calc_reads_writes_purity'.
    // The thread synchronization is implicit in the order of compilation.
    bitset_uint_t* m_reads = nullptr;
    bitset_uint_t* m_writes = nullptr;

    ramb_bitset_t m_read_blocks = 0;
    ramb_bitset_t m_write_blocks = 0;

    // If the function doesn't do I/O.
    // (Using mutable memory state is OK.)
    // Gets set by 'calc_reads_writes_purity'.
    bool m_io_pure = false;

    // All ram values used to hold this fn's local variables:
    ram_bitset_t m_local_ram = {};

private:
    // Holds bitsets of 'm_reads' and 'm_writes'
    static inline std::mutex bitset_pool_mutex;
    static inline array_pool_t<bitset_uint_t> bitset_pool;
};

class var_t
{
public:
    var_t(global_t& global, ramb_ht ram_block) 
    : m_global(&global)
    , m_ramb(ram_block)
    {}

    global_t& global() const { return *m_global; }
    ramb_ht ramb() const { return m_ramb; }
    ramb_bitset_t ramb_bitset() const { return 1ull << m_ramb.value; }

private:
    global_t* m_global;
    ramb_ht m_ramb;
};

class ram_block_t
{
public:
    std::mutex vars_mutex;
    std::vector<ggvar_ht> vars;
};

class table_t
{
public:
    table_t(global_t& global, bank_ht bank)
    : m_global(&global)
    , m_bank(bank)
    {}

    global_t& global() const { return *m_global; }
    bank_ht bank() const { return m_bank; }
private:
    global_t* m_global;
    bank_ht m_bank;
};

class bank_t
{
public:
    std::mutex tables_mutex;
    std::vector<gtab_ht> tables;
};

class global_t
{
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
    global_class_t gclass() const 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_gclass;
    }

    // Helpers that delegate to 'define':
    fn_t& define_fn(pstring_t pstring, type_t type, global_t::ideps_set_t&& ideps, fn_def_t&& fn_def);
    ggvar_ht define_var(pstring_t pstring, type_t type, global_t::ideps_set_t&& ideps, group_ht group);
    gconst_ht define_const(pstring_t pstring, type_t type, global_t::ideps_set_t&& ideps, bank_ht bank);

private:
    // Sets the variables of the global:
    unsigned define(pstring_t pstring, global_class_t gclass, type_t type, 
                    unsigned index, ideps_set_t&& ideps, bool permit_redefine = false);

    void compile();
public:
    // Allocates memory for expressions / labels:
    static token_t const* new_expr(token_t const* begin, token_t const* end);
    static label_t* new_label();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with said name.
    static global_t& lookup(pstring_t name, char const* source);

    // Creates a bank if it doesn't exist.
    static bank_ht lookup_bank(pstring_t name, char const* source);

    // Creates a group if it doesn't exist.
    static group_ht lookup_group(pstring_t name, char const* source);

    inline static std::size_t num_vars() 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return var_vec.size();
    }

    // Call after parsing to build 'm_iuses' and 'm_ideps_left',
    // among other things.
    // This function isn't thread-safe.
    // Call from a single thread only.
    static void build_order();

    // Call after 'build_order' to well... compile everything!
    static void compile_all();

private:
    // Returns and pops the next ready global from the ready list.
    static global_t* await_ready_global();
private:
    inline static std::mutex label_pool_mutex;
    inline static array_pool_t<label_t> label_pool;

    inline static std::mutex expr_pool_mutex;
    inline static array_pool_t<token_t> expr_pool;

    inline static std::mutex global_pool_mutex;
    inline static rh::robin_auto_table<global_t*> global_pool_map;
    static std::deque<global_t> global_pool;

    inline static std::mutex fn_vec_mutex;
    static std::vector<fn_t> fn_vec;

    inline static std::mutex var_vec_mutex;
    static std::vector<var_t> var_vec;

    inline static std::mutex const_vec_mutex;
    static std::vector<const_t> const_vec;

    inline static std::mutex group_vec_mutex;
    static std::vector<group_t> group_vec;

    inline static std::mutex bank_vec_mutex;
    static std::vector<bank_t> bank_vec;

    // These represent a queue of globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    static std::vector<global_t*> ready;
    inline static unsigned globals_left;
};

*/

#endif
