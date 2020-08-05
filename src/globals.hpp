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
#include "handle.hpp"
#include "ir.hpp"
#include "parser_types.hpp"
#include "phase.hpp"
#include "ram.hpp"
#include "stmt.hpp"
#include "symbol_table.hpp"
#include "types.hpp"

namespace bc = boost::container;

#define GLOBAL_CLASS_XENUM \
    X(GLOBAL_UNDEFINED) \
    X(GLOBAL_FN) \
    X(GLOBAL_CONST) \
    X(GLOBAL_VAR)

enum global_class_t : std::uint8_t
{
#define X(x) x,
    GLOBAL_CLASS_XENUM
#undef X
};

std::string to_string(global_class_t gclass);

using gvar_ht = handle_t<unsigned, struct gvar_ht_tag, ~0>;
class fn_t;
class fn_def_t;

struct global_t
{
public:
    using ideps_set_t = fc::vector_set<global_t*>;
    pstring_t const name = {};
private:
    // These variables are set only by 'define', as soon
    // as the global is parsed.
    std::mutex m_define_mutex;
    global_class_t m_gclass = GLOBAL_UNDEFINED;
    type_t m_type = TYPE_VOID;
    union impl_t
    {
        unsigned index;
        fn_t* fn;
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
    explicit global_t(pstring_t name) : name(name) {}

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

    gvar_ht var() const 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        assert(gclass() == GLOBAL_VAR); 
        return { m_impl.index };
    }

    ideps_set_t const& ideps() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        return m_ideps;
    }

    fn_t& define_fn(type_t type, global_t::ideps_set_t&& ideps,
                    fn_def_t&& fn_def);
    gvar_ht define_var(type_t type, global_t::ideps_set_t&& ideps);

private:
    void define(global_class_t gclass, type_t type, impl_t impl, 
                ideps_set_t&& ideps);
    void compile();
public:
    // Allocates an expression.
    static token_t const* new_expr(token_t const* begin, token_t const* end);
    static label_t* new_label();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(pstring_t name);

    // Looks up a global variable given a gvar_ht index.
    // This function can only be called after 'var_vec' is 100% built.
    inline static global_t& lookup(gvar_ht gvar)
    {
        assert(compiler_phase() > PHASE_PARSE);
        return *var_vec[gvar.value];
    }

    inline static std::size_t num_vars() 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return var_vec.size();
    }

    inline static global_t& get_var(gvar_ht vh)
    { 
        assert(compiler_phase() > PHASE_PARSE);
        assert(vh.value < var_vec.size());
        return *var_vec[vh.value];
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
    static std::vector<global_t*> var_vec;

    // These represent a queue globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    static std::vector<global_t*> ready;
    inline static unsigned globals_left;
};

class fn_def_t
{
public:
    unsigned num_params = 0;
    // First elems are params
    std::vector<var_decl_t> local_vars;

    std::vector<stmt_t> stmts;

    stmt_t const& operator[](stmt_ht h) const { return stmts[h.value]; }
    stmt_t& operator[](stmt_ht h) { return stmts[h.value]; }

    stmt_ht next_stmt() const { return { stmts.size() }; }

    stmt_ht push_stmt(stmt_t stmt) 
    { 
        stmt_ht handle = next_stmt();
        stmts.push_back(stmt); 
        return handle;
    }

    stmt_ht push_var_init(unsigned name, token_t const* expr)
    { 
        stmt_ht handle = next_stmt();
        stmts.push_back({ static_cast<stmt_name_t>(~name), {}, expr }); 
        return handle;
    }
};

class fn_t
{
public:
    explicit fn_t(fn_def_t fn_def) : def(std::move(fn_def)) {}

    // TODO
    //std::vector<type_t> arg_bytes_types;
    //std::vector<addr16_t> arg_bytes;
    //std::vector<addr16_t> return_bytes;

    void calc_reads_writes(ir_t const& ir);

    // These are only valid after 'calc_reads_writes' has ran.
    bitset_uint_t const* reads() const  { assert(m_reads);  return m_reads; }
    bitset_uint_t const* writes() const { assert(m_writes); return m_writes; }

public:
    fn_def_t const def;
private:
    // Bitsets of all global vars read/written in fn (deep)
    // These get assigned by 'calc_reads_writes'.
    // The thread synchronization is implicit in the order of compilation.
    bitset_uint_t* m_reads = nullptr;
    bitset_uint_t* m_writes = nullptr;

private:
    // Holds bitsets of 'm_reads' and 'm_writes'
    static inline std::mutex bitset_pool_mutex;
    static inline array_pool_t<bitset_uint_t> bitset_pool;
};

#endif
