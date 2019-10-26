#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <ostream>

#include <boost/container/deque.hpp>

#include "robin/collection.hpp"
#include "robin/set.hpp"

#include "flat/flat_set.hpp"

#include "file.hpp"
#include "handle.hpp"
#include "ir.hpp"
#include "parser_types.hpp"
#include "ram.hpp"
#include "symbol_table.hpp"
#include "types.hpp"

namespace bc = boost::container;

class fn_t;
class global_t;

using stmt_handle_t = handle_t<unsigned, struct stmt_handle_tag_t>;

#define STMT_ENUM \
    X(STMT_END_BLOCK)\
    X(STMT_EXPR)\
    X(STMT_IF)\
    X(STMT_ELSE)\
    X(STMT_WHILE)\
    X(STMT_DO)\
    X(STMT_RETURN)\
    X(STMT_BREAK)\
    X(STMT_CONTINUE)\
    X(STMT_LABEL)\
    X(STMT_GOTO)

// Negative values represent var inits, where 
enum stmt_name_t : int
{
    STMT_MIN_VAR_DECL = INT_MIN,
    STMT_MAX_VAR_DECL = -1,
#define X(x) x,
    STMT_ENUM
#undef X
};

std::string to_string(stmt_name_t);

constexpr bool is_var_init(stmt_name_t stmt_name)
{
    return stmt_name < STMT_END_BLOCK;
}

constexpr unsigned get_local_var_i(stmt_name_t stmt_name)
{
    assert(is_var_init(stmt_name));
    return ~static_cast<unsigned>(stmt_name);
}

#define GLOBAL_CLASS_ENUM \
    X(GLOBAL_UNDEFINED) \
    X(GLOBAL_FN) \
    X(GLOBAL_CONST) \
    X(GLOBAL_VAR)

enum global_class_t
{
#define X(x) x,
    GLOBAL_CLASS_ENUM
#undef X
};

std::string to_string(global_class_t gclass);

enum toposort_mark_t
{
    MARK_NONE,
    MARK_TEMPORARY,
    MARK_PERMANENT,
};

struct global_t
{
    pstring_t name;
    global_class_t gclass;
    type_t type;
    union
    {
        class fn_t* fn;
        token_t* expr;
    };

    fc::vector_set<global_t*> deps;

    ds_region_t ds_region;
    ds_bitset_t modifies;
    toposort_mark_t mark;
};

struct label_t
{
    cfg_node_t* node;
    stmt_handle_t stmt_h;
    unsigned goto_count;
    std::vector<cfg_node_t*> inputs;
};

struct stmt_t
{
    stmt_name_t name;
    pstring_t pstring;
    union
    {
        token_t* expr;
        label_t* label;
    };
};

class fn_t
{
public:
    fn_t(var_decl_t const* params_begin, var_decl_t const* params_end)
    : num_params(params_end - params_begin)
    , local_vars(params_begin, params_end) 
    {}

    stmt_t const& operator[](stmt_handle_t h) const { return stmts[h.value]; }
    stmt_t& operator[](stmt_handle_t h) { return stmts[h.value]; }

    stmt_handle_t next_stmt() const { return { stmts.size() }; }

    stmt_handle_t push_stmt(stmt_t stmt) 
    { 
        stmt_handle_t handle = next_stmt();
        stmts.push_back(stmt); 
        return handle;
    }

    stmt_handle_t push_var_init(unsigned name, token_t* expr)
    { 
        stmt_handle_t handle = next_stmt();
        stmts.push_back({ static_cast<stmt_name_t>(~name), {}, expr }); 
        return handle;
    }

    unsigned num_params;
    std::vector<var_decl_t> local_vars; // First elems are params.
    std::vector<stmt_t> stmts;
    array_pool_t<label_t> label_pool;
};

class global_manager_t
{
public:
    global_manager_t() = default;
    global_manager_t(global_manager_t const&) = delete;
    global_manager_t& operator=(global_manager_t const&) = delete;

    global_t const& operator[](unsigned i) const { return globals[i]; }
    global_t& operator[](unsigned i) { return globals[i]; }

    // Creates a global if it doesn't exist.
    unsigned get_index(pstring_t name);
    global_t& get(pstring_t name) { return globals[get_index(name)]; }

    global_t& new_fn(
        pstring_t name, 
        var_decl_t const* params_begin, 
        var_decl_t const* params_end, 
        type_t return_type);
    global_t& new_const(pstring_t name, type_t type);
    global_t& new_var(pstring_t name, type_t type);

    std::vector<global_t*> toposort_deps();

    void finish();

    void debug_print();
    std::ostream& gv_deps(std::ostream& o);

private:
    static void toposort_visit(std::vector<global_t*>&, global_t&);
    void verify_undefined(global_t& global);

    rh::robin_auto_table<unsigned> global_map;
    bc::deque<global_t> globals;
    bc::deque<fn_t> fns;
    ds_manager_t ds_manager;
};

/* TODO
void compile(global_t const& global)
{
    switch(global.gclass)
    {
    default:
    case GLOBAL_UNDEFINED:
        break;

    case GLOBAL_FN:
        // Compile the FN.
        break;

    case GLOBAL_CONST:
        // Evaluate at runtime.
        break;

    case GLOBAL_VAR:
        // Allocate memory
        global.ram_region = alloc_ram();
        break;
    }
}
*/

#endif
