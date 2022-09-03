#ifndef FN_DEF_HPP
#define FN_DEF_HPP

#include <vector>
#include <boost/container/small_vector.hpp>

#include "stmt.hpp"
#include "ir_decl.hpp"
#include "handle.hpp"
#include "parser_decl.hpp"
#include "pstring.hpp"
#include "mods.hpp"
#include "locator.hpp"
#include "rval.hpp"

namespace bc = ::boost::container;

struct local_const_t
{
    var_decl_t var_decl;
    ast_node_t const* expr;
    rval_t value;

    type_t type() const { return var_decl.src_type.type; }
    bool is_label() const { return !expr; }
};

// Represents function data right after parsing
class fn_def_t
{
public:
    unsigned num_params = 0;
    src_type_t return_type;
    std::vector<var_decl_t> local_vars; // First elems are params
    std::vector<local_const_t> local_consts;
    std::vector<stmt_t> stmts;
    std::vector<mods_t> mods;

    // Used to implement operator '.' for fns.
    // The first elements are the parameters, in order (size 'num_params').
    // Following that, assembly labels in the order they appear in 'local_consts'.
    std::vector<std::uint64_t> name_hashes;

    var_decl_t const& var_decl(int i) const
    {
        if(i < 0)
            return local_consts[-i-1].var_decl;
        return local_vars[i];
    }

    stmt_t const& operator[](stmt_ht h) const { assert(h.id < stmts.size()); return stmts[h.id]; }
    stmt_t& operator[](stmt_ht h) { assert(h.id < stmts.size()); return stmts[h.id]; }

    mods_t const& operator[](stmt_mods_ht h) const { assert(h.id < mods.size()); return mods[h.id]; }
    mods_t& operator[](stmt_mods_ht h) { assert(h.id < mods.size()); return mods[h.id]; }

    mods_t const* mods_of(stmt_ht h) const;

    mods_t const* maybe_mods(stmt_mods_ht h) const { return h ? &operator[](h) : nullptr; }

    stmt_ht next_stmt() const { return { stmts.size() }; }

    stmt_ht push_stmt(stmt_t stmt);
    stmt_ht push_var_init(unsigned name, ast_node_t const* expr, pstring_t pstring);
    stmt_mods_ht push_mods(std::unique_ptr<mods_t> m);

    // Returns the first pstring matching 'global'.
    // Intended to be used for error messages.
    pstring_t find_global(global_t const* global) const;
};

#endif
