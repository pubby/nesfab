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

struct local_t : public modded_t
{
    local_t(var_decl_t const& decl, std::unique_ptr<mods_t> mods)
    : modded_t(std::move(mods))
    , decl(decl)
    {}

    var_decl_t decl;

    type_t type() const { return decl.src_type.type; }
};

struct local_var_t : public local_t
{
    using local_t::local_t;
};

struct local_const_t : public local_t
{
    local_const_t(var_decl_t const& decl, std::unique_ptr<mods_t> mods, ast_node_t const* expr = nullptr)
    : local_t(decl, std::move(mods))
    , expr(expr)
    {}

    ast_node_t const* expr;
    rval_t value;

    bool is_label() const { return !expr; }
};

struct paa_def_t
{
    std::vector<local_const_t> local_consts;

    // Used to implement operator '.'.
    // Elements are local consts.
    std::vector<std::uint64_t> name_hashes;

    // Offsets of labels, pairing with 'local_consts'.
    std::vector<std::uint32_t> offsets;
};


// Represents function data right after parsing
struct fn_def_t
{
    std::uint16_t num_params = 0;
    std::uint16_t default_label = ENTRY_LABEL;
    src_type_t return_type;
    std::vector<local_var_t> local_vars; // First elems are params
    std::vector<local_const_t> local_consts;
    std::vector<stmt_t> stmts;
    std::vector<mods_t> mods;

    // Used to implement operator '.' for fns.
    // The first elements are the parameters, in order (size 'num_params').
    // Following that, local consts.
    std::vector<std::uint64_t> name_hashes;

    var_decl_t const& var_decl(int i) const
    {
        if(i < 0)
            return local_consts[-i-1].decl;
        return local_vars[i].decl;
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
