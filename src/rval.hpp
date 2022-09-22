#ifndef RVAL_HPP
#define RVAL_HPP

#include <functional>
#include <memory>
#include <variant>
#include <vector>
#include <limits>

#include <boost/container/small_vector.hpp>

#include "robin/map.hpp"

#include "decl.hpp"
#include "type.hpp"
#include "ir_edge.hpp"
#include "parser_decl.hpp"

namespace bc = boost::container;
struct ast_node_t;
class global_t;

using ct_array_t = std::shared_ptr<ssa_value_t[]>;
using ct_variant_t = std::variant<ssa_value_t, ct_array_t, ast_node_t const*>;
using rval_t = bc::small_vector<ct_variant_t, 1>;
struct rpair_t { rval_t value; type_t type; };

inline ct_array_t make_ct_array(unsigned size)
{
    // TODO: Change this to 'make_shared' when std library updates to c++20.
    return ct_array_t(new ssa_value_t[size]());
}

inline ssa_value_t const* ct_array(ct_variant_t const& variant)
{
    if(ct_array_t const* array = std::get_if<ct_array_t>(&variant))
        return array->get();
    return nullptr;
}

bool is_ct(rval_t const& rval);
bool is_lt(rval_t const& rval);

// Appends 'rval' onto 'vec'
void append_locator_bytes(std::vector<locator_t>& vec, rval_t const& rval, type_t type, pstring_t pstring);

using lval_flags_t = std::uint8_t;
constexpr lval_flags_t LVALF_IS_GLOBAL = 1 << 0;
constexpr lval_flags_t LVALF_DID_PERIOD  = 1 << 1; // if operator '.' was used on the value
constexpr lval_flags_t LVALF_INDEX_16  = 1 << 2;

struct lval_t
{
    static constexpr std::int16_t RETURN_ARG = std::numeric_limits<std::int16_t>::max();

    lval_flags_t flags = 0;
    std::int8_t atom = -1; // negative means no atom.
    std::uint16_t member = 0;
    std::int16_t arg = -1;
    std::uint16_t label = -1;
    union
    {
        unsigned vvar_i = ~0u;
        global_t const* vglobal;
    };
    ssa_value_t index = {};

    unsigned var_i() const { assert(is_var()); return vvar_i; }
    global_t const& global() const { assert(is_global()); assert(vglobal); return *vglobal; }

    void set_var_i(unsigned i) { flags &= ~LVALF_IS_GLOBAL; vvar_i = i; }
    void set_global(global_t const* g) { flags |= LVALF_IS_GLOBAL; vglobal = g; }

    bool is_global() const { return (flags & LVALF_IS_GLOBAL); }
    bool is_var() const { return !(flags & LVALF_IS_GLOBAL); }

    unsigned uatom() const { return atom < 0 ? 0 : atom; }
    unsigned ulabel() const;
};

struct deref_t
{
    ssa_value_t ptr = {};
    ssa_value_t bank = {};
    ssa_value_t index = {};
};

struct strval_t
{
    charmap_t const* charmap = nullptr;
    bool compressed = false;
    unsigned index = 0;

    std::string const& get_string() const;
};

fixed_t fixed(rval_t const& rval, type_t type, pstring_t pstring);
fixed_t sfixed(rval_t const& rval, type_t type, pstring_t pstring);

struct expr_value_t
{
    std::variant<rval_t, lval_t, deref_t, strval_t> val;
    type_t type = TYPE_VOID;
    pstring_t pstring = {};
    value_time_t time = {};

    rval_t const* is_rval() const { return std::get_if<rval_t>(&val); }
    rval_t* is_rval() { return std::get_if<rval_t>(&val); }
    lval_t const* is_lval() const { return std::get_if<lval_t>(&val); }
    lval_t* is_lval() { return std::get_if<lval_t>(&val); }
    deref_t const* is_deref() const { return std::get_if<deref_t>(&val); }
    deref_t* is_deref() { return std::get_if<deref_t>(&val); }
    strval_t const* is_strval() const { return std::get_if<strval_t>(&val); }
    strval_t* is_strval() { return std::get_if<strval_t>(&val); }

    rval_t const& rval() const { return std::get<rval_t>(val); }
    rval_t& rval() { return std::get<rval_t>(val); }
    lval_t const& lval() const { return std::get<lval_t>(val); }
    lval_t& lval() { return std::get<lval_t>(val); }
    deref_t const& deref() const { return std::get<deref_t>(val); }
    deref_t& deref() { return std::get<deref_t>(val); }

    fixed_t fixed() const;
    fixed_t sfixed() const;

    fixed_uint_t u() const { return fixed().value; }
    fixed_sint_t s() const { return static_cast<fixed_sint_t>(sfixed().value); }
    fixed_uint_t whole() const { return u() >> fixed_t::shift; }
    fixed_sint_t swhole() const { return s() >> fixed_t::shift; }

    ssa_value_t const& ssa(unsigned member = 0) const
        { return std::get<ssa_value_t>(rval()[member]); }
    ssa_value_t& ssa(unsigned member = 0)
        { return std::get<ssa_value_t>(rval()[member]); }

    ct_array_t ct_array(unsigned member = 0) const
        { return std::get<ct_array_t>(rval()[member]); }

    bool is_ct() const { return time == CT && is_rval() && ::is_ct(rval()); }
    bool is_lt() const { return time == LT || (is_rval() && ::is_lt(rval())); }
};

#endif
