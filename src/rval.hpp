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

struct var_ht : handle_t<var_ht, std::uint32_t, ~0u> {};
struct vec_t;
using ct_array_t = std::shared_ptr<ssa_value_t[]>;
using vec_ptr_t = std::shared_ptr<vec_t>;
using ct_variant_t = std::variant<ssa_value_t, ct_array_t, vec_ptr_t>;
using rval_t = bc::small_vector<ct_variant_t, 1>;
struct vec_t
{
    std::vector<rval_t> data;
};
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

value_time_t calc_time(type_t const& type, rval_t const& rval);

// Appends 'rval' onto 'vec'
void append_locator_bytes(bool new_format, std::vector<locator_t>& vec, rval_t const& rval, type_t type, pstring_t pstring, unsigned member = 0, int index = -1);

rval_t default_init(type_t type, pstring_t at);

using lval_flags_t = std::uint8_t;
constexpr lval_flags_t LVALF_IS_GLOBAL = 1 << 0;
constexpr lval_flags_t LVALF_DID_PERIOD  = 1 << 1; // if operator '.' was used on the value
constexpr lval_flags_t LVALF_INDEX_16  = 1 << 2;

struct lval_t
{
    static constexpr std::int16_t RETURN_ARG = std::numeric_limits<std::int16_t>::max();
    static constexpr std::int16_t READY_ARG = RETURN_ARG - 1;
    static constexpr std::int16_t SYSTEM_ARG = RETURN_ARG - 2;
    static constexpr std::int16_t STATE_ARG = RETURN_ARG - 3;
    static constexpr std::int16_t MAPPER_DETAIL_ARG = RETURN_ARG - 4;
    static constexpr std::int16_t MAPPER_RESET_ARG = RETURN_ARG - 5;
    static constexpr std::int16_t NMI_COUNTER_ARG = RETURN_ARG - 6;
    static constexpr std::int16_t ILLEGAL_ARG = RETURN_ARG - 7;
    static constexpr std::int16_t MAPPER_ARG = RETURN_ARG - 8;
    static constexpr std::int16_t CONTROLLERS_ARG = RETURN_ARG - 9;
    static constexpr std::int16_t EXPANSION_AUDIO_ARG = RETURN_ARG - 10;
    static constexpr std::int16_t SECTOR_SIZE_ARG = RETURN_ARG - 11;
    static constexpr std::int16_t FIXED_ARG = RETURN_ARG - 12;

    enum access_class_t
    {
        ACCESS_INDEX,
        ACCESS_FIELD,
    };

    struct access_t
    {
        access_class_t aclass;
        union
        {
            struct
            {
                std::int32_t field;
                std::int32_t member;
            };
            ssa_value_t index;
        };
    };

    lval_flags_t flags = 0;
    std::int8_t atom = -1; // negative means no atom.
    std::int16_t arg = -1;
    std::int16_t label = -1;
    bc::small_vector<access_t, 2> accesses;

    union
    {
        var_ht vvar_i = {};
        global_t const* vglobal;
    };

    ssa_value_t index() const
    {
        for(access_t const& access : accesses)
            if(access.aclass == ACCESS_INDEX)
                return access.index;
        return {};
    }

    unsigned member() const
    {
        unsigned member = 0;
        for(access_t const& access : accesses)
            if(access.aclass == ACCESS_FIELD)
                member += access.member;
        return member;
    }

    void add_index(ssa_value_t v) { accesses.push_back({ .aclass = ACCESS_INDEX, .index = v }); }
    void add_field(unsigned field, unsigned member) 
    { 
        access_t access = { .aclass = ACCESS_FIELD };
        access.field = field;
        access.member = member;
        accesses.push_back(std::move(access)); 
    }

    var_ht var_i() const { assert(is_var()); return vvar_i; }
    global_t const& global() const { assert(is_global()); assert(vglobal); return *vglobal; }

    void set_var_i(var_ht i) { flags &= ~LVALF_IS_GLOBAL; vvar_i = i; }
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
    
    value_time_t calc_time() const { return ::calc_time(type, rval()); }

    void assert_valid() const { passert(!is_rval() || calc_time() <= time, (int)calc_time(), (int)time, type); }

    bool is_ct() const { assert_valid(); return time == CT; }
    bool is_lt() const { assert_valid(); return time == LT; }
    bool is_rt() const { assert_valid(); return time == RT; }
};

struct lt_pair_t
{
    rval_t value;
    type_t type;
};

#endif
