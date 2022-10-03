#ifndef DECL_HPP
#define DECL_HPP

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <deque>
#include <type_traits>

#include "flat/flat_map.hpp"

#include "phase.hpp"
#include "handle.hpp"

constexpr unsigned MAX_FN_ARGS = 32;
constexpr unsigned MAX_ASM_LOCAL_VARS = 128;
constexpr unsigned MAX_MEMBERS = 256;
constexpr unsigned MAX_ATOMS = 8;
constexpr unsigned MAX_PAA_SIZE = 8192;
constexpr unsigned MAX_CFG_INPUT = 256;
constexpr unsigned MAX_CFG_OUTPUT = 256;

struct group_t;
class global_t;
class global_datum_t;
class fn_t;
class gvar_t;
class gmember_t;
class const_t;
class struct_t;
class charmap_t;
class group_vars_t;
class group_data_t;
struct field_t;
struct lt_value_t;

#define GLOBAL_CLASS_XENUM \
    X(GLOBAL_UNDEFINED) \
    X(GLOBAL_FN) \
    X(GLOBAL_VAR) \
    X(GLOBAL_CONST) \
    X(GLOBAL_STRUCT) \
    X(GLOBAL_CHARMAP)

enum global_class_t : std::uint8_t
{
#define X(x) x,
    GLOBAL_CLASS_XENUM
#undef X
};

#define GROUP_CLASS_XENUM \
    X(GROUP_UNDEFINED, undefined) \
    X(GROUP_VARS, vars) \
    X(GROUP_DATA, data)

enum group_class_t : std::uint8_t
{
#define X(e, s) e,
    GROUP_CLASS_XENUM
#undef X
};

constexpr char const* group_class_keyword(group_class_t gc)
{
    switch(gc)
    {
    default: return "undefined";
#define X(e, s) case e: return #s;
    GROUP_CLASS_XENUM
#undef X
    }
}

struct lt_ht : pool_handle_t<lt_ht, std::deque<lt_value_t>, PHASE_COMPILE> {};

struct global_ht : pool_handle_t<global_ht, std::deque<global_t>, PHASE_PARSE> {};
struct fn_ht : pool_handle_t<fn_ht, std::deque<fn_t>, PHASE_PARSE> {};
struct gvar_ht : pool_handle_t<gvar_ht, std::deque<gvar_t>, PHASE_PARSE> {};
struct const_ht : pool_handle_t<const_ht, std::deque<const_t>, PHASE_PARSE> {};
struct struct_ht : pool_handle_t<struct_ht, std::deque<struct_t>, PHASE_PARSE> {};
struct gmember_ht : pool_handle_t<gmember_ht, std::vector<gmember_t>, PHASE_COUNT_MEMBERS> {};
struct charmap_ht : pool_handle_t<charmap_ht, std::deque<charmap_t>, PHASE_PARSE> {};

struct group_ht : pool_handle_t<group_ht, std::deque<group_t>, PHASE_PARSE> 
{
    group_data_t* data() const; // Defined in group.cpp
};
struct group_vars_ht : pool_handle_t<group_vars_ht, std::deque<group_vars_t>, PHASE_PARSE> {};
struct group_data_ht : pool_handle_t<group_data_ht, std::deque<group_data_t>, PHASE_PARSE> {};

DEF_HANDLE_HASH(fn_ht);
DEF_HANDLE_HASH(gvar_ht);
DEF_HANDLE_HASH(const_ht);
DEF_HANDLE_HASH(struct_ht);
DEF_HANDLE_HASH(charmap_ht);
DEF_HANDLE_HASH(gmember_ht);
DEF_HANDLE_HASH(group_ht);
DEF_HANDLE_HASH(group_vars_ht);
DEF_HANDLE_HASH(group_data_ht);

enum fn_class_t : char
{
    FN_FN,
    FN_CT,
    FN_NMI,
    FN_MODE,
};

constexpr char const* fn_class_keyword(fn_class_t fc)
{
    switch(fc)
    {
    default: return "undefined";
    case FN_FN: return "fn";
    case FN_CT: return "ct";
    case FN_MODE: return "mode";
    case FN_NMI: return "nmi";
    }
}

///////////
// ideps //
///////////

enum idep_class_t : std::int8_t
{
    BAD_IDEP = 0,
    IDEP_TYPE,
    IDEP_PRECHECK,
    IDEP_VALUE,
    NUM_IDEP_CLASSES
};

struct idep_pair_t
{
    idep_class_t calc;
    idep_class_t depends_on;
};

using ideps_map_t = fc::vector_map<global_t*, idep_pair_t>;

void add_idep(ideps_map_t& map, global_t* global, idep_pair_t pair);

#endif
