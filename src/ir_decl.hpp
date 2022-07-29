#ifndef IR_DECL_HPP
#define IR_DECL_HPP

#include <cstdint>

#include "static_pool.hpp"

////////////////////////////////////////
// Main types
////////////////////////////////////////

class ir_t;
class cfg_node_t;
class ssa_node_t;
struct ssa_fwd_edge_t;
struct ssa_bck_edge_t;
struct cfg_fwd_edge_t;
struct cfg_bck_edge_t;
struct ssa_value_t;

using ssa_data_pool = static_any_pool_t<class ssa_node_t>;
using cfg_data_pool = static_any_pool_t<class cfg_node_t>;

using ssa_pool = static_intrusive_pool_t<class ssa_node_t>;
using cfg_pool = static_intrusive_pool_t<class cfg_node_t>;

using ssa_ht = ssa_pool::handle_t;
using cfg_ht = cfg_pool::handle_t;

////////////////////////////////////////
// Flags
////////////////////////////////////////

// Flags that are useful among different passes.
// These apply to both CFG and SSA nodes (though not all make sense for both)

enum mark_t : std::uint16_t
{
    MARK_NONE      = 0,
    MARK_TEMPORARY = 1,
    MARK_PERMANENT = 2,
    MARK_DELAYED   = 3,
};

constexpr std::uint16_t MARK_OFFSET         = 0;
constexpr std::uint16_t MARK_MASK           = 0b11;

constexpr std::uint16_t FLAG_PRUNED         = 1ull << 2;
constexpr std::uint16_t FLAG_DAISY          = 1ull << 3;

constexpr std::uint16_t FLAG_IN_WORKLIST    = 1ull << 4;
constexpr std::uint16_t FLAG_PROCESSED      = 1ull << 5;
constexpr std::uint16_t FLAG_IRREDUCIBLE    = 1ull << 6;
constexpr std::uint16_t FLAG_COALESCED      = 1ull << 7;

#endif
