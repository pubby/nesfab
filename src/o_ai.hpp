#ifndef O_ABSTRACT_INTERPRET_HPP
#define O_ABSTRACT_INTERPRET_HPP
 
// This pass implements abstract interpretation to fold constants,
// thread jumps, and simplify conditional expressions.
 
// https://en.wikipedia.org/wiki/Abstract_interpretation
 
// The model is contained in 'constraints.hpp',
// and is defined as the intersection of an interval and a bitset.
// Widening is used to ensure termination.
 
// Conditionals are handled with something I call "tracing".
// The pass tries to prove which values are known inside specific branches,
// temporarily adding SSA nodes to track this.
// At the end of the pass, these temporary nodes are deleted.

#include <cstdint>

#include "debug_print.hpp"
#include "ir_decl.hpp"
#include "constraints.hpp"

struct ai_prep_t
{
    std::unique_ptr<constraints_t> constraints;
};

inline thread_local std::vector<ai_prep_t> ai_prep_vec;

inline ai_prep_t& ai_prep(ssa_ht ssa) { assert(ssa.id < ai_prep_vec.size()); return ai_prep_vec[ssa.id]; }
inline void reset_ai_prep() { ai_prep_vec.clear(); }
inline void resize_ai_prep() { ai_prep_vec.resize(ssa_pool::array_size()); }

bool o_abstract_interpret(log_t* os, ir_t& ir, bool byteified);

#endif
