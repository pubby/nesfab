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
#include <ostream>

#include "constraints.hpp"
#include "ir_decl.hpp"

bool o_abstract_interpret(ir_t& ir, std::ostream* os);

// TODO: one day we may implement this
//std::size_t ai_constraints_size(ssa_value_t value);
//constraints_t ai_get_constraints(ssa_value_t value, unsigned i = 0);

#endif
