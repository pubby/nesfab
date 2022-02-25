#ifndef O_ABSTRACT_INTERPRET_HPP
#define O_ABSTRACT_INTERPRET_HPP

#include <cstdint>

#include "constraints.hpp"
#include "ir_decl.hpp"

bool o_abstract_interpret(ir_t& ir);

std::size_t ai_constraints_size(ssa_value_t value);
constraints_t ai_get_constraints(ssa_value_t value, unsigned i = 0);

#endif
