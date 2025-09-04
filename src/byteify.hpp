#ifndef CG_BYTEIFY_HPP
#define CG_BYTEIFY_HPP

// Converts arithmetic types in the IR to all be bytes.

#include "cg.hpp"

void byteify(class ir_t& ir, fn_t const& fn);

void remove_type_tags(ir_t& ir);
bool insert_signed_mul_subtractions(ir_t& ir);
bool shifts_to_rotates(ir_t& ir, bool handle_constant_shifts);

#endif
