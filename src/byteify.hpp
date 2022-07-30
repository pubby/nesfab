#ifndef CG_BYTEIFY_HPP
#define CG_BYTEIFY_HPP

// Converts arithmetic types in the IR to all be bytes.

#include "cg.hpp"

void byteify(class ir_t& ir, fn_t const& fn);
bool shifts_to_rotates(ir_t& ir, bool handle_constant_shifts = true);

#endif
