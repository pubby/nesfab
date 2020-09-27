#ifndef BYTEIFY_HPP
#define BYTEIFY_HPP

// Converts arithmetic types in the IR to all be bytes.

#include "cg.hpp"

void byteify(class ir_t& ir, struct global_t& global);

#endif
