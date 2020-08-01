#ifndef CG_BYTEIFY_HPP
#define CG_BYTEIFY_HPP

// Converts arithmetic types in the IR to all be bytes.

#include "cg.hpp"

void byteify(ir_t& ir, class global_manager_t& globals, 
             struct global_t& global);

#endif
