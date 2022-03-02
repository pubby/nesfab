#ifndef O_IDENTITY_HPP
#define O_IDENTITY_HPP

#include "ir_decl.hpp"

// Removes identity elements, like the zero in 'foo + 0' or one in 'bar * 1'
bool o_remove_identity_elements(ir_t& ir);

// Applies various math identities to the code.
bool o_identities(ir_t& ir);

#endif
