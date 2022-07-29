#ifndef O_IDENTITY_HPP
#define O_IDENTITY_HPP

#include <ostream>

class ir_t;

// Applies various math identities to the code.
bool o_identities(ir_t& ir, std::ostream* os);

#endif
