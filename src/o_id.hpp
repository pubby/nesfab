#ifndef O_IDENTITY_HPP
#define O_IDENTITY_HPP

#include "debug_print.hpp"

class ir_t;

// Applies various math identities to the code.
bool o_identities(log_t* log, ir_t& ir);

#endif
