#ifndef CG_PTR_HPP
#define CG_PTR_HPP

#include "ir_decl.hpp"

class locator_t;

// Returns the dominating bankswitch in the IR.
// A.K.A. the optimal bank the function should be in when called.
locator_t first_bank_switch(ir_t& ir);

#endif
