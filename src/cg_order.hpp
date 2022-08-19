#ifndef CG_ORDER_HPP
#define CG_ORDER_HPP

#include <vector>

#include "ir_decl.hpp"

// Orders CFG basic blocks, trying to find an optimal layout for branches.
std::vector<cfg_ht> order_ir(ir_t& ir);

#endif
