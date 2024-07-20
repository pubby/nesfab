#ifndef CG_SCHEDULE_HPP
#define CG_SCHEDULE_HPP

#include "ir_decl.hpp"

void schedule_ir(ir_t& ir);

// Optimize the IR after scheduling:
void o_schedule(ir_t& ir);

#endif
