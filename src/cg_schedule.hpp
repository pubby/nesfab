#ifndef CG_SCHEDULE_HPP
#define CG_SCHEDULE_HPP

#include <vector>

#include "ir_decl.hpp"

using cfg_schedule_d = std::vector<ssa_ht>;
extern std::vector<cfg_schedule_d> cfg_schedule_pool;

inline cfg_schedule_d& get_schedule(cfg_ht h)
{ 
    assert(h.index < cfg_schedule_pool.size()); 
    return cfg_schedule_pool[h.index]; 
}

void schedule_ir(ir_t& ir);

#endif
