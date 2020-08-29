#ifndef CG_SCHEDULE_HPP
#define CG_SCHEDULE_HPP

// Schedules the IR, meaning to put all the nodes in some total order.

#include <vector>

#include "cg.hpp"

using schedule_t = std::vector<ssa_ht>;

schedule_t schedule_cfg_node(cfg_ht cfg_h);

#endif
