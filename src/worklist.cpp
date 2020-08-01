#include "worklist.hpp"

#include "ir.hpp"

thread_local std::vector<cfg_ht> cfg_workvec;
thread_local std::vector<ssa_ht> ssa_workvec;
