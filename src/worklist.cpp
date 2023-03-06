#include "worklist.hpp"

TLS worklist_t<cfg_ht> cfg_worklist;
TLS worklist_t<ssa_ht> ssa_worklist;

// These two vectors can also be used by optimization passes, if need be.
TLS std::vector<cfg_ht> cfg_workvec;
TLS std::vector<ssa_ht> ssa_workvec;
