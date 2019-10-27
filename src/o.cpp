#include "o.hpp"

template<> std::vector<ssa_node_t*> ssa_worklist::stack{};
template<> std::vector<cfg_node_t*> cfg_worklist::stack{};

std::vector<ssa_node_t*> cfg_workvec{};
std::vector<ssa_node_t*> ssa_workvec{};
