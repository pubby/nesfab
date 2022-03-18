#include "robin/set.hpp"

static thread_local rh::joker_set<cpair_t> cpair_map;

cpair_t const* new_cpair(cpair_t const& cpair) { cpair_map.insert(cpair); }
cpair_t const* new_cpair(cpair_t&& cpair) { cpair_map.insert(std::move(cpair)); }
