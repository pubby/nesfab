#ifndef GRAPHVIZ_HPP
#define GRAPHVIZ_HPP

#include <ostream>

class ir_t;

void graphviz_ssa(std::ostream& o, ir_t const& ir);
void graphviz_cfg(std::ostream& o, ir_t const& ir);

#endif
