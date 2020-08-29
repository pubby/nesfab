#include "ssa_op.hpp"

std::string_view to_string(ssa_op_t op)
{
    using namespace std::literals;
    switch(op)
    {
#define SSA_DEF(x, ...) case SSA_##x: return #x##sv;
#include "ssa_op.inc"
    default: return "???"sv;
    }
}

std::ostream& operator<<(std::ostream& o, ssa_op_t node_type)
{
    o << to_string(node_type);
    return o;
}

