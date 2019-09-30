#include "ssa_op.hpp"

std::array<signed char, NUM_SSA_OPS> const ssa_argn_table =
{{
#define SSA_DEF(x, argn, flags) argn,
#include "ssa_op.inc"
}};

std::array<unsigned, NUM_SSA_OPS> const ssa_flags_table =
{{
#define SSA_DEF(x, argn, flags) flags,
#include "ssa_op.inc"
}};

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

