#include "ir_util.hpp"

#include "globals.hpp"

bool io_pure(ssa_node_t const& ssa_node)
{
    if(ssa_flags(ssa_node.op()) & SSAF_IO_IMPURE)
        return false;
    if(ssa_node.op() == SSA_fn_call)
        return get_fn(ssa_node)->ir_io_pure();
    return false;
}



