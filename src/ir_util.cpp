#include "ir_util.hpp"

#include "globals.hpp"

bool io_pure(ssa_node_t const& ssa_node)
{
    if(ssa_flags(ssa_node.op()) & SSAF_IO_IMPURE)
        return false;
    if(ssa_node.op() == SSA_fn_call)
        return get_fn(ssa_node)->ir_io_pure();
    if(ssa_node.op() == SSA_read_ptr)
    {
        using namespace ssai::rw_ptr;
        return !ssa_node.input(BANK);
    }
    return true;
}

bool pure(ssa_node_t const& ssa_node)
{
    if(!io_pure(ssa_node))
       return false;
    if(ssa_node.op() == SSA_fn_call)
        return get_fn(ssa_node)->ir_writes().all_clear();
    return true;
}

unsigned estimate_cost(ssa_node_t const& ssa_node)
{
    if(ssa_input0_class(ssa_node.op()) == INPUT_LINK)
        return 0;

    if(ssa_flags(ssa_node.op()) & SSAF_FREE)
        return 0;

    if(ssa_flags(ssa_node.op()) & SSAF_EXPENSIVE)
        return 256;

    if(ssa_flags(ssa_node.op()) & SSAF_CONDITIONAL)
        return 4; // somewhat arbitrary

    if(ssa_flags(ssa_node.op()) & SSAF_INDEXES_PTR)
        return 2; // somewhat arbitrary

    if(ssa_flags(ssa_node.op()) & SSAF_INDEXES_ARRAY8)
        return 2; // somewhat arbitrary

    if(ssa_flags(ssa_node.op()) & SSAF_INDEXES_ARRAY16)
        return 8; // somewhat arbitrary

    unsigned cost = 0;

    unsigned const input_size = ssa_node.input_size();
    for(unsigned i = 0; i < input_size; ++i)
        cost += ssa_node.input(i).type().size_of();

    cost = std::max<unsigned>(cost, ssa_node.type().size_of());

    return cost;
}
