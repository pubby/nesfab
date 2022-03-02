#include "ir_edge.hpp"

#include "ir.hpp"

std::ostream& operator<<(std::ostream& o, ssa_fwd_edge_t s)
{
    if(s.is_handle())
        o << "handle " << s.handle().index;
    else if(s.is_num())
        o << "num " << to_double(s.fixed());
    else if(s.is_locator())
        o << "locator " << s.locator();
    //else if(s.is_ptr()) TODO: remove?
        //o << "ptr " << s.ptr<void>();
    return o;
}

////////////////////////////////////////
// edge functions                     //
////////////////////////////////////////

ssa_bck_edge_t* ssa_fwd_edge_t::output() const
{
    if(!holds_ref())
        return nullptr;
    assert(index() < handle()->output_size());
    return &handle()->m_io.output(index());
}

ssa_fwd_edge_t& ssa_bck_edge_t::input() const
{
    assert(index < handle->input_size());
    return handle->m_io.input(index);
}

type_t ssa_fwd_edge_t::type() const
{ 
    assert(operator bool());
    if(is_num())
        return { TYPE_NUM };
    if(holds_ref())
        return handle()->type();
    return { TYPE_VOID };
}

input_class_t ssa_bck_edge_t::input_class() const
{
    assert(index < handle->input_size());
    if(index == 0)
        return ssa_input0_class(handle->op());
    if(handle->op() == SSA_trace)
        return INPUT_LINK;
    return INPUT_VALUE;
}

cfg_bck_edge_t& cfg_fwd_edge_t::output() const
{
    assert(index < handle->output_size());
    return handle->m_io.output(index);
}

cfg_fwd_edge_t& cfg_bck_edge_t::input() const
{
    assert(index < handle->input_size());
    return handle->m_io.input(index);
}

ssa_value_t ssa_fwd_edge_t::mem_head() const
{
    if(is_locator())
        return locator().mem_head();
    return *this;
}

std::size_t ssa_fwd_edge_t::mem_size() const
{
    if(is_locator())
        return locator().mem_size();
    return 1;
}


