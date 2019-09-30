#include "worklist.hpp"

#include "ir.hpp"

void worklist_t::insert_entire_ssa()
{
    list.reserve(ir_ptr->ssa_pool.size());
    ir_ptr->ssa_pool.foreach([this](ssa_node_t& node)
    {
        node.worklist_flags |= flag;
        list.push_back(&node);
    });
}

void worklist_t::clear()
{
    for(ssa_node_t* node : list)
    {
        assert(node->worklist_flags & flag);
        node->worklist_flags ^= flag;
    }
    list.clear();
}

void worklist_t::insert(ssa_node_t& node)
{
    if(node.worklist_flags & flag)
        return;
    node.worklist_flags |= flag;
    list.push_back(&node);
}

ssa_node_t* worklist_t::pop()
{ 
    if(empty())
        return nullptr;
    ssa_node_t* ret = list.back(); 
    list.pop_back(); 
    assert(ret->worklist_flags & flag);
    ret->worklist_flags ^= flag;
    return ret;
}

void worklists_t::insert_entire_ssa()
{
    inputs_changed.ir_ptr->ssa_pool.foreach([this](ssa_node_t& node)
    {
        node.worklist_flags |= 0b11;
        inputs_changed.list.push_back(&node);
        users_changed.list.push_back(&node);
    });
}
