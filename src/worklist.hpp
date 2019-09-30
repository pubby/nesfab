#ifndef WORKLIST_HPP
#define WORKLIST_HPP

#include <vector>

class ir_t;
class ssa_node_t;

template<typename T>
class worklist_t
{
friend class worklists_t;
public:
    worklist_t(ir_t& ir, unsigned flag) : ir_ptr(&ir), flag(flag) {}
    ~worklist_t() { clear(); }

    void insert_entire_ssa();

    void clear();
    bool empty() const { return list.empty(); }
    void insert(ssa_node_t& node);
    ssa_node_t* pop(); // Returns nullptr if empty.

private:
    ir_t* ir_ptr;
    unsigned flag;
    std::vector<ssa_node_t*> list;
};

class worklists_t
{
public:
    explicit worklists_t(ir_t& ir) 
    : inputs_changed(ir, 1 << 0)
    , users_changed(ir,  1 << 1)
    {}

    void insert_entire_ssa();

    void clear() { inputs_changed.clear(); users_changed.clear(); }
    bool empty() const 
        { return inputs_changed.empty() && users_changed.empty(); }

    worklist_t inputs_changed;
    worklist_t users_changed;
};

#endif
