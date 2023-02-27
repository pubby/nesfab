#ifndef WORKLIST_HPP
#define WORKLIST_HPP

#include <deque>

#include "ir_decl.hpp"

// Worklists used by several optimization passes.
template<typename H>
class worklist_t
{
public:
    std::deque<H> container;

    void push(H h)
    {
        if(h->test_flags(FLAG_IN_WORKLIST))
            return;
        h->set_flags(FLAG_IN_WORKLIST);
        container.push_back(h);
    }

    void queue(H h)
    {
        if(h->test_flags(FLAG_IN_WORKLIST))
            return;
        h->set_flags(FLAG_IN_WORKLIST);
        container.push_front(h);
    }

    H top() { return container.back(); }

    H pop()
    {
        H ret = top();
        ret->clear_flags(FLAG_IN_WORKLIST);
        container.pop_back();
        return ret;
    }

    void clear()
    {
        for(H handle : container)
            handle->clear_flags(FLAG_IN_WORKLIST);
        container.clear();
    }
    
    bool empty() { return container.empty(); }
};

inline thread_local worklist_t<cfg_ht> cfg_worklist;
inline thread_local worklist_t<ssa_ht> ssa_worklist;

// These two vectors can also be used by optimization passes, if need be.
inline thread_local std::vector<cfg_ht> cfg_workvec;
inline thread_local std::vector<ssa_ht> ssa_workvec;

#endif
