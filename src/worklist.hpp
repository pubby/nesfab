#ifndef WORKLIST_HPP
#define WORKLIST_HPP

#include <vector>

#include "ir_decl.hpp"

// Worklists used by several optimization passes.
template<typename H>
class static_worklist_t
{
private:
    static thread_local std::vector<H> stack;
public:
    static void push(H handle)
    {
        auto& node = *handle;
        if(node.test_flags(FLAG_IN_WORKLIST))
            return;
        node.set_flags(FLAG_IN_WORKLIST);
        stack.push_back(handle);
    }

    static H top() { return stack.back(); }

    static H pop()
    {
        H ret = top();
        ret->clear_flags(FLAG_IN_WORKLIST);
        stack.pop_back();
        return ret;
    }

    static void clear()
    {
        for(H handle : stack)
            handle->clear_flags(FLAG_IN_WORKLIST);
        stack.clear();
    }
    
    static bool empty() { return stack.empty(); }
};

template<typename H>
thread_local std::vector<H> static_worklist_t<H>::stack;

using cfg_worklist = static_worklist_t<cfg_ht>;
using ssa_worklist = static_worklist_t<ssa_ht>;

// These two vectors can also be used by optimization passes, if need be.
extern thread_local std::vector<cfg_ht> cfg_workvec;
extern thread_local std::vector<ssa_ht> ssa_workvec;

#endif
