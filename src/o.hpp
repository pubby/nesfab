#ifndef O_HPP
#define O_HPP

#include <vector>

#include "ir_decl.hpp"
#include "o_ai.hpp"
#include "o_phi.hpp"

// Worklists used by several optimization passes.
template<typename H>
class static_worklist_t
{
private:
    inline static std::vector<H> stack = {};
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

using cfg_worklist = static_worklist_t<cfg_ht>;
using ssa_worklist = static_worklist_t<ssa_ht>;

// These two vectors can also be used by optimization passes, if need be.
extern std::vector<cfg_ht> cfg_workvec;
extern std::vector<ssa_ht> ssa_workvec;

#endif
