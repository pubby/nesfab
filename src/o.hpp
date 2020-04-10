#ifndef O_HPP
#define O_HPP

#include <vector>

#include "ir_decl.hpp"
#include "o_abstract_interpret.hpp"
#include "o_phi.hpp"

// Generic flags
// These start at 16. More specialized flags start at 0.
constexpr std::uint32_t FLAG_IN_WORKLIST = 1ull << 16;

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
        if(node.flags & FLAG_IN_WORKLIST)
            return;
        node.flags |= FLAG_IN_WORKLIST;
        stack.push_back(handle);
    }

    static H top() { return stack.back(); }

    static H pop()
    {
        H ret = top();
        ret->flags &= ~FLAG_IN_WORKLIST;
        stack.pop_back();
        return ret;
    }

    static void clear()
    {
        for(H handle : stack)
            handle->flags &= ~FLAG_IN_WORKLIST;
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
