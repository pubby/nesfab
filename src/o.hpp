#ifndef O_HPP
#define O_HPP

#include <vector>

#include "o_phi.hpp"

// Generic flags
// These start at 32. More specialized flags start at 0.
constexpr std::uint64_t FLAG_IN_WORKLIST = 1 << 32;

class ir_t;
class cfg_node_t;
class ssa_node_t;

// Worklists used by several optimization passes.
template<typename T>
class static_worklist_t
{
private:
    static std::vector<T*> stack;
public:
    static void push(T& node)
    {
        if(node.flags & FLAG_IN_WORKLIST)
            return;
        node.flags |= FLAG_IN_WORKLIST;
        stack.push_back(&node);
    }

    static T& top() { return *stack.back(); }

    static T& pop()
    {
        T& ret = top();
        ret.flags &= ~FLAG_IN_WORKLIST;
        stack.pop_back();
        return ret;
    }

    static void clear()
    {
        for(T* node : stack)
            node->flags &= ~FLAG_IN_WORKLIST;
        stack.clear();
    }
    
    static bool empty() { return stack.empty(); }
};

using cfg_worklist = static_worklist_t<cfg_node_t>;
using ssa_worklist = static_worklist_t<ssa_node_t>;

template<> std::vector<ssa_node_t*> ssa_worklist::stack;
template<> std::vector<cfg_node_t*> cfg_worklist::stack;

// These two vectors can also be used by optimization passes, if need be.
extern std::vector<ssa_node_t*> cfg_workvec;
extern std::vector<ssa_node_t*> ssa_workvec;

// o_abstract_interpret.cpp:
void o_abstract_interpret(ir_t& ir);

#endif
