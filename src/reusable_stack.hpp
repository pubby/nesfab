#ifndef REUSABLE_STACK_HPP
#define REUSABLE_STACK_HPP

#include <cassert>
#include <vector>

//TODO: remove this file?

// A stack which 'clear's objects on pop, rather than destructing them.
// (This allows memory to be reused more efficiently.)
template<typename T>
class reusable_stack
{
public:
    using value_type = T;

    value_type const& top() const { return stack[index]; }
    value_type& top() { return stack[index]; }

    void push()
    {
        ++index;
        if(index == stack.size())
            stack.emplace_back();
        assert(!empty());
    }

    void pop()
    {
        assert(!empty());
        top().clear();
        --index;
    }

    void clear()
    {
        while(!empty())
            pop();
    }

    std::size_t size() const { return index + 1; }
    bool empty() const { return index == -1ull; }

private:
    std::size_t index = -1ull;
    std::vector<value_type> stack;
};

#endif
