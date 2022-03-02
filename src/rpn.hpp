#ifndef RPN_HPP
#define RPN_HPP

// Stack-based RPN utilities, used to translate expression ASTs.

#include <cassert>
#include <cstdint>

#include <boost/container/small_vector.hpp>

#include "types.hpp"
#include "pstring.hpp"
#include "ir_edge.hpp"
#include "compiler_error.hpp"

namespace bc = boost::container;

enum value_category_t : char
{
    RVAL, 
    LVAL,
    LVAL_INDEX,
};

inline constexpr value_category_t to_indexed(value_category_t vc) { return vc == LVAL ? LVAL_INDEX : vc; }

// compile-time-value
using cval_t = bc::small_vector<bc::small_vector<ssa_value_t, 1>, 1>;


// STRUCT OF ARRAYS

// Convert everything to a flat structure
// - args
// - atoms

// ARRAY OF STRUCTS
// - 

// structs[x] = structs[y]
// - translates to multiple array reads / writes, one per struct field

/* TODO
class cval_t
{
public:
    using ptr_t = std::unique_ptr<cval_t[]>;

    bool single() const { return size < 0; }

    cval_t(cval_t const& o)
    {
        if(o.single())
            value = o.value();
        else
        {
            ptr = new[o.size];
            std::copy(o.ptr, o.ptr + o.size, ptr);
            size = o.size;
        }
    }

    ~cval_t()
    {
        if(size >= 0)
            delete[] ptr;
    }

private:
    static_assert(std::is_trivially_destructible_v<ssa_value_t>);
    union
    {
        ssa_value_t value;
        cval_t* ptr;
    };
    int size = -1;
};
*/

struct cpair_t
{
    cval_t value;
    type_t type;
};

// Expressions are stored in RPN form.
// This struct is what the RPN stack holds.
struct rpn_value_t
{
    ssa_value_t value = {};
    ssa_value_t index = {};
    bc::small_vector<std::uint8_t, 4> members;
    value_category_t category = RVAL;
    type_t type = TYPE_VOID;
    pstring_t pstring = {};
    unsigned var_i = 0;

    fixed_t fixed() const
    { 
        if(!value)
            compiler_error(pstring, "Value is uninitialized.");
        if(!value.is_num())
            compiler_error(pstring, "Expecting constant numeric expression.");
        assert(is_masked(value.fixed(), type.name()));
        return value.fixed();
    }

    unsigned whole() const { return fixed().whole(); }
};

class rpn_stack_t
{
private:
    bc::small_vector<rpn_value_t, 32> stack;
public:
    void clear() { stack.clear(); }

    std::size_t size() const { return stack.size(); }

    // For when the stack has exactly 1 element, returns that element
    rpn_value_t& only1() { assert(stack.size() == 1); return stack[0]; }

    rpn_value_t& peek(int i) { assert(i < (int)stack.size()); return stack.rbegin()[i]; }

    void pop() { assert(!stack.empty()); stack.pop_back(); }

    void pop(unsigned i) { assert(i <= stack.size()); stack.resize(stack.size() - i); }

    void push(rpn_value_t value) { stack.push_back(std::move(value)); }

    void tuck(rpn_value_t value, unsigned place) 
    { 
        assert(place <= stack.size());
        stack.insert(stack.end() - place, std::move(value));
    }

    rpn_value_t* past_top() { return &*stack.end(); }
};

#endif
