#ifndef RPN_HPP
#define RPN_HPP

// Stack-based RPN utilities, used to translate expression ASTs.

#include <cassert>
#include <cstdint>

#include <boost/container/small_vector.hpp>

#include "fixed.hpp"
#include "type.hpp"
#include "type_mask.hpp"
#include "pstring.hpp"
#include "ir_edge.hpp"
#include "compiler_error.hpp"

#include <iostream> // TODO: remove

namespace bc = boost::container;

enum value_category_t : char
{
    RVAL, 
    LVAL,
    LVAL_INDEX, // TODO: remove?
};

inline constexpr value_category_t to_indexed(value_category_t vc) { return vc == LVAL ? LVAL_INDEX : vc; }

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

    sfixed_int_t sfixed() const { return to_signed(fixed().value, type.name()); }
    fixed_int_t whole() const { return fixed().value >> fixed_t::shift; }
    sfixed_int_t swhole() const { return sfixed() >> fixed_t::shift; }
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

    void push(rpn_value_t const& value) { stack.push_back(value); }
    void push(rpn_value_t&& value) { stack.push_back(std::move(value)); }

    void tuck(rpn_value_t value, unsigned place) 
    { 
        assert(place <= stack.size());
        stack.insert(stack.end() - place, std::move(value));
    }

    rpn_value_t* past_top() { return &*stack.end(); }
};

#endif
