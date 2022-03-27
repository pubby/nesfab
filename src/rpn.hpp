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
#include "cval.hpp"

namespace bc = boost::container;

enum value_category_t : char
{
    RVAL, 
    LVAL,
};

// Expressions are stored in RPN form.
// This struct is what the RPN stack holds.
struct rpn_value_t
{
    sval_t sval = {};
    ssa_value_t index = {};
    unsigned member = 0;
    value_category_t category = RVAL;
    type_t type = TYPE_VOID;
    pstring_t pstring = {};
    unsigned var_i = ~0u;
    // TODO
    bool ct = false; // If value was computed at compile-time.

    fixed_t fixed() const
    { 
        ssa_value_t const* v;

        if(sval.size() != 1)
        {
            std::puts("1");
            goto not_cne;
        }

        if(!(v = std::get_if<ssa_value_t>(&sval[0])))
        {
            std::puts("2");
            goto not_cne;
        }

        if(!*v)
            compiler_error(pstring, "Value is uninitialized.");

        if(!v->is_num() || !is_arithmetic(type.name()))
        {
            std::puts("3");
            goto not_cne;
        }

        assert(is_masked(v->fixed(), type.name()));
        return v->fixed();

    not_cne:
        compiler_error(pstring, "Expecting constant numeric expression.");
    }

    fixed_uint_t u() const { return fixed().value; }
    fixed_sint_t s() const { return to_signed(fixed().value, type.name()); }
    fixed_uint_t whole() const { return u() >> fixed_t::shift; }
    fixed_sint_t swhole() const { return s() >> fixed_t::shift; }

    ssa_value_t& ssa(unsigned member = 0)
    {
        assert(member < sval.size());
        return std::get<ssa_value_t>(sval[member]);
    }

    ssa_value_t const& ssa(unsigned member = 0) const
    {
        assert(member < sval.size());
        return std::get<ssa_value_t>(sval[member]);
    }

    ct_array_t ct_array(unsigned member = 0) const
    {
        assert(member < sval.size());
        return std::get<ct_array_t>(sval[member]);
    }
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
