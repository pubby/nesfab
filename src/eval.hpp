#ifndef INTERPRET_HPP
#define INTERPRET_HPP

#include <string>
#include <exception>
#include <vector>
#include <variant>

#include <boost/container/small_vector.hpp>

#include "flat/small_map.hpp"

#include "bitset.hpp"
#include "pstring.hpp"
#include "globals.hpp"
#include "byte_block.hpp"

namespace bc = ::boost::container;

class ir_t;
class fn_t;
class eval_t;
class type_t;
class locator_t;
struct rpair_t;
struct pstring_t;
struct token_t;

// Thrown when the interpreter takes too much time to complete
struct out_of_time_t : public std::exception
{
    explicit out_of_time_t(std::string const& msg)
    : msg(msg) {}

    virtual const char* what() const noexcept { return msg.c_str(); }
    std::string msg;
};

struct var_lookup_error_t : public std::exception
{
    virtual const char* what() const noexcept { return "Failed var lookup."; }
};

struct fn_not_rt_t : public std::exception
{
    explicit fn_not_rt_t(pstring_t pstring) : pstring(pstring) {}

    virtual const char* what() const noexcept { return "Function is not defined for run-time."; }
    pstring_t pstring;
};

void check_local_const(pstring_t pstring, fn_t const* fn, ast_node_t const& expr,
                       local_const_t const* local_consts);

rpair_t interpret_local_const(pstring_t pstring, fn_t const* fn, ast_node_t const& expr,
                              type_t expected_type, local_const_t const* local_consts);

rpair_t interpret_expr(pstring_t pstring, ast_node_t const& ast,
                       type_t expected_type, eval_t* env = nullptr);

byte_block_data_t interpret_byte_block(
    pstring_t pstring, ast_node_t const& ast, fn_t const* fn = nullptr, 
    local_const_t const* local_consts = nullptr);

precheck_tracked_t build_tracked(fn_t const& fn, local_const_t const* local_consts);

void build_ir(ir_t& ir, fn_t const& fn, local_const_t const* local_consts);

#endif
