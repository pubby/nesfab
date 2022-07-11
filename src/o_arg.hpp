#ifndef O_ARG_HPP
#define O_ARG_HPP

#include "ir_decl.hpp"

class fn_t;

bool o_remove_unused_arguments(ir_t& ir, fn_t const& fn, bool byteified);

#endif
