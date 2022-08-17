#ifndef O_ARG_HPP
#define O_ARG_HPP

#include "debug_print.hpp"
#include "ir_decl.hpp"

class fn_t;

bool o_remove_unused_arguments(log_t* log, ir_t& ir, fn_t const& fn, bool byteified);

#endif
