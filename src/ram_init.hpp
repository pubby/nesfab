#ifndef RAM_INIT_HPP
#define RAM_INIT_HPP

#include <vector>

class gvar_ht;
class asm_proc_t;

bool gen_group_var_inits(std::vector<gvar_ht> const& gvars, asm_proc_t& proc);
void gen_group_var_inits();

#endif

