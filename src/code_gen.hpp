#ifndef CODE_GEN_HPP
#define CODE_GEN_HPP

#include <cstdint>

#include "asm_decl.hpp"
#include "asm_tables.hpp"
#include "ir_decl.hpp"

class global_manager_t;
struct global_t;

struct instr_t
{
    asm_op_name_t op_name;
    addr_mode_t mode;
    std::uint16_t addr;
};

void byteify(ir_t& ir, global_manager_t& globals, global_t& global);
void make_conventional(ir_t& ir);

#endif
