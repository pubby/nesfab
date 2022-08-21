#ifndef IASM_HPP
#define IASM_HPP

// Inline assembly

#include <vector>

#include "asm.hpp" 
#include "asm_proc.hpp" 
#include "mods.hpp" 

struct token_t;

enum iasm_class_t
{
    IASM_OP = 0,
    IASM_CALL,
    IASM_GOTO,
    IASM_GOTO_MODE,
    IASM_WAIT_NMI,
};

struct iasm_inst_t
{
    iasm_class_t iclass = {};
    op_t op = {};
    void* ptr = {};
    std::unique_ptr<mods_t> mods;

    bool has_expr() const { return iclass == IASM_OP; }

    token_t const* expr() const { assert(has_expr()); return static_cast<token_t const*>(ptr); }
    global_t const* global() const { assert(!has_expr()); return static_cast<global_t const*>(ptr); }
};

struct iasm_def_t
{
    std::vector<iasm_inst_t> code;
    void clear() { code.clear(); }
};

#endif
