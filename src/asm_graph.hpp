#ifndef ASM_GRAPH_HPP
#define ASM_GRAPH_HPP

#include <vector>

#include "debug_print.hpp"

struct asm_inst_t;
class locator_t;
class fn_t;
class lvars_manager_t;

// Converts the assembly code to a graph, optimizes it, then spits back out assembly code.
// Also generates lvars information.
// REQUIRES LOOP INFORMATION BUILT.
std::vector<asm_inst_t> run_asm_graph(
    log_t* log, fn_t const& fn, lvars_manager_t& lvars,
    std::vector<asm_inst_t> const& code, locator_t entry_label);


// TODO: 

// - remove useless labels (DONE)

// - reorder CFG (DONE)

// - liveness

// - move stores

// - peephole

#endif
