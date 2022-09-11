#ifndef BYTE_BLOCK_HPP
#define BYTE_BLOCK_HPP

#include <variant>
#include <vector>

#include "locator.hpp"
#include "asm_proc.hpp"

using byte_block_data_t = std::variant<std::vector<locator_t>, asm_proc_t>;

#endif
