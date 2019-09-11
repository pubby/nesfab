#include <array>
#include <cstdint>
#include <vector>
#include <iostream>

#include <boost/preprocessor/stringize.hpp>

enum addr_mode_t
{
#define ADDR_MODE(name) MODE_##name,
#include "addr_mode.inc"
#undef ADDR_MODE
};

struct instr_t
{
    addr_mode_t addr_mode;
    std::uint8_t code;
    unsigned size;
    unsigned cycles;
};

struct op_t
{
    std::string name;
    std::vector<instr_t> instrs;
};

char const* addr_mode_name(addr_mode_t addr_mode)
{
    switch(addr_mode)
    {
#define ADDR_MODE(name) case MODE_##name: return BOOST_PP_STRINGIZE(name);
#include "addr_mode.inc"
#undef ADDR_MODE
    }
    return nullptr;
}

void trim_spaces(std::string& str)
{
    while(str.size() && std::isspace(str.back()))
        str.pop_back();
}

void get_trim_line(std::istream& istr, std::string& str)
{
    std::getline(istr, str);
    trim_spaces(str);
}

op_t read_op()
{
    op_t op = {};

    // Read the opcode name
    get_trim_line(std::cin, op.name);

    std::string line;
    get_trim_line(std::cin, line);
    while(std::cin && !line.empty())
    {
        instr_t instr = {};

        if(line == "Implied" || line == "Accumulator")
            instr.addr_mode = MODE_IMPLIED;
        else if(line == "Immediate")
            instr.addr_mode = MODE_IMMEDIATE;
        else if(line == "Zero Page")
            instr.addr_mode = MODE_ZERO_PAGE;
        else if(line == "Zero Page,X")
            instr.addr_mode = MODE_ZERO_PAGE_X;
        else if(line == "Zero Page,Y")
            instr.addr_mode = MODE_ZERO_PAGE_Y;
        else if(line == "Absolute")
            instr.addr_mode = MODE_ABSOLUTE;
        else if(line == "Absolute,X")
            instr.addr_mode = MODE_ABSOLUTE_X;
        else if(line == "Absolute,Y")
            instr.addr_mode = MODE_ABSOLUTE_Y;
        else if(line == "Indirect")
            instr.addr_mode = MODE_INDIRECT;
        else if(line == "(Indirect,X)")
            instr.addr_mode = MODE_INDIRECT_X;
        else if(line == "(Indirect),Y")
            instr.addr_mode = MODE_INDIRECT_Y;
        else if(line == "Relative")
            instr.addr_mode = MODE_RELATIVE;
        else
            throw std::runtime_error(op.name + " bad addr mode: " + line);

        // Read the instruction code
        get_trim_line(std::cin, line);
        line.resize(3);
        line[0] = ' ';
        instr.code = std::stoi(line, nullptr, 16);

        // Read the size
        get_trim_line(std::cin, line);
        instr.size = line[0] - '0';

        // Read the cycles
        get_trim_line(std::cin, line);
        instr.cycles = line[0] - '0';

        op.instrs.push_back(instr);

        get_trim_line(std::cin, line);
    }

    return op;
}

int main(int argc, char** argv)
{
    std::vector<op_t> ops;

    while(std::cin)
        ops.push_back(read_op());

    std::array<addr_mode_t, 256> addr_mode_table = {};
    std::array<std::uint8_t, 256> size_table = {};
    std::array<std::uint8_t, 256> cycle_table = {};
    std::array<std::uint8_t, 256> name_table = {};

    for(op_t const& op : ops)
    {
        for(instr_t const& instr : op.instrs)
        {
            addr_mode_table[instr.code] = instr.addr_mode;
            size_table[instr.code] = instr.size;
            cycle_table[instr.code] = instr.cycles;
            name_table[instr.code] = &op - ops.data();
        }
    }

    std::cout << "enum asm_op_name_t : std::uint8_t \n{\n";
    for(op_t const& op : ops)
        std::cout << "    " << op.name << ",\n";
    std::cout << "};\n";

    std::cout << "enum asm_op_t : std::uint8_t \n{\n";
    for(op_t const& op : ops)
    {
        for(instr_t const& instr : op.instrs)
        {
            std::cout << "    " << op.name << '_' 
                      << addr_mode_name(instr.addr_mode) 
                      << " = " << (int)instr.code << ",\n";
        }
    }
    std::cout << "};\n";

    std::cout << "\nconstexpr addr_mode_t asm_addr_mode_table[256] =\n{\n";
    for(addr_mode_t addr_mode : addr_mode_table)
        std::cout << "    " << addr_mode << ",\n";
    std::cout << "};\n";

    std::cout << "\nconstexpr std::uint8_t asm_size_table[256] =\n{\n";
    for(int i : size_table)
        std::cout << "    " << i << ",\n";
    std::cout << "};\n";

    std::cout << "\nconstexpr std::uint8_t asm_cycle_table[256] =\n{\n";
    for(int i : cycle_table)
        std::cout << "    " << i << ",\n";
    std::cout << "};\n";
}
