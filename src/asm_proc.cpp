#include "asm_proc.hpp"

#include "format.hpp"

std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst)
{
    o << "{ " << to_string(inst.op) << ", " << inst.arg << "   (" << inst.ssa_op << ") }";
    return o;
}

void asm_proc_t::push_inst(asm_inst_t inst)
{
    if(inst.op == ASM_LABEL)
    {
        auto result = labels.insert({ inst.arg, code.size() });
        assert(result.inserted);
    }
    else
    {
        code.push_back(inst);

        if(!is_label(inst.arg.lclass()))
            mem_usage[inst.arg.mem_head()] += 1;
    }

}

void asm_proc_t::expand_branch_ops()
{
    bool progress; 
    do
    {
        progress = false;

        for(unsigned i = 0; i < code.size(); ++i)
        {
            asm_inst_t& inst = code[i];

            if(!is_relative_branch(inst.op))
                continue;

            unsigned const label_i = labels[inst.arg];
            int const dist = bytes_between(i+1, label_i);

            if(dist > 127 || dist < -128)
            {
                inst.op = get_op(op_name(inst.op), MODE_LONG);
                progress = true;
            }
        }
    }
    while(progress);
}

void asm_proc_t::nopify_short_jumps()
{
    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t& inst = code[i];

        if(inst.op != JMP_ABSOLUTE)
            continue;

        unsigned const label_i = labels[inst.arg];
        int const dist = bytes_between(i+1, label_i);

        if(dist == 1)
        {
            inst.op = SKB_IMPLIED;
            inst.arg = {};
        }
        else if(dist == 2 && op_code(code[i+1].op) != 0x20) // Check for 0x20 to avoid reading a PPU register
        {
            inst.op = IGN_IMPLIED;
            inst.arg = {};
        }
    }
}

void asm_proc_t::make_relocatable()
{
    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t& inst = code[i];

        if(is_label(inst.arg.lclass()))
        {
            assert(labels.count(inst.arg));
            unsigned const label_i = labels[inst.arg];

            if(op_addr_mode(inst.op) == MODE_RELATIVE)
            {
                int const dist = bytes_between(i+1, label_i);
                assert(dist <= 127 && dist >= -128);
                inst.arg = locator_t::const_byte(dist);
            }
            else 
                inst.arg = locator_t::relocation_addr(bytes_between(0, label_i));
        }
    }
}

int asm_proc_t::bytes_between(unsigned ai, unsigned bi) const
{
    if(bi < ai)
        return -bytes_between(bi, ai);

    int bytes = 0;
    for(unsigned i = ai; i < bi; ++i)
    {
        assert(i < code.size());
        bytes += op_size(code[i].op);
    }

    return bytes;
}

void asm_proc_t::write_binary(loc_mem_map_t const& lmm, std::uint8_t* rom, addr16_t start_addr) const
{
    rom += start_addr;

    for(asm_inst_t const& inst : code)
    {
        switch(op_addr_mode(inst.op))
        {
        case MODE_IMPLIED:
            *rom++ = op_code(inst.op);
            break;

        case MODE_IMMEDIATE:
        case MODE_RELATIVE:
            assert(inst.arg.lclass() == LOC_CONST_BYTE);
            *rom++ = op_code(inst.op);
            *rom++ = inst.arg.data();
            break;

        case MODE_ZERO_PAGE:
        case MODE_ZERO_PAGE_X:
        case MODE_ZERO_PAGE_Y:
        case MODE_INDIRECT_X:
        case MODE_INDIRECT_Y:
            {
                addr16_t const addr = lmm.get_addr(inst.arg, start_addr);
                assert(addr < 0x100);
                *rom++ = op_code(inst.op);
                *rom++ = addr;
            }
            break;

        case MODE_LONG:
            *rom++ = op_code(get_op(invert_branch(op_name(inst.op)), MODE_RELATIVE));
            *rom++ = 3; // branch over upcoming jmp
            *rom++ = op_code(JMP_ABSOLUTE);
            goto absolute_addr;

        case MODE_ABSOLUTE:
        case MODE_ABSOLUTE_X:
        case MODE_ABSOLUTE_Y:
        case MODE_INDIRECT:
            {
                *rom++ = op_code(inst.op);
            absolute_addr:
                addr16_t const addr = lmm.get_addr(inst.arg, start_addr);
                *rom++ = addr & 0xFF;
                *rom++ = addr >> 8;
            }
            break;

        default:
            throw std::runtime_error("Invalid addressing mode.");
        }
    }
}

///////////////////
// loc_mem_map_t //
///////////////////


addr16_t loc_mem_map_t::get_addr(locator_t loc, addr16_t relocation_offset) const
{
    if(loc.lclass() == LOC_RELOCATION_ADDR)
        return relocation_offset + loc.data();

    if(auto* result = map.find(loc))
        return result->second.addr;

    throw std::runtime_error(fmt("loc_mem_map_t: missing locator %i", loc));
}
