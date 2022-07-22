#include "asm_proc.hpp"

#include "format.hpp"
#include "globals.hpp"

std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst)
{
    o << "{ " << to_string(inst.op) << ", " << inst.arg;
    if(inst.ptr_hi)
        o << " hi: " << inst.ptr_hi;
    o << "   (" << inst.ssa_op << ") }";
    o << "   (" << (float(inst.cost) / 256.0f) << ") }";
    return o;
}

void asm_proc_t::push_inst(asm_inst_t inst)
{
    if(inst.op == ASM_LABEL)
    {
        auto result = labels.insert({ inst.arg, code.size() });
        assert(result.second);
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
    // Loop until we can do no more work.
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

            // Change to long pseudo instruction when out of range
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

        if(inst.op == JMP_ABSOLUTE)
        {
            unsigned const label_i = labels[inst.arg];
            int const dist = bytes_between(i+1, label_i);
            
            if(dist == 0)
            {
                // Prune unnecessary jumps
                inst.op = ASM_PRUNED;
                inst.arg = {};
            }
            else if(dist == 1)
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
        else if(op_flags(inst.op) & ASMF_BRANCH)
        {
            // Prune unecessary branches

            unsigned const label_i = labels[inst.arg];
            int const dist = bytes_between(i+1, label_i);

            if(dist == 0)
            {
                inst.op = ASM_PRUNED;
                inst.arg = {};
            }
            else if(dist == 2 && code[i+1].op == invert_branch(inst.op))
            {
                // Handles code like:
                //  BEQ l1
                //  BNE l2
                //  L1:
                // (Removes the first BEQ)

                if(code[i+1].arg == code[i].arg)
                {
                    // Prune both
                    code[i].op = code[i+1].op = ASM_PRUNED;
                    code[i].arg = code[i+1].arg = {};
                }
                else
                {
                    // Prune the useless branch op
                    code[i] = code[i+1];
                    code[i+1].op = ASM_PRUNED;
                    code[i+1].arg = {};
                }
            }
        }
    }
}

void asm_proc_t::optimize()
{
    nopify_short_jumps();
    expand_branch_ops(); // Call after 'nopify_short_jumps'
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

void asm_proc_t::write_assembly(std::ostream& os, fn_t const& fn) const
{
    os << fn.global.name << ":\n";
    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t const& inst = code[i];

        for(auto const& pair : labels)
        {
            if(pair.second == i)
                os << "LABEL " << pair.first << ":\n";
        }

        if(inst.op == ASM_PRUNED)
            continue;

        os << to_string(inst.op) << ' ';

        switch(inst.arg.lclass())
        {
        case LOC_CONST_BYTE:
            os << "#" << inst.arg.data();
            break;
        case LOC_IOTA:
            os << "__iota";
            break;
        case LOC_FN:
            os << "fn " << inst.arg.fn()->global.name;
            break;
        case LOC_GMEMBER:
            os << "gmember " << inst.arg.gmember()->gvar.global.name << ' ' << inst.arg.gmember()->member() << " " << inst.arg.gmember()->span(inst.arg.atom());
            break;
        case LOC_ARG:
        case LOC_RETURN:
        case LOC_PHI:
        case LOC_SSA:
            os << "lvar " << fn.lvar_span(fn.lvars().index(inst.arg)) << "   " << inst.arg;
            break;

        case LOC_NONE:
            break;

        default:
            os << "???" << ' ' << inst.arg;
            break;
        }

        os << '\n';
    }
}

void asm_proc_t::write_binary(std::uint8_t* rom, addr16_t start_addr) const
{
    /*
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
    */
}
