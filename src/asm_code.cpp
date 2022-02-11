#include "asm_code.hpp"

void asm_proc_t::push_inst(asm_inst_t inst)
{
    if(inst.op == ASM_LABEL)
    {
        auto result = reloc.labels.insert({ inst.arg, reloc.code.size() });
        assert(result.inserted);
    }
    else
    {
        if((op_flags(inst.op) & ASMF_BRANCH) && op_addr_mode(inst.op) == MODE_RELATIVE)
            reloc.relative_branch_indices.push_back(reloc.code.size());
        else if(inst.op == JMP_RELATIVE)
            absolute_jmps.push_back(reloc.code.size);
        reloc.code.push_back(inst);
    }

}

void asm_proc_t::expand_branch_ops()
{
    bool progress; 
    do
    {
        progress = false;

        for(auto it = relative_branches.begin(); it != relative_branches.end();)
        {
            unsigned const branch_i = *it;
            cg_inst_t& inst = code[branch_i];
            assert(is_relative_branch(inst.op));

            unsigned const label_i = labels[inst.arg];
            int const dist = bytes_between(branch_i+1, label_i);

            if(dist > 127 || dist < -128)
            {
                inst.op = get_op(op_name(inst.op), MODE_LONG);
                it = relative_branches.erase(it);
                progress = true;
            }
            else
                ++it;
        }
    }
    while(progress);
}

void asm_proc_t::nopify_short_jumps()
{
    for(auto it = absolute_jmps.begin(); it != absolute_jmps.end();)
    {
        unsigned const jmp_i = *it;
        cg_inst_t& inst = code[jmp_i];
        assert(inst.op == JMP_ABSOLUTE);

        unsigned const label_i = labels[inst.arg];
        int const dist = bytes_between(jmp_i+1, label_i);

        if(dist == 1)
        {
            inst.op = SKB_JUMP;
            inst.arg = {};
            it = absolute_jmps.erase(it);
        }
        else if(dist == 2 && op_code(code[jmp_i+1].op) != 0x20) // Check for 0x20 to avoid reading a PPU register
        {
            inst.op = IGN_JUMP;
            inst.arg = {};
            it = absolute_jmps.erase(it);
        }
        else
            ++it;
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
