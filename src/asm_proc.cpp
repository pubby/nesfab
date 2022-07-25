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

        //if(!is_label(inst.arg.lclass()))
            //mem_usage[inst.arg.mem_head()] += 1;
    }

}

void asm_proc_t::absolute_to_zp()
{
    for(asm_inst_t& inst : code)
    {
        if(inst.ptr_hi || inst.arg.lclass() != LOC_ADDR || inst.arg.data() >= 0x100)
            continue;

        switch(op_addr_mode(inst.op))
        {
        case MODE_ABSOLUTE:
            inst.op = get_op(op_name(inst.op), MODE_ZERO_PAGE); 
            break;
        case MODE_ABSOLUTE_X:
            inst.op = get_op(op_name(inst.op), MODE_ZERO_PAGE_X); 
            break;
        case MODE_ABSOLUTE_Y: 
            inst.op = get_op(op_name(inst.op), MODE_ZERO_PAGE_Y); 
            break;
        default: 
            continue;
        }
    }
}

void asm_proc_t::convert_long_branch_ops()
{
    // Loop until we can do no more work.
    bool progress; 
    do
    {
        progress = false;

        for(unsigned i = 0; i < code.size(); ++i)
        {
            asm_inst_t& inst = code[i];

            if(!is_branch(inst.op))
                continue;

            unsigned const label_i = labels[inst.arg];
            int dist = bytes_between(i+1, label_i);

            if(is_relative_branch(inst.op))
            {
                // Change to long pseudo instruction when out of range
                if(dist > 127 || dist < -128)
                {
                    inst.op = get_op(op_name(inst.op), MODE_LONG);
                    progress = true;
                }
            }
            else if(is_long_branch(inst.op))
            {
                op_t const new_op = get_op(op_name(inst.op), MODE_RELATIVE);
                int const size_diff = int(op_size(inst.op)) - int(op_size(new_op));

                dist -= size_diff;

                // Change to short instruction when in range
                if(dist <= 127 || dist >= -128)
                {
                    inst.op = new_op;
                    progress = true;
                    
                    assert(bytes_between(i+1, label_i) <= 127);
                    assert(bytes_between(i+1, label_i) >= -128);
                }
            }
        }
    }
    while(progress);
}

void asm_proc_t::optimize_short_jumps(bool initial)
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
            else if(!initial && dist == 1)
            {
                inst.op = SKB_IMPLIED;
                inst.arg = {};
            }
            else if(!initial && dist == 2 && op_code(code[i+1].op) != 0x20) // Check for 0x20 to avoid reading a PPU register
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

void asm_proc_t::optimize(bool initial)
{
    // Order matters here.
    absolute_to_zp();
    optimize_short_jumps(true);
    convert_long_branch_ops();
}

void asm_proc_t::initial_optimize() { optimize(true); }

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

void asm_proc_t::write_bytes(std::uint8_t* const start, int bank) const
{
    std::uint8_t* at = start;

    auto const write = [&](std::uint8_t data) { *at++ = data; };

    auto const from_locator = [&](locator_t loc) -> std::uint8_t
    {
        loc = loc.link(fn, bank);
        if(!is_const(loc.lclass()))
            throw std::runtime_error(fmt("Unable to link %", loc));
        std::uint16_t data = loc.data() + loc.offset();
        if(loc.high())
            return data >>= 8;
        return data;
    };

    for(asm_inst_t const& inst : code)
    {
        if(inst.op == ASM_PRUNED)
            continue;

        std::uint8_t const op = op_code(inst.op);

        switch(op_addr_mode(inst.op))
        {
        case MODE_IMPLIED:
            write(op);
            break;

        case MODE_IMMEDIATE:
        case MODE_RELATIVE:
        case MODE_ZERO_PAGE:
        case MODE_ZERO_PAGE_X:
        case MODE_ZERO_PAGE_Y:
        case MODE_INDIRECT_X:
        case MODE_INDIRECT_Y:
            write(op);
            write(from_locator(inst.arg));
            break;

        case MODE_LONG:
            {
                std::uint8_t const inverted_op = op_code(get_op(invert_branch(op_name(inst.op)), MODE_RELATIVE));
                write(inverted_op);
                write(3); // Branch over upcoming jmp
                write(op_code(JMP_ABSOLUTE));
            }
            goto absolute_addr;

        case MODE_ABSOLUTE:
        case MODE_ABSOLUTE_X:
        case MODE_ABSOLUTE_Y:
        case MODE_INDIRECT:
            {
                write(op);
            absolute_addr:
                locator_t lo = inst.arg;
                locator_t hi = inst.ptr_hi;

                if(!hi)
                {
                    hi = lo;
                    lo.set_high(false);
                    hi.set_high(true);
                }

                write(from_locator(lo));
                write(from_locator(hi));
            }
            break;

        default:
            throw std::runtime_error("Invalid addressing mode.");
        }
    }
}

void asm_proc_t::link(int bank)
{
#ifndef NDEBUG
    std::size_t const pre_size = size();
#endif

    for(asm_inst_t& inst : code)
    {
        inst.arg = inst.arg.link(fn, bank);
        inst.ptr_hi = inst.ptr_hi.link(fn, bank);
    }

    optimize(false);

    assert(pre_size >= size());
}

void asm_proc_t::relocate(std::uint16_t addr)
{
    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t& inst = code[i];

        if(!is_label(inst.arg.lclass()))
            continue;

        assert(labels.count(inst.arg));
        unsigned const label_i = labels[inst.arg];

        if(op_addr_mode(inst.op) == MODE_RELATIVE)
        {
            int const dist = bytes_between(i+1, label_i);
            assert(dist <= 127 && dist >= -128);
            inst.arg = locator_t::const_byte(dist);
        }
        else 
            inst.arg = locator_t::addr(addr + bytes_between(0, label_i));
    }
}

