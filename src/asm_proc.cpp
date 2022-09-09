#include "asm_proc.hpp"

#include "format.hpp"
#include "globals.hpp"
#include "runtime.hpp"
#include "compiler_error.hpp"

bool is_return(asm_inst_t const& inst)
{
    return ((op_flags(inst.op) & ASMF_RETURN) 
            || ((op_flags(inst.op) & ASMF_JUMP) 
                && !is_label(inst.arg.lclass())));
}

bool mem_inst(asm_inst_t const& inst)
{
    return (op_input_regs(inst.op) | op_output_regs(inst.op)) & REGF_M;
}


std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst)
{
    o << "{ " << to_string(inst.op) << ", " << inst.arg;
    o << " hi: " << inst.alt;
    o << "   (" << inst.ssa_op << ") }";
    //o << "   (" << (float(inst.cost) / 256.0f) << ") }";
    return o;
}

asm_proc_t::asm_proc_t(fn_ht fn, std::vector<asm_inst_t> code_, locator_t entry_label)
: fn(fn)
, entry_label(entry_label)
, code(std::move(code_))
{
    rebuild_label_map();
}

void asm_proc_t::rebuild_label_map()
{
    labels.clear();

    for(unsigned i = 0; i < code.size(); ++i)
    {
        if(code[i].op == ASM_LABEL)
        {
            auto result = labels.insert({ code[i].arg.mem_head(), { .index = i }});
            assert(result.second);
        }
    }

    assert(!entry_label || labels.count(entry_label));
}

void asm_proc_t::build_label_offsets()
{
    int offset = 0;
    for(asm_inst_t const& inst : code)
    {
        if(inst.op == ASM_LABEL)
            get_label(inst.arg).offset = offset;
        offset += op_size(inst.op);
    }
}

asm_inst_t* asm_proc_t::prev_inst(int i)
{
    for(--i; i >= 0; --i)
        if(op_size(code[i].op))
            return &code[i];
    return nullptr;
}

asm_inst_t* asm_proc_t::next_inst(int i) 
{
    for(++i; i < int(code.size()); ++i)
        if(op_size(code[i].op))
            return &code[i];
    return nullptr;
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

void asm_proc_t::push_inst(asm_inst_t inst)
{
    if(inst.op == ASM_LABEL)
    {
        auto result = labels.insert({ inst.arg.with_is(IS_DEREF), { .index = code.size() }});
        assert(result.second);
    }

    code.push_back(inst);
}

void asm_proc_t::absolute_to_zp()
{
    for(asm_inst_t& inst : code)
    {
        // TODO: implement this
        if(inst.alt || inst.arg.lclass() != LOC_ADDR || inst.arg.data() >= 0x100)
            continue;

        switch(op_addr_mode(inst.op))
        {
        case MODE_ABSOLUTE:
            if(op_t new_op = get_op(op_name(inst.op), MODE_ZERO_PAGE))
                inst.op = new_op;
            break;

        // These are *generally* safe, but aren't if arrays can start in ZP but end outside of it.
        // TODO: Better specify this by possibly adding new pseudo asm ops.
        case MODE_ABSOLUTE_X:
            if(op_t new_op = get_op(op_name(inst.op), MODE_ZERO_PAGE_X))
                inst.op = new_op;
            break;
        case MODE_ABSOLUTE_Y: 
            if(op_t new_op = get_op(op_name(inst.op), MODE_ZERO_PAGE_Y))
                inst.op = new_op;
            break;

        default: 
            break;
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

            unsigned const label_i = get_label(inst.arg).index;
            asm_inst_t const* next = next_inst(i);
            if(!next)
                continue;
            int dist = bytes_between(next - code.data(), label_i);

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
                if(dist <= 127 && dist >= -128)
                {
                    inst.op = new_op;
                    progress = true;

                    assert(bytes_between(next - code.data(), label_i) <= 127);
                    assert(bytes_between(next - code.data(), label_i) >= -128);
                }
            }
        }
    }
    while(progress);
}

// Note: 'use_nops' can be dangerous if applied too early,
// as it hardcodes the relative jump distance.
void asm_proc_t::optimize_short_jumps(bool use_nops)
{
    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t& inst = code[i];

        asm_inst_t* next = next_inst(i);
        if(!next)
            continue;

        if(inst.op == JMP_ABSOLUTE)
        {
            unsigned const label_i = get_label(inst.arg).index;
            int const dist = bytes_between(next - code.data(), label_i);
            
            if(dist == 0)
            {
                // Prune unnecessary jumps
                inst.op = ASM_PRUNED;
                inst.arg = {};
            }
            else if(use_nops && dist == 1)
            {
                inst.op = SKB_IMPLIED;
                inst.arg = {};
            }
            else if(use_nops && dist == 2)
            {
                auto o = op_code(next->op);
                if(o < 0x20 || o >= 0x42) // Avoid reading PPU / APU registers, etc.
                {
                    inst.op = IGN_IMPLIED;
                    inst.arg = {};
                }
            }
        }
        else if(op_flags(inst.op) & ASMF_BRANCH)
        {
            // Prune unecessary branches

            unsigned const label_i = get_label(inst.arg).index;
            int const dist = bytes_between(next - code.data(), label_i);

            if(dist == 0)
            {
                inst.op = ASM_PRUNED;
                inst.arg = {};
            }
            else if(dist == 2 && next->op == invert_branch(inst.op))
            {
                // Handles code like:
                //  BEQ l1
                //  BNE l2
                //  L1:
                // (Removes the first BEQ)

                if(next->arg == code[i].arg)
                {
                    // Prune both
                    code[i].op = next->op = ASM_PRUNED;
                    code[i].arg = next->arg = {};
                }
                else
                {
                    // Prune the useless branch op
                    code[i] = *next;
                    next->op = ASM_PRUNED;
                    next->arg = {};
                }
            }
        }
    }
}

void asm_proc_t::optimize(bool initial)
{
    // Order matters here.
    absolute_to_zp();
    optimize_short_jumps(!initial);
    convert_long_branch_ops();
}

void asm_proc_t::initial_optimize()
{
    // Order matters here.
    optimize(true);
}

void asm_proc_t::link(romv_t romv, int bank)
{
#ifndef NDEBUG
    std::size_t const pre_size = size();
#endif

    for(asm_inst_t& inst : code)
    {
        inst.arg = inst.arg.link(romv, fn, bank);
        inst.alt = inst.alt.link(romv, fn, bank);
    }

    if(!fn || !fn->iasm)
        optimize(false);
    assert(pre_size >= size());
}

void asm_proc_t::write_assembly(std::ostream& os, romv_t romv) const
{
    if(fn)
        os << fn->global.name << ":\n";

    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t const& inst = code[i];

        for(auto const& pair : labels)
            if(pair.second.index == i)
                os << "LABEL " << pair.first << ":\n";

        if(inst.op == ASM_PRUNED || inst.op == ASM_LABEL)
            continue;

        os << "-   " << to_string(inst.op) << ' ';

        switch(inst.arg.lclass())
        {
        case LOC_CONST_BYTE:
            os << "#" << inst.arg.data();
            break;
        case LOC_GMEMBER:
            os << "gmember " << inst.arg.gmember()->gvar.global.name << ' ' << inst.arg.gmember()->member() 
               << " " << inst.arg.gmember()->span(inst.arg.atom());
            break;
        default:
            os << inst.arg;

            if(has_fn(inst.arg.lclass()) && inst.arg.fn())
            {
                fn_ht fn = inst.arg.fn();
                int const index = fn->lvars().index(inst.arg);
                if(index >= 0)
                    os << " lvar " << fn->lvar_span(romv, index);
            }

            break;
        }

        os << '\n';
    }
}

void asm_proc_t::write_bytes(std::uint8_t* const start, romv_t romv, int bank) const
{
    std::uint8_t* at = start;

    auto const write = [&](std::uint8_t data) { *at++ = data; };

    auto const from_locator = [&](locator_t loc) -> std::uint8_t
    {
        loc = loc.link(romv, fn, bank);
        if(!is_const(loc.lclass()))
            throw std::runtime_error(fmt("Unable to link %", loc));
        assert(loc.offset() == 0);

        std::uint16_t data = loc.data(); // TODO

        if(loc.is() == IS_PTR_HI)
            data >>= 8;

        return data;
    };

    auto const absolute_locs = [](asm_inst_t const& inst)
    {
        locator_t lo = inst.arg;
        locator_t hi = inst.alt;

        if(!hi)
            hi = lo;

        lo.set_is(IS_PTR);
        hi.set_is(IS_PTR_HI);

        return std::make_pair(lo, hi);
    };

    auto const write_inst = [&](asm_inst_t const& inst)
    {
        passert(!(op_flags(inst.op) & ASMF_FAKE), to_string(inst.op), inst.arg);
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
                auto locs = absolute_locs(inst);
                write(from_locator(locs.first));
                write(from_locator(locs.second));
            }
            break;

        default:
            throw std::runtime_error("Invalid addressing mode.");
        }
    };

    for(asm_inst_t const& inst : code)
    {
        if(op_size(inst.op) == 0)
            continue;

        if(inst.op == STORE_C_ABSOLUTE)
        {
            write_inst({ .op = PHP_IMPLIED });
            write_inst({ .op = PHA_IMPLIED });
            write_inst({ .op = LDA_IMMEDIATE, .arg = locator_t::const_byte(0) });
            write_inst({ .op = ROL_IMPLIED });
            write_inst({ .op = STA_ABSOLUTE, .arg = inst.arg });
            write_inst({ .op = PLA_IMPLIED });
            write_inst({ .op = PLP_IMPLIED });
            // total bytes: 1+1+2+1+3+1+1 = 10
        }
        else if(inst.op == STORE_Z_ABSOLUTE)
        {
            write_inst({ .op = PHP_IMPLIED });
            write_inst({ .op = PHA_IMPLIED });
            write_inst({ .op = PHP_IMPLIED });
            write_inst({ .op = PLA_IMPLIED });
            write_inst({ .op = ALR_IMMEDIATE, .arg = locator_t::const_byte(0b10) });
            write_inst({ .op = STA_ABSOLUTE, .arg = inst.arg });
            write_inst({ .op = PLA_IMPLIED });
            write_inst({ .op = PLP_IMPLIED });
            // total bytes: 1+1+1+1+2+3+1+1 = 11
        }
        if(inst.op == BANKED_Y_JSR || inst.op == BANKED_Y_JMP)
        {
            assert(!inst.alt);
            auto locs = absolute_locs(inst);

            write_inst({ .op = LDA_IMMEDIATE, .arg = locs.first });
            write_inst({ .op = LDX_IMMEDIATE, .arg = locs.second });
            if(inst.op == BANKED_Y_JSR)
                write_inst({ .op = JSR_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jsr_y_trampoline) });
            else 
            {
                assert(inst.op == BANKED_Y_JMP);
                write_inst({ .op = JMP_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jmp_y_trampoline) });
            }
        }
        else
            write_inst(inst);

    }
}

void asm_proc_t::relocate(std::uint16_t addr)
{
    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t& inst = code[i];

        if(!is_label(inst.arg.lclass()))
            continue;

        assert(inst.arg.is() != IS_BANK);
        assert(labels.count(inst.arg.with_is(IS_DEREF)));
        unsigned const label_i = get_label(inst.arg).index;

        if(op_addr_mode(inst.op) == MODE_RELATIVE)
        {
            int const dist = bytes_between(i, label_i) - op_size(inst.op);
            if(dist > 127 || dist < -128)
            {
                std::string what = fmt("Unable to relocate branch instruction %. Destination outside valid range.", 
                                       op_name(inst.op));
                if(fn)
                {
                    pstring_t pstring = fn->global.pstring();
                    if(inst.iasm_stmt >= 0)
                        pstring = fn->def().stmts[inst.iasm_stmt].pstring;
                    compiler_error(pstring, std::move(what));
                }
                throw std::runtime_error(std::move(what)); // TODO: make it a real compiler_error
            }
            inst.arg = locator_t::const_byte(dist);
        }
        else if(op_addr_mode(inst.op) == MODE_IMMEDIATE)
            inst.arg = locator_t::const_byte(addr + bytes_between(0, label_i));
        else
            inst.arg = locator_t::addr(addr + bytes_between(0, label_i));
    }
}
