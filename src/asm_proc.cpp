#include "asm_proc.hpp"

#ifndef NDEBUG
#include <iostream>
#endif

#include "flat/small_set.hpp"

#include "format.hpp"
#include "globals.hpp"
#include "runtime.hpp"
#include "compiler_error.hpp"
#include "rom.hpp"

bool is_return(asm_inst_t const& inst)
{
    return ((op_flags(inst.op) & ASMF_RETURN) 
            || ((op_flags(inst.op) & ASMF_JUMP) 
                && !(op_flags(inst.op) & ASMF_SWITCH)
                && !is_label(inst.arg.lclass())));
}

bool mem_inst(asm_inst_t const& inst)
{
    return (op_input_regs(inst.op) | op_output_regs(inst.op)) & REGF_M;
}

bool o_peephole(asm_inst_t* begin, asm_inst_t* end)
{
    bool changed = false;

    for_each_peephole(begin, end, [&](asm_inst_t& a, asm_inst_t& b, asm_inst_t* c)
    {
        auto const replace_op = [&](op_t op)
        {
            a.op = op;
            b.op = ASM_PRUNED;
            changed = true;
        };

        // Converts RMW operations to their illegal versions.
        // e.g.:
        //     DEC foo
        //     CMP foo
        // becomes:
        //     DCP foo 
        auto const peep_rmw = [&](op_name_t second, op_name_t replace)
        {
            if(b.op == get_op(second, op_addr_mode(a.op))
               && a.arg == b.arg && a.alt == b.alt)
            {
                if(op_t new_op = get_op(replace, op_addr_mode(a.op)))
                {
                    replace_op(new_op);
                    return true;
                }
            }

            return false;
        };

        // Converts load, increment, store, into a RMW operation.
        // e.g.:
        //     LDX foo
        //     INX
        //     STX foo
        // becomes:
        //     INC foo
        //     LDX foo
        auto const peep_inxy = [&](op_name_t second, op_name_t store, op_name_t replace)
        {
            if(c && op_name(b.op) == second && op_name(c->op) == store 
               && op_addr_mode(a.op) == op_addr_mode(c->op)
               && a.arg == c->arg && a.alt == c->alt
               && (!a.arg || a.arg.known_variable())
               && (!a.alt || a.alt.known_variable()))
            {
                if(op_t new_op = get_op(replace, op_addr_mode(a.op)))
                {
                    c->op = a.op;
                    replace_op(new_op);
                    return true;
                }
            }

            return false;
        };

        // Converts load, load, into a transfer
        // e.g.:
        //     LDX foo
        //     LDA foo
        // becomes:
        //     LDX foo
        //     TXA
        auto const peep_transfer = [&](op_name_t second, op_t replace)
        {
            if(op_name(b.op) == second 
               && op_addr_mode(a.op) == op_addr_mode(b.op)
               && a.arg == b.arg
               && a.alt == b.alt
               && (!a.arg || a.arg.known_variable()))
            {
                b.op = replace;
                b.arg = b.alt = {};
                changed = true;
                return true;
            }

            return false;
        };

        auto const peep_lax = [&](op_name_t second)
        {
#ifndef LEGAL
            op_t replace = get_op(LAX, op_addr_mode(a.op));

            if(replace
               && op_name(b.op) == second 
               && op_addr_mode(a.op) == op_addr_mode(b.op)
               && a.arg == b.arg
               && a.alt == b.alt)
            {
                replace_op(replace);
                return true;
            }
#endif

            return false;
        };

        // Converts store, load, into a transfer
        // e.g.:
        //     STX foo
        //     LDA foo
        // becomes:
        //     STX foo
        //     TXA
        auto const peep_transfer2 = [&](op_name_t second, op_t replace)
        {
            if(op_name(b.op) == second 
               && (op_addr_mode(b.op) == MODE_ZERO_PAGE || op_addr_mode(b.op) == MODE_ABSOLUTE)
               && a.arg == b.arg
               && a.alt == b.alt
               && (!a.arg || a.arg.known_variable()))
            {
                b.op = replace;
                b.arg = b.alt = {};
                changed = true;
                return true;
            }

            return false;
        };

        // Converts load, store, into a load
        // e.g.:
        //     LDA foo
        //     STA foo
        // becomes:
        //     LDA foo
        auto const peep_remove_store = [&](op_name_t second)
        {
            if(op_name(b.op) == second 
               && (op_addr_mode(b.op) == MODE_ZERO_PAGE || op_addr_mode(b.op) == MODE_ABSOLUTE)
               && a.arg == b.arg
               && a.alt == b.alt
               && (!a.arg || a.arg.known_variable()))
            {
                b.op = ASM_PRUNED;
                b.arg = b.alt = {};
                changed = true;
                return true;
            }

            // Also check if a single irrelevant op is in-between:
            if(c 
               && op_name(c->op) == second 
               && (op_addr_mode(c->op) == MODE_ZERO_PAGE || op_addr_mode(c->op) == MODE_ABSOLUTE)
               && a.arg == c->arg
               && a.alt == c->alt
               && (!a.arg || a.arg.known_variable())
               && ((op_output_regs(b.op) & REGF_M) == 0 
                   || (op_name(b.op) == op_name(c->op) 
                       && (op_addr_mode(b.op) == MODE_ZERO_PAGE || op_addr_mode(b.op) == MODE_ABSOLUTE)
                       && (!b.arg || b.arg.known_variable())))
               && ((op_output_regs(b.op) & op_input_regs(c->op)) == 0)
               && op_flags(b.op) == 0)
            {
                c->op = ASM_PRUNED;
                c->arg = c->alt = {};
                changed = true;
                return true;
            }

            return false;
        };

        // Prune load, load
        // e.g.:
        //     LDA foo
        //     LAX foo
        // becomes:
        //     LAX foo
        auto const peep_remove_load = [&](op_name_t second, bool prune_second)
        {
            if(op_name(b.op) == second 
               && (a.arg == b.arg || !b.arg)
               && a.alt == b.alt
               && (!a.arg || a.arg.known_variable())
               && (!b.arg || b.arg.known_variable()))
            {
                if(prune_second)
                {
                    b.op = ASM_PRUNED;
                    b.arg = b.alt = {};
                }
                else
                {
                    a.op = ASM_PRUNED;
                    a.arg = a.alt = {};
                }
                changed = true;
                return true;
            }

            return false;
        };

    retry:

        // Prepare for ALR
        if(a.op == LDA_IMMEDIATE && op_name(b.op) == AND && !a.alt && !b.alt)
        {
            locator_t const imm = a.arg;

            a.op = get_op(LDA, op_addr_mode(b.op));
            a.arg = b.arg;
            b.op = AND_IMMEDIATE;
            b.arg = imm;
        }

        switch(b.op)
        {
        default: break;
        case AND_IMMEDIATE:
            if(!b.alt && b.arg == locator_t::const_byte(0xFF)
               && op_normal(a.op) && !(op_flags(a.op) & ASMF_FAKE)
               && (op_output_regs(a.op) & op_output_regs(AND_IMMEDIATE)) == op_output_regs(AND_IMMEDIATE))
            {
                goto prune_b;
            }
            break;
        case EOR_IMMEDIATE:
        case ORA_IMMEDIATE:
            if(!b.alt && b.arg == locator_t::const_byte(0)
               && op_normal(a.op) && !(op_flags(a.op) & ASMF_FAKE)
               && (op_output_regs(a.op) & op_output_regs(EOR_IMMEDIATE)) == op_output_regs(EOR_IMMEDIATE))
            {
            prune_b:
                b.op = ASM_PRUNED;
                b.arg = {};
                changed = true;
                goto retry;
            }
            break;
        }

        // Converts store, op, store into op, store
        // e.g.:
        //     STX foo
        //     STA bar
        //     STX foo
        // becomes:
        //     STA bar
        //     STX foo
        if(c && is_simple_store(op_name(a.op)) && a == *c 
           && (!a.arg || a.arg.known_variable()) 
           && (!a.alt || a.alt.known_variable())
           && op_flags(b.op) == 0
           && (op_output_regs(b.op) & op_input_regs(a.op)) == 0
           && (op_input_regs(b.op) & REGF_M) == 0)
        {
            if(!(op_output_regs(b.op) & REGF_M)
               || (!b.arg || b.arg.known_variable())
               || (!b.alt || b.alt.known_variable()))
            {
                a.op = ASM_PRUNED;
                changed = true;
            }
        }

        // Converts load, op, load into load, op
        // e.g.:
        //     LDX foo
        //     STA bar
        //     LDX foo
        // becomes:
        //     LDX foo
        //     STA bar
        if(c 
           && is_simple_load(op_name(a.op)) 
           && is_simple_load(op_name(c->op)) 
           && a.arg == c->arg && a.alt == c->alt
           && (!a.arg || a.arg.known_memory()) 
           && (!a.alt || a.alt.known_memory())
           && op_flags(b.op) == 0
           && (op_output_regs(b.op) & (op_output_regs(a.op) | REGF_M)) == 0)
        {
            switch(op_name(a.op))
            {
            default:
                break;
            case LDA:
                switch(op_name(c->op))
                {
                default:
                    break;
                case LDA:
                    lol_prune_c:
                    c->op = ASM_PRUNED;
                    lol_wipe_arg_alt:
                    c->arg = {};
                    c->alt = {};
                    changed = true;
                    break;
                case LDX:
                    c->op = TAX_IMPLIED;
                    goto lol_wipe_arg_alt;
                case LDY:
                    c->op = TAY_IMPLIED;
                    goto lol_wipe_arg_alt;
                }
                break;
            case LDX:
                switch(op_name(c->op))
                {
                default:
                    break;
                case LDX:
                    goto lol_prune_c;
                case LDA:
                    c->op = TXA_IMPLIED;
                    goto lol_wipe_arg_alt;
                }
                break;
            case LDY:
                switch(op_name(c->op))
                {
                default:
                    break;
                case LDY:
                    goto lol_prune_c;
                case LDA:
                    c->op = TYA_IMPLIED;
                    goto lol_wipe_arg_alt;
                }
                break;
            }
        }

        // Removes idempotent ops:
        if(a == b
           && (op_flags(a.op) & (ASMF_IDEMPOTENT | ASMF_FAKE)) == ASMF_IDEMPOTENT
           && (!a.arg || a.arg.known_variable())
           && (!a.alt || a.alt.known_variable()))
        {
            a.op = ASM_PRUNED;
            a.arg = a.alt = {};
            changed = true;
        }

        // Remove pointlesss ops:
        if((op_output_regs(a.op) & (~op_output_regs(b.op) | ~REGF_6502)) == 0
           && (op_input_regs(a.op) & ~REGF_6502) == 0
           && (op_input_regs(b.op) & ~REGF_6502) == 0
           && (op_input_regs(b.op) & op_output_regs(a.op)) == 0
           && op_flags(a.op) == 0
           && op_flags(b.op) == 0
           && (!a.arg || a.arg.known_variable())
           && (!a.alt || a.alt.known_variable())
           && (!b.arg || b.arg.known_variable())
           && (!b.alt || b.alt.known_variable()))
        {
            a.op = ASM_PRUNED;
            a.arg = a.alt = {};
            changed = true;
        }

        switch(op_name(a.op))
        {
        default: break;
#ifndef LEGAL
        case DEC: peep_rmw(CMP, DCP); break;
        case INC: peep_rmw(SBC, ISC); break;
        case ROL: peep_rmw(AND, RLA); break;
        case ROR: peep_rmw(ADC, RRA); break;
        case ASL: peep_rmw(ORA, SLO); break;
        case LSR: peep_rmw(EOR, SRE); break;
#endif
        case AND:
#ifndef LEGAL
            if(a.op == AND_IMMEDIATE && b.op == LSR_IMPLIED)
            {
                replace_op(ALR_IMMEDIATE);
                goto retry;
            }

            if(a.op == AND_IMMEDIATE && b.op == ALR_IMMEDIATE 
               && a.arg == b.arg && !a.alt && !b.alt)
            {
                goto prune_a;
            }
#endif
            // fall-through
        case ORA:
            if(op_name(a.op) == op_name(b.op)
               && op_addr_mode(a.op) == op_addr_mode(b.op)
               && a.arg == b.arg && !a.alt && !b.alt)
            {
            prune_a:
                a.op = ASM_PRUNED;
                a.arg = {};
            }
            break;
        case LAX:
            if(peep_remove_load(LDA, true)) 
                goto retry;
            if(peep_remove_load(LDX, true)) 
                goto retry;
            if(peep_remove_load(TAX, true)) 
                goto retry;
            if(peep_remove_load(TXA, true)) 
                goto retry;
            break;
        case LDX:
            if(peep_inxy(INX, STX, INC)) 
                goto retry;
            if(peep_inxy(DEX, STX, DEC)) 
                goto retry;
            if(peep_lax(LDA)) 
                goto retry;
            if(peep_remove_store(STX)) 
                goto retry;
            if(peep_remove_load(TAX, false)) 
                goto retry;
#ifndef LEGAL
            if(op_t lax = get_op(LAX, op_addr_mode(a.op)))
            {
                if(peep_remove_load(TXA, true)) 
                {
                    a.op = lax;
                    goto retry;
                }
            }
#endif
            break;
        case LDY:
            if(peep_inxy(INY, STY, INC))
                goto retry;
            if(peep_inxy(DEY, STY, DEC))
                goto retry;
            if(peep_transfer(LDA, TYA_IMPLIED))
                goto retry;
            if(peep_remove_store(STY)) 
                goto retry;
            if(peep_remove_load(TAY, false)) 
                goto retry;
            break;
        case LDA:
            if(peep_inxy(ASL, STA, ASL))
                goto retry;
            if(peep_inxy(LSR, STA, LSR))
                goto retry;
            if(peep_inxy(ROL, STA, ROL))
                goto retry;
            if(peep_inxy(ROR, STA, ROR))
                goto retry;
            if(peep_lax(LDX))
                goto retry;
            if(peep_transfer(LDY, TAY_IMPLIED))
                goto retry;
            if(peep_remove_store(STA)) 
                goto retry;
            if(peep_remove_load(TXA, false)) 
                goto retry;
            if(peep_remove_load(TYA, false)) 
                goto retry;
#ifndef LEGAL
            if(op_t lax = get_op(LAX, op_addr_mode(a.op)))
            {
                if(peep_remove_load(TAX, true)) 
                {
                    a.op = lax;
                    goto retry;
                }
            }
#endif
            break;
        case STA:
            if(peep_transfer2(LDX, TAX_IMPLIED))
                goto retry;
            if(peep_transfer2(LDY, TAY_IMPLIED))
                goto retry;
        common_store:
            if(b == a && a.arg.known_variable() && a.arg.known_variable(true))
                goto prune_a;
            break;
        case STX:
            if(peep_transfer2(LDA, TXA_IMPLIED))
                goto retry;
            goto common_store;
        case STY:
            if(peep_transfer2(LDA, TYA_IMPLIED))
                goto retry;
            goto common_store;
#ifndef LEGAL
        case SAX:
            goto common_store;
        case ALR:
            assert(a.op == ALR_IMMEDIATE);
            if(!a.alt && a.arg == locator_t::const_byte(1) && b.op == ROL_IMPLIED)
                replace_op(ANC_IMMEDIATE);
            break;
#endif

        case RTS:
        case JMP:
            // Code like:
            //   rts
            //   label:
            //   rts
            // Becomes:
            // label:
            //   rts

            if(c && c->op == a.op && b.op == ASM_LABEL && a.arg == c->arg && a.alt == c->alt)
            {
                a.op = ASM_PRUNED;
                a.arg = c->alt = {};
                changed = true;
            }
            break;
        }
    });

    return changed;
}


std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst)
{
    if(inst.op != ASM_LABEL)
        o << "  " ;
    o << "{ " << to_string(inst.op) << ", " << inst.arg;
    o << " hi: " << inst.alt;
    o << "   (" << inst.ssa_op << ") }";
#ifndef NDEBUG
    o << "   (" << (float(inst.cost) / 256.0f) << ") }";
#endif
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
    return ::prev_inst(&*code.begin(), &*code.end(), i + code.data());
}

asm_inst_t* asm_proc_t::next_inst(int i) 
{
    return ::next_inst(&*code.begin(), &*code.end(), i + code.data());
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
        auto result = labels.insert({ inst.arg.mem_head(), { .index = code.size() }});
        passert(result.second, inst);
    }

    code.push_back(inst);
}

void asm_proc_t::absolute_to_zp()
{
    for(asm_inst_t& inst : code)
    {
        // A hi-byte implies absolute.
        if(inst.alt && !inst.alt.eq_const(0))
            continue;

        if(inst.arg.is() != IS_DEREF && inst.arg.is() != IS_PTR)
            continue;

        locator_t arg = inst.arg;
        if(compiler_phase() >= PHASE_RUNTIME && inst.arg.lclass() == LOC_RUNTIME_RAM)
            arg = arg.link(ROMV_MODE, {}, -1);

        if(arg.lclass() == LOC_ADDR)
        {
           if(arg.data() + arg.offset() >= 0x100)
               continue;
        }
        else if(compiler_phase() != PHASE_COMPILE || !arg.mem_zp_only())
            continue;

        // OK! Replace with zp:

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

bool asm_proc_t::remove_banked_jsr(romv_t romv, int bank)
{
    if(bank < 0)
        return false;

    if(fn && fn->iasm)
        return false;

    bool did_something = false;

    for(unsigned i = 0; i < code.size(); ++i)
    {
        auto& inst = code[i];

        if(inst.alt || inst.arg.lclass() != LOC_FN)
            continue;

        op_t const op = unbanked_call_op(inst.op);
        if(!op || op != JSR_ABSOLUTE)
            continue;

        fn_ht const call = inst.arg.fn();
        if(!call || call->bank_switches() || call->returns_in_different_bank())
            continue;

        // Check if our banks are the same.
        rom_alloc_ht alloc = call->rom_proc()->find_alloc(romv);
        bool same_bank = false;
        alloc.for_each_bank([&](int other_bank)
        {
            same_bank |= bank == other_bank;
        });
        if(!same_bank)
            continue;

        // Remove the bank load if possible.
        if(op_t load_op = banked_call_load_op(inst.op))
        {
            if(i >= 2 && i+1 < code.size())
            {
                auto& load_bank = code[i-2];
                auto& read = code[i-1];
                auto& write = code[i+1];

                if(load_bank.op == load_op
                   && load_bank.arg == inst.arg.with_is(IS_BANK)
                   && !op_normal(read.op)
                   && !op_normal(write.op)
                   && (op_output_regs(write.op) & op_output_regs(load_bank.op)) == op_output_regs(load_bank.op))
                {
                    load_bank.op = ASM_PRUNED;
                    load_bank.arg = load_bank.alt = {};
                }
            }
        }

        inst.op = op;
        did_something = true;
    }

    return did_something;
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
            int dist = bytes_between(i+1, label_i);

            if(is_relative_branch(inst.op))
            {
                if(inst.arg.lclass() == LOC_CONST_BYTE)
                    continue;

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

                // Change to short instruction when in range
                if(dist <= 127 && dist >= -128 - size_diff)
                {
                    inst.op = new_op;
                    progress = true;

                    passert(bytes_between(i+1, label_i) <=  127, bytes_between(i+1, label_i));
                    passert(bytes_between(i+1, label_i) >= -128, bytes_between(i+1, label_i));
                }
            }
        }
    }
    while(progress);
}

void asm_proc_t::convert_indirect_jsr()
{
    fc::small_map<std::pair<locator_t, locator_t>, locator_t, 4> map;

    unsigned const code_size = code.size();
    for(unsigned i = 0; i < code_size; ++i)
    {
        asm_inst_t inst = code[i];
        if(inst.op != JSR_INDIRECT)
            continue;

        auto result = map.emplace(std::make_pair(inst.arg, inst.alt), locator_t());

        if(result.second)
        {
            result.first.underlying->second = push_label(next_label_id());
            inst.op = JMP_INDIRECT;
            push_inst(inst); // Invalidates code refs
        }

        code[i].op = JSR_ABSOLUTE;
        code[i].arg = result.first->second;
        code[i].alt = {};
    }
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
#ifndef LEGAL
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
#endif
        }
        else if(op_flags(inst.op) & ASMF_BRANCH)
        {
            // Prune unnecessary branches

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
    o_peephole(&*code.begin(), &*code.end());
    live_peephole(REGF_6502, code.data(), code.size());
    absolute_to_zp();
    optimize_short_jumps(!initial);
    convert_long_branch_ops();
    if(initial)
        convert_indirect_jsr();
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

    assert(pre_size >= size());
}

void asm_proc_t::link_variables(romv_t romv)
{
#ifndef NDEBUG
    std::size_t const pre_size = size();
#endif

    for(asm_inst_t& inst : code)
    {
        if(is_var_like(inst.arg.lclass()))
            inst.arg = inst.arg.link(romv, fn, -1);
        if(is_var_like(inst.alt.lclass()))
            inst.alt = inst.alt.link(romv, fn, -1);
    }

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
            os << "#" << inst.arg.data() << "   " << inst.arg;
            break;
        case LOC_GMEMBER:
            os << "gmember " << inst.arg.gmember()->gvar.global.name << ' ' << inst.arg.gmember()->member() 
               << " " << inst.arg.gmember()->span(inst.arg.atom()) << "   " << inst.arg;
            break;
        default:
            os << inst.arg << ' ';
            os << inst.alt << ' ';

            fn_ht lfn = fn;
            if(has_fn(inst.arg.lclass()) && inst.arg.fn())
                lfn = inst.arg.fn();

            if(lfn)
            {
                int const index = lfn->lvars().index(inst.arg);
                if(index >= 0)
                    os << " lvar " << lfn->lvar_span(romv, index);
            }

            break;
        }

        os << '\n';
    }
}

static std::pair<locator_t, locator_t> absolute_locs(asm_inst_t const& inst)
{
    locator_t lo = inst.arg;
    locator_t hi = inst.alt;

    if(indirect_addr_mode(op_addr_mode(inst.op)))
        hi = lo.with_is(IS_PTR_HI);
    else if(!hi)
    {
        if(lo.is() == IS_PTR || lo.is() == IS_DEREF)
            hi = lo.with_is(IS_PTR_HI);
        else
            hi = locator_t::const_byte(0);
    }

    return std::make_pair(lo, hi);
}

template<typename Fn>
void asm_proc_t::for_each_inst(Fn const& fn) const
{
    for(asm_inst_t const& inst : code)
    {
        if(op_size(inst.op) == 0)
            continue;

        switch(inst.op)
        {
        case STORE_C_ABSOLUTE:
            fn(asm_inst_t{ .op = PHP_IMPLIED });
            fn(asm_inst_t{ .op = PHA_IMPLIED });
            fn(asm_inst_t{ .op = LDA_IMMEDIATE, .arg = locator_t::const_byte(0) });
            fn(asm_inst_t{ .op = ROL_IMPLIED });
            fn(asm_inst_t{ .op = STA_ABSOLUTE, .arg = inst.arg });
            fn(asm_inst_t{ .op = PLA_IMPLIED });
            fn(asm_inst_t{ .op = PLP_IMPLIED });
            // total bytes: 1+1+2+1+3+1+1 = 10
            break;

        case STORE_C_ABSOLUTE_FAST:
            fn(asm_inst_t{ .op = LDA_IMMEDIATE, .arg = locator_t::const_byte(0) });
            fn(asm_inst_t{ .op = ROL_IMPLIED });
            fn(asm_inst_t{ .op = STA_ABSOLUTE, .arg = inst.arg });
            // total bytes: 2+1+3 = 6
            break;

        case STORE_Z_ABSOLUTE:
            fn(asm_inst_t{ .op = PHP_IMPLIED });
            fn(asm_inst_t{ .op = PHA_IMPLIED });
            fn(asm_inst_t{ .op = PHP_IMPLIED });
            fn(asm_inst_t{ .op = PLA_IMPLIED });
#ifndef LEGAL
            fn(asm_inst_t{ .op = ALR_IMMEDIATE, .arg = locator_t::const_byte(0b10) });
#else
            fn(asm_inst_t{ .op = LSR_IMPLIED });
            fn(asm_inst_t{ .op = AND_IMMEDIATE, .arg = locator_t::const_byte(0b1) });
#endif
            fn(asm_inst_t{ .op = STA_ABSOLUTE, .arg = inst.arg });
            fn(asm_inst_t{ .op = PLA_IMPLIED });
            fn(asm_inst_t{ .op = PLP_IMPLIED });
            // total bytes: 1+1+1+1+2+3+1+1 = 11   (+1 for LEGAL)
            break;

        case STORE_Z_ABSOLUTE_FAST:
            fn(asm_inst_t{ .op = PHP_IMPLIED });
            fn(asm_inst_t{ .op = PLA_IMPLIED });
#ifndef LEGAL
            fn(asm_inst_t{ .op = ALR_IMMEDIATE, .arg = locator_t::const_byte(0b10) });
#else
            fn(asm_inst_t{ .op = LSR_IMPLIED });
            fn(asm_inst_t{ .op = AND_IMMEDIATE, .arg = locator_t::const_byte(0b1) });
#endif
            fn(asm_inst_t{ .op = STA_ABSOLUTE, .arg = inst.arg });
            // total bytes: 1+1+2+3 = 7
            break;

        case STORE_N_ABSOLUTE:
            fn(asm_inst_t{ .op = PHP_IMPLIED });
            fn(asm_inst_t{ .op = PHA_IMPLIED });
            fn(asm_inst_t{ .op = PHP_IMPLIED });
            fn(asm_inst_t{ .op = PLA_IMPLIED });
#ifndef LEGAL
            fn(asm_inst_t{ .op = ANC_IMMEDIATE, .arg = locator_t::const_byte(0x80) });
#else
            fn(asm_inst_t{ .op = CMP_IMMEDIATE, .arg = locator_t::const_byte(0x80) });
            fn(asm_inst_t{ .op = LDA_IMMEDIATE, .arg = locator_t::const_byte(0x00) });
#endif
            fn(asm_inst_t{ .op = ROL_IMPLIED });
            fn(asm_inst_t{ .op = STA_ABSOLUTE, .arg = inst.arg });
            fn(asm_inst_t{ .op = PLA_IMPLIED });
            fn(asm_inst_t{ .op = PLP_IMPLIED });
            // total bytes: 1+1+1+1+2+1+3+1+1 = 12
            break;

        case STORE_N_ABSOLUTE_FAST:
            fn(asm_inst_t{ .op = PHP_IMPLIED });
            fn(asm_inst_t{ .op = PLA_IMPLIED });
#ifndef LEGAL
            fn(asm_inst_t{ .op = ANC_IMMEDIATE, .arg = locator_t::const_byte(0x80) });
#else
            fn(asm_inst_t{ .op = CMP_IMMEDIATE, .arg = locator_t::const_byte(0x80) });
            fn(asm_inst_t{ .op = LDA_IMMEDIATE, .arg = locator_t::const_byte(0x00) });
#endif
            fn(asm_inst_t{ .op = ROL_IMPLIED });
            fn(asm_inst_t{ .op = STA_ABSOLUTE, .arg = inst.arg });
            // total bytes: 1+1+2+1+3 = 8
            break;

        case BANKED_X_JSR:
        case BANKED_X_JMP:
            {
                assert(bankswitch_use_x());
                assert(!inst.alt);
                auto locs = absolute_locs(inst);

                fn(asm_inst_t{ .op = LDA_IMMEDIATE, .arg = locs.first });
                fn(asm_inst_t{ .op = LDY_IMMEDIATE, .arg = locs.second });
                if(inst.op == BANKED_X_JSR)
                    fn(asm_inst_t{ .op = JSR_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jsr_xy_trampoline) });
                else 
                {
                    assert(inst.op == BANKED_X_JMP);
                    fn(asm_inst_t{ .op = JMP_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jmp_xy_trampoline) });
                }
            }
            break;

        case BANKED_Y_JSR:
        case BANKED_Y_JMP:
            {
                assert(!bankswitch_use_x());
                assert(!inst.alt);
                auto locs = absolute_locs(inst);

                fn(asm_inst_t{ .op = LDA_IMMEDIATE, .arg = locs.first });
                fn(asm_inst_t{ .op = LDX_IMMEDIATE, .arg = locs.second });
                if(inst.op == BANKED_Y_JSR)
                    fn(asm_inst_t{ .op = JSR_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jsr_xy_trampoline) });
                else 
                {
                    assert(inst.op == BANKED_Y_JMP);
                    fn(asm_inst_t{ .op = JMP_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jmp_xy_trampoline) });
                }
            }
            break;

        case BANKED_JSR:
        case BANKED_JMP:
            {
                assert(!inst.alt);
                auto locs = absolute_locs(inst);

                fn(asm_inst_t{ .op = LDA_IMMEDIATE, .arg = locs.first });
                fn(asm_inst_t{ .op = LDX_IMMEDIATE, .arg = locs.second });
                if(inst.op == BANKED_JSR)
                    fn(asm_inst_t{ .op = JSR_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jsr_trampoline) });
                else 
                {
                    assert(inst.op == BANKED_JMP);
                    fn(asm_inst_t{ .op = JMP_ABSOLUTE, .arg = locator_t::runtime_rom(RTROM_jmp_trampoline) });
                }
            }
            break;

        case ASM_X_SWITCH:
            fn(asm_inst_t{ .op = LDA_ABSOLUTE_X, .arg = inst.alt.with_is(IS_DEREF) });
            fn(asm_inst_t{ .op = PHA_IMPLIED });
            fn(asm_inst_t{ .op = LDA_ABSOLUTE_X, .arg = inst.arg.with_is(IS_DEREF) });
            fn(asm_inst_t{ .op = PHA_IMPLIED });
            fn(asm_inst_t{ .op = RTS_IMPLIED });
            break;

        case ASM_Y_SWITCH:
            fn(asm_inst_t{ .op = LDA_ABSOLUTE_Y, .arg = inst.alt.with_is(IS_DEREF) });
            fn(asm_inst_t{ .op = PHA_IMPLIED });
            fn(asm_inst_t{ .op = LDA_ABSOLUTE_Y, .arg = inst.arg.with_is(IS_DEREF) });
            fn(asm_inst_t{ .op = PHA_IMPLIED });
            fn(asm_inst_t{ .op = RTS_IMPLIED });
            break;

        default:
            fn(inst);
            break;
        }
    }
}

template<typename Fn>
void asm_proc_t::for_each_locator(Fn const& fn) const
{
    for_each_inst([&](asm_inst_t const& inst)
    {
        if(inst.op == ASM_DATA)
        {
            fn(inst.arg);
            return;
        }

        passert(!(op_flags(inst.op) & ASMF_FAKE), to_string(inst.op), inst.arg);
        locator_t const op = locator_t::const_byte(op_code(inst.op));

        switch(op_addr_mode(inst.op))
        {
        case MODE_IMPLIED:
            fn(op);
            break;

        case MODE_IMMEDIATE:
        case MODE_BUGGY_IMMEDIATE:
        case MODE_RELATIVE:
        case MODE_ZERO_PAGE:
        case MODE_ZERO_PAGE_X:
        case MODE_ZERO_PAGE_Y:
        case MODE_INDIRECT_X:
        case MODE_INDIRECT_Y:
            fn(op);
            fn(inst.arg);
            break;

        case MODE_LONG:
            {
                locator_t const inverted_op = locator_t::const_byte(
                    op_code(get_op(invert_branch(op_name(inst.op)), MODE_RELATIVE)));
                fn(inverted_op);
                fn(locator_t::const_byte(3)); // Branch over upcoming jmp
                fn(locator_t::const_byte(op_code(JMP_ABSOLUTE)));
            }
            goto absolute_addr;

        case MODE_ABSOLUTE:
        case MODE_ABSOLUTE_X:
        case MODE_ABSOLUTE_Y:
        case MODE_INDIRECT:
            {
                fn(op);
            absolute_addr:
                auto locs = absolute_locs(inst);
                passert(locs.first && locs.second, inst);
                fn(locs.first);
                fn(locs.second);
            }
            break;

        default:
            throw std::runtime_error("Invalid addressing mode.");
        }
    });
}

loc_vec_t asm_proc_t::loc_vec() const
{
    loc_vec_t ret;
    ret.reserve(size());
    for_each_locator([&](locator_t loc){ ret.push_back(loc); });
    return ret;
}

static std::uint8_t get_byte(locator_t loc)
{
    assert(is_const(loc.lclass()));
    assert(loc.offset() == 0);

    if(loc.is() == IS_PTR_HI)
        return loc.data() >> 8;
    return loc.data();
}

void asm_proc_t::write_bytes(std::uint8_t* const start, romv_t romv, int bank) const
{
    std::uint8_t* at = start;

    auto const from_locator = [&](locator_t loc) -> std::uint8_t
    {
        loc = loc.link(romv, fn, bank);
        if(!is_const(loc.lclass()))
        {
            compiler_warning(fmt("Unable to link % (This is probably a bug in the compiler) (%)", loc, fn ? fn->global.name : ""));
            loc = locator_t::addr(0);
        }
        return get_byte(loc);
    };

    for_each_locator([&](locator_t loc){ *at++ = from_locator(loc); });
}

void asm_proc_t::relocate(locator_t from)
{
    std::uint16_t addr = linked_to_rom(from, true);

    for(unsigned i = 0; i < code.size(); ++i)
    {
        asm_inst_t& inst = code[i];

        auto const relocate1 = [&](locator_t loc)
        {
            int dist;
            unsigned label_i;

            if(op_addr_mode(inst.op) == MODE_RELATIVE && loc.lclass() == LOC_ADDR)
            {
                if(!is_const(from.lclass()))
                   throw std::runtime_error(fmt("Unable to relocate %", from));
                dist = linked_to_rom(loc) - addr - int(op_size(inst.op));
                goto have_dist;
            }

            if(!is_label(loc.lclass()))
                return loc;

            if(loc.is() == IS_BANK)
                return loc;

            {
                auto* mapped = labels.mapped(loc.mem_head());
                if(!mapped)
                    return loc;

                label_i = mapped->index;
            }

            if(op_addr_mode(inst.op) == MODE_RELATIVE)
            {
                dist = bytes_between(i, label_i) - int(op_size(inst.op)) + static_cast<std::int16_t>(loc.offset());
            have_dist:

                if(dist > 127 || dist < -128)
                {
                    std::string what = fmt("Unable to relocate branch instruction %. Destination outside valid range. (%)", 
                                           op_name(inst.op), dist);
                    if(fn)
                        compiler_error(get_pstring(inst), std::move(what));

                    throw relocate_error_t(std::move(what));
                }
                return locator_t::const_byte(dist);
            }
            else
                return from.with_advance_offset(loc.offset() + bytes_between(0, label_i)).with_is(loc.is());
        };

        inst.arg = relocate1(inst.arg);
        inst.alt = relocate1(inst.alt);
        addr += op_size(inst.op);
    }
}

pstring_t asm_proc_t::get_pstring(asm_inst_t const& inst)
{
    pstring_t pstring = {};

    if(inst.iasm_child >= 0)
    {
        assert(std::size_t(inst.iasm_child) < pstrings.size());
        pstring = pstrings[inst.iasm_child];
    }

    if(!pstring && fn)
        pstring = fn->global.pstring();

    return pstring;
}

unsigned asm_proc_t::add_pstring(pstring_t pstring)
{
    unsigned const ret = pstrings.size();
    pstrings.push_back(pstring);
    return ret;
}

void asm_proc_t::append(asm_proc_t const& proc)
{
    for(asm_inst_t inst : proc.code)
    {
        if(inst.iasm_child >= 0)
        {
            assert(std::size_t(inst.iasm_child) < proc.pstrings.size());
            inst.iasm_child = add_pstring(proc.pstrings[inst.iasm_child]);
        }

        push_inst(inst);
    }
}

unsigned asm_proc_t::next_label_id() const
{
    unsigned next_id = 0;

    for(auto const& inst : code)
        if(inst.op == ASM_LABEL && inst.arg.lclass() == LOC_MINOR_LABEL)
            next_id = std::max<unsigned>(next_id, inst.arg.data() + 1);

    return next_id;
}


void asm_proc_t::verify_addr_modes()
{
    if(!fn)
        return;

    for(auto const& inst : code)
    {
        if(zp_addr_mode(op_addr_mode(inst.op), true))
        {
            auto locs = absolute_locs(inst);
            if(get_byte(locs.second) != 0)
            {
                throw compiler_error_t(
                    fmt_error(get_pstring(inst), fmt("Operator '%' expects a zero-page address, but none was given.", inst.op))
                    + fmt_note("The +zero_page modifier ensures variables are allocated to the zero page."));
            }
        }
    }
}

void asm_proc_t::verify_legal()
{
#ifdef LEGAL
    for(auto const& inst : code)
        if(op_illegal(inst.op))
            compiler_warning(get_pstring(inst), fmt("Illegal opcode % used.", inst.op));
#endif
}

bool live_peephole(regs_t live_out, asm_inst_t* code, std::size_t size, log_t* log)
{
    log = &stdout_log;
    bool changed = false;

    static TLS std::vector<regs_t> live_regs;

    live_regs.clear();
    live_regs.resize(size, 0);

    for(int i = int(size) - 1; i >= 0; --i)
    {
        asm_inst_t const& inst = code[i];

        live_regs[i] = live_out;

        if(inst.op == ASM_LABEL || (op_flags(inst.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH | ASMF_FENCE)))
            live_out = REGF_6502;
        else
        {
            live_out &= ~op_output_regs(inst.op);
            live_out |= op_input_regs(inst.op);
        }
    }

    // TODO: put in asm.hpp
    auto const simple_addr_mode = [](addr_mode_t addr_mode)
    {
        switch(addr_mode)
        {
        case MODE_IMPLIED:
        case MODE_IMMEDIATE:
        case MODE_ZERO_PAGE:
        case MODE_ABSOLUTE:
            return true;
        default:
            return false;
        }
    };


    // Now attempt to optimize out redundant loads following stores.
    // e.g. in:
    //     STX foo
    //     . . .
    //     LDX foo
    // The LDX can be removed in some circumstances.
    //
    // Also replace redundant loads following loads.
    // e.g. in:
    //     LDX foo
    //     . . .
    //     LDX foo
    // The LDX can be removed in some circumstances.

    {
        locator_t last_a = locator_t();
        locator_t last_x = locator_t();
        locator_t last_y = locator_t();

        for(unsigned i = 0; i < size; ++i)
        {
            asm_inst_t& inst = code[i];

            if((op_flags(inst.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH | ASMF_FENCE))
               || inst.op == ASM_LABEL
               || inst.has_alt()
               || !simple_addr_mode(op_addr_mode(inst.op)))
            {
                last_a = locator_t();
                last_x = locator_t();
                last_y = locator_t();
                continue;
            }

            if(!inst.has_alt() && inst.arg.known_variable())
            {
                switch(op_name(inst.op))
                {
                case LDA:
                    if(!(live_regs[i] & (REGF_N | REGF_Z)) && last_a == inst.arg)
                    {
                        dprint(log, "REGLIVE_PRUNE_LDA", __LINE__);
                        inst.prune();
                        changed = true;
                        continue;
                    }
                    else if(last_x == inst.arg)
                    {
                        dprint(log, "REGLIVE_PRUNE_LDA", __LINE__);
                        inst.prune(TXA_IMPLIED);
                        changed = true;
                case TXA:
                        last_a = last_x;
                        continue;
                    }
                    else if(last_y == inst.arg)
                    {
                        dprint(log, "REGLIVE_PRUNE_LDA", __LINE__);
                        inst.prune(TYA_IMPLIED);
                        changed = true;
                case TYA:
                        last_a = last_y;
                        continue;
                    }
                    // fall-thru
                case STA:
                    last_a = inst.arg;
                    continue;

                case LDX:
                    if(!(live_regs[i] & (REGF_N | REGF_Z)) && last_x == inst.arg)
                    {
                        dprint(log, "REGLIVE_PRUNE_LDX", __LINE__);
                        inst.prune();
                        changed = true;
                        continue;
                    }
                    else if(last_a == inst.arg)
                    {
                        dprint(log, "REGLIVE_PRUNE_LDX", __LINE__);
                        inst.prune(TAX_IMPLIED);
                        changed = true;
                case TAX:
                        last_x = last_a;
                        continue;
                    }
                    // fall-thru
                case STX:
                    last_x = inst.arg;
                    continue;

                case LDY:
                    if(!(live_regs[i] & (REGF_N | REGF_Z)) && last_y == inst.arg)
                    {
                        dprint(log, "REGLIVE_PRUNE_LDY", __LINE__);
                        inst.prune();
                        changed = true;
                        continue;
                    }
                    else if(last_a == inst.arg)
                    {
                        dprint(log, "REGLIVE_PRUNE_LDY", __LINE__);
                        inst.prune(TAY_IMPLIED);
                        changed = true;
                case TAY:
                        last_y = last_a;
                        continue;
                    }
                    // fall-thru
                case STY:
                    last_y = inst.arg;
                    continue;

                default:
                    break;
                }
            }

            regs_t const outputs = op_output_regs(inst.op);

            if(outputs & REGF_A)
                last_a = locator_t();
            if(outputs & REGF_X)
                last_x = locator_t();
            if(outputs & REGF_Y)
                last_y = locator_t();
            if(outputs & REGF_M)
            {
                if(inst.arg == last_a)
                    last_a = locator_t();

                if(inst.arg == last_x)
                    last_x = locator_t();

                if(inst.arg == last_y)
                    last_y = locator_t();
            }
        }
    }

    // Now attempt to optimize out redundant stores following stores.
    // e.g. in:
    //     STX foo
    //     . . .
    //     STX foo
    // The STX can be removed in some circumstances.

    {
        constexpr unsigned MAX_SET_SIZE = 8;
        using set_t = fc::small_set<locator_t, MAX_SET_SIZE>;
        set_t a_set;
        set_t x_set;
        set_t y_set;

        for(unsigned i = 0; i < size; ++i)
        {
            asm_inst_t& inst = code[i];

            if((op_flags(inst.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH | ASMF_FENCE))
               || inst.op == ASM_LABEL
               || inst.has_alt()
               || !simple_addr_mode(op_addr_mode(inst.op)))
            {
                a_set.clear();
                x_set.clear();
                y_set.clear();
                continue;
            }

            if(!inst.has_alt() && inst.arg.known_variable())
            {
                switch(op_name(inst.op))
                {
                case STA:
                    if(!a_set.insert(inst.arg).second)
                    {
                        dprint(log, "REGLIVE_PRUNE_STA", inst.arg, __LINE__);
                        inst.prune();
                        changed = true;
                    }
                    continue;

                case STX:
                    if(!x_set.insert(inst.arg).second)
                    {
                        dprint(log, "REGLIVE_PRUNE_STX", inst.arg, __LINE__);
                        inst.prune();
                        changed = true;
                    }
                    continue;

                case STY:
                    if(!y_set.insert(inst.arg).second)
                    {
                        dprint(log, "REGLIVE_PRUNE_STY", inst.arg, __LINE__);
                        inst.prune();
                        changed = true;
                    }
                    continue;
                default:
                    break;
                }
            }

            regs_t const outputs = op_output_regs(inst.op);

            if(outputs & REGF_A)
                a_set.clear();
            if(outputs & REGF_X)
                x_set.clear();
            if(outputs & REGF_Y)
                y_set.clear();
            if((outputs & REGF_M) && inst.arg)
            {
                a_set.erase(inst.arg);
                x_set.erase(inst.arg);
                y_set.erase(inst.arg);
            }
        }
    }

    // Perform some peep-hole optimization using the liveness analysis:

    {
        for_each_peephole(code, code + size, [&](asm_inst_t& a, asm_inst_t& b, asm_inst_t* c)
        {
            unsigned const ai = &a - code;
            unsigned const bi = &b - code;

            // Prune ops that have no effect:
            if(!(op_flags(a.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH | ASMF_FAKE | ASMF_IMPURE))
               && a.op < NUM_NORMAL_OPS
               && !(REGF_M & op_output_regs(a.op))
               && !(live_regs[ai] & op_output_regs(a.op))
               && (!a.arg || a.arg.known_variable())
               && !a.has_alt())
            {
                dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                a.prune();
                changed = true;
                return;
            }

            // Remove unnecessary transfers:
            switch(b.op)
            {
            default: break;
            case CMP_IMMEDIATE:
            case CPX_IMMEDIATE:
            case CPY_IMMEDIATE:
                if(!b.has_alt() && b.arg == locator_t::const_byte(0)
                   && op_normal(a.op) && !(op_flags(a.op) & ASMF_FAKE)
                   && (op_output_regs(a.op) & (op_input_regs(b.op) | REGF_NZ)) == (op_input_regs(b.op) | REGF_NZ)
                   && !(live_regs[bi] & (op_output_regs(b.op) & ~(REGF_NZ))))
                {
                    goto prune_b;
                }
                break;
            case TAY_IMPLIED:
            case TAX_IMPLIED:
            case TYA_IMPLIED:
            case TXA_IMPLIED:
                if(op_normal(a.op) && !(op_flags(a.op) & ASMF_FAKE)
                   && (op_output_regs(a.op) & (op_input_regs(b.op) | REGF_NZ)) == (op_input_regs(b.op) | REGF_NZ)
                   && !(live_regs[bi] & (op_output_regs(b.op) & ~(REGF_NZ))))
                {
                prune_b:
                    dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                    b.prune();
                    changed = true;
                }
                break;
            }

            // Convert code like:
            //     LDA foo
            //     TAY
            // To:
            //     LDY foo
            switch(op_name(a.op))
            {
            case LDA:
                if(b.op == TAX_IMPLIED && !(live_regs[bi] & REGF_A))
                {
                    if(op_t op = get_op(LDX, op_addr_mode(a.op)))
                    {
                        a.op = op;
                        dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                        b.prune();
                        changed = true;
                    }
                }
                else if(b.op == TAY_IMPLIED && !(live_regs[bi] & REGF_A))
                {
                    if(op_t op = get_op(LDY, op_addr_mode(a.op)))
                    {
                        a.op = op;
                        dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                        b.prune();
                        changed = true;
                    }
                }
                break;

            case LDX:
                if(b.op == TXA_IMPLIED && !(live_regs[bi] & REGF_X))
                {
                    if(op_t op = get_op(LDA, op_addr_mode(a.op)))
                    {
                        a.op = op;
                        dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                        b.prune();
                        changed = true;
                    }
                }
                break;

            case LDY:
                if(b.op == TYA_IMPLIED && !(live_regs[bi] & REGF_Y))
                {
                    if(op_t op = get_op(LDA, op_addr_mode(a.op)))
                    {
                        a.op = op;
                        dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                        b.prune();
                        changed = true;
                    }
                }
                break;

#ifndef LEGAL
            case LAX:
                // Convert LAX to either LDA or LDX:
                if(!(live_regs[ai] & REGF_A))
                {
                    if(op_t op = get_op(LDX, op_addr_mode(a.op)))
                    {
                        dprint(log, "REGLIVE_LAX", __LINE__);
                        a.op = op;
                        changed = true;
                    }
                }
                else if(!(live_regs[ai] & REGF_X))
                {
                    if(op_t op = get_op(LDA, op_addr_mode(a.op)))
                    {
                        dprint(log, "REGLIVE_LAX", __LINE__);
                        a.op = op;
                        changed = true;
                    }
                }
                break;
#endif

            default:
                break;
            }

#ifndef LEGAL
            // Convert code like:
            //    CMP #$80
            //    ROR
            //    CMP #$80
            // To:
            //    CMP #$80
            //    ARR #$FF
            if(c && a.op == CMP_IMMEDIATE 
               && b.op == ROR_IMPLIED
               && !a.has_alt()  && a.arg  == locator_t::const_byte(0x80)
               && !c->has_alt() && c->arg == locator_t::const_byte(0x80)
               && !(live_regs[ai] & REGF_V))
            {
                dprint(log, "REGLIVE_ARR",  __LINE__);
                b = a;
                c->op = ARR_IMMEDIATE;
                c->arg = locator_t::const_byte(0xFF);
                a.prune();
                changed = true;
            }
#endif
        });
    }


    // Now attempt to optimize out redundant loads following loads.
    // e.g. in:
    //     LDX #0
    //     . . .
    //     LDA #0
    // The LDA can be removed in some circumstances.

    {
        using map_t = fc::small_map<locator_t, unsigned, 16>;
        map_t map;

        auto const store_name = [](op_t op) -> op_name_t
        {
            switch(op)
            {
            case LDA_IMMEDIATE: return STA;
            case LDX_IMMEDIATE: return STX;
            case LDY_IMMEDIATE: return STY;
            default: return {};
            }
        };

        for_each_peephole(code, code + size, [&](asm_inst_t& a, asm_inst_t& b, asm_inst_t* c)
        {
            unsigned const ai = &a - code;
            unsigned const bi = &b - code;

            if((op_flags(a.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH | ASMF_FENCE)))
            {
                map.clear();
                return;
            }

            if(a.has_alt() || b.has_alt())
                return;

            if(op_name_t store = store_name(a.op))
            {
                auto result = map.emplace(a.arg, ai);

                if(!result.second)
                {
                    // See if we can map.

                    // - next ops must be STA
                    // - no mention of the store in-between

                    unsigned const prev_i = result.first->second;
                    assert(prev_i < ai);

                    if(op_name(b.op) == store 
                       && !(live_regs[bi] & op_output_regs(a.op))
                       && b.arg.known_variable())
                    {
                        for(unsigned i = prev_i; i < ai; ++i)
                        {
                            auto const& inst = code[i];
                            if(inst.arg == b.arg || inst.alt == b.arg
                               || !simple_addr_mode(op_addr_mode(inst.op)))
                            {
                                goto fail;
                            }
                        }

                        // OK! We can relocate.

                        // Prune 'a':
                        a.prune();

                        // Shift every op forwards one:
                        std::move(code + prev_i + 1, code + ai, 
                                  code + prev_i + 2);

                        // Move 'b' into the new hole:
                        dprint(log, "REGLIVE_PRUNE_3", b);
                        auto& b_dest = code[prev_i + 1];
                        b.op = get_op(store_name(code[prev_i].op), op_addr_mode(b.op));
                        assert(b.op);
                        b_dest = b;
                        b.prune();
                        changed = true;
                        return;
                    }

                fail:
                    result.first.underlying->second = ai;
                }
            }

        });
    }

    // Convert MAYBE stores to FAST versions.
    for(unsigned i = 0; i < size; ++i)
    {
        asm_inst_t& inst = code[i];
        if(op_t fast = fast_op(inst.op))
        {
            if((live_regs[i] & op_output_regs(fast) & REGF_6502) == 0)
            {
                inst.op = fast;
                changed = true;
            }
        }
    }

    return changed;
}
