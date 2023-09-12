#include "runtime.hpp"

#include <vector>

#include "options.hpp"
#include "locator.hpp"
#include "asm_proc.hpp"
#include "rom.hpp"
#include "hw_reg.hpp"
#include "pow2.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "ram_init.hpp"
#include "compiler_error.hpp"

rom_proc_ht reset_proc = {};

namespace // anonymous
{
    class runtime_ram_allocator_t
    {
    public:
        runtime_ram_allocator_t()
        : m_non_zp(span_t{ 0x200, 0x600 })
        {}

        span_t alloc_zp(std::uint16_t size)
        {
            if(m_next_zp + size >= 256)
                return {};
            span_t const ret = { .addr = m_next_zp, .size = size };
            m_next_zp += size;
            return mark_allocated({ ret, ret });
        }

        span_t alloc_non_zp(std::uint16_t size, unsigned alignment = 0)
        {
            return mark_allocated(m_non_zp.alloc(size, alignment));
        }

        span_t mark_allocated(span_allocation_t spans)
        {
            allocated |= ram_bitset_t::filled(spans.allocation.addr, spans.allocation.size);
            return spans.object;
        }

        ram_bitset_t allocated = {};

    private:
        span_allocator_t m_non_zp;
        unsigned m_next_zp = 0;
    };
}

static std::array<std::array<span_t, NUM_ROMV>, NUM_RTRAM> _rtram_spans = {};
static std::array<std::array<span_t, NUM_ROMV>, NUM_RTROM> _rtrom_spans = {};
static std::array<rom_data_ht, NUM_RTROM> _rtrom_data = {};

std::array<std::array<span_t, NUM_ROMV>, NUM_RTRAM> const& rtram_spans()
    { return _rtram_spans; }
std::array<std::array<span_t, NUM_ROMV>, NUM_RTROM> const& rtrom_spans()
    { return _rtrom_spans; }

span_t runtime_span(runtime_ram_name_t name, romv_t romv) 
    { return _rtram_spans[name][romv]; }
span_t runtime_span(runtime_rom_name_t name, romv_t romv)
    { return _rtrom_spans[name][romv]; }

ram_bitset_t alloc_runtime_ram()
{
    runtime_ram_allocator_t a;
    a.allocated = stack_bitset; // Don't allocate in stack space.

    unsigned const temp_size = (mapper().bus_conflicts && state_size()) ? 3 : 2;
    _rtram_spans[RTRAM_ptr_temp] = {{ a.alloc_zp(temp_size), a.alloc_zp(temp_size), a.alloc_zp(temp_size) }};
    if(global_t::has_nmi())
    {
        _rtram_spans[RTRAM_nmi_index]       = {{ a.alloc_zp(1) }};
        _rtram_spans[RTRAM_nmi_saved_x]     = {{ a.alloc_zp(1) }};
        _rtram_spans[RTRAM_nmi_saved_y]     = {{ a.alloc_zp(1) }};
    }
    _rtram_spans[RTRAM_nmi_counter]     = {{ a.alloc_zp(1) }};
    _rtram_spans[RTRAM_nmi_ready]       = {{ a.alloc_zp(1) }};

    if(global_t::has_irq() && !fn_t::solo_irq())
    {
        _rtram_spans[RTRAM_irq_index]       = {{ a.alloc_zp(1) }};
        _rtram_spans[RTRAM_irq_saved_x]     = {{ a.alloc_zp(1) }};
        _rtram_spans[RTRAM_irq_saved_y]     = {{ a.alloc_zp(1) }};
    }
    _rtram_spans[RTRAM_mapper_state] = {{ a.alloc_zp(state_size()) }};
    _rtram_spans[RTRAM_mapper_detail] = {{ a.alloc_zp(detail_size()) }};

    // Allocate optional stuff last, for a consistent memory layout.
    if(mapper().bankswitches())
    {
        _rtram_spans[RTRAM_nmi_saved_bank] = {{ a.alloc_zp(1) }};
        if(!fn_t::solo_irq())
            _rtram_spans[RTRAM_irq_saved_bank] = {{ a.alloc_zp(1) }};
    }

    if(compiler_options().nes_system == NES_SYSTEM_DETECT)
        _rtram_spans[RTRAM_system] = {{ a.alloc_zp(1) }};

    return a.allocated;
}

static void _push_addr(loc_vec_t& vec, locator_t addr)
{
    assert(addr.is() == IS_DEREF);
    vec.push_back(addr.with_is(IS_PTR));
    vec.push_back(addr.with_is(IS_PTR_HI));
}

static loc_vec_t make_vectors()
{
    loc_vec_t ret;
    _push_addr(ret, locator_t::runtime_rom(RTROM_nmi));
    _push_addr(ret, locator_t::runtime_rom(RTROM_reset));
    if(fn_t const* irq = fn_t::solo_irq())
        _push_addr(ret, locator_t::fn(irq->handle()));
    else
        _push_addr(ret, locator_t::runtime_rom(RTROM_irq));
    return ret;
}

unsigned bankswitch_a(asm_proc_t& proc, unsigned next_label, bool x)
{
    if(!mapper().bankswitches())
        return next_label;

    std::uint16_t const addr = bankswitch_addr(mapper().type);

    if(mapper().type == MAPPER_MMC1)
    {
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            for(unsigned i = 0; i < 4; ++i)
            {
                proc.push_inst(LSR_IMPLIED);
                proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            }
        }
        else
        {
            if(!x)
                proc.push_inst(TAX_IMPLIED);
            proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            locator_t const retry = proc.push_label(next_label++);
            proc.push_inst(TXA_IMPLIED);
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            for(unsigned i = 0; i < 4; ++i)
            {
                proc.push_inst(LSR_IMPLIED);
                proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            }
            proc.push_inst(CPY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            locator_t const done = proc.make_label(next_label++);
            proc.push_inst(BEQ_RELATIVE, done);
            proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_mapper_reset));
            proc.push_inst(JMP_ABSOLUTE, retry);
            proc.push_inst(ASM_LABEL, done);
        }
    }
    else if(mapper().type == MAPPER_MMC3)
    {
        locator_t retry;

        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            retry = proc.push_label(next_label++);
        }
        proc.push_inst(LDX_IMMEDIATE, locator_t::const_byte(0b111110));
        proc.push_inst(STX_ABSOLUTE, locator_t::addr(0x8000));
        proc.push_inst(SAX_ABSOLUTE, locator_t::addr(0x8001));
        proc.push_inst(INX_IMPLIED);
        proc.push_inst(STX_ABSOLUTE, locator_t::addr(0x8000));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x8001));
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(CPY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }
    else if(mapper().bus_conflicts)
    {
        if(state_size())
        {
            if(compiler_options().unsafe_bank_switch)
            {
                proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(TAX_IMPLIED);
                proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
            }
            else
            {
                proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 2));
                locator_t const retry = proc.push_label(next_label++);
                proc.push_inst(LAX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 2));
                proc.push_inst(TAY_IMPLIED);
                proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
                proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(BNE_RELATIVE, retry);
            }
        }
        else
        {
            if(!x)
                proc.push_inst(TAX_IMPLIED);
            proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
        }
    }
    else if(state_size())
    {
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
        }
        else
        {
            locator_t const retry = proc.push_label(next_label++);
            if(!x)
                proc.push_inst(TAX_IMPLIED);
            proc.push_inst(LDA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(TAY_IMPLIED);
            proc.push_inst(ORA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            proc.push_inst(CPY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }
    else
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(addr));

    return next_label;
}

unsigned bankswitch_x(asm_proc_t& proc, unsigned next_label)
{
    if(!mapper().bankswitches())
        return next_label;

    std::uint16_t const addr = bankswitch_addr();

    if(mapper().type == MAPPER_MMC1)
    {
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(TXA_IMPLIED);
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            for(unsigned i = 0; i < 4; ++i)
            {
                proc.push_inst(LSR_IMPLIED);
                proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            }
        }
        else
        {
            proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            locator_t const retry = proc.push_label(next_label++);
            proc.push_inst(TXA_IMPLIED);
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            for(unsigned i = 0; i < 4; ++i)
            {
                proc.push_inst(LSR_IMPLIED);
                proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            }
            proc.push_inst(CPY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            locator_t const done = proc.make_label(next_label++);
            proc.push_inst(BEQ_RELATIVE, done);
            proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_mapper_reset));
            proc.push_inst(JMP_ABSOLUTE, retry);
            proc.push_inst(ASM_LABEL, done);
        }
    }
    else if(mapper().type == MAPPER_MMC3)
    {
        locator_t retry;

        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            retry = proc.push_label(next_label++);
        }
        proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0b111110));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x8000));
        proc.push_inst(SAX_ABSOLUTE, locator_t::addr(0x8001));
        proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0b111111));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x8000));
        proc.push_inst(STX_ABSOLUTE, locator_t::addr(0x8001));
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(CPY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }
    else if(mapper().bus_conflicts)
    {
        if(state_size())
        {
            if(compiler_options().unsafe_bank_switch)
            {
                proc.push_inst(TXA_IMPLIED);
                proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(TAX_IMPLIED);
                proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
            }
            else
            {
                proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 2));
                locator_t const retry = proc.push_label(next_label++);
                proc.push_inst(LAX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 2));
                proc.push_inst(TAY_IMPLIED);
                proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
                proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(BNE_RELATIVE, retry);
            }
        }
        else
        {
            proc.push_inst(TXA_IMPLIED);
            proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
        }
    }
    else if(state_size())
    {
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(TXA_IMPLIED);
            proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
        }
        else
        {
            locator_t const retry = proc.push_label(next_label++);
            proc.push_inst(LDA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(TAY_IMPLIED);
            proc.push_inst(ORA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            proc.push_inst(CPY_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }
    else
        proc.push_inst(STX_ABSOLUTE, locator_t::addr(addr));

    return next_label;
}

unsigned bankswitch_y(asm_proc_t& proc, unsigned next_label)
{
    if(!mapper().bankswitches())
        return next_label;

    std::uint16_t const addr = bankswitch_addr();

    if(mapper().type == MAPPER_MMC1)
    {
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(TYA_IMPLIED);
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            for(unsigned i = 0; i < 4; ++i)
            {
                proc.push_inst(LSR_IMPLIED);
                proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            }
        }
        else
        {
            proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            locator_t const retry = proc.push_label(next_label++);
            proc.push_inst(TYA_IMPLIED);
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            for(unsigned i = 0; i < 4; ++i)
            {
                proc.push_inst(LSR_IMPLIED);
                proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            }
            proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            locator_t const done = proc.make_label(next_label++);
            proc.push_inst(BEQ_RELATIVE, done);
            proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_mapper_reset));
            proc.push_inst(JMP_ABSOLUTE, retry);
            proc.push_inst(ASM_LABEL, done);
        }
    }
    else if(mapper().type == MAPPER_MMC3)
    {
        locator_t retry;

        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            retry = proc.push_label(next_label++);
        }
        proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0b111111));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x8000));
        proc.push_inst(STY_ABSOLUTE, locator_t::addr(0x8001));
        proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0b111110));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x8000));
        proc.push_inst(DEY_IMPLIED);
        proc.push_inst(STY_ABSOLUTE, locator_t::addr(0x8001));
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }
    else if(mapper().bus_conflicts)
    {
        if(state_size())
        {
            if(compiler_options().unsafe_bank_switch)
            {
                proc.push_inst(TYA_IMPLIED);
                proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(TAY_IMPLIED);
                proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
            }
            else
            {
                proc.push_inst(STY_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 2));
                locator_t const retry = proc.push_label(next_label++);
                proc.push_inst(LAX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 2));
                proc.push_inst(TAY_IMPLIED);
                proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
                proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
                proc.push_inst(BNE_RELATIVE, retry);
            }
        }
        else
        {
            proc.push_inst(TYA_IMPLIED);
            proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
        }
    }
    else if(state_size())
    {
        if(compiler_options().unsafe_bank_switch)
        {
            proc.push_inst(TYA_IMPLIED);
            proc.push_inst(ORA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
        }
        else
        {
            locator_t const retry = proc.push_label(next_label++);
            proc.push_inst(LAX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(ORA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }
    else
        proc.push_inst(STY_ABSOLUTE, locator_t::addr(addr));

    return next_label;
}

// Clobbers A, not X or Y.
static asm_proc_t make_mmc1_mapper_reset()
{
    asm_proc_t proc;

    proc.push_label(0);
    proc.push_inst(INC_ABSOLUTE, locator_t::minor_label(0)); // Reset mapper
    proc.push_inst(LDA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x8000));
    for(unsigned i = 0; i < 4; ++i)
    {
        proc.push_inst(LSR_IMPLIED);
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x8000));
    }
    proc.push_inst(RTS_IMPLIED);

    proc.initial_optimize();
    return proc;
}

static void lda_this_bank(asm_proc_t& proc)
{
    if(mapper().this_bank_addr())
        proc.push_inst(LDA_ABSOLUTE, locator_t::this_bank());
    else
        proc.push_inst(LDA_IMMEDIATE, locator_t::this_bank());
}

static asm_proc_t make_irq()
{
    asm_proc_t proc;

    unsigned next_label = 0;

    // Store registers:
    proc.push_inst(PHA);
    proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_saved_x));
    proc.push_inst(STY_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_saved_y));

    if(mapper().type == MAPPER_MMC1)
    {
        proc.push_inst(INC_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
        if(!compiler_options().unsafe_bank_switch)
        {
            locator_t const retry = proc.push_label(next_label++);
            proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_mapper_reset));
            proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }

    proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_index));
    proc.push_inst(LDA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_irq_lo_table));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(LDA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_irq_hi_table));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));

    if(mapper().bankswitches())
    {
        // Save current bank
        lda_this_bank(proc);
        proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_saved_bank));

        proc.push_inst(LAX_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_irq_bank_table));
        next_label = bankswitch_a(proc, next_label, true);
    }

    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_irq_exit()
{
    asm_proc_t proc;

    if(mapper().type == MAPPER_MMC3 && !compiler_options().unsafe_bank_switch)
        proc.push_inst(INC_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));

    if(mapper().bankswitches())
    {
        proc.push_inst(LAX_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_saved_bank));
        bankswitch_a(proc, 0, true);
    }
    proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_saved_x));
    proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_saved_y));
    proc.push_inst(PLA);
    proc.push_inst(RTI);

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_short_irq()
{
    asm_proc_t proc;
    proc.push_inst(RTI);
    return proc;
}

static asm_proc_t make_nmi()
{
    asm_proc_t proc;

    unsigned next_label = 0;

    // Store registers:
    proc.push_inst(PHA);
    proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_x));
    proc.push_inst(STY_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_y));

    if(mapper().type == MAPPER_MMC1)
    {
        proc.push_inst(INC_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
        if(!compiler_options().unsafe_bank_switch)
        {
            locator_t const retry = proc.push_label(next_label++);
            proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_mapper_reset));
            proc.push_inst(CPX_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));
            proc.push_inst(BNE_RELATIVE, retry);
        }
    }

    proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_index));
    proc.push_inst(LDA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_nmi_lo_table));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(LDA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_nmi_hi_table));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));

    if(mapper().bankswitches())
    {
        // Save current bank
        lda_this_bank(proc);
        proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_bank));

        proc.push_inst(LAX_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_nmi_bank_table));
        next_label = bankswitch_a(proc, next_label, true);
    }

    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_nmi_exit()
{
    asm_proc_t proc;

    if(mapper().type == MAPPER_MMC3 && !compiler_options().unsafe_bank_switch)
        proc.push_inst(INC_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_detail));

    if(mapper().bankswitches())
    {
        proc.push_inst(LAX_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_bank));
        bankswitch_a(proc, 0, true);
    }
    proc.push_inst(INC_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_counter));
    proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_x));
    proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_y));
    proc.push_inst(PLA);
    proc.push_inst(RTI);

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_short_nmi()
{
    asm_proc_t proc;
    proc.push_inst(INC_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_counter));
    proc.push_inst(RTI);
    return proc;
}

static asm_proc_t make_wait_nmi()
{
    ///////////////////////////////////////////////////
    // !! THIS FUNCTION SHOULD NOT CLOBBER X or Y !! //
    ///////////////////////////////////////////////////

    asm_proc_t proc;

    proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(1));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_ready));
    proc.push_inst(LDA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_counter));
    locator_t const label = proc.push_label(0);
    proc.push_inst(CMP_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_counter));
    proc.push_inst(BEQ_RELATIVE, label);
    proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_ready));
    proc.push_inst(RTS);

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_reset()
{
    asm_proc_t proc;

    // Turn off decimal mode, just in case the code gets run on wonky hardware.
    proc.push_inst(CLD);

    if(state_size())
    {
        proc.push_inst(LDA, 0);
        proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
        switch(mapper().type)
        {
        default:
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(bankswitch_addr()));
            break;
        case MAPPER_MMC1:
            proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_mapper_reset));
            break;
        }
    }

    // Jump to the init proc:
    proc.push_inst(LDY_IMMEDIATE, locator_t(LOC_RESET_PROC).with_is(IS_BANK));
    bankswitch_y(proc, 0);
    proc.push_inst(JMP_ABSOLUTE, LOC_RESET_PROC);

    proc.initial_optimize();

    return proc;
}

static asm_proc_t make_reset_proc()
{
    unsigned next_label = 0;
    asm_proc_t proc;

    // Ignore IRQs:
    proc.push_inst(SEI);
    
    // Disable NMI and rendering:
    proc.push_inst(LDA, 0);
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(PPUCTRL));
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(PPUMASK));

    // Reset runtime vars:
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_ready));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_counter));

    // Disable DMC IRQ
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x4010));

    // Read the status registers to handle stray NMIs and DMC IRQs across resets
    proc.push_inst(LDA_ABSOLUTE, locator_t::addr(PPUSTATUS));
    proc.push_inst(LDA_ABSOLUTE, locator_t::addr(SNDCHN));

    // Disable APU frame counter IRQ.
    // (See http://wiki.nesdev.com/w/index.php/APU_Frame_Counter )
    proc.push_inst(LDA, 0b01000000);
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x4017));

    // Disable DMC but initialize the other channels.
    proc.push_inst(LDA, 0x0F);
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(SNDCHN));

    // Set the stack pointer.
    proc.push_inst(LDX, 0xFF);
    proc.push_inst(TXS);

    // Now wait two frames until the PPU stabilizes.
    // Can't use NMI yet, so we'll spin on bit 7 of PPUSTATUS to determine
    // when those frames pass.
    locator_t const wait_frame_1 = proc.push_label(next_label++);
    proc.push_inst(BIT_ABSOLUTE, locator_t::addr(PPUSTATUS));
    proc.push_inst(BPL_RELATIVE, wait_frame_1);

    {
        if(compiler_options().ram_init)
        {
            proc.push_inst(LDA, 0);
            proc.push_inst(TAX);
            locator_t const ram_loop = proc.push_label(next_label++);
            for(unsigned i = 0; i < 8; ++i)
                proc.push_inst(STA_ABSOLUTE_X, locator_t::addr(i * 256));
            proc.push_inst(INX);
            proc.push_inst(BNE_RELATIVE, ram_loop);
        }

        if(compiler_options().sram_init)
        {
            if(!mapper().sram)
                compiler_warning("Zero-initializing SRAM, but no SRAM is included on this mapper.");

            if(!compiler_options().ram_init)
            {
                proc.push_inst(LDA, 0);
                proc.push_inst(TAX);
            }
            locator_t const sram_loop = proc.push_label(next_label++);
            for(unsigned i = 0; i < 32; ++i)
                proc.push_inst(STA_ABSOLUTE_X, locator_t::addr(0x6000 + i*256));
            proc.push_inst(INX);
            proc.push_inst(BNE_RELATIVE, sram_loop);
        }

        if(compiler_options().vram_init)
        {
            if(!compiler_options().ram_init && !compiler_options().sram_init)
            {
                proc.push_inst(LDA, 0);
                proc.push_inst(TAX);
            }
            proc.push_inst(STX_ABSOLUTE, locator_t::addr(PPUADDR));
            proc.push_inst(STX_ABSOLUTE, locator_t::addr(PPUADDR));
            proc.push_inst(LDY, 48);
            locator_t const vram_outer_loop = proc.push_label(next_label++);
            locator_t const vram_inner_loop = proc.push_label(next_label++);
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(PPUDATA));
            proc.push_inst(INX);
            proc.push_inst(BNE_RELATIVE, vram_inner_loop);
            proc.push_inst(DEY);
            proc.push_inst(BNE_RELATIVE, vram_outer_loop);

            // Palette
            proc.push_inst(LDX, 0x3F);
            proc.push_inst(STX_ABSOLUTE, locator_t::addr(PPUADDR));
            proc.push_inst(LDX, 0x00);
            proc.push_inst(STX_ABSOLUTE, locator_t::addr(PPUADDR));
            proc.push_inst(LDX, 32);
            locator_t const vram_palette_loop = proc.push_label(next_label++);
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(PPUDATA));
            proc.push_inst(DEX);
            proc.push_inst(BNE_RELATIVE, vram_palette_loop);
        }
    }

    // Wait for the second frame.
    locator_t const wait_frame_2 = proc.push_label(next_label++);
    proc.push_inst(BIT_ABSOLUTE, locator_t::addr(PPUSTATUS));
    proc.push_inst(BPL_RELATIVE, wait_frame_2);

    // Reset mode state:
    fn_t const& main = get_main_mode();
    main.mode_group_vars().for_each([&](group_vars_ht gv)
    {
        if((*gv)->vars()->has_init())
        {
            if(mapper().bankswitches())
            {
                if(bankswitch_use_x())
                {
                    proc.push_inst(LDX_IMMEDIATE, locator_t::reset_group_vars(gv).with_is(IS_BANK));
                    proc.push_inst(BANKED_X_JSR, locator_t::reset_group_vars(gv));
                }
                else
                {
                    proc.push_inst(LDY_IMMEDIATE, locator_t::reset_group_vars(gv).with_is(IS_BANK));
                    proc.push_inst(BANKED_Y_JSR, locator_t::reset_group_vars(gv));
                }
            }
            else
                proc.push_inst(JSR_ABSOLUTE, locator_t::reset_group_vars(gv));
        }
    });
    

    if(compiler_options().nes_system == NES_SYSTEM_DETECT)
    {
        // Use the default NMI handler:
        proc.push_inst(LDX_IMMEDIATE, locator_t::const_byte(0));
        proc.push_inst(LDY_IMMEDIATE, locator_t::const_byte(0));
        if(global_t::has_nmi())
            proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_index));

        proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0x80));
        proc.push_inst(BIT_ABSOLUTE, locator_t::addr(PPUSTATUS));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(PPUCTRL));

        proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_wait_nmi));
        proc.push_inst(LDA_ZERO_PAGE, locator_t::runtime_ram(RTRAM_nmi_counter));

        locator_t const wait = proc.push_label(next_label++);
        // Each iteration takes 11 cycles.
        // NTSC NES: 29780 cycles or 2707 = $A93 iterations
        // PAL NES:  33247 cycles or 3022 = $BCE iterations
        // Dendy:    35464 cycles or 3224 = $C98 iterations
        // so we can divide by $100 (rounding down), subtract ten,
        // and end up with 0=ntsc, 1=pal, 2=dendy, 3=unknown
        proc.push_inst(INX_IMPLIED);
        proc.push_inst(BNE_RELATIVE, locator_t::const_byte(1));
        proc.push_inst(INY_IMPLIED);
        proc.push_inst(CMP_ZERO_PAGE, locator_t::runtime_ram(RTRAM_nmi_counter));
        proc.push_inst(BEQ_RELATIVE, wait);
        proc.push_inst(TYA_IMPLIED);
        proc.push_inst(SEC_IMPLIED);
        proc.push_inst(SBC_IMMEDIATE, locator_t::const_byte(10));
        proc.push_inst(CMP_IMMEDIATE, locator_t::const_byte(3));
        proc.push_inst(BCC_RELATIVE, locator_t::const_byte(2));
        proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(3));
        proc.push_inst(STA_ZERO_PAGE, locator_t::runtime_ram(RTRAM_system));

        proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0));
        proc.push_inst(BIT_ABSOLUTE, locator_t::addr(PPUSTATUS));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(PPUCTRL));
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(PPUMASK));
    }

    // Init the NMI index
    if(global_t::has_nmi())
    {
        proc.push_inst(LDA_IMMEDIATE, locator_t::nmi_index(main.mode_nmi()));
        proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_index));
    }

    // Init the IRQ index
    if(global_t::has_irq() && !fn_t::solo_irq())
    {
        proc.push_inst(LDA_IMMEDIATE, locator_t::irq_index(main.mode_irq()));
        proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_irq_index));
    }

    // Init vars
    gen_group_var_inits(gvar_t::groupless_gvars(), proc);

    // Jump to our entry point.
    if(mapper().bankswitches())
    {
        if(bankswitch_use_x())
        {
            proc.push_inst(LDX_IMMEDIATE, locator_t::fn(main.handle()).with_is(IS_BANK));
            proc.push_inst(BANKED_X_JMP, locator_t::fn(main.handle()));
        }
        else
        {
            proc.push_inst(LDY_IMMEDIATE, locator_t::fn(main.handle()).with_is(IS_BANK));
            proc.push_inst(BANKED_Y_JMP, locator_t::fn(main.handle()));
        }
    }
    else
        proc.push_inst(JMP_ABSOLUTE, locator_t::fn(main.handle()));

    proc.initial_optimize();
    return proc;
}

void create_reset_proc()
{
    reset_proc = rom_proc_ht::pool_make(romv_allocs_t{}, ROMVF_IN_MODE, false);
}

void set_reset_proc()
{
    reset_proc.safe().assign(make_reset_proc());
    reset_proc.safe().mark_aligned();
}

static asm_proc_t make_jsr_xy_trampoline()
{
    bool const xy = bankswitch_use_x();
    asm_proc_t proc;

    unsigned next_label = 0;

    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(xy ? STY_ABSOLUTE : STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));
    lda_this_bank(proc);
    proc.push_inst(PHA);
    next_label = xy ? bankswitch_x(proc, next_label) : bankswitch_y(proc, next_label);
    proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_jmp_indirect));
    proc.push_inst(PLA);
    next_label = bankswitch_a(proc, next_label);
    proc.push_inst(RTS);

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_jmp_xy_trampoline()
{
    bool const xy = bankswitch_use_x();
    asm_proc_t proc;

    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(xy ? STY_ABSOLUTE : STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));
    xy ? bankswitch_x(proc, 0) : bankswitch_y(proc, 0);
    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_jsr_trampoline()
{
    asm_proc_t proc;

    unsigned next_label = 0;

    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));
    lda_this_bank(proc);
    proc.push_inst(PHA);
    proc.push_inst(JSR_ABSOLUTE, locator_t::runtime_rom(RTROM_jmp_indirect));
    proc.push_inst(PLA);
    next_label = bankswitch_a(proc, next_label);
    proc.push_inst(RTS);

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_jmp_trampoline()
{
    asm_proc_t proc;

    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));
    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_jmp_indirect()
{
    asm_proc_t proc;
    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));
    return proc;
}

// From https://www.nesdev.org/wiki/8-bit_Multiply
// @param A one factor
// @param Y another factor
// @return low 8 bits in A; high 8 bits in Y
asm_proc_t make_mul8()
{
    asm_proc_t proc;

    unsigned next_label_id = 0;

    locator_t const early_return = proc.make_label(++next_label_id);
    locator_t const prodlo = locator_t::runtime_ram(RTRAM_ptr_temp, 0);
    locator_t const factor2 = locator_t::runtime_ram(RTRAM_ptr_temp, 1);

    proc.push_inst(LSR_IMPLIED);
    proc.push_inst(STA_ABSOLUTE, prodlo);
    proc.push_inst(TYA_IMPLIED);
    proc.push_inst(BEQ_RELATIVE, early_return);
    proc.push_inst(DEY_IMPLIED);
    proc.push_inst(STY_ZERO_PAGE, factor2);
    proc.push_inst(LDA_IMMEDIATE, locator_t::const_byte(0));

    for(unsigned i = 0; i < 8; ++i)
    {
        locator_t const label = proc.make_label(++next_label_id);
        if(i != 0)
            proc.push_inst(ROR_ZERO_PAGE, prodlo);
        proc.push_inst(BCC_RELATIVE, label);
        proc.push_inst(ADC_ZERO_PAGE, factor2);
        proc.push_inst(ASM_LABEL, label);
        proc.push_inst(ROR_IMPLIED);
    }

    proc.push_inst(TAY_IMPLIED);
    proc.push_inst(LDA_ZERO_PAGE, prodlo);
    proc.push_inst(ROR_IMPLIED);
    proc.push_inst(ASM_LABEL, early_return);
    proc.push_inst(RTS_IMPLIED);

    proc.initial_optimize();
    return proc;
}

static loc_vec_t make_iota()
{
    loc_vec_t ret;
    ret.reserve(256);
    for(unsigned i = 0; i < 256; ++i)
        ret.push_back(locator_t::const_byte(i));
    return ret;
}

static loc_vec_t make_shl_table(unsigned amount)
{
    assert(amount < 8);
    loc_vec_t ret;
    ret.resize(1 << (8 - amount));
    for(unsigned i = 0; i < ret.size(); ++i)
        ret[i] = locator_t::const_byte(i << amount);
    return ret;
}

namespace 
{
    struct interrupt_tables_t
    {
        loc_vec_t lo;
        loc_vec_t hi;
        loc_vec_t bank;
        unsigned alignment = 0;
    };
}

static interrupt_tables_t make_nmi_tables()
{
    interrupt_tables_t t;

    t.lo.reserve(global_t::nmis().size() + 1);
    t.hi.reserve(global_t::nmis().size() + 1);
    t.bank.reserve(global_t::nmis().size() + 1);

    // Zeroth NMI does nothing:
    t.lo.push_back(locator_t::runtime_rom(RTROM_nmi_exit).with_is(IS_PTR));
    t.hi.push_back(locator_t::runtime_rom(RTROM_nmi_exit).with_is(IS_PTR_HI));
    t.bank.push_back(locator_t::runtime_rom(RTROM_nmi_exit).with_is(IS_BANK));

    for(fn_t const* nmi : global_t::nmis())
    {
        t.lo.push_back(locator_t::fn(nmi->handle()).with_is(IS_PTR));
        t.hi.push_back(locator_t::fn(nmi->handle()).with_is(IS_PTR_HI));
        t.bank.push_back(locator_t::fn(nmi->handle()).with_is(IS_BANK));
    }

    t.alignment = next_pow2(t.lo.size());

    return t;
}

static interrupt_tables_t make_irq_tables()
{
    interrupt_tables_t t;

    t.lo.reserve(global_t::irqs().size() + 1);
    t.hi.reserve(global_t::irqs().size() + 1);
    t.bank.reserve(global_t::irqs().size() + 1);

    // Zeroth irq does nothing:
    t.lo.push_back(locator_t::runtime_rom(RTROM_irq_exit).with_is(IS_PTR));
    t.hi.push_back(locator_t::runtime_rom(RTROM_irq_exit).with_is(IS_PTR_HI));
    t.bank.push_back(locator_t::runtime_rom(RTROM_irq_exit).with_is(IS_BANK));

    for(fn_t const* irq : global_t::irqs())
    {
        t.lo.push_back(locator_t::fn(irq->handle()).with_is(IS_PTR));
        t.hi.push_back(locator_t::fn(irq->handle()).with_is(IS_PTR_HI));
        t.bank.push_back(locator_t::fn(irq->handle()).with_is(IS_BANK));
    }

    t.alignment = next_pow2(t.lo.size());

    return t;
}

span_allocator_t alloc_runtime_rom()
{
    span_allocator_t a(mapper().fixed_rom_span());

    auto const alloc = [&](runtime_rom_name_t name, auto&& data, romv_flags_t flags = ROMVF_IN_MODE, 
                           std::uint16_t alignment=1, std::uint16_t after=0)
    {
        std::size_t const max_size = data.size();

        bitset_for_each(flags, [&](unsigned romv)
        {
            if(!_rtrom_spans[name][romv])
                _rtrom_spans[name][romv] = a.alloc_linear(max_size, alignment, after).object;
            if(!_rtrom_spans[name][romv])
                throw std::runtime_error(fmt("Unable to allocate runtime library %.", name));
        });

        romv_allocs_t allocs = {};
        bitset_for_each(flags, [&](unsigned romv)
        {
            allocs[romv] = rom_static_ht::pool_make(romv_t(romv), _rtrom_spans[name][romv]);
            assert(allocs[romv].get()->romv == romv_t(romv));
        });

        _rtrom_data[name] = to_rom_data(std::move(data), false, true, allocs, flags);

        bitset_for_each(flags, [&](unsigned romv)
        {
            allocs[romv].get()->data = _rtrom_data[name];
        });
    };

    // Pre-allocate.
    auto& iota = _rtrom_spans[RTROM_iota][0];
    iota = {};
    if(mapper().bus_conflicts)
        iota = a.alloc_at({ bankswitch_addr(), 256 }).object;
    if(!iota)
        iota = a.alloc(256, 256).object;
    _rtrom_spans[RTROM_vectors][0] = a.alloc_at({ 0xFFFA, 6 }).object;
    assert(_rtrom_spans[RTROM_vectors][0]);

    if(compiler_options().action53)
    {
        if(mapper().prg_size() > 0x10000)
            compiler_warning("--action53 is enabled with a PRG size above 64KiB. This may be incompatible.");

        switch(mapper().type)
        {
        case MAPPER_NROM:
        case MAPPER_CNROM:
            break;
        default:
            compiler_warning(fmt("--action53 is enabled with mapper %. This may be incompatible.", mapper_name(mapper().type)));
            // fall-through
        case MAPPER_ANROM:
        case MAPPER_BNROM:
        case MAPPER_UNROM:
            span_t span = a.alloc_at({ 0xFFD0, 0x2A }).object;
            assert(span);
            break;
        }
    }

    // These have to be defined in a toposorted order.
    alloc(RTROM_iota, make_iota());
    alloc(RTROM_shl4_table, make_shl_table(4));
    alloc(RTROM_shl5_table, make_shl_table(5));
    alloc(RTROM_shl6_table, make_shl_table(6));

    if(global_t::has_nmi())
    {
        alloc(RTROM_nmi, make_nmi(), ROMVF_IN_NMI);
        alloc(RTROM_nmi_exit, make_nmi_exit(), ROMVF_IN_NMI);
    }
    else
    {
        alloc(RTROM_nmi, make_short_nmi(), ROMVF_IN_NMI);
        _rtrom_spans[RTROM_nmi_exit][ROMV_NMI] = _rtrom_spans[RTROM_nmi][ROMV_NMI];
    }

    if(global_t::has_irq() && !fn_t::solo_irq())
    {
        alloc(RTROM_irq, make_irq(), ROMVF_IN_IRQ);
        alloc(RTROM_irq_exit, make_irq_exit(), ROMVF_IN_IRQ);
    }
    else
    {
        alloc(RTROM_irq, make_short_irq(), ROMVF_IN_IRQ);
        _rtrom_spans[RTROM_irq_exit][ROMV_IRQ] = _rtrom_spans[RTROM_irq][ROMV_IRQ];
    }

    alloc(RTROM_wait_nmi, make_wait_nmi());
    alloc(RTROM_reset, make_reset());
    alloc(RTROM_vectors, make_vectors());
    alloc(RTROM_jmp_indirect, make_jmp_indirect(), ROMVF_ALL);

    if(mapper().bankswitches())
    {
        alloc(RTROM_jsr_xy_trampoline, make_jsr_xy_trampoline(), ROMVF_ALL);
        alloc(RTROM_jmp_xy_trampoline, make_jmp_xy_trampoline(), ROMVF_ALL);
        alloc(RTROM_jsr_trampoline, make_jsr_trampoline(), ROMVF_ALL);
        alloc(RTROM_jmp_trampoline, make_jmp_trampoline(), ROMVF_ALL);
    }

    alloc(RTROM_mul8, make_mul8(), ROMVF_ALL);

    if(has_mapper_reset())
    {
        if(mapper().type == MAPPER_MMC1)
            alloc(RTROM_mapper_reset, make_mmc1_mapper_reset(), ROMVF_ALL, 1, 0xC000);
        else
            assert(false);
    }

    if(global_t::has_nmi())
    {
        auto nmi_tables = make_nmi_tables();
        alloc(RTROM_nmi_lo_table, std::move(nmi_tables.lo), ROMVF_IN_MODE, nmi_tables.alignment);
        alloc(RTROM_nmi_hi_table, std::move(nmi_tables.hi), ROMVF_IN_MODE, nmi_tables.alignment);
        alloc(RTROM_nmi_bank_table, std::move(nmi_tables.bank), ROMVF_IN_MODE, nmi_tables.alignment);
    }

    if(global_t::has_irq() && !fn_t::solo_irq())
    {
        auto irq_tables = make_irq_tables();
        alloc(RTROM_irq_lo_table, std::move(irq_tables.lo), ROMVF_IN_MODE, irq_tables.alignment);
        alloc(RTROM_irq_hi_table, std::move(irq_tables.hi), ROMVF_IN_MODE, irq_tables.alignment);
        alloc(RTROM_irq_bank_table, std::move(irq_tables.bank), ROMVF_IN_MODE, irq_tables.alignment);
    }

    assert(runtime_span(RTROM_nmi_exit, ROMV_NMI));
    assert(runtime_span(RTROM_irq_exit, ROMV_IRQ));

    return a;
}

rom_data_ht runtime_data(runtime_rom_name_t name)
{
    return _rtrom_data[name];
}

std::string to_string(runtime_ram_name_t name)
{
    switch(name)
    {
#define RT(name) case RTRAM_##name: return #name;
RTRAM_X
#undef RT
    default: return "bad rtram";
    }
}

std::string to_string(runtime_rom_name_t name)
{
    switch(name)
    {
#define RT(name) case RTROM_##name: return #name;
RTROM_X
#undef RT
    default: return "bad rtrom";
    }
}

std::ostream& operator<<(std::ostream& o, runtime_ram_name_t name)
{
    o << to_string(name);
    return o;
}

std::ostream& operator<<(std::ostream& o, runtime_rom_name_t name)
{
    o << to_string(name);
    return o;
}
