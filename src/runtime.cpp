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
            return mark_allocated(ret);
        }

        span_t alloc_non_zp(std::uint16_t size, unsigned alignment = 0)
        {
            return mark_allocated(m_non_zp.alloc(size, alignment));
        }

        span_t mark_allocated(span_t span)
        {
            allocated |= ram_bitset_t::filled(span.addr, span.size);
            return span;
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

    _rtram_spans[RTRAM_ptr_temp]        = {{ a.alloc_zp(2), a.alloc_zp(2) }};
    _rtram_spans[RTRAM_nmi_index]       = {{ a.alloc_zp(1) }};
    //_rtram_spans[RTRAM_nmi_call_ptr]    = {{ a.alloc_zp(2) }}; // TODO
    _rtram_spans[RTRAM_nmi_saved_x]     = {{ a.alloc_zp(1) }};
    _rtram_spans[RTRAM_nmi_saved_y]     = {{ a.alloc_zp(1) }};
    _rtram_spans[RTRAM_nmi_counter]     = {{ a.alloc_zp(1) }};
    _rtram_spans[RTRAM_nmi_ready]       = {{ a.alloc_zp(1) }};
    //_rtram_spans[RTRAM_buttons_held]    = {{ a.alloc_zp(2) }}; TODO
    //_rtram_spans[RTRAM_buttons_pressed] = {{ a.alloc_zp(2) }}; TODO
    _rtram_spans[RTRAM_mapper_state] = {{ a.alloc_zp(state_size(mapper().type)) }};

    //_rtram_spans[RTRAM_oam] = {{ a.alloc_non_zp(256, 256) }}; TODO

    // Allocate optional stuff last, for a consistent memory layout.
    if(mapper().bankswitches())
        _rtram_spans[RTRAM_nmi_saved_bank] = {{ a.alloc_zp(1) }};

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
    _push_addr(ret, locator_t::runtime_rom(RTROM_irq));
    return ret;
}

static void _load_bankswitch_ax(asm_proc_t& proc, locator_t load)
{
    if(!mapper().bankswitches())
        return;

    std::uint16_t const addr = bankswitch_addr(mapper().type);

    if(load)
    {
        if(load.is_immediate())
        {
            proc.push_inst(LDA_IMMEDIATE, load);
            if(has_bus_conflicts(mapper().type))
                proc.push_inst(TAX);
        }
        else
            proc.push_inst(LAX_ABSOLUTE, load);
    }

    if(has_bus_conflicts(mapper().type))
        proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
    else
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(addr));
}

void bankswitch_x(asm_proc_t& proc)
{
    if(!mapper().bankswitches())
        return;

    std::uint16_t const addr = bankswitch_addr(mapper().type);

    if(has_bus_conflicts(mapper().type))
    {
        proc.push_inst(TXA_IMPLIED);
        proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
    }
    else
        proc.push_inst(STX_ABSOLUTE, locator_t::addr(addr));
}

void bankswitch_y(asm_proc_t& proc)
{
    if(!mapper().bankswitches())
        return;

    std::uint16_t const addr = bankswitch_addr(mapper().type);

    if(has_bus_conflicts(mapper().type))
    {
        proc.push_inst(TYA_IMPLIED);
        proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
    }
    else
        proc.push_inst(STY_ABSOLUTE, locator_t::addr(addr));
}

void bankswitch_ax(asm_proc_t& proc)
{
    if(!mapper().bankswitches())
        return;

    std::uint16_t const addr = bankswitch_addr(mapper().type);

    if(has_bus_conflicts(mapper().type))
        proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
    else
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(addr));
}

void bankswitch_ay(asm_proc_t& proc)
{
    if(!mapper().bankswitches())
        return;

    std::uint16_t const addr = bankswitch_addr(mapper().type);

    if(has_bus_conflicts(mapper().type))
        proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
    else
        proc.push_inst(STA_ABSOLUTE, locator_t::addr(addr));
}

static asm_proc_t make_nmi()
{
    asm_proc_t proc;

    // Store registers:
    proc.push_inst(PHA);
    proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_x));
    proc.push_inst(STY_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_y));

    proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_index));
    proc.push_inst(LDA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_nmi_lo_table));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(LDA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_nmi_hi_table));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));

    if(mapper().bankswitches())
    {
        std::uint16_t const addr = bankswitch_addr(mapper().type);

        // Save current bank
        proc.push_inst(LDA_IMMEDIATE, locator_t::this_bank());
        proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_bank));

        proc.push_inst(LAX_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_nmi_bank_table));
        if(has_bus_conflicts(mapper().type))
            proc.push_inst(STA_ABSOLUTE_X, locator_t::runtime_rom(RTROM_iota));
        else
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(addr));
    }

    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_nmi_exit()
{
    asm_proc_t proc;

    _load_bankswitch_ax(proc, locator_t::runtime_ram(RTRAM_nmi_saved_bank));

    proc.push_inst(INC_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_counter));
    proc.push_inst(LDX_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_x));
    proc.push_inst(LDY_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_saved_y));
    proc.push_inst(PLA);
    proc.push_inst(RTI);

    proc.initial_optimize();
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

static asm_proc_t make_irq()
{
    asm_proc_t proc;
    proc.push_inst(RTI);
    return proc;
}

static asm_proc_t make_reset()
{
    asm_proc_t proc;

    // Turn off decimal mode, just in case the code gets run on wonky hardware.
    proc.push_inst(CLD);

    // Jump to the init proc:
    _load_bankswitch_ax(proc, locator_t(LOC_RESET_PROC).with_is(IS_BANK));
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
    if(state_size(mapper().type))
        proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_mapper_state));
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

    // Wait for the second frame.
    locator_t const wait_frame_2 = proc.push_label(next_label++);
    proc.push_inst(BIT_ABSOLUTE, locator_t::addr(PPUSTATUS));
    proc.push_inst(BPL_RELATIVE, wait_frame_2);

    // Reset mode state:
    fn_t const& main = get_main_mode();
    main.precheck_group_vars().for_each([&](group_vars_ht gv)
    {
        if(gv->has_init())
        {
            proc.push_inst(LDY_IMMEDIATE, locator_t::reset_group_vars(gv).with_is(IS_BANK));
            proc.push_inst(BANKED_Y_JSR, locator_t::reset_group_vars(gv));
        }
    });
    
    // Init the NMI index
    proc.push_inst(LDA_IMMEDIATE, locator_t::nmi_index(main.mode_nmi()));
    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_nmi_index));

    // Jump to our entry point.
    _load_bankswitch_ax(proc, locator_t::fn(main.handle()).with_is(IS_BANK));
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
}

asm_proc_t make_bnrom_jsr_y_trampoline()
{
    asm_proc_t proc;

    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));
    proc.push_inst(LDA_IMMEDIATE, locator_t::this_bank());
    proc.push_inst(PHA);
    proc.push_inst(TYA);
    proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
    proc.push_inst(JSR_ABSOLUTE, locator_t::minor_label(0));
    proc.push_inst(PLA);
    proc.push_inst(TAY);
    proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
    proc.push_inst(RTS);
    proc.push_label(0);
    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));

    proc.initial_optimize();
    return proc;
}

asm_proc_t make_bnrom_jmp_y_trampoline()
{
    asm_proc_t proc;

    proc.push_inst(STA_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 0));
    proc.push_inst(STX_ABSOLUTE, locator_t::runtime_ram(RTRAM_ptr_temp, 1));
    proc.push_inst(TYA);
    proc.push_inst(STA_ABSOLUTE_Y, locator_t::runtime_rom(RTROM_iota));
    proc.push_inst(JMP_INDIRECT, locator_t::runtime_ram(RTRAM_ptr_temp));

    proc.initial_optimize();
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

namespace 
{
    struct nmi_tables_t
    {
        loc_vec_t lo;
        loc_vec_t hi;
        loc_vec_t bank;
        unsigned alignment = 0;
    };
}

static nmi_tables_t make_nmi_tables()
{
    nmi_tables_t t;

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

span_allocator_t alloc_runtime_rom()
{
    span_allocator_t a(mapper().rom_span());

    auto const alloc = [&](runtime_rom_name_t name, auto&& data, romv_flags_t flags = ROMVF_IN_MODE, std::uint16_t alignment=1)
    {
        std::size_t const max_size = data.size();

        bitset_for_each(flags, [&](unsigned romv)
        {
            if(!_rtrom_spans[name][romv])
                _rtrom_spans[name][romv] = a.alloc(max_size, alignment);
            if(!_rtrom_spans[name][romv])
                throw std::runtime_error(fmt("Unable to allocate runtime library %.", name));
        });

        romv_allocs_t allocs = {};
        bitset_for_each(flags, [&](unsigned romv)
        {
            allocs[romv] = rom_static_ht::pool_make(romv_t(romv), _rtrom_spans[name][romv]);
        });

        _rtrom_data[name] = to_rom_data(std::move(data), false, allocs);

        bitset_for_each(flags, [&](unsigned romv)
        {
            allocs[romv].get()->data = _rtrom_data[name];
        });
    };

    // Pre-allocate.
    if(has_bus_conflicts(mapper().type))
        _rtrom_spans[RTROM_iota][0] = a.alloc_at({ bankswitch_addr(mapper().type), 256 });
    else
        _rtrom_spans[RTROM_iota][0] = a.alloc(256, 256);
    _rtrom_spans[RTROM_vectors][0] = a.alloc_at({ 0xFFFA, 6 });

    // These have to be defined in a toposorted order.
    alloc(RTROM_iota, make_iota());
    alloc(RTROM_nmi, make_nmi());
    alloc(RTROM_nmi_exit, make_nmi_exit());
    alloc(RTROM_wait_nmi, make_wait_nmi());
    alloc(RTROM_irq, make_irq());
    alloc(RTROM_reset, make_reset());
    alloc(RTROM_vectors, make_vectors());

    alloc(RTROM_jsr_y_trampoline, make_bnrom_jsr_y_trampoline(), ROMVF_ALL);
    alloc(RTROM_jmp_y_trampoline, make_bnrom_jmp_y_trampoline(), ROMVF_ALL);
    alloc(RTROM_mul8, make_mul8(), ROMVF_ALL);

    auto tables = make_nmi_tables();
    alloc(RTROM_nmi_lo_table, std::move(tables.lo), ROMVF_IN_MODE, tables.alignment);
    alloc(RTROM_nmi_hi_table, std::move(tables.hi), ROMVF_IN_MODE, tables.alignment);
    alloc(RTROM_nmi_bank_table, std::move(tables.bank), ROMVF_IN_MODE, tables.alignment);

    assert(runtime_span(RTROM_nmi_exit, {}));

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
