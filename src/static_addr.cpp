#include "static_addr.hpp"

#include <vector>

#include "options.hpp"
#include "locator.hpp"
#include "asm_proc.hpp"
#include "rom_array.hpp"

namespace // anonymous
{
    constexpr locator_t PPUCTRL   = locator_t::addr(0x2000);
    constexpr locator_t PPUMASK   = locator_t::addr(0x2001);
    constexpr locator_t PPUSTATUS = locator_t::addr(0x2002);
    constexpr locator_t OAMADDR   = locator_t::addr(0x2003);
    constexpr locator_t OAMDATA   = locator_t::addr(0x2004);
    constexpr locator_t PPUSCROLL = locator_t::addr(0x2005);
    constexpr locator_t PPUADDR   = locator_t::addr(0x2006);
    constexpr locator_t PPUDATA   = locator_t::addr(0x2007);
    constexpr locator_t OAMDMA    = locator_t::addr(0x4014);
    constexpr locator_t SNDCHN    = locator_t::addr(0x4015);

    class static_ram_allocator_t
    {
    public:
        static_ram_allocator_t()
        : m_non_zp(span_t{ 0x200, 0x600 })
        {}

        span_t alloc_zp(std::uint16_t size)
        {
            if(m_next_zp + size >= 256)
                return {};
            span_t const ret = { .addr = m_next_zp, .size = size };
            m_next_zp += size;
            return ret;
        }

        span_t alloc_non_zp(std::uint16_t size, unsigned alignment = 0)
        {
            return m_non_zp.alloc(size, alignment);
        }

    private:
        span_allocator_t m_non_zp;
        unsigned m_next_zp = 0;
    };
}

static std::array<span_t, NUM_SRAM> _sram_spans = {};
static std::array<span_t, NUM_SROM> _srom_spans = {};
static std::array<rom_data_ht, NUM_SROM> _srom_data = {};

std::array<span_t, NUM_SRAM> const& sram_spans() { return _sram_spans; }
std::array<span_t, NUM_SROM> const& srom_spans() { return _srom_spans; }

span_t static_span(static_ram_name_t name) { return _sram_spans[name]; }
span_t static_span(static_rom_name_t name) { return _srom_spans[name]; }

locator_t static_locator(static_ram_name_t name, std::uint16_t offset)
{
    assert(static_span(name));
    return locator_t::addr(static_span(name).addr, offset);
}

locator_t static_locator(static_rom_name_t name, std::uint16_t offset)
{
    assert(static_span(name));
    return locator_t::addr(static_span(name).addr, offset);
}

ram_bitset_t alloc_static_ram()
{
    static_ram_allocator_t a;

    _sram_spans[SRAM_ptr_temp]        = a.alloc_zp(2);
    _sram_spans[SRAM_nmi_call_ptr]    = a.alloc_zp(2);
    if(bankswitch_addr(mapper().type))
    {
        _sram_spans[SRAM_nmi_call_bank] = a.alloc_zp(1);
        _sram_spans[SRAM_nmi_saved_bank] = a.alloc_zp(1);
    }
    _sram_spans[SRAM_nmi_saved_x]     = a.alloc_zp(1);
    _sram_spans[SRAM_nmi_saved_y]     = a.alloc_zp(1);
    _sram_spans[SRAM_nmi_counter]     = a.alloc_zp(1);
    _sram_spans[SRAM_buttons_held]    = a.alloc_zp(2);
    _sram_spans[SRAM_buttons_pressed] = a.alloc_zp(2);
    _sram_spans[SRAM_mapper_state] = a.alloc_zp(state_size(mapper().type));

    _sram_spans[SRAM_oam] = a.alloc_non_zp(256, 256);

    ram_bitset_t ret = stack_bitset;
    for(span_t span : _sram_spans)
        ret |= ram_bitset_t::filled(span.size, span.addr);

    return ret;
}

static void _push_addr(rom_array_t& rom_array, locator_t addr)
{
    assert(!addr.high());
    rom_array.data.push_back(addr.with_high(false));
    rom_array.data.push_back(addr.with_high(true));
}

static rom_array_t make_vectors()
{
    rom_array_t ret;
    _push_addr(ret, static_locator(SROM_nmi));
    _push_addr(ret, static_locator(SROM_reset));
    _push_addr(ret, static_locator(SROM_irq));
    return ret;
}

static void _bankswitch_ax(asm_proc_t& proc, locator_t load)
{
    if(auto const addr = bankswitch_addr(mapper().type))
    {
        if(load)
        {
            if(load.byteified())
            {
                proc.push_inst(LDA_IMMEDIATE, load);
                if(has_bus_conflicts(mapper().type))
                    proc.push_inst(TAX);
            }
            else
                proc.push_inst(LAX_ABSOLUTE, load);
        }

        if(has_bus_conflicts(mapper().type))
            proc.push_inst(STA_ABSOLUTE_X, static_locator(SROM_iota));
        else
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(addr));
    }
}

static asm_proc_t make_nmi()
{
    asm_proc_t proc;

    // Store registers:
    proc.push_inst(PHA);
    proc.push_inst(STX_ABSOLUTE, static_locator(SRAM_nmi_saved_x));
    proc.push_inst(STY_ABSOLUTE, static_locator(SRAM_nmi_saved_y));

    if(auto const addr = bankswitch_addr(mapper().type))
    {
        // Save current bank
        proc.push_inst(LDA_IMMEDIATE, LOC_THIS_BANK);
        proc.push_inst(STA_ABSOLUTE, static_locator(SRAM_nmi_saved_bank));

        proc.push_inst(LAX_ABSOLUTE, static_locator(SRAM_nmi_call_bank));
        if(has_bus_conflicts(mapper().type))
            proc.push_inst(STA_ABSOLUTE_X, static_locator(SROM_iota));
        else
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(addr));
    }

    proc.push_inst(JMP_INDIRECT, static_locator(SRAM_nmi_call_ptr));

    proc.initial_optimize();
    return proc;
}

static asm_proc_t make_nmi_exit()
{
    asm_proc_t proc;

    _bankswitch_ax(proc, static_locator(SRAM_nmi_saved_bank));

    proc.push_inst(INC_ABSOLUTE, static_locator(SRAM_nmi_counter));
    proc.push_inst(LDX_ABSOLUTE, static_locator(SRAM_nmi_saved_x));
    proc.push_inst(LDY_ABSOLUTE, static_locator(SRAM_nmi_saved_y));
    proc.push_inst(PLA);
    proc.push_inst(RTI);

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
    // TODO: the bulk of this should go in a single bank

    unsigned next_label = 0;
    asm_proc_t proc;

    // Ignore IRQs:
    proc.push_inst(SEI);
    
    // Turn off decimal mode, just in case the code gets run on wonky hardware.
    proc.push_inst(CLD);

    // Disable NMI and rendering:
    proc.push_inst(LDA, 0);
    proc.push_inst(STA_ABSOLUTE, PPUCTRL);
    proc.push_inst(STA_ABSOLUTE, PPUMASK);

    // Disable DMC IRQ
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x4010));

    // Read the status registers to handle stray NMIs and DMC IRQs across resets
    proc.push_inst(LDA_ABSOLUTE, PPUSTATUS);
    proc.push_inst(LDA_ABSOLUTE, SNDCHN);

    // Disable APU frame counter IRQ.
    // (See http://wiki.nesdev.com/w/index.php/APU_Frame_Counter )
    proc.push_inst(LDA, 0b01000000);
    proc.push_inst(STA_ABSOLUTE, locator_t::addr(0x4017));

    // Disable DMC but initialize the other channels.
    proc.push_inst(LDA, 0x0F);
    proc.push_inst(STA_ABSOLUTE, SNDCHN);

    // Set the stack pointer.
    proc.push_inst(LDX, 0xFF);
    proc.push_inst(TXS);

    // Now wait two frames until the PPU stabilizes.
    // Can't use NMI yet, so we'll spin on bit 7 of PPUSTATUS to determine
    // when those frames pass.
    locator_t const wait_frame_1 = proc.push_label(next_label++);
    proc.push_inst(BIT_ABSOLUTE, PPUSTATUS);
    proc.push_inst(BPL_RELATIVE, wait_frame_1);

    // Wait for the second frame.
    locator_t const wait_frame_2 = proc.push_label(next_label++);
    proc.push_inst(BIT_ABSOLUTE, PPUSTATUS);
    proc.push_inst(BPL_RELATIVE, wait_frame_2);

    // Init the NMI pointer
    proc.push_inst(LDA_IMMEDIATE, static_locator(SROM_nmi_exit));
    proc.push_inst(STA_ABSOLUTE, static_locator(SRAM_nmi_call_ptr, 0));
    proc.push_inst(LDA_IMMEDIATE, static_locator(SROM_nmi_exit).with_high(true));
    proc.push_inst(STA_ABSOLUTE, static_locator(SRAM_nmi_call_ptr, 1));
    if(bankswitch_addr(mapper().type))
    {
        proc.push_inst(LDA, 0);
        proc.push_inst(STA_ABSOLUTE, static_locator(SRAM_nmi_call_bank));
    }

    // Jump to our entry point.
    _bankswitch_ax(proc, locator_t(LOC_MAIN_ENTRY_BANK).with_byteified(true));
    proc.push_inst(JMP_ABSOLUTE, LOC_MAIN_ENTRY);

    proc.initial_optimize();
    return proc;
}

asm_proc_t make_bnrom_trampoline()
{
    asm_proc_t proc;

    proc.push_inst(STA_ABSOLUTE, static_locator(SRAM_ptr_temp, 0));
    proc.push_inst(STX_ABSOLUTE, static_locator(SRAM_ptr_temp, 1));
    proc.push_inst(LDA_IMMEDIATE, LOC_THIS_BANK);
    proc.push_inst(PHA);
    proc.push_inst(TYA);
    proc.push_inst(STA_ABSOLUTE_Y, static_locator(SROM_iota));
    proc.push_inst(JSR_ABSOLUTE, locator_t::minor_label(0));
    proc.push_inst(PLA);
    proc.push_inst(TAY);
    proc.push_inst(STA_ABSOLUTE_Y, static_locator(SROM_iota));
    proc.push_inst(RTS);
    proc.push_label(0);
    proc.push_inst(JMP_INDIRECT, static_locator(SRAM_ptr_temp));

    proc.initial_optimize();
    return proc;
}

static rom_array_t make_iota()
{
    rom_array_t ret;
    ret.data.reserve(256);
    for(unsigned i = 0; i < 256; ++i)
        ret.data.push_back(locator_t::const_byte(i));
    return ret;
}

span_allocator_t alloc_static_rom()
{
    span_allocator_t a(mapper().rom_span());

    auto const alloc = [&](static_rom_name_t name, auto&& data)
    {
        _srom_data[name] = to_rom_data(std::move(data));

        if(_srom_spans[name])
            return;

        _srom_spans[name] = a.alloc(_srom_data[name].max_size(), 0);
    };

    // Pre-allocate.
    _srom_spans[SROM_iota] = a.alloc(256, 256);
    _srom_spans[SROM_vectors] = a.alloc_at({ 0xFFFA, 6 });

    // This have to be defined in a toposorted order.
    alloc(SROM_iota, make_iota());
    alloc(SROM_nmi, make_nmi());
    alloc(SROM_nmi_exit, make_nmi_exit());
    alloc(SROM_irq, make_irq());
    alloc(SROM_reset, make_reset());
    alloc(SROM_vectors, make_vectors());

    return a;
}

rom_data_ht static_data(static_rom_name_t name)
{
    return _srom_data[name];
}
