#include "cg_isel.hpp"

#include <cstdint>
#include <iostream> // TODO
#include <functional>
#include <vector>

#include "robin/hash.hpp"
#include "robin/map.hpp"

#include "array_pool.hpp"
#include "format.hpp"

#define CONT_TEMPLATE 
#define CONT cont_fn_t cont

namespace isel
{
    using options_flags_t = std::uint32_t;
    constexpr options_flags_t OPT_CONDITIONAL = 1 << 0;

    // Options, to be passed to various construction functions:
    template<regs_t CanSet = REGF_CPU, regs_t SetMask = REGF_CPU, options_flags_t Flags = 0>
    struct options
    {
        static constexpr regs_t can_set = CanSet;
        static constexpr regs_t set_mask = SetMask;
        static constexpr options_flags_t flags = Flags;

        template<regs_t Regs>
        using restrict_to = options<CanSet & Regs, SetMask, Flags>;

        template<regs_t Regs>
        using mask = options<CanSet, SetMask & Regs, Flags>;

        template<options_flags_t NewFlags>
        using add_flags = options<CanSet, SetMask, Flags | NewFlags>;

        template<options_flags_t NewFlags>
        using remove_flags = options<CanSet, SetMask, Flags & ~NewFlags>;
    };

    // An approximation of the CPU's state at a given position.
    struct cpu_t
    {
        std::array<ssa_value_t, NUM_CPU_REGS> regs;

        // When implementing minor branches (such as in multi-byte comparisons),
        // some registers will be conditionally set.
        // These flags track which registers are conditional:
        regs_t conditional_regs;

        // This bitset keeps track of which variables must be stored.
        // To shrink the size down to 64 bits, a rolling window is used
        // based around the live ranges occuring within a single CFG node.
        std::uint64_t req_store;

        bool operator==(cpu_t const& o) const
        { 
            return (req_store == o.req_store 
                    && conditional_regs == o.conditional_regs
                    && regs == o.regs);
        }

        bool reg_eq(regs_t reg, ssa_value_t v) const
        {
            assert(is_orig_def(regs[reg]));
            assert(is_orig_def(v));
        #ifndef NDEBUG
            if(reg == REG_C && v.is_num() && regs[reg].is_num())
            {
                assert(regs[reg].whole() <= 1);
                assert(v.whole() <= 1);
            }
        #endif
            return regs[reg] == v;
        }

        template<typename Options, regs_t Reg>
        void set_reg(ssa_value_t value)
        {
            if(Options::flags & OPT_CONDITIONAL)
                conditional_regs |= 1 << Reg;
            regs[Reg] = (Options::set_mask & (1 << Reg)) ? value : ssa_value_t{};
        }

        template<typename Options, regs_t Regs>
        void set_regs(ssa_value_t value)
        {
            assert(!value.is_num() || (value.whole() & 0xFF) == value.whole());

            if(Options::flags & OPT_CONDITIONAL)
                conditional_regs |= Regs;
            if(Regs & REGF_A)
                set_reg<typename Options::remove_flags<OPT_CONDITIONAL>, REG_A>(value);
            if(Regs & REGF_X)
                set_reg<typename Options::remove_flags<OPT_CONDITIONAL>, REG_X>(value);
            if(Regs & REGF_Y)
                set_reg<typename Options::remove_flags<OPT_CONDITIONAL>, REG_Y>(value);
            if(Regs & REGF_C)
                set_reg<typename Options::remove_flags<OPT_CONDITIONAL>, REG_C>(value);
            if(Regs & REGF_Z)
                set_reg<typename Options::remove_flags<OPT_CONDITIONAL>, REG_Z>(value);
        }

        // Dumbly sets registers based on 'op_output_regs'.
        // Use 'set_regs_for' for the smarter version!
        template<typename Options, op_t Op>
        void set_output_regs(ssa_value_t value)
        {
            assert(!value.is_num());
            set_regs<Options, op_output_regs(Op) & REGF_CPU>(value);
        }

        // Sets registers based on an assembly op.
        // If the registers and inputs are known constants,
        // the set values may be constants too.
        template<typename Options, op_t Op>
        void set_regs_for(ssa_value_t def, ssa_value_t arg);

        // Queries if multiple registers are known numeric constants.
        template<regs_t Regs>
        bool all_num() const
        {
            bool result = true;

            if(Regs & REGF_A)
                result &= regs[REG_A].is_num();
            if(Regs & REGF_X)
                result &= regs[REG_X].is_num();
            if(Regs & REGF_Y)
                result &= regs[REG_Y].is_num();
            if(Regs & REGF_C)
                result &= regs[REG_C].is_num();
            if(Regs & REGF_Z)
                result &= regs[REG_Z].is_num();

            return result;
        }

    };
}

namespace std
{
    template<>
    struct hash<isel::cpu_t>
    {
        std::size_t operator()(isel::cpu_t const& cpu) const noexcept
        {
            std::size_t h = rh::hash_finalize(
                cpu.req_store ^ cpu.conditional_regs);
            for(ssa_value_t const& v : cpu.regs)
                h = rh::hash_combine(h, v.target());
            return h;
        }
    };
}

namespace isel
{
    /* TODO
    // Extra data added onto the schedule:
    struct isel_schedule_d
    {
        std::uint64_t store_mask = 0;
        std::uint64_t last_use = 0;
    };
    */

    template<typename Options, op_t Op>
    struct set_regs_for_impl
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            cpu.set_output_regs<Options, Op>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ADC_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_A | REGF_C>())
            {
                static_assert(op_output_regs(ADC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_C));
                unsigned const result = cpu.regs[REG_A].whole() + cpu.regs[REG_C].carry() + arg.whole();
                cpu.set_reg<Options, REG_A>(result & 0xFF);
                cpu.set_reg<Options, REG_Z>((result & 0xFF) == 0);
                cpu.set_reg<Options, REG_C>(!!(result & 0x100));
            }
            else
                cpu.set_output_regs<Options, ADC_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, AND_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(AND_IMMEDIATE) == (REGF_A | REGF_Z));
                unsigned const result = cpu.regs[REG_A].whole() & arg.whole();
                cpu.set_reg<Options, REG_A>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
            }
            else if(arg.is_num() && arg.whole() == 0)
            {
                cpu.set_reg<Options, REG_A>(0u);
                cpu.set_reg<Options, REG_Z>(1u);
            }
            else
                cpu.set_output_regs<Options, AND_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ASL_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ASL_IMPLIED) == (REGF_A | REGF_Z | REGF_C));
                unsigned const a = (cpu.regs[REG_A].whole() << 1) & 0xFF;
                unsigned const c = !!(cpu.regs[REG_A].whole() & 128);
                cpu.set_reg<Options, REG_A>(a);
                cpu.set_reg<Options, REG_Z>(a == 0);
                cpu.set_reg<Options, REG_C>(c);
            }
            else
                cpu.set_output_regs<Options, ASL_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, CLC_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            cpu.set_reg<Options, REG_C>(0u);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, CMP_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(CMP_IMMEDIATE) == (REGF_Z | REGF_C));
                unsigned const z = arg.whole() == cpu.regs[REG_A].whole();
                unsigned const c = arg.whole() <= cpu.regs[REG_A].whole();
                cpu.set_reg<Options, REG_Z>(z);
                cpu.set_reg<Options, REG_C>(c);
            }
            else if(arg.is_num() && arg.whole() == 0)
            {
                cpu.set_reg<Options, REG_Z>(def);
                cpu.set_reg<Options, REG_C>(1u);
            }
            else
                cpu.set_output_regs<Options, CMP_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, CPX_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(CPX_IMMEDIATE) == (REGF_Z | REGF_C));
                unsigned const z = arg.whole() == cpu.regs[REG_X].whole();
                unsigned const c = arg.whole() <= cpu.regs[REG_X].whole();
                cpu.set_reg<Options, REG_Z>(z);
                cpu.set_reg<Options, REG_C>(c);
            }
            else if(arg.is_num() && arg.whole() == 0)
            {
                cpu.set_reg<Options, REG_Z>(def);
                cpu.set_reg<Options, REG_C>(1u);
            }
            else
                cpu.set_output_regs<Options, CPX_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, CPY_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(CPY_IMMEDIATE) == (REGF_Z | REGF_C));
                unsigned const z = arg.whole() == cpu.regs[REG_Y].whole();
                unsigned const c = arg.whole() <= cpu.regs[REG_Y].whole();
                cpu.set_reg<Options, REG_Z>(z);
                cpu.set_reg<Options, REG_C>(c);
            }
            else if(arg.is_num() && arg.whole() == 0)
            {
                cpu.set_reg<Options, REG_Z>(def);
                cpu.set_reg<Options, REG_C>(1u);
            }
            else
                cpu.set_output_regs<Options, CPY_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, DEX_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(DEX_IMPLIED) == (REGF_Z | REGF_X));
                unsigned const result = (cpu.regs[REG_X].whole() - 1) & 0xFF;
                cpu.set_reg<Options, REG_X>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
            }
            else
                cpu.set_output_regs<Options, DEX_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, DEY_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(DEY_IMPLIED) == (REGF_Z | REGF_Y));
                unsigned const result = (cpu.regs[REG_Y].whole() - 1) & 0xFF;
                cpu.set_reg<Options, REG_Y>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
            }
            else
                cpu.set_output_regs<Options, DEY_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, EOR_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(EOR_IMMEDIATE) == (REGF_Z | REGF_A));
                unsigned const result = (cpu.regs[REG_Y].whole() ^ arg.whole());
                cpu.set_reg<Options, REG_A>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
            }
            else
                cpu.set_output_regs<Options, EOR_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, INX_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(INX_IMPLIED) == (REGF_Z | REGF_X));
                unsigned const result = (cpu.regs[REG_X].whole() + 1) & 0xFF;
                cpu.set_reg<Options, REG_X>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
            }
            else
                cpu.set_output_regs<Options, INX_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, INY_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(INY_IMPLIED) == (REGF_Z | REGF_Y));
                unsigned const result = (cpu.regs[REG_Y].whole() + 1) & 0xFF;
                cpu.set_reg<Options, REG_Y>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
            }
            else
                cpu.set_output_regs<Options, INY_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, LDA_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num())
            {
                static_assert(op_output_regs(LDA_IMMEDIATE) == (REGF_Z | REGF_A));
                cpu.set_reg<Options, REG_A>(arg);
                cpu.set_reg<Options, REG_Z>(arg.whole() == 0);
            }
            else
                cpu.set_output_regs<Options, LDA_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, LDX_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num())
            {
                static_assert(op_output_regs(LDX_IMMEDIATE) == (REGF_Z | REGF_X));
                cpu.set_reg<Options, REG_X>(arg);
                cpu.set_reg<Options, REG_Z>(arg.whole() == 0);
            }
            else
                cpu.set_output_regs<Options, LDX_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, LDY_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num())
            {
                static_assert(op_output_regs(LDY_IMMEDIATE) == (REGF_Z | REGF_Y));
                cpu.set_reg<Options, REG_Y>(arg);
                cpu.set_reg<Options, REG_Z>(arg.whole() == 0);
            }
            else
                cpu.set_output_regs<Options, LDY_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, LSR_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(LSR_IMPLIED) == (REGF_A | REGF_Z | REGF_C));
                unsigned const a = cpu.regs[REG_A].whole() >> 1;
                unsigned const c  = cpu.regs[REG_A].whole() & 1;
                cpu.set_reg<Options, REG_A>(a);
                cpu.set_reg<Options, REG_Z>(a == 0);
                cpu.set_reg<Options, REG_C>(c);
            }
            else
                cpu.set_output_regs<Options, LSR_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ORA_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ORA_IMMEDIATE) == (REGF_A | REGF_Z));
                unsigned const result = (cpu.regs[REG_Y].whole() | arg.whole());
                cpu.set_reg<Options, REG_A>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
            }
            else if(arg.is_num() && arg.whole() == 0xFF)
            {
                cpu.set_reg<Options, REG_A>(0xFFu);
                cpu.set_reg<Options, REG_Z>(0u);
            }
            else
                cpu.set_output_regs<Options, ORA_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ROL_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            static_assert(op_output_regs(ROL_IMPLIED) == (REGF_A | REGF_Z | REGF_C));

            if(cpu.all_num<REGF_A>())
            {
                unsigned const c = !!(cpu.regs[REG_A].whole() & 128);
                cpu.set_reg<Options, REG_C>(c);

                if(cpu.all_num<REGF_C>())
                {
                    unsigned const a = ((cpu.regs[REG_A].whole() << 1) | cpu.regs[REG_C].carry()) & 0xFF;
                    cpu.set_reg<Options, REG_A>(a);
                    cpu.set_reg<Options, REG_Z>(a == 0);
                }
                else
                {
                    cpu.set_reg<Options, REG_A>(def);
                    cpu.set_reg<Options, REG_Z>(def);
                }
            }
            else if(cpu.all_num<REGF_C>() && cpu.regs[REG_C].carry())
            {
                cpu.set_reg<Options, REG_A>(def);
                cpu.set_reg<Options, REG_C>(def);
                cpu.set_reg<Options, REG_Z>(0u);
            }
            else
                cpu.set_output_regs<Options, ROL_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ROR_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            static_assert(op_output_regs(ROR_IMPLIED) == (REGF_A | REGF_Z | REGF_C));

            if(cpu.all_num<REGF_A>())
            {
                unsigned const c = (cpu.regs[REG_A].whole() & 1);
                cpu.set_reg<Options, REG_C>(c);

                if(cpu.all_num<REGF_C>())
                {
                    unsigned const a = ((cpu.regs[REG_A].whole() >> 1) | (cpu.regs[REG_C].carry() << 7)) & 0xFF;
                    cpu.set_reg<Options, REG_A>(a);
                    cpu.set_reg<Options, REG_Z>(a == 0);
                }
                else
                {
                    cpu.set_reg<Options, REG_A>(def);
                    cpu.set_reg<Options, REG_Z>(def);
                }
            }
            else if(cpu.all_num<REGF_C>() && cpu.regs[REG_C].carry())
            {
                cpu.set_reg<Options, REG_A>(def);
                cpu.set_reg<Options, REG_C>(def);
                cpu.set_reg<Options, REG_Z>(0u);
            }
            else
                cpu.set_output_regs<Options, ROR_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, SBC_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_A | REGF_C>())
            {
                static_assert(op_output_regs(SBC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_C));
                unsigned const result = cpu.regs[REG_A].whole() + cpu.regs[REG_C].carry() + ~arg.whole();
                cpu.set_reg<Options, REG_A>(result & 0xFF);
                cpu.set_reg<Options, REG_Z>((result & 0xFF) == 0);
                cpu.set_reg<Options, REG_C>(!!(result & 0x100));
            }
            else
                cpu.set_output_regs<Options, SBC_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, TAX_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            static_assert(op_output_regs(TAX_IMPLIED) == (REGF_X | REGF_Z));
            if(cpu.all_num<REGF_A>())
            {
                cpu.set_reg<Options, REG_X>(cpu.regs[REG_A]);
                cpu.set_reg<Options, REG_Z>(cpu.regs[REG_A].whole() == 0);
            }
            else
                cpu.set_output_regs<Options, TAX_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, TAY_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            static_assert(op_output_regs(TAY_IMPLIED) == (REGF_Y | REGF_Z));
            if(cpu.all_num<REGF_A>())
            {
                cpu.set_reg<Options, REG_Y>(cpu.regs[REG_A]);
                cpu.set_reg<Options, REG_Z>(cpu.regs[REG_A].whole() == 0);
            }
            else
                cpu.set_output_regs<Options, TAY_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, TXA_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            static_assert(op_output_regs(TXA_IMPLIED) == (REGF_A | REGF_Z));
            if(cpu.all_num<REGF_X>())
            {
                cpu.set_reg<Options, REG_A>(cpu.regs[REG_X]);
                cpu.set_reg<Options, REG_Z>(cpu.regs[REG_X].whole() == 0);
            }
            else
                cpu.set_output_regs<Options, TXA_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, TYA_IMPLIED>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            static_assert(op_output_regs(TYA_IMPLIED) == (REGF_A | REGF_Z));
            if(cpu.all_num<REGF_Y>())
            {
                cpu.set_reg<Options, REG_A>(cpu.regs[REG_Y]);
                cpu.set_reg<Options, REG_Z>(cpu.regs[REG_Y].whole() == 0);
            }
            else
                cpu.set_output_regs<Options, TYA_IMPLIED>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, AXS_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_X | REGF_A>())
            {
                static_assert(op_output_regs(AXS_IMMEDIATE) == (REGF_Z | REGF_X | REGF_C));
                unsigned const result = (cpu.regs[REG_A].whole() & cpu.regs[REG_X].whole()) + ~arg.whole() + 1;
                cpu.set_reg<Options, REG_X>(result & 0xFF);
                cpu.set_reg<Options, REG_Z>((result & 0xFF) == 0);
                cpu.set_reg<Options, REG_C>(!!(result & 0x100));
            }
            else if(arg.is_num() && arg.whole() == 0)
            {
                cpu.set_reg<Options, REG_X>(def);
                cpu.set_reg<Options, REG_Z>(def);
                cpu.set_reg<Options, REG_C>(1u);
            }
            else
                cpu.set_output_regs<Options, AXS_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ANC_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ANC_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
                unsigned const result = (cpu.regs[REG_A].whole() & arg.whole());
                cpu.set_reg<Options, REG_A>(result);
                cpu.set_reg<Options, REG_Z>(result == 0);
                cpu.set_reg<Options, REG_C>(!!(result & 128));
            }
            else if(arg.is_num() && arg.whole() == 0)
            {
                cpu.set_reg<Options, REG_A>(0u);
                cpu.set_reg<Options, REG_Z>(1u);
                cpu.set_reg<Options, REG_C>(0u);
            }
            else
                cpu.set_output_regs<Options, ANC_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ALR_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            if(arg.is_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ALR_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
                unsigned const and_ = (cpu.regs[REG_A].whole() & arg.whole());
                unsigned const a = and_ >> 1;
                unsigned const c = and_ & 1;
                cpu.set_reg<Options, REG_A>(a);
                cpu.set_reg<Options, REG_Z>(a == 0);
                cpu.set_reg<Options, REG_C>(c);
            }
            else if(arg.is_num() && arg.whole() == 0)
            {
                cpu.set_reg<Options, REG_A>(0u);
                cpu.set_reg<Options, REG_Z>(1u);
                cpu.set_reg<Options, REG_C>(0u);
            }
            else
                cpu.set_output_regs<Options, ALR_IMMEDIATE>(def);
        }
    };

    template<typename Options>
    struct set_regs_for_impl<Options, ARR_IMMEDIATE>
    {
        static void call(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
        {
            static_assert(op_output_regs(ARR_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
            if(arg.is_num() && cpu.all_num<REGF_A>())
            {
                unsigned const and_ = cpu.regs[REG_A].whole() & arg.whole();
                unsigned const c = !!(and_ & 128);
                cpu.set_reg<Options, REG_C>(c);

                if(cpu.all_num<REGF_C>())
                {
                    unsigned const a = ((and_ >> 1) | (cpu.regs[REG_C].carry() << 7)) & 0xFF;
                    cpu.set_reg<Options, REG_A>(a);
                    cpu.set_reg<Options, REG_Z>(a == 0);
                }
                else
                {
                    cpu.set_reg<Options, REG_A>(def);
                    cpu.set_reg<Options, REG_Z>(def);
                }
            }
            else if(cpu.all_num<REGF_C>() && cpu.regs[REG_C].carry())
            {
                cpu.set_reg<Options, REG_A>(def);
                cpu.set_reg<Options, REG_C>(def);
                cpu.set_reg<Options, REG_Z>(0u);
            }
            else
                cpu.set_output_regs<Options, ARR_IMMEDIATE>(def);
        }
    };

    template<typename Options, op_t Op>
    void cpu_t::set_regs_for(ssa_value_t def, ssa_value_t arg)
    {
        set_regs_for_impl<Options, Op>::call(*this, def, arg);
    }

    struct state_t
    {
        rh::batman_map<cpu_t, sel_t const*> map;
        rh::batman_map<cpu_t, sel_t const*> next_map;

        array_pool_t<sel_t> sel_pool;

        unsigned best_cost = ~0;
        unsigned next_best_cost = ~0;
        sel_t const* best_sel = nullptr;

        cfg_ht cfg_node;
        //std::vector<isel_schedule_d> schedule_data;

        unsigned next_label = 0;
    };

    // Main global state of the instruction selection algorithm.
    static thread_local state_t state;

    ssa_value_t asm_arg(ssa_value_t v)
    {
        if(v.holds_ref())
            if(locator_t loc = cset_locator(v.handle()))
                return loc;
        return v;
    }

    template<typename Tag>
    struct param
    {
        static inline thread_local ssa_value_t _node = {};
        static inline thread_local ssa_value_t _value = {};
        static inline thread_local ssa_value_t _trans = {};

        static void set(ssa_value_t v)
        {
            _node = v;
            _value = orig_def(v);
            _trans = asm_arg(v);
        }

        [[gnu::always_inline]]
        static ssa_value_t node() { return _node; }

        [[gnu::always_inline]]
        static ssa_value_t value() { return _value; }

        [[gnu::always_inline]]
        static ssa_value_t trans() { return _trans; }
    };

    template<typename Param>
    struct array_index
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(1); }

        [[gnu::always_inline]]
        static ssa_value_t value() { return orig_def(node()); }

        [[gnu::always_inline]]
        static ssa_value_t trans() { return asm_arg(node()); }

    };

    struct null_
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return {}; }

        [[gnu::always_inline]]
        static ssa_value_t value() { return {}; }

        [[gnu::always_inline]]
        static ssa_value_t trans() { return {}; }

        // Pass-thru
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            Cont::run(cpu, prev);
        }
    };

    template<unsigned I>
    struct const_
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return {}; }

        [[gnu::always_inline]]
        static ssa_value_t value() { return I; }

        [[gnu::always_inline]]
        static ssa_value_t trans() { return I; }
    };

    unsigned get_cost(sel_t const* sel)
    {
        return sel ? sel->cost : 0;
    }

    template<op_t Op>
    constexpr unsigned op_penalty()
    {
        switch(op_name(Op))
        {
        default: 
            return 0;
        // Very slightly penalize ROL/ROR, to prefer LSR/ASL:
        case ROL:
        case ROR:
        // Very slighty penalize LAX, to prefer LDA or LDX:
        case LAX: 
        // Same with SAX and AND:
        case SAX: 
            return 1;
        }
    }

    template<op_t Op>
    constexpr unsigned cost_fn = (op_cycles(Op) << 8) + op_size(Op) + op_penalty<Op>();

    // THIS DETERMINES HOW BIG THE MAP GETS, AND HOW WIDE THE SEARCH IS:
    constexpr unsigned COST_CUTOFF = cost_fn<LDA_ABSOLUTE> * 3;

    template<typename Options, op_t Op>
    sel_t& alloc_sel(sel_t const* prev, ssa_value_t arg, unsigned extra_cost = 0)
    {
        unsigned cost = cost_fn<Op> + extra_cost;
        if(Options::flags & OPT_CONDITIONAL)
            cost >>= 1;
        unsigned const total_cost = get_cost(prev) + cost;
        return state.sel_pool.emplace(prev, total_cost, ainst_t{ Op, arg });
    }

    template<typename Options, typename Label>
    struct label
    {
        template<typename Cont>
        static void run(cpu_t cpu, sel_t const* prev) 
        {
            assert(Label::trans().is_locator());
            Cont::run(cpu, &alloc_sel<Options, ASM_LABEL>(prev, Label::trans()));
        }
    };

    struct finish
    {
        static void run(cpu_t cpu, sel_t const* sel)
        {
            auto result = state.next_map.insert({ cpu, sel });

            unsigned const sel_cost = get_cost(sel);

            if(!result.inserted && sel_cost < get_cost(*result.mapped))
                *result.mapped = sel;

            if(sel_cost < state.next_best_cost)
            {
                state.next_best_cost = sel_cost;
                state.best_sel = sel;
            }
        }
    };

    // Combines multiple 'run' functions into a single one.
    template<typename... Fns>
    struct chain
    {};

    template<typename Fn, typename... Fns>
    struct chain<Fn, Fns...>
    {
        [[gnu::always_inline]]
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::template run<chain<Fns...>>(cpu, sel);
        }

        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::template run<chain<Fns..., Cont>>(cpu, sel);
        }
    };

    template<typename Fn>
    struct chain<Fn>
    {
        [[gnu::always_inline]]
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::run(cpu, sel);
        }

        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::template run<Cont>(cpu, sel);
        }
    };

    // Runs the function and adds the results to the state map.
    template<typename Fn>
    void select_step(Fn fn)
    {
        state.next_map.clear();
        state.next_best_cost = ~0 - COST_CUTOFF;

        for(auto const& pair : state.map)
            if(get_cost(pair.second) < state.best_cost + COST_CUTOFF)
                fn(pair.first, pair.second);

        if(state.next_map.empty())
            throw std::runtime_error("Instruction selection failed to make progress.");

        state.map.swap(state.next_map);
        state.best_cost = state.next_best_cost;
    }

    template<typename Options, regs_t Regs, typename Param,
             bool Enable = (Regs & Options::can_set & REGF_CPU) == (Regs & REGF_CPU)>
    struct set_regs
    {
        template<typename Cont>
        static void run(cpu_t cpu, sel_t const* prev)
        {
            static_assert((Regs & Options::can_set) == Regs);
            cpu.set_regs<Options, Regs>(Param::value());
            Cont::run(cpu, prev);
        }
    };

    template<typename Options, regs_t Regs, typename Param>
    struct set_regs<Options, Regs, Param, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {}
    };

    template<typename Options, op_t Op, typename Param>
    using set_regs_for = set_regs<Options, op_output_regs(Op) & REGF_CPU, Param>;

    template<typename Options, op_t Op>
    constexpr bool can_set_regs_for = 
        ((Options::can_set & op_output_regs(Op) & REGF_CPU) == (op_output_regs(Op) & REGF_CPU));

    struct clear_conditional
    {
        template<typename Cont>
        static void run(cpu_t cpu, sel_t const* prev) 
        {
            if(cpu.conditional_regs & REGF_A)
                cpu.regs[REG_A] = {};
            if(cpu.conditional_regs & REGF_X)
                cpu.regs[REG_X] = {};
            if(cpu.conditional_regs & REGF_Y)
                cpu.regs[REG_Y] = {};
            if(cpu.conditional_regs & REGF_C)
                cpu.regs[REG_C] = {};
            if(cpu.conditional_regs & REGF_Z)
                cpu.regs[REG_Z] = {};
            cpu.conditional_regs = 0;
            Cont::run(cpu, prev);
        }
    };

    // Generates an op, picking the addressing mode based on its paramters.
    template<typename Options, op_name_t OpName, typename Def = null_, typename Arg = null_>
    struct pick_op
    {
        template<typename Cont>
        static void run(cpu_t cpu, sel_t const* prev);
    };


    // Spits out the op specified.
    template<typename Options, op_t Op, typename Def = null_, typename Arg = null_>
    struct exact_op
    {
        template<typename Cont>
        static void run(cpu_t cpu, sel_t const* prev)
        {
            if(can_set_regs_for<Options, Op>)
            {
                cpu.set_regs_for<Options, Op>(Def::value(), Arg::trans());
                Cont::run(cpu, &alloc_sel<Options, Op>(prev, Arg::trans()));
            }
        }
    };

    // Generates an op using the 0..255 table.
    template<typename Options, op_t Op, typename Def = null_>
    struct iota_op
    {
        template<typename Cont>
        static void run(cpu_t cpu, sel_t const* prev)
        {
            static_assert(op_addr_mode(Op) == MODE_ABSOLUTE_X || op_addr_mode(Op) == MODE_ABSOLUTE_Y);
            if(can_set_regs_for<Options, Op>)
            {
                cpu.set_output_regs<Options, Op>(Def::value());
                Cont::run(cpu, &alloc_sel<Options, Op>(prev, locator_t::iota()));
            }
        }
    };

    template<typename Options, typename Def, 
             bool Enable = Options::can_set & REGF_Z>
    struct load_Z
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_Z);

            if(cpu.reg_eq(REG_Z, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_A, Def::value()))
            {
                exact_op<typename Options::mask<REGF_Z>, EOR_IMMEDIATE, Def, const_<0>>
                ::template run<Cont>(cpu, prev);

                exact_op<Options, TAX_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);

                exact_op<Options, TAY_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            }
            else if(cpu.reg_eq(REG_X, Def::value()))
            {
                chain
                < exact_op<Options, INX_IMPLIED>
                , exact_op<Options, DEX_IMPLIED, Def>
                >::template run<Cont>(cpu, prev);

                chain
                < exact_op<typename Options::mask<REGF_Z>, CPX_IMMEDIATE, Def, const_<0u>>
                , set_regs<Options, REGF_C, const_<1u>>
                >::template run<Cont>(cpu, prev);

                exact_op<Options, TXA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            }
            else if(cpu.reg_eq(REG_Y, Def::value()))
            {
                chain
                < exact_op<Options, INY_IMPLIED>
                , exact_op<Options, DEY_IMPLIED, Def>
                >::template run<Cont>(cpu, prev);

                chain
                < exact_op<typename Options::mask<REGF_Z>, CPY_IMMEDIATE, Def, const_<0u>>
                , set_regs<Options, REGF_C, const_<1u>>
                >::template run<Cont>(cpu, prev);

                exact_op<Options, TYA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            }
            else
            {
                pick_op<Options, LDA, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LDX, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LDY, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LAX, Def, Def>
                ::template run<Cont>(cpu, prev);
            }
        }
    };

    template<typename Options, typename Def>
    struct load_Z<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_Z, Def::value()))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename Def,
             bool Enable = Options::can_set & REGF_A>
    struct load_A
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_A);

            if(cpu.reg_eq(REG_A, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_X, Def::value()))
                exact_op<Options, TXA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else if(cpu.reg_eq(REG_Y, Def::value()))
                exact_op<Options, TYA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else
            {
                pick_op<Options, LDA, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LAX, Def, Def>
                ::template run<Cont>(cpu, prev);

                if(Def::value().is_num())
                {
                    cpu_t cpu_copy;

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, ANC_IMMEDIATE>({}, Def::value());
                    if(cpu_copy.regs[REG_A] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, ANC_IMMEDIATE>(prev, Def::value()));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, LSR_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_A] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, LSR_IMPLIED>(prev, {}));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, ASL_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_A] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, ASL_IMPLIED>(prev, {}));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, ROL_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_A] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, ROR_IMPLIED>(prev, {}));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, ROR_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_A] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, ROL_IMPLIED>(prev, {}));
                }
            }
        }
    };

    template<typename Options, typename Def>
    struct load_A<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, Def::value()))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename Def, 
             bool Enable = Options::can_set & REGF_X>
    struct load_X
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_X);

            if(cpu.reg_eq(REG_X, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_A, Def::value()))
                exact_op<Options, TAX_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else
            {
                if(cpu.reg_eq(REG_Y, Def::value()))
                    chain
                    < exact_op<Options, TYA_IMPLIED>
                    , exact_op<Options, TAX_IMPLIED, Def>
                    >::template run<Cont>(cpu, prev);

                pick_op<Options, LDX, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LAX, Def, Def>
                ::template run<Cont>(cpu, prev);

                if(Def::value().is_num())
                {
                    cpu_t cpu_copy;

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, INX_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_X] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, INX_IMPLIED>(prev, {}));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, DEX_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_X] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, DEX_IMPLIED>(prev, {}));
                }
            }
        }
    };

    template<typename Options, typename Def>
    struct load_X<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_X, Def::value()))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename Def, 
             bool Enable = Options::can_set & REGF_Y>
    struct load_Y
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_Y);

            if(cpu.reg_eq(REG_Y, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_A, Def::value()))
                exact_op<Options, TAY_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else
            {
                if(cpu.reg_eq(REG_X, Def::value()))
                    chain
                    < exact_op<Options, TXA_IMPLIED>
                    , exact_op<Options, TAY_IMPLIED, Def>
                    >::template run<Cont>(cpu, prev);

                pick_op<Options, LDY, Def, Def>
                ::template run<Cont>(cpu, prev);

                if(Def::value().is_num())
                {
                    cpu_t cpu_copy;

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, INY_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_Y] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, INY_IMPLIED>(prev, {}));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, DEY_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_Y] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, DEY_IMPLIED>(prev, {}));
                }
            }
        }
    };

    template<typename Options, typename Def>
    struct load_Y<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_Y, Def::value()))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename A, typename X,
             bool Enable = Options::can_set & REGF_AX>
    struct load_AX
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            chain
            < load_A<Options, A>
            , load_X<typename Options::restrict_to<~REGF_A>, X>
            >::template run<Cont>(cpu, prev);

            chain
            < load_X<Options, X>
            , load_A<typename Options::restrict_to<~REGF_X>, A>
            >::template run<Cont>(cpu, prev);
        }
    };

    template<typename Options, typename A, typename X>
    struct load_AX<Options, A, X, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, A::value() && cpu.reg_eq(REG_X, X::value())))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename A, typename Y,
             bool Enable = Options::can_set & REGF_AY>
    struct load_AY
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            chain
            < load_A<Options, A>
            , load_X<typename Options::restrict_to<~REGF_A>, Y>
            >::template run<Cont>(cpu, prev);

            chain
            < load_X<Options, Y>
            , load_A<typename Options::restrict_to<~REGF_Y>, A>
            >::template run<Cont>(cpu, prev);
        }
    };

    template<typename Options, typename A, typename Y>
    struct load_AY<Options, A, Y, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, A::value() && cpu.reg_eq(REG_Y, Y::value())))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename C,
             bool Enable = (Options::can_set & REGF_C)>
    struct load_C
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_C, C::value()))
                Cont::run(cpu, prev);
            else if(C::value().is_num())
            {
                if(C::value().carry())
                    exact_op<Options, SEC_IMPLIED, C>
                    ::template run<Cont>(cpu, prev);
                else
                    exact_op<Options, CLC_IMPLIED, C>
                    ::template run<Cont>(cpu, prev);
            }
            else
            {
                chain
                < load_A<Options, C>
                , exact_op<Options, LSR_IMPLIED, C>
                >::template run<Cont>(cpu, prev);

                if((Options::can_set & (REGF_Z | REGF_A)) == REGF_Z)
                {
                    // We need a version that preserves 'A', so do things this way:

                    sel_t const* sel = prev;
                    sel = &alloc_sel<Options, PHA_IMPLIED>(sel, {});
                    sel = &alloc_sel<Options, LDA_ABSOLUTE>(sel, C::trans());
                    sel = &alloc_sel<Options, LSR_IMPLIED>(sel, {});
                    sel = &alloc_sel<Options, PLA_IMPLIED>(sel, {});

                    cpu_t new_cpu = cpu;
                    new_cpu.set_reg<Options, REG_C>(C::value());
                    if(new_cpu.regs[REG_A].is_num())
                        new_cpu.set_reg<Options, REG_Z>(new_cpu.regs[REG_A].whole() == 1);
                    else
                        new_cpu.set_reg<Options, REG_Z>(new_cpu.regs[REG_A]);

                    Cont::run(new_cpu, sel);
                }
            }
        }
    };

    template<typename Options, typename A, typename C,
             bool Enable = Options::can_set & REGF_AC>
    struct load_AC
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_C, C::value()))
                load_A<typename Options::restrict_to<~REGF_C>, A>
                ::template run<Cont>(cpu, prev);
            else if(C::value().is_num())
            {
                chain
                < load_A<Options, A>
                , load_C<typename Options::restrict_to<~REGF_A>, C>
                >::template run<Cont>(cpu, prev);

                chain
                < load_A<Options, C>
                , exact_op<typename Options::mask<REGF_C>, LSR_IMPLIED, C>
                , load_A<typename Options::restrict_to<~REGF_C>, A>
                >::template run<Cont>(cpu, prev);
            }
        }
    };

    template<typename Options, typename A, typename C>
    struct load_AC<Options, A, C, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, A::value() && cpu.reg_eq(REG_C, C::value())))
                Cont::run(cpu, prev);
        }
    };


    // pick_op impl
    template<typename Options, op_name_t OpName, typename Def, typename Arg>
    template<typename Cont>
    void pick_op<Options, OpName, Def, Arg>
    ::run(cpu_t cpu, sel_t const* prev)
    {
        constexpr op_t implied    = get_op(OpName, MODE_IMPLIED);
        constexpr op_t relative   = get_op(OpName, MODE_RELATIVE);
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);
        //constexpr op_t absolute_X = get_op(OpName, MODE_ABSOLUTE_X);
        //constexpr op_t absolute_Y = get_op(OpName, MODE_ABSOLUTE_Y);

        if(implied && !Arg::trans())
        {
            if(can_set_regs_for<Options, implied>)
                set_regs_for<Options, implied, Def>
                ::template run<Cont>(cpu, &alloc_sel<Options, implied>(prev, {}));
        }
        else if(relative && Arg::trans().is_locator())
        {
            assert(Arg::trans().locator().is_label());
            assert((op_output_regs(implied) & REGF_CPU) == 0);
            Cont::run(cpu, &alloc_sel<Options, relative>(prev, Arg::trans()));
        }
        else if(immediate && Arg::trans().is_num())
        {
            if(can_set_regs_for<Options, immediate>)
                set_regs_for<Options, immediate, Def>
                ::template run<Cont>(cpu, &alloc_sel<Options, immediate>(prev, Arg::trans()));
        }
        else if(absolute && !Arg::trans().is_num())
        {
            if(Arg::trans().is_const())
            {
                if(can_set_regs_for<Options, absolute>)
                    set_regs_for<Options, absolute, Def>
                    ::template run<Cont>(cpu, &alloc_sel<Options, absolute>(prev, Arg::trans()));
                return;
            }

            assert(Arg::trans().is_handle());
            ssa_ht h = Arg::trans().handle();
            auto& d = cg_data(h);

            unsigned store_penalty = cost_fn<STA_ABSOLUTE> / 2;
            cpu_t new_cpu = cpu;
            if(h->cfg_node() == state.cfg_node && d.isel.store_mask)
            {
                // Nodes that belong to this CFG node are tracked more
                // precisely using the 'req_store' part of 'cpu_t'.

                unsigned const stores = builtin::popcount(~cpu.req_store & d.isel.store_mask);

                // This may equal 0 if 'req_store' already holds the bits.
                store_penalty = cost_fn<STA_ABSOLUTE> * stores;

                new_cpu.req_store |= d.isel.store_mask;
            }

            if(can_set_regs_for<Options, absolute>)
                set_regs_for<Options, absolute, Def>
                ::template run<Cont>(new_cpu, &alloc_sel<Options, absolute>(prev, h, store_penalty));

            // If the node(s) were already stored, no point in checking
            // other methods of loading:
            if(store_penalty == 0)
                return;

            // If the node is an array read, try loading either X or Y
            // with the index, then using an indexed addressing mode.
            if(Arg::node().holds_ref() && Arg::node()->op() == SSA_read_array)
            {
                /*
                (load_X<Options::mask(REGF_X)>{ h->input(1) }
                >>= set_regs_for<absolute_X, Opt>{def} 
                >>= cont)(cpu, &alloc_sel<absolute_X>(prev, h, store_penalty));

                >>= def_op<LSR>{ cdef }
                >>= load_A<Options::mask(~REGF_C)>{ a }
                >>= cont)(cpu, prev);
                */


                /*
                ssa_value_t array = Arg::node()->input(0);

                if(can_set_regs_for<Options, absolute_X>)
                    chain
                    < load_X<Options::restrict_to(REGF_X), array_index<Arg>>
                    , set_regs_for<Options, absolute_X, Def>
                    >::template run<Cont>(cpu, &alloc_sel<absolute_X>(prev, array));

                if(can_set_regs_for<Options, absolute_Y>)
                    chain
                    < load_Y<Options::restrict_to(REGF_Y), array_index<Arg>>
                    , set_regs_for<Options, absolute_Y, Def>
                    >::template run<Cont>(cpu, &alloc_sel<absolute_Y>(prev, array));


                impl_addr_X<OpName, Opt>::func(
                    def, h, cpu, prev, cont);

                impl_addr_Y<OpName, Opt>::func(
                    def, h, cpu, prev, cont);
                    */
            }
        }
    }

    // Adds a store operation.
    // 'Maybe' means the store may not be required in the final code;
    // such instructions can be pruned later.
    template<typename Options, op_name_t StoreOp, typename Param, bool Maybe = true>
    struct store
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            // Store the node, locally:
            if(Maybe && Param::trans().holds_ref())
            {
                switch(StoreOp)
                {
                case STA: Cont::run(cpu, &alloc_sel<Options, MAYBE_STA>(prev, Param::trans())); break;
                case STX: Cont::run(cpu, &alloc_sel<Options, MAYBE_STX>(prev, Param::trans())); break;
                case STY: Cont::run(cpu, &alloc_sel<Options, MAYBE_STY>(prev, Param::trans())); break;
                case SAX: Cont::run(cpu, &alloc_sel<Options, MAYBE_SAX>(prev, Param::trans())); break;
                default: assert(false); break;
                }
            }
            else
                 Cont::run(cpu, &alloc_sel<Options, get_op(StoreOp, MODE_ABSOLUTE)>(prev, Param::trans()));
        }
    };

    template<typename Options, typename Def, typename Load, typename Store>
    struct load_then_store
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            chain
            < load_A<Options, Load>
            , set_regs<Options, REGF_A, Def>
            , store<Options, STA, Store>
            >::template run<Cont>(cpu, prev);

            chain
            < load_X<Options, Load>
            , set_regs<Options, REGF_X, Def>
            , store<Options, STX, Store>
            >::template run<Cont>(cpu, prev);

            chain
            < load_Y<Options, Load>
            , set_regs<Options, REGF_Y, Def>
            , store<Options, STY, Store>
            >::template run<Cont>(cpu, prev);
        }
    };

    template<typename Options, typename Condition, typename Then, typename Else = null_>
    struct if_
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(Condition::value())
                Then::template run<Cont>(cpu, prev);
            else
                Else::template run<Cont>(cpu, prev);
        }
    };

    template<typename Tag>
    struct condition
    {
        static inline thread_local bool b;
        static void set(bool s) { b = s; }
        static bool value() { return b; }
    };

    template<typename Tag, unsigned I>
    struct i_tag {};

    using p_def = param<struct def_tag>;
    template<unsigned I>
    using p_arg = param<i_tag<struct arg_tag, I>>;
    using p_lhs = p_arg<0>;
    using p_rhs = p_arg<1>;
    using p_carry = param<struct carry_tag>;
    using p_temp = param<struct temp_tag>;
    template<unsigned I>
    using p_label = param<i_tag<struct label_tag, I>>;
    using p_condition = condition<struct condition_tag>;

    /* TODO: remove?
    template<typename Param, unsigned I>
    struct param_arg
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(I); }

        [[gnu::always_inline]]
        static ssa_value_t value() { return orig_def(node()); }

        [[gnu::always_inline]]
        static ssa_value_t trans() { return asm_arg(node()); }
    };

    template<typename Options, op_name_t BranchOp, typename FailLabel, typename SuccessLabel, unsigned I>
    struct eq_branch
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            constexpr op_name_t InverseOp = get_op(invert_branch(BranchOp), MODE_RELATIVE);

            using lhs = param_arg<Node, I>;
            using rhs = param_arg<Node, I+1>;

            if(

            chain
            < load_A<OC, lhs>
            , if_<OC, p_condition, pick_op<OC, CMP, {}, p_rhs>>
            , exact_op<OC, InverseOp, null_, FailLabel>
            >::run<eq_branch<Options, BranchOp, FailLabel, SuccessLabel, I-2>>(cpu, prev);

            chain
            < load_X<OC, p_lhs>
            , if_<OC, p_condition, pick_op<OC, CPX, {}, p_rhs>>
            , exact_op<OC, InverseOp, null_, FailLabel>
            >::run<eq_branch<Options, BranchOp, FailLabel, SuccessLabel, I-2>>(cpu, prev);

            chain
            < load_Y<OC, p_lhs>
            , if_<OC, p_condition, pick_op<OC, CPY, {}, p_rhs>>
            , exact_op<OC, InverseOp, null_, FailLabel>
            >::run<eq_branch<Options, BranchOp, FailLabel, SuccessLabel, I-2>>(cpu, prev);
        }
    }

    template<typename Options, op_name_t BranchOp, typename FailLabel, typename SuccessLabel>
    struct eq_branch<Options, BranchOp, FailLabel, SuccessLabel, 0>
    {
        constexpr op_name_t Op = get_op(BranchOp, MODE_RELATIVE);

        chain
        < exact_op<OC, Op, null_, SuccessLabel>
        , clear_conditional
        >::run<finish>;
    }
    */

    template<typename Options, op_name_t BranchOp, typename FailLabel, typename SuccessLabel>
    void eq_branch(ssa_ht h)
    {
        using OC = typename Options::add_flags<OPT_CONDITIONAL>;
        constexpr op_t InverseOp = get_op(invert_branch(BranchOp), MODE_RELATIVE);

        for(unsigned i = 0; i < h->input_size(); i += 2)
        {
            select_step([&, i](cpu_t cpu, sel_t const* const prev)
            {
                for(unsigned j = 0; j < 2; ++j)
                {
                    p_lhs::set(h->input(i + j));
                    p_rhs::set(h->input(i + 1-j));
                    
                    if(p_rhs::value().eq_whole(0))
                    {
                        chain
                        < load_Z<OC, p_lhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);
                    }
                    else
                    {
                        chain
                        < load_A<OC, p_lhs>
                        , pick_op<OC, CMP, null_, p_rhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);

                        chain
                        < load_X<OC, p_lhs>
                        , pick_op<OC, CPX, null_, p_rhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);

                        chain
                        < load_Y<OC, p_lhs>
                        , pick_op<OC, CPY, null_, p_rhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);
                    }
                }
            });
        }

        constexpr op_t Op = get_op(BranchOp, MODE_RELATIVE);
        select_step(
            chain
            < exact_op<OC, Op, null_, SuccessLabel>
            , clear_conditional
            >::template run<finish>);
    }

    template<op_name_t BranchOp>
    void eq_store(ssa_ht h)
    {
        using fail = p_label<0>;
        using success = p_label<1>;
        using complete = p_label<2>;

        using O = options<>;

        fail::set(locator_t::minor_label(state.next_label++));
        success::set(locator_t::minor_label(state.next_label++));
        complete::set(locator_t::minor_label(state.next_label++));

        select_step([&](cpu_t cpu, sel_t const* const prev)
        {
            exact_op<O, LDA_IMMEDIATE, null_, const_<0>>
            ::template run<finish>(cpu, prev);

            exact_op<O, LDX_IMMEDIATE, null_, const_<0>>
            ::template run<finish>(cpu, prev);

            exact_op<O, LDY_IMMEDIATE, null_, const_<0>>
            ::template run<finish>(cpu, prev);

            finish::run(cpu, prev);
        });

        using OC = O::add_flags<OPT_CONDITIONAL>;
        eq_branch<options<>, BranchOp, fail, success>(h);

        p_def::set(h);
        
        select_step([&](cpu_t cpu, sel_t const* const prev)
        {
            if(cpu.regs[REG_X].eq_whole(0) && (O::can_set & REGF_X))
            {
                chain
                < label<OC, success>
                , exact_op<OC, INX_IMPLIED>
                , clear_conditional
                , label<O, fail>
                , set_regs<O, REGF_X, p_def>
                , store<O, STX, p_def>
                >::template run<finish>(cpu, prev);
            }
            else if(cpu.regs[REG_Y].eq_whole(0) && (O::can_set & REGF_Y))
            {
                chain
                < label<OC, success>
                , exact_op<OC, INY_IMPLIED>
                , clear_conditional
                , label<O, fail>
                , set_regs<O, REGF_Y, p_def>
                , store<O, STY, p_def>
                >::template run<finish>(cpu, prev);
            }
            else if(cpu.regs[REG_A].eq_whole(0) && (O::can_set & REGF_A))
            {
                chain
                < label<OC, success>
                , exact_op<OC, LDA_IMMEDIATE, null_, const_<1>>
                , clear_conditional
                , label<O, fail>
                , set_regs<O, REGF_Y, p_def>
                , store<O, STY, p_def>
                >::template run<finish>(cpu, prev);
            }
            else
            {
                chain
                < label<OC, fail>
                , exact_op<OC, LDA_ABSOLUTE, null_, const_<0>>
                , exact_op<OC, JMP_ABSOLUTE, null_, complete>
                , clear_conditional
                , label<OC, success>
                , exact_op<OC, LDA_ABSOLUTE, null_, const_<1>>
                , label<OC, complete>
                , clear_conditional
                , set_regs<O, REGF_A, p_def>
                , store<options<>, STA, p_def>
                >::template run<finish>(cpu, prev);

                chain
                < label<OC, fail>
                , exact_op<OC, LDX_ABSOLUTE, null_, const_<0>>
                , exact_op<OC, JMP_ABSOLUTE, null_, complete>
                , clear_conditional
                , label<OC, success>
                , exact_op<OC, LDX_ABSOLUTE, null_, const_<1>>
                , label<OC, complete>
                , clear_conditional
                , set_regs<O, REGF_X, p_def>
                , store<options<>, STX, p_def>
                >::template run<finish>(cpu, prev);

                chain
                < label<OC, fail>
                , exact_op<OC, LDY_ABSOLUTE, null_, const_<0>>
                , exact_op<OC, JMP_ABSOLUTE, null_, complete>
                , clear_conditional
                , label<OC, success>
                , exact_op<OC, LDY_ABSOLUTE, null_, const_<1>>
                , label<OC, complete>
                , clear_conditional
                , set_regs<O, REGF_Y, p_def>
                , store<options<>, STY, p_def>
                >::template run<finish>(cpu, prev);
            }
        });
    }

    template<typename Options, typename FailLabel, typename SuccessLabel>
    void lt_branch(ssa_ht h)
    {
        //if(OrEqual)
            //std::swap(fail_label, success_label);

        using OC = typename Options::add_flags<OPT_CONDITIONAL>;

        int const input_size = h->input_size();
        assert(input_size >= 2);
        assert(input_size % 2 == 0);

        using last_iter = condition<struct lt_last_iter_tag>;
        using next_label = param<struct lt_next_label_tag>;
        using maybe_next_label = param<struct lt_maybe_next_label_tag>;

        for(int i = input_size - 2; i >= 0; i -= 2)
        {
            last_iter::set(i == 0);
            next_label::set(locator_t::minor_label(state.next_label++));

            select_step([&, i](cpu_t cpu, sel_t const* const prev)
            {
                p_lhs::set(h->input(i + 0));
                p_rhs::set(h->input(i + 1));
                
                if(p_lhs::value().eq_whole(0))
                {
                    if(i == 0)
                    {
                        chain
                        < load_Z<OC, p_rhs>
                        , exact_op<OC, BNE_RELATIVE, null_, SuccessLabel>
                        >::template run<finish>(cpu, prev);
                    }
                    else
                    {
                        chain
                        < load_Z<OC, p_rhs>
                        , exact_op<OC, BNE_RELATIVE, null_, SuccessLabel>
                        >::template run<finish>(cpu, prev);
                    }
                }
                else if(p_rhs::value().eq_whole(0))
                {
                    chain
                    < load_Z<OC, p_rhs>
                    , exact_op<OC, BNE_RELATIVE, null_, SuccessLabel>
                    /* maybe BEQ to fail label */
                    >::template run<finish>(cpu, prev);

                    if(i == 0)
                        exact_op<OC, JMP_ABSOLUTE, null_, FailLabel>
                        ::template run<finish>(cpu, prev);
                    else
                        chain
                        < load_Z<OC, p_lhs>
                        , exact_op<OC, BNE_RELATIVE, null_, FailLabel>
                        /* maybe BEQ to fail label */
                        >::template run<finish>(cpu, prev);
                }
                else
                {
                    if(i == 0)
                        maybe_next_label::set(FailLabel::value());
                    else
                        maybe_next_label::set(next_label::value());

                    using normal = chain
                        < if_<OC, last_iter, null_, exact_op<OC, BEQ_RELATIVE, null_, next_label>>
                        , exact_op<OC, BCS_RELATIVE, null_, FailLabel>
                        , exact_op<OC, BCC_RELATIVE, null_, SuccessLabel>
                        , if_<OC, last_iter, null_, label<OC, next_label>>
                        >;

                    using inverted = chain
                        < if_<OC, last_iter, exact_op<OC, BEQ_RELATIVE, null_, FailLabel>,
                                             exact_op<OC, BEQ_RELATIVE, null_, next_label>>
                        , exact_op<OC, BCC_RELATIVE, null_, FailLabel>
                        , exact_op<OC, BCS_RELATIVE, null_, SuccessLabel>
                        , if_<OC, last_iter, null_, label<OC, next_label>>
                        >;

                    chain
                    < load_A<OC, p_lhs>
                    , pick_op<OC, CMP, null_, p_rhs>
                    , normal
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_A<OC, p_rhs>
                    , pick_op<OC, CMP, null_, p_lhs>
                    , inverted
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_X<OC, p_lhs>
                    , pick_op<OC, CPX, null_, p_rhs>
                    , normal
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_X<OC, p_rhs>
                    , pick_op<OC, CPX, null_, p_lhs>
                    , inverted
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_Y<OC, p_lhs>
                    , pick_op<OC, CPY, null_, p_rhs>
                    , normal
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_Y<OC, p_rhs>
                    , pick_op<OC, CPY, null_, p_lhs>
                    , inverted
                    >::template run<finish>(cpu, prev);
                }
            });
        }

        select_step(
            chain
            //< exact_op<OC, BCC_RELATIVE, null_, SuccessLabel>
            < clear_conditional
            >::template run<finish>);
    }

    template<typename Options>
    void write_globals(ssa_ht h)
    {
        for_each_written_global(h, [h](ssa_value_t def, locator_t loc)
        {
            if(def.is_handle())
            {
                //assert(def->op() == SSA_early_store || def->op() == SSA_aliased_store);

                //if(def->test_flags(FLAG_COALESCED))
                    //return;

                if(cset_locator(def.handle()) == loc)
                    return;

                def = def->input(0);
            }

            p_def::set(def);
            p_arg<0>::set(loc);

            select_step([loc, def](cpu_t cpu, sel_t const* prev)
            {
                load_then_store<Options, p_def, p_def, p_arg<0>>
                ::template run<finish>(cpu, prev);
            });
        });
    }

    void isel_node_simple(ssa_ht h, cpu_t cpu, sel_t const* prev)
    {
        auto const commutative = [](ssa_ht h, auto fn)
        {
            p_lhs::set(h->input(0));
            p_rhs::set(h->input(1));
            fn();

            p_lhs::set(h->input(1));
            p_rhs::set(h->input(0));
            fn();
        };

        cfg_ht const cfg_node = h->cfg_node();
        p_def::set(h);

        using O = options<>;
        using OC = O::add_flags<OPT_CONDITIONAL>;

        switch(h->op())
        {
        case SSA_carry:
            if(h->output_size() > 1 || (h->output_size() == 1 && h->output(0)->cfg_node() != h->cfg_node()))
            {
                p_label<0>::set(locator_t::minor_label(state.next_label++));

                chain
                < exact_op<O, AND_IMMEDIATE, null_, const_<0>>
                , exact_op<O, ROL_IMPLIED>
                , store<O, STA, p_def, false>
                >::run<finish>(cpu, prev);

                chain
                < load_X<O, const_<0>>
                , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<typename O::add_flags<OPT_CONDITIONAL>, INX_IMPLIED>
                , label<O, p_label<0>>
                , clear_conditional
                , store<O, STX, p_def, false>
                >::run<finish>(cpu, prev);

                chain
                < load_Y<O, const_<0>>
                , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<typename O::add_flags<OPT_CONDITIONAL>, INY_IMPLIED>
                , label<O, p_label<0>>
                , clear_conditional
                , store<O, STY, p_def, false>
                >::run<finish>(cpu, prev);
            }
            else
            {
                cpu_t new_cpu = cpu;
                new_cpu.set_reg<O, REG_C>(p_def::value());
                finish::run(new_cpu, &alloc_sel<O, MAYBE_STORE_C>(prev, p_def::trans()));
            }
            break;

        case SSA_add:
            p_carry::set(h->input(2));
            commutative(h, [&]()
            {
                chain
                < load_AC<O, p_lhs, p_carry>
                , pick_op<O, ADC, p_def, p_rhs>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AC<O, p_lhs, p_carry>
                , load_X<O::restrict_to<REGF_X>, p_rhs>
                , iota_op<O, ADC_ABSOLUTE_X, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AC<O, p_lhs, p_carry>
                , load_Y<O::restrict_to<REGF_Y>, p_rhs>
                , iota_op<O, ADC_ABSOLUTE_Y, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                if(p_rhs::value().is_const())
                {
                    p_label<0>::set(locator_t::minor_label(state.next_label++));

                    if(p_rhs::value().whole() == 0 && carry_output_i(*h) == -1)
                    {
                        chain
                        < load_C<O, p_carry>
                        , load_X<O, p_lhs>
                        , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, INX_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_X, p_def>
                        , store<O, STX, p_def>
                        >::run<finish>(cpu, prev);

                        chain
                        < load_C<O, p_carry>
                        , load_Y<O, p_lhs>
                        , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, INY_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_Y, p_def>
                        , store<O, STY, p_def>
                        >::run<finish>(cpu, prev);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            pick_op<O, INC, p_def, p_lhs>
                            ::run<finish>(cpu, prev);
                        }

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                            , pick_op<OC, INC, p_def, p_def>
                            , label<O, p_label<0>>
                            , clear_conditional
                            >::run<finish>(cpu, prev);
                        }
                    }

                    if(p_rhs::value().whole() == 0xFF && carry_output_i(*h) == -1)
                    {
                        p_label<0>::set(locator_t::minor_label(state.next_label++));

                        chain
                        < load_C<O, p_carry>
                        , load_X<O, p_lhs>
                        , exact_op<O, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, DEX_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_X, p_def>
                        , store<O, STX, p_def>
                        >::run<finish>(cpu, prev);

                        chain
                        < load_C<O, p_carry>
                        , load_Y<O, p_lhs>
                        , exact_op<O, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, DEY_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_Y, p_def>
                        , store<O, STY, p_def>
                        >::run<finish>(cpu, prev);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<O, BCS_RELATIVE, null_, p_label<0>>
                            , pick_op<OC, DEC, p_def, p_def>
                            , label<O, p_label<0>>
                            , clear_conditional
                            >::run<finish>(cpu, prev);
                        }
                    }

                    if(p_carry::value().is_const())
                    {
                        p_temp::set((0x100 - p_rhs::value().whole() - p_carry::value().carry()) & 0xFF);

                        chain
                        < load_AX<O, p_lhs, p_lhs>
                        , exact_op<O, AXS_IMMEDIATE, p_def, p_temp>
                        , store<O, STX, p_def>
                        >::run<finish>(cpu, prev);

                        if((p_rhs::value().whole() + p_carry::value().carry()) == 1 && carry_output_i(*h) == -1)
                        {
                            chain
                            < load_X<O, p_lhs>
                            , exact_op<O, INX_IMPLIED, p_def>
                            , store<O, STX, p_def>
                            >::run<finish>(cpu, prev);

                            chain
                            < load_Y<O, p_lhs>
                            , exact_op<O, INY_IMPLIED, p_def>
                            , store<O, STY, p_def>
                            >::run<finish>(cpu, prev);

                            if(p_def::trans() == p_lhs::trans())
                                pick_op<O, INC, p_def, p_def>
                                ::run<finish>(cpu, prev);
                        }

                        if(((p_rhs::value().whole() + p_carry::value().carry()) & 0xFF) == 0xFF && carry_output_i(*h) == -1)
                        {
                            chain
                            < load_X<O, p_lhs>
                            , exact_op<O, DEX_IMPLIED, p_def>
                            , store<O, STX, p_def>
                            >::run<finish>(cpu, prev);

                            chain
                            < load_Y<O, p_lhs>
                            , exact_op<O, DEY_IMPLIED, p_def>
                            , store<O, STY, p_def>
                            >::run<finish>(cpu, prev);

                            if(p_def::trans() == p_lhs::trans())
                                pick_op<O, DEC, p_def, p_def>
                                ::run<finish>(cpu, prev);
                        }
                    }
                }
            });
            break;

        case SSA_and:
            commutative(h, [&]()
            {
                chain
                < load_A<O, p_lhs>
                , pick_op<O, AND, p_def, p_rhs>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , exact_op<O, AXS_IMMEDIATE, p_def, const_<0>>
                , store<O, STX, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , iota_op<O, AND_ABSOLUTE_X, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AY<O, p_lhs, p_rhs>
                , iota_op<O, AND_ABSOLUTE_Y, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , store<O, SAX, p_def, false>
                >::run<finish>(cpu, prev);

                // TODO: consider using subtraction instructions,
                // checking constraints when it's applicable.
            });
            break;

        case SSA_or:
            commutative(h, [&]()
            {
                chain
                < load_A<O, p_lhs>
                , pick_op<O, ORA, p_def, p_rhs>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , iota_op<O, ORA_ABSOLUTE_X, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AY<O, p_lhs, p_rhs>
                , iota_op<O, ORA_ABSOLUTE_Y, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                // TODO: consider using add instructions, 
                // checking constraints when it's applicable.
            });
            break;

        case SSA_early_store:
            p_arg<0>::set(h->input(0));
            load_then_store<O, p_def, p_arg<0>, p_def>
            ::run<finish>(cpu, prev);
            break;

        case SSA_read_global:
            if(h->input(1).locator() != cset_locator(h))
            {
                p_arg<0>::set(h->input(1));
                load_then_store<O, p_def, p_arg<0>, p_def>
                ::run<finish>(cpu, prev);
            }
            else
                finish::run(cpu, prev);
            break;

        case SSA_phi_copy:
            if(!h->input(0).holds_ref() || cset_head(h) != cset_head(h->input(0).handle()))
            {
                p_arg<0>::set(h->input(0));
                load_then_store<O, p_def, p_arg<0>, p_def>
                ::run<finish>(cpu, prev);
            }
            else
                finish::run(cpu, prev);
            break;

        case SSA_phi:
            assert(h->input_size() > 0);
            if(cset_head(h) != cset_head(h->input(0).handle()))
            {
                p_arg<0>::set(h->input(0));
                load_then_store<O, p_def, p_arg<0>, p_def>
                ::run<finish>(cpu, prev);
            }
            else
                finish::run(cpu, prev);
            break;

        case SSA_fn_call:
            p_arg<0>::set(h->input(0));
            chain
            < exact_op<O, JSR_ABSOLUTE, null_, p_arg<0>>
            , set_regs<O, REGF_CPU, null_>
            >::run<finish>(cpu, prev);
            break;

        case SSA_return:
            exact_op<O, RTS_IMPLIED>
            ::run<finish>(cpu, prev);
            break;

        case SSA_jump:
            assert(cfg_node->output_size() == 1);
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            exact_op<O, JMP_ABSOLUTE, null_, p_label<0>>
            ::run<finish>(cpu, prev);
            break;

            /* TODO: remove?
        case SSA_if:
            (load_Z<>{ h->input(0) }
            >>= def_op<BNE>{ {}, locator_t::cfg_label(cfg_node->output(0)) }
            >>= def_op<BEQ>{ {}, locator_t::cfg_label(cfg_node->output(1)) }
            >>= finish)(cpu, prev);
            break;
            */

        case SSA_eq:
        case SSA_not_eq:
        case SSA_branch_eq:
        case SSA_branch_not_eq:
        case SSA_branch_lt:
        case SSA_entry:
        case SSA_aliased_store:
        case SSA_uninitialized:
            finish::run(cpu, prev);
            break;
        default:
            throw std::runtime_error(fmt("Unhandled SSA op in code gen: %i", h->op()));
        }
    }

    void isel_node(ssa_ht h)
    {
        using O = options<>;

        cfg_ht const cfg_node = h->cfg_node();

        switch(h->op())
        {
        case SSA_eq:     
            eq_store<BEQ>(h); 
            break;

        case SSA_not_eq: 
            eq_store<BNE>(h); 
            break;

        // Branch ops jump directly:
        case SSA_branch_eq:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            eq_branch<O, BEQ, p_label<0>, p_label<1>>(h);
            break;
        case SSA_branch_not_eq:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            eq_branch<O, BNE, p_label<0>, p_label<1>>(h);
            break;

        case SSA_branch_lt:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            lt_branch<O, p_label<0>, p_label<1>>(h);
            break;

            /*
        case SSA_branch_lte:
            lt_branch<true>(h, locator_t::cfg_label(cfg_node->output(0)), 
                               locator_t::cfg_label(cfg_node->output(1)));
            break;
            */

        case SSA_return:
        case SSA_fn_call:
            write_globals<O>(h);
            break;

        default: 
            break;
        }

        select_step([h](cpu_t cpu, sel_t const* prev)
        {
            isel_node_simple(h, cpu, prev);
        });
    }

} // namespace isel

std::vector<ainst_t> select_instructions(cfg_ht cfg_node)
{
    using namespace isel;

    auto& cd = cg_data(cfg_node);

    //////////////////////////////
    // SETUP THE ROLLING WINDOW //
    //////////////////////////////

    // A bitset is used to track which variables have been stored.
    // To shrink the bitset size down to 64 bits, a rolling window is used
    // based around the live ranges occuring within a single CFG node.

    // This code finds that rolling window and allocates each node to a bit:
    std::uint64_t free = ~0ull; // Set of availible bits to use
    for(ssa_ht h : cd.schedule)
    {
        if(free)
        {
            std::uint64_t const allocated = 1ull << builtin::ctz(free);
            assert(free && (free | allocated) == free);
            free &= ~allocated;

            ssa_ht last_use = h;
            for(unsigned j = 0; j < h->output_size(); ++j)
            {
                ssa_ht const output = h->output(j);
                // Assume that all stores used across basic block
                // boundaries must be stored. 
                // (They may not be in the final generated code.)
                // This is highly pessimistic, but simplifies the code gen.
                if(output->cfg_node() != h->cfg_node())
                    goto skip;

                if(cg_data(output).schedule.index > cg_data(last_use).schedule.index)
                    last_use = output;
            }

            cg_data(last_use).isel.last_use |= allocated;
            cg_data(h).isel.store_mask = allocated;
        }
    skip:
        // Reclaim bits after the last use has been seen:
        assert((free & cg_data(h).isel.last_use) == 0);
        free |= cg_data(h).isel.last_use;
    }

    ///////////////////////
    // DO THE SELECTIONS //
    ///////////////////////

    state.sel_pool.clear();
    state.map.clear();
    assert(state.map.empty());
    state.best_cost = ~0 - COST_CUTOFF;
    state.best_sel = nullptr;

    // Starting state:
    state.map.insert({ cpu_t{}, nullptr });

    for(ssa_ht h : cd.schedule)
        isel_node(h);

    std::vector<ainst_t> code;
    for(sel_t const* sel = state.best_sel; sel; sel = sel->prev)
        code.push_back(sel->inst);
    code.push_back({ ASM_LABEL, cfg_node.index });
    std::reverse(code.begin(), code.end());

    return code;
}
