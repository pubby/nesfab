#include "cg_isel_cpu.hpp"

namespace isel
{

template<op_t Op>
struct set_defs_for_impl
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        cpu.set_output_defs<Op>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ADC_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ADC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_N | REGF_C));

        if(arg.is_const_num() && cpu.are_known(REGF_A | REGF_C))
        {
            unsigned const result = cpu.known[REG_A] + cpu.known[REG_C] + arg.data();

            cpu.set_output_defs_impl<ADC_IMMEDIATE>(opt, def);
            cpu.set_known(REG_A, result);
            cpu.set_known(REG_Z, !(result & 0xFF));
            cpu.set_known(REG_N, !!(result & 0x80));
            cpu.set_known(REG_C, !!(result & 0x100));
        }
        else
            cpu.set_output_defs_impl<ADC_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<AND_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(AND_IMMEDIATE) == (REGF_A | REGF_N | REGF_Z));

        if(arg.is_const_num())
        {
            if(cpu.are_known(REGF_A))
            {
                std::uint8_t const result = cpu.known[REG_A] & arg.data();

                cpu.set_output_defs_impl<AND_IMMEDIATE>(opt, def);
                cpu.set_known(REG_A, result);
                cpu.set_known(REG_Z, !result);
                cpu.set_known(REG_N, !!(result & 0x80));
            }
            else
            {
                cpu.set_output_defs_impl<AND_IMMEDIATE>(opt, def);

                if(arg.data() == 0)
                {
                    cpu.set_known(REG_A, 0);
                    cpu.set_known(REG_Z, 1);
                }

                if(!(arg.data() & 0x80))
                    cpu.set_known(REG_N, 0);
            }
        }
        else
            cpu.set_output_defs_impl<AND_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ASL_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ASL_IMPLIED) == (REGF_A | REGF_Z | REGF_N | REGF_C));

        if(cpu.are_known(REGF_A))
        {
            unsigned const result = cpu.known[REG_A] << 1;

            cpu.set_output_defs_impl<ASL_IMPLIED>(opt, def);
            cpu.set_known(REG_A, result);
            cpu.set_known(REG_Z, !(result & 0xFF));
            cpu.set_known(REG_N, !!(result & 0x80));
            cpu.set_known(REG_C, !!(result & 0x100));
        }
        else
            cpu.set_output_defs_impl<ASL_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<CLC_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        cpu.set_output_defs_impl<CLC_IMPLIED>(opt, def);
        cpu.set_known(REG_C, 0u);
    }
};

template<>
struct set_defs_for_impl<CMP_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(CMP_IMMEDIATE) == (REGF_Z | REGF_N | REGF_C));

        if(arg.is_const_num() && cpu.are_known(REGF_A))
        {
            bool const z = arg.data() == cpu.known[REG_A];
            bool const n = (arg.data() - cpu.known[REG_A]) & 0x80;
            bool const c = arg.data() <= cpu.known[REG_A];

            cpu.set_output_defs_impl<CMP_IMMEDIATE>(opt, def);
            cpu.set_known(REG_Z, z);
            cpu.set_known(REG_N, n);
            cpu.set_known(REG_C, c);
        }
        else if(arg.eq_const(0))
        {
            cpu.set_output_defs_impl<CMP_IMMEDIATE>(opt, def);
            cpu.set_known(REG_C, 1);
        }
        else
            cpu.set_output_defs_impl<CMP_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<CPX_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(CPX_IMMEDIATE) == (REGF_Z | REGF_N | REGF_C));

        if(arg.is_const_num() && cpu.are_known(REGF_X))
        {
            bool const z = arg.data() == cpu.known[REG_X];
            bool const n = (arg.data() - cpu.known[REG_X]) & 0x80;
            bool const c = arg.data() <= cpu.known[REG_X];

            cpu.set_output_defs_impl<CPX_IMMEDIATE>(opt, def);
            cpu.set_known(REG_Z, z);
            cpu.set_known(REG_N, n);
            cpu.set_known(REG_C, c);
        }
        else if(arg.is_const_num() && arg.data() == 0)
        {
            cpu.set_output_defs_impl<CPX_IMMEDIATE>(opt, def);
            cpu.set_known(REG_C, 1);
        }
        else
            cpu.set_output_defs_impl<CPX_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<CPY_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(CPY_IMMEDIATE) == (REGF_Z | REGF_N | REGF_C));

        if(arg.is_const_num() && cpu.are_known(REGF_Y))
        {
            bool const z = arg.data() == cpu.known[REG_Y];
            bool const n = (arg.data() - cpu.known[REG_Y]) & 0x80;
            bool const c = arg.data() <= cpu.known[REG_Y];

            cpu.set_output_defs_impl<CPY_IMMEDIATE>(opt, def);
            cpu.set_known(REG_Z, z);
            cpu.set_known(REG_N, n);
            cpu.set_known(REG_C, c);
        }
        else if(arg.is_const_num() && arg.data() == 0)
        {
            cpu.set_output_defs_impl<CPY_IMMEDIATE>(opt, def);
            cpu.set_known(REG_C, 1);
        }
        else
            cpu.set_output_defs_impl<CPY_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<DEX_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(DEX_IMPLIED) == (REGF_N | REGF_Z | REGF_X));

        if(cpu.are_known(REGF_X))
        {
            std::uint8_t const result = cpu.known[REG_X] - 1;

            cpu.set_output_defs_impl<DEX_IMPLIED>(opt, def);
            cpu.set_known(REG_X, result);
            cpu.set_known(REG_Z, !result);
            cpu.set_known(REG_N, !!(result & 0x80));
        }
        else
            cpu.set_output_defs_impl<DEX_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<DEY_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(DEY_IMPLIED) == (REGF_N | REGF_Z | REGF_Y));

        if(cpu.are_known(REGF_Y))
        {
            std::uint8_t const result = cpu.known[REG_Y] - 1;

            cpu.set_output_defs_impl<DEY_IMPLIED>(opt, def);
            cpu.set_known(REG_Y, result);
            cpu.set_known(REG_Z, !result);
            cpu.set_known(REG_N, !!(result & 0x80));
        }
        else
            cpu.set_output_defs_impl<DEY_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<EOR_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(EOR_IMMEDIATE) == (REGF_N | REGF_Z | REGF_A));

        if(cpu.are_known(REGF_A))
        {
            std::uint8_t const result = cpu.known[REG_A] ^ arg.data();

            cpu.set_output_defs_impl<EOR_IMMEDIATE>(opt, def);
            cpu.set_known(REG_Y, result);
            cpu.set_known(REG_Z, !result);
            cpu.set_known(REG_N, !!(result & 0x80));
        }
        else
            cpu.set_output_defs_impl<EOR_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<INX_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(INX_IMPLIED) == (REGF_N | REGF_Z | REGF_X));

        if(cpu.are_known(REGF_X))
        {
            std::uint8_t const result = cpu.known[REG_X] + 1;

            cpu.set_output_defs_impl<INX_IMPLIED>(opt, def);
            cpu.set_known(REG_X, result);
            cpu.set_known(REG_Z, !result);
            cpu.set_known(REG_N, !!(result & 0x80));
        }
        else
            cpu.set_output_defs_impl<INX_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<INY_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(INY_IMPLIED) == (REGF_N | REGF_Z | REGF_Y));

        if(cpu.are_known(REGF_Y))
        {
            std::uint8_t const result = cpu.known[REG_Y] + 1;

            cpu.set_output_defs_impl<INY_IMPLIED>(opt, def);
            cpu.set_known(REG_Y, result);
            cpu.set_known(REG_Z, !result);
            cpu.set_known(REG_N, !!(result & 0x80));
        }
        else
            cpu.set_output_defs_impl<INY_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<LDA_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(LDA_IMMEDIATE) == (REGF_Z | REGF_N | REGF_A));

        cpu.set_output_defs_impl<LDA_IMMEDIATE>(opt, def);

        if(arg.is_const_num())
        {
            cpu.set_known(REG_A, arg.data());
            cpu.set_known(REG_Z, !arg.data());
            cpu.set_known(REG_N, !!(arg.data() & 0x80));
        }
    }
};

template<>
struct set_defs_for_impl<LDX_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(LDX_IMMEDIATE) == (REGF_Z | REGF_N | REGF_X));

        cpu.set_output_defs_impl<LDX_IMMEDIATE>(opt, def);

        if(arg.is_const_num())
        {
            cpu.set_known(REG_X, arg.data());
            cpu.set_known(REG_Z, !arg.data());
            cpu.set_known(REG_N, !!(arg.data() & 0x80));
        }
    }
};

template<>
struct set_defs_for_impl<LDY_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(LDY_IMMEDIATE) == (REGF_Z | REGF_N | REGF_Y));

        cpu.set_output_defs_impl<LDY_IMMEDIATE>(opt, def);

        if(arg.is_const_num())
        {
            cpu.set_known(REG_Y, arg.data());
            cpu.set_known(REG_Z, !arg.data());
            cpu.set_known(REG_N, !!(arg.data() & 0x80));
        }
    }
};

template<>
struct set_defs_for_impl<LSR_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(LSR_IMPLIED) == (REGF_A | REGF_Z | REGF_N | REGF_C));

        if(cpu.are_known(REGF_A))
        {
            std::uint8_t const a = cpu.known[REG_A] >> 1;
            std::uint8_t const c  = cpu.known[REG_A] & 1;

            cpu.set_output_defs_impl<LSR_IMPLIED>(opt, def);
            cpu.set_known(REG_A, a);
            cpu.set_known(REG_Z, !a);
            cpu.set_known(REG_N, 0);
            cpu.set_known(REG_C, c);
        }
        else
            cpu.set_output_defs_impl<LSR_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ORA_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ORA_IMMEDIATE) == (REGF_A | REGF_N | REGF_Z));

        if(arg.is_const_num())
        {
            if(cpu.are_known(REGF_A))
            {
                std::uint8_t const result = cpu.known[REG_A] | arg.data();

                cpu.set_output_defs_impl<ORA_IMMEDIATE>(opt, def);
                cpu.set_known(REG_A, result);
                cpu.set_known(REG_Z, !result);
                cpu.set_known(REG_N, !!(result & 0x80));
            }
            else
            {
                cpu.set_output_defs_impl<ORA_IMMEDIATE>(opt, def);

                if(arg.data() == 0xFF)
                    cpu.set_known(REG_A, 0xFF);

                if(arg.data())
                    cpu.set_known(REG_Z, 0);

                if(arg.data() & 0x80)
                    cpu.set_known(REG_N, 1u);
            }
        }
        else
            cpu.set_output_defs_impl<ORA_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ROL_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ROL_IMPLIED) == (REGF_A | REGF_Z | REGF_N | REGF_C));

        if(cpu.are_known(REGF_A))
        {
            bool const c = cpu.known[REG_A] & 128;

            if(cpu.are_known(REGF_C))
            {
                std::uint8_t const a = (cpu.known[REG_A] << 1) | cpu.known[REG_C];

                cpu.set_output_defs_impl<ROL_IMPLIED>(opt, def);
                cpu.set_known(REG_A, a);
                cpu.set_known(REG_Z, !a);
                cpu.set_known(REG_N, !!(a & 0x80));
                cpu.set_known(REG_C, c);
            }
            else
            {
                cpu.set_output_defs_impl<ROL_IMPLIED>(opt, def);
                cpu.set_known(REG_C, c);
            }
        }
        else if(cpu.are_known(REGF_C) && cpu.known[REG_C])
        {
            cpu.set_output_defs_impl<ROL_IMPLIED>(opt, def);
            cpu.set_known(REG_Z, 0);
        }
        else
            cpu.set_output_defs_impl<ROL_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ROR_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ROR_IMPLIED) == (REGF_A | REGF_Z | REGF_N | REGF_C));

        if(cpu.are_known(REGF_A))
        {
            bool c = cpu.known[REG_A] & 1;

            if(cpu.are_known(REGF_C))
            {
                std::uint8_t const a = (cpu.known[REG_A] >> 1) | (!!cpu.known[REG_C] << 7);

                cpu.set_output_defs_impl<ROR_IMPLIED>(opt, def);
                cpu.set_known(REG_A, a);
                cpu.set_known(REG_Z, !a);
                cpu.set_known(REG_N, !!(a & 0x80));
                cpu.set_known(REG_C, c);
            }
            else
            {
                cpu.set_output_defs_impl<ROR_IMPLIED>(opt, def);
                cpu.set_known(REG_C, c);
            }
        }
        else if(cpu.are_known(REGF_C) && cpu.known[REG_C])
        {
            cpu.set_output_defs_impl<ROR_IMPLIED>(opt, def);
            cpu.set_known(REG_Z, 0);
            cpu.set_known(REG_N, 1);
        }
        else
            cpu.set_output_defs_impl<ROR_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<SBC_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(SBC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_N | REGF_C));

        if(arg.is_const_num() && cpu.are_known(REGF_A | REGF_C))
        {
            unsigned const result = cpu.known[REG_A] + cpu.known[REG_C] + ~arg.data();

            cpu.set_output_defs_impl<SBC_IMMEDIATE>(opt, def);
            cpu.set_known(REG_A, result);
            cpu.set_known(REG_Z, !(result & 0xFF));
            cpu.set_known(REG_N, !!(result & 0x80));
            cpu.set_known(REG_C, !!(result & 0x100));
        }
        else
            cpu.set_output_defs_impl<SBC_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<TAX_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(TAX_IMPLIED) == (REGF_X | REGF_Z | REGF_N));

        cpu_t const old = cpu;

        if(!def || def.is_const_num())
            def = cpu.defs[REG_A];

        cpu.set_output_defs_impl<TAX_IMPLIED>(opt, def);
        assert(!cpu.is_known(REG_X) && !cpu.is_known(REG_Z) && !cpu.is_known(REG_N));

        if(old.are_known(REGF_A))
        {
            cpu.set_known(REG_X, old.known[REG_A]);
            cpu.set_known(REG_Z, !old.known[REG_A]);
            cpu.set_known(REG_N, !!(old.known[REG_A] & 0x80));
        }
        else
        {
            if(old.are_known(REGF_Z))
                cpu.set_known(REG_Z, old.known[REG_Z]);

            if(old.are_known(REGF_N))
                cpu.set_known(REG_N, old.known[REG_N]);
        }
    }
};

template<>
struct set_defs_for_impl<TAY_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(TAY_IMPLIED) == (REGF_Y | REGF_Z | REGF_N));

        cpu_t const old = cpu;

        if(!def || def.is_const_num())
            def = cpu.defs[REG_A];

        cpu.set_output_defs_impl<TAY_IMPLIED>(opt, def);
        assert(!cpu.is_known(REG_Y) && !cpu.is_known(REG_Z) && !cpu.is_known(REG_N));

        if(old.are_known(REGF_A))
        {
            cpu.set_known(REG_Y, old.known[REG_A]);
            cpu.set_known(REG_Z, !old.known[REG_A]);
            cpu.set_known(REG_N, !!(old.known[REG_A] & 0x80));
        }
        else
        {
            if(old.are_known(REGF_Z))
                cpu.set_known(REG_Z, old.known[REG_Z]);

            if(old.are_known(REGF_N))
                cpu.set_known(REG_N, old.known[REG_N]);
        }
    }
};

template<>
struct set_defs_for_impl<TXA_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(TXA_IMPLIED) == (REGF_A | REGF_Z | REGF_N));

        cpu_t const old = cpu;

        if(!def || def.is_const_num())
            def = cpu.defs[REG_X];

        cpu.set_output_defs_impl<TXA_IMPLIED>(opt, def);
        assert(!cpu.is_known(REG_A) && !cpu.is_known(REG_Z) && !cpu.is_known(REG_N));

        if(old.are_known(REGF_X))
        {
            cpu.set_known(REG_A, old.known[REG_X]);
            cpu.set_known(REG_Z, !old.known[REG_X]);
            cpu.set_known(REG_N, !!(old.known[REG_X] & 0x80));
        }
        else
        {
            if(old.are_known(REGF_Z))
                cpu.set_known(REG_Z, old.known[REG_Z]);

            if(old.are_known(REGF_N))
                cpu.set_known(REG_N, old.known[REG_N]);
        }
    }
};

template<>
struct set_defs_for_impl<TYA_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(TYA_IMPLIED) == (REGF_A | REGF_Z | REGF_N));

        cpu_t const old = cpu;

        if(!def || def.is_const_num())
            def = cpu.defs[REG_Y];

        cpu.set_output_defs_impl<TYA_IMPLIED>(opt, def);
        assert(!cpu.is_known(REG_A) && !cpu.is_known(REG_Z) && !cpu.is_known(REG_N));

        if(old.are_known(REGF_Y))
        {
            cpu.set_known(REG_A, old.known[REG_Y]);
            cpu.set_known(REG_Z, !old.known[REG_Y]);
            cpu.set_known(REG_N, !!(old.known[REG_Y] & 0x80));
        }
        else
        {
            if(old.are_known(REGF_Z))
                cpu.set_known(REG_Z, old.known[REG_Z]);

            if(old.are_known(REGF_N))
                cpu.set_known(REG_N, old.known[REG_N]);
        }
    }
};

template<>
struct set_defs_for_impl<AXS_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(AXS_IMMEDIATE) == (REGF_Z | REGF_N | REGF_X | REGF_C));

        if(arg.is_const_num() && cpu.are_known(REGF_X | REGF_A))
        {
            unsigned const result = (cpu.known[REG_A] & cpu.known[REG_X]) + ~arg.data() + 1;

            cpu.set_output_defs_impl<AXS_IMMEDIATE>(opt, def);
            cpu.set_known(REG_X, result);
            cpu.set_known(REG_Z, !(result & 0xFF));
            cpu.set_known(REG_N, !!(result & 0x80));
            cpu.set_known(REG_C, !!(result & 0x100));
        }
        else if(arg.eq_const(0))
        {
            cpu.set_output_defs_impl<AXS_IMMEDIATE>(opt, def);
            cpu.set_known(REG_C, 1);
        }
        else
            cpu.set_output_defs_impl<AXS_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ANC_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ANC_IMMEDIATE) == (REGF_Z | REGF_N | REGF_A | REGF_C));

        if(arg.is_const_num())
        {
            if(cpu.are_known(REGF_A))
            {
                std::uint8_t const result = cpu.known[REG_A] & arg.data();

                cpu.set_output_defs_impl<ANC_IMMEDIATE>(opt, def);
                cpu.set_known(REG_A, result);
                cpu.set_known(REG_Z, !result);
                cpu.set_known(REG_N, !!(result & 0x80));
                cpu.set_known(REG_C, !!(result & 0x80));
            }
            else
            {
                cpu.set_output_defs_impl<ANC_IMMEDIATE>(opt, def);

                if(arg.data() == 0)
                {
                    cpu.set_known(REG_A, 0u);
                    cpu.set_known(REG_Z, 1u);
                }

                if(!(arg.data() & 0x80))
                {
                    cpu.set_known(REG_N, 0u);
                    cpu.set_known(REG_C, 0u);
                }
            }
        }
        else
            cpu.set_output_defs_impl<ANC_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ALR_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ALR_IMMEDIATE) == (REGF_Z | REGF_N | REGF_A | REGF_C));

        if(arg.is_const_num())
        {
            if(cpu.are_known(REGF_A))
            {
                std::uint8_t const and_ = cpu.known[REG_A] & arg.data();
                std::uint8_t const a = and_ >> 1;
                std::uint8_t const c = and_ & 1;

                cpu.set_output_defs_impl<ALR_IMMEDIATE>(opt, def);
                cpu.set_known(REG_A, a);
                cpu.set_known(REG_Z, !a);
                cpu.set_known(REG_N, 0);
                cpu.set_known(REG_C, c);
            }
            else
            {
                cpu.set_output_defs_impl<ALR_IMMEDIATE>(opt, def);

                if((arg.data() >> 1) == 0)
                {
                    cpu.set_known(REG_A, 0);
                    cpu.set_known(REG_Z, 1);
                }

                if(arg.data() & 1)
                    cpu.set_known(REG_C, 0);

                cpu.set_known(REG_N, 0);
            }
        }
        else
            cpu.set_output_defs_impl<ALR_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<ARR_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(ARR_IMMEDIATE) == (REGF_Z | REGF_N | REGF_A | REGF_C));

        if(arg.is_const_num() && cpu.are_known(REGF_A))
        {
            std::uint8_t const and_ = cpu.known[REG_A] & arg.data();
            std::uint8_t const c = !!(and_ & 128);

            if(cpu.are_known(REGF_C))
            {
                std::uint8_t const a = (and_ >> 1) | (cpu.known[REG_C] << 7);

                cpu.set_output_defs_impl<ARR_IMMEDIATE>(opt, def);
                cpu.set_known(REG_A, a);
                cpu.set_known(REG_Z, !a);
                cpu.set_known(REG_N, !!(a & 0x80));
                cpu.set_known(REG_C, c);
            }
            else
            {
                cpu.set_output_defs_impl<ALR_IMMEDIATE>(opt, def);
                cpu.set_known(REG_C, c);
            }
        }
        else if(cpu.are_known(REGF_C) && cpu.known[REG_C])
        {
            cpu.set_output_defs_impl<ARR_IMMEDIATE>(opt, def);
            cpu.set_known(REG_Z, 0);
            cpu.set_known(REG_N, 1);
        }
        else
            cpu.set_output_defs_impl<ARR_IMMEDIATE>(opt, def);
    }
};

template<op_t Op> [[gnu::noinline]]
bool cpu_t::set_defs_for(options_t opt, locator_t def, locator_t arg)
{
    constexpr regs_t Regs = op_output_regs(Op) & REGF_CPU;
    if((Regs & opt.can_set) != Regs)
        return false;
    if(opt.flags & OPT_CONDITIONAL)
        conditional_regs |= Regs;
    set_defs_for_impl<Op>::call(opt, *this, def, arg);
    return true;
}

// Explicit instantiations
#define OP(name) template bool cpu_t::set_defs_for<name>(options_t, locator_t, locator_t);
#include "op.inc"
#undef OP

} // end namespace isel

