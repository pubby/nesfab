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
        static_assert(op_output_regs(ADC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_N | REGF_C | REGF_V));

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
struct set_defs_for_impl<SEC_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        cpu.set_output_defs_impl<SEC_IMPLIED>(opt, def);
        cpu.set_known(REG_C, 1u);
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
struct set_defs_for_impl<LAX_BUGGY_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(LAX_BUGGY_IMMEDIATE) == (REGF_Z | REGF_N | REGF_A | REGF_X));

        cpu.set_output_defs_impl<LAX_BUGGY_IMMEDIATE>(opt, def);

        if(arg.is_const_num())
        {
            cpu.set_known(REG_A, arg.data());
            cpu.set_known(REG_X, arg.data());
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
        else if(cpu.is_known(REG_C))
        {
            bool c = cpu.known[REG_C] & 1;

            cpu.set_output_defs_impl<ROR_IMPLIED>(opt, def);
            if(c)
                cpu.set_known(REG_Z, 0);
            cpu.set_known(REG_N, c);
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
        static_assert(op_output_regs(SBC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_N | REGF_C | REGF_V));

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
    }
};

#ifndef LEGAL
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
        else if(cpu.is_known(REG_C))
        {
            bool c = cpu.known[REG_C] & 1;

            cpu.set_output_defs_impl<ARR_IMMEDIATE>(opt, def);
            if(c)
                cpu.set_known(REG_Z, 0);
            cpu.set_known(REG_N, c);
        }
        else
            cpu.set_output_defs_impl<ARR_IMMEDIATE>(opt, def);
    }
};
#endif

template<>
struct set_defs_for_impl<BEQ_RELATIVE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(BEQ_RELATIVE) == 0);
        cpu.conditional_regs |= REGF_Z;
        cpu.set_known(REG_Z, 0);
        assert(cpu.known_array_valid());
    }
};

template<>
struct set_defs_for_impl<BNE_RELATIVE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(BNE_RELATIVE) == 0);
        cpu.conditional_regs |= REGF_Z;
        cpu.set_known(REG_Z, 1);
        assert(cpu.known_array_valid());
    }
};

template<>
struct set_defs_for_impl<BMI_RELATIVE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(BMI_RELATIVE) == 0);
        cpu.conditional_regs |= REGF_N;
        cpu.set_known(REG_N, 0);
        assert(cpu.known_array_valid());
    }
};

template<>
struct set_defs_for_impl<BPL_RELATIVE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(BPL_RELATIVE) == 0);
        cpu.conditional_regs |= REGF_N;
        cpu.set_known(REG_N, 1);
        assert(cpu.known_array_valid());
    }
};

template<>
struct set_defs_for_impl<BCS_RELATIVE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(BCS_RELATIVE) == 0);
        cpu.conditional_regs |= REGF_C;
        cpu.set_known(REG_C, 0);
        assert(cpu.known_array_valid());
    }
};

template<>
struct set_defs_for_impl<BCC_RELATIVE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(BCC_RELATIVE) == 0);
        cpu.conditional_regs |= REGF_C;
        cpu.set_known(REG_C, 1);
        assert(cpu.known_array_valid());
    }
};

#ifdef ISA_65C02
template<>
struct set_defs_for_impl<INC_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(INC_IMPLIED) == (REGF_N | REGF_Z | REGF_A));

        if(cpu.are_known(REGF_A))
        {
            std::uint8_t const result = cpu.known[REG_A] + 1;

            cpu.set_output_defs_impl<INC_IMPLIED>(opt, def);
            cpu.set_known(REG_A, result);
            cpu.set_known(REG_Z, !result);
            cpu.set_known(REG_N, !!(result & 0x80));
        }
        else
            cpu.set_output_defs_impl<INC_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<DEC_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(DEC_IMPLIED) == (REGF_N | REGF_Z | REGF_A));

        if(cpu.are_known(REGF_A))
        {
            std::uint8_t const result = cpu.known[REG_A] - 1;

            cpu.set_output_defs_impl<INC_IMPLIED>(opt, def);
            cpu.set_known(REG_A, result);
            cpu.set_known(REG_Z, !result);
            cpu.set_known(REG_N, !!(result & 0x80));
        }
        else
            cpu.set_output_defs_impl<INC_IMPLIED>(opt, def);
    }
};

template<>
struct set_defs_for_impl<BIT_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(BIT_IMMEDIATE) == (REGF_Z));

        if(arg.is_const_num())
        {
            if(cpu.are_known(REGF_A))
            {
                std::uint8_t const result = cpu.known[REG_A] & arg.data();

                cpu.set_output_defs_impl<BIT_IMMEDIATE>(opt, def);
                cpu.set_known(REG_Z, !result);
            }
            else
            {
                cpu.set_output_defs_impl<BIT_IMMEDIATE>(opt, def);

                if(arg.data() == 0)
                    cpu.set_known(REG_Z, 1);
            }
        }
        else
            cpu.set_output_defs_impl<BIT_IMMEDIATE>(opt, def);
    }
};

#endif

#ifdef ISA_SNES

template<>
struct set_defs_for_impl<TXY_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(TXY_IMPLIED) == (REGF_Y | REGF_Z | REGF_N));

        cpu_t const old = cpu;

        if(!def || def.is_const_num())
            def = cpu.defs[REG_X];

        cpu.set_output_defs_impl<TXY_IMPLIED>(opt, def);
        assert(!cpu.is_known(REG_Y) && !cpu.is_known(REG_Z) && !cpu.is_known(REG_N));

        if(old.are_known(REGF_X))
        {
            cpu.set_known(REG_Y, old.known[REG_X]);
            cpu.set_known(REG_Z, !old.known[REG_X]);
            cpu.set_known(REG_N, !!(old.known[REG_X] & 0x80));
        }
    }
};

template<>
struct set_defs_for_impl<TYX_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(TYX_IMPLIED) == (REGF_X | REGF_Z | REGF_N));

        cpu_t const old = cpu;

        if(!def || def.is_const_num())
            def = cpu.defs[REG_Y];

        cpu.set_output_defs_impl<TYX_IMPLIED>(opt, def);
        assert(!cpu.is_known(REG_X) && !cpu.is_known(REG_Z) && !cpu.is_known(REG_N));

        if(old.are_known(REGF_Y))
        {
            cpu.set_known(REG_X, old.known[REG_Y]);
            cpu.set_known(REG_Z, !old.known[REG_Y]);
            cpu.set_known(REG_N, !!(old.known[REG_Y] & 0x80));
        }
    }
};

// NOTE: For switches, 'def' is ignored. Set it at the call site.
template<>
struct set_defs_for_impl<XBA_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(XBA_IMPLIED) == (REGF_A | REGF_A_HI | REGF_NZ));

        cpu.swap_regs<REG_A, REG_A_HI>(opt);
        cpu.set_defs<REG_N | REG_Z>(opt, locator_t());

        if(cpu.are_known(REGF_A))
        {
            cpu.set_known(REG_Z, !cpu.known[REG_A]);
            cpu.set_known(REG_N, !!(cpu.known[REG_A] & 0x80));
        }
    }
};

template<>
struct set_defs_for_impl<REP_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        if(arg.is_const_num())
        {
            if(arg.data() & PFLAG_C)
            {
                cpu.set_defs<REGF_C>(opt, def);
                cpu.set_known(REG_C, 0u);
            }

            if(arg.data() & PFLAG_Z)
            {
                cpu.set_defs<REGF_Z>(opt, def);
                cpu.set_known(REG_Z, 0u);
            }

            if(arg.data() & PFLAG_V)
            {
                cpu.set_defs<REGF_V>(opt, def);
                cpu.set_known(REG_V, 0u);
            }

            if(arg.data() & PFLAG_M)
            {
                cpu.set_defs<REGF_M16>(opt, def);
                cpu.set_known(REG_M16, 0u);
            }

            if(arg.data() & PFLAG_BX)
            {
                cpu.set_defs<REGF_X16>(opt, def);
                cpu.set_known(REG_X16, 0u);
            }
        }
        else
            cpu.set_output_defs_impl<REP_IMMEDIATE>(opt, def);
    }
};

template<>
struct set_defs_for_impl<SEP_IMMEDIATE>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        if(arg.is_const_num())
        {
            if(arg.data() & PFLAG_C)
            {
                cpu.set_defs<REGF_C>(opt, def);
                cpu.set_known(REG_C, 1u);
            }

            if(arg.data() & PFLAG_Z)
            {
                cpu.set_defs<REGF_Z>(opt, def);
                cpu.set_known(REG_Z, 1u);
            }

            if(arg.data() & PFLAG_V)
            {
                cpu.set_defs<REGF_V>(opt, def);
                cpu.set_known(REG_V, 1u);
            }

            if(arg.data() & PFLAG_M)
            {
                cpu.set_defs<REGF_M16>(opt, def);
                cpu.set_known(REG_M16, 1u);
            }

            if(arg.data() & PFLAG_BX)
            {
                cpu.set_defs<REGF_X16>(opt, def);
                cpu.set_known(REG_X16, 1u);
            }
        }
        else
            cpu.set_output_defs_impl<SEP_IMMEDIATE>(opt, def);
    }
};

#endif

#ifdef ISA_PCE

// NOTE: For switches, 'def' is ignored. Set it at the call site.
template<>
struct set_defs_for_impl<SAX_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(SAX_IMPLIED) == (REGF_A | REGF_X));
        cpu.swap_regs<REG_A, REG_X>(opt);
    }
};

// NOTE: For switches, 'def' is ignored. Set it at the call site.
template<>
struct set_defs_for_impl<SAY_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(SAY_IMPLIED) == (REGF_A | REGF_Y));
        cpu.swap_regs<REG_A, REG_Y>(opt);
    }
};

// NOTE: For switches, 'def' is ignored. Set it at the call site.
template<>
struct set_defs_for_impl<SXY_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(SXY_IMPLIED) == (REGF_X | REGF_Y));
        cpu.swap_regs<REG_X, REG_Y>(opt);
    }
};

template<>
struct set_defs_for_impl<CLA_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(CLA_IMPLIED) == (REGF_A));

        cpu.set_output_defs_impl<CLA_IMPLIED>(opt, def);
        cpu.set_known(REG_A, 0);
    }
};

template<>
struct set_defs_for_impl<CLX_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(CLX_IMPLIED) == (REGF_X));

        cpu.set_output_defs_impl<CLX_IMPLIED>(opt, def);
        cpu.set_known(REG_X, 0);
    }
};

template<>
struct set_defs_for_impl<CLY_IMPLIED>
{
    static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
    {
        static_assert(op_output_regs(CLY_IMPLIED) == (REGF_Y));

        cpu.set_output_defs_impl<CLY_IMPLIED>(opt, def);
        cpu.set_known(REG_Y, 0);
    }
};

#endif

template<op_t Op> [[gnu::noinline]]
std::enable_if_t<Op < NUM_NORMAL_OPS, bool> cpu_t::set_defs_for(options_t opt, locator_t def, locator_t arg)
{
    assert(def.lclass() != 0xFF);
    assert(arg.lclass() != 0xFF);

    constexpr regs_t Regs = op_output_regs(Op) & REGF_ISEL;
    if((Regs & opt.can_set) != Regs)
        return false;
    if((op_flags(Op) & ASMF_BRANCH) && !(conditional_regs & CONDITIONAL_EXEC))
        conditional_regs = CONDITIONAL_EXEC;
    conditional_regs |= Regs & REGF_ISEL;
    set_defs_for_impl<Op>::call(opt, *this, def, arg);
    return true;
}

// Explicit instantiations
#define OP(name, flags) template bool cpu_t::set_defs_for<name>(options_t, locator_t, locator_t);
#include "op.inc"
#undef OP

///////////
// CROSS //
///////////

cross_cpu_t::cross_cpu_t(cpu_t const& cpu, carry_t carry0, carry_t carry1, bool strip_phi)
{
    auto const convert = [&](regs_t reg) -> locator_t
    {
        if(!cpu.defs[reg] && (cpu.known_mask & (1 << reg)))
            return locator_t::const_byte(cpu.known[reg]);

        if(strip_phi && cpu.defs[reg].lclass() == LOC_PHI)
            return LOC_NONE;

        return cpu.defs[reg];
    };

    for(regs_t reg = 0; reg < NUM_CROSS_REGS; ++reg)
        defs[reg] = convert(reg);
    if(defs[REG_C].lclass() != LOC_CONST_BYTE)
    {
        if(carry_const(carry0) || carry_const(carry1))
        {
            if(carry0 == carry1)
                defs[REG_C] = locator_t::const_byte(carry0 == CARRY_SET);
            else
                defs[REG_C] = locator_t::carry_pair(carry0, carry1);
        }
        else
            defs[REG_C] = LOC_NONE;
    }
}

cpu_t cross_cpu_t::to_cpu() const
{
    static_assert(REGF_CROSS == (REGF_A | REGF_X | REGF_Y | REGF_C));
    cpu_t ret = {};
    ret.set_def<REG_A>({}, defs[REG_A]);
    ret.set_def<REG_X>({}, defs[REG_X]);
    ret.set_def<REG_Y>({}, defs[REG_Y]);
    ret.set_def<REG_C>({}, defs[REG_C]);
#ifndef NDEBUG
    for(locator_t def : ret.defs)
        assert(def.lclass() != 0xFF);
#endif
    return ret;
}

} // end namespace isel

