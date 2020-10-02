#include "cg_isel.hpp"

#include <cstdint>
#include <iostream> // TODO
#include <functional>
#include <vector>

#include "robin/hash.hpp"
#include "robin/map.hpp"

#include "array_pool.hpp"


// An approximation of the CPU's state at a given position.
struct cpu_t
{
    std::array<ssa_value_t, NUM_CPU_REGS> regs;

    // This bitset keeps track of which variables must be stored.
    // To shrink the size down to 64 bits, a rolling window is used
    // based around the live ranges occuring within a single CFG node.
    std::uint64_t req_store;

    // When implementing minor branches (such as in multi-byte comparisons),
    // some registers will be conditionally set.
    // These flags track which registers are conditional:
    regs_t conditional_regs;

    // Note: doesn't consider 'temp_regs'.
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
};

namespace std
{
    template<>
    struct hash<cpu_t>
    {
        // Note: doesn't consider 'temp_regs'.
        std::size_t operator()(cpu_t const& cpu) const noexcept
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
    // Options to be passed to various construction functions:
    struct opt_t
    {
        regs_t can_set = REGF_CPU;
        bool conditional = false;
        
        constexpr opt_t mask(regs_t mask) const
        {
            return { can_set & mask, conditional };
        }
    };

    /* TODO
    // Extra data added onto the schedule:
    struct isel_schedule_d
    {
        std::uint64_t store_mask = 0;
        std::uint64_t last_use = 0;
    };
    */

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

    ssa_value_t mem_arg(ssa_value_t v)
    {
        if(v.holds_ref())
            if(locator_t loc = cset_locator(v.handle()))
                return loc;
        return v;
    }

    unsigned get_cost(sel_t const* sel)
    {
        return sel ? sel->cost : 0;
    }

    template<op_t Op>
    constexpr unsigned op_penalty()
    {
        switch(op_name(Op))
        {
        default: return 0;
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
    constexpr unsigned cost_fn = 
        (op_cycles(Op) << 8) + op_size(Op) + op_penalty<Op>();

    // THIS DETERMINES HOW BIG THE MAP GETS, AND HOW WIDE THE SEARCH IS:
    constexpr unsigned COST_CUTOFF = cost_fn<LDA_ABSOLUTE> * 3;

    template<op_t Op, typename... Ts>
    sel_t& alloc_sel(sel_t const* prev, ssa_value_t arg, 
                     unsigned extra_cost = 0)
    {
        unsigned const cost = get_cost(prev) + cost_fn<Op> + extra_cost;
        return state.sel_pool.emplace(prev, cost, ainst_t{ Op, arg });
    }

    template<typename A, typename B> [[gnu::flatten]]
    auto operator>>=(A const& a, B const& b)
    { 
        return [&](cpu_t cpu, sel_t const* prev)
        { 
            a(cpu, prev, b); 
        };
    }

    template<typename Fn>
    void select_step(Fn fn)
    {
        state.next_map.clear();
        state.next_best_cost = ~0 - COST_CUTOFF;

        for(auto const& pair : state.map)
            if(get_cost(pair.second) < state.best_cost + COST_CUTOFF)
                fn(pair.first, pair.second);

        assert(!state.next_map.empty());
        state.map.swap(state.next_map);
        state.best_cost = state.next_best_cost;
    }


    template<typename Fn>
    struct if_
    {
        bool b;
        Fn fn;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            if(b)
                fn(cpu, prev, cont);
            else
                cont(cpu, prev);
        }
    };

    template<regs_t Regs, opt_t Opt = opt_t{}>
    struct set_regs
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(is_orig_def(v));

            if((Regs & Opt.can_set) == Regs)
            {
                if(Opt.conditional)
                    cpu.conditional_regs |= Regs;
                if(Regs & REGF_A)
                    cpu.regs[REG_A] = v;
                if(Regs & REGF_X)
                    cpu.regs[REG_X] = v;
                if(Regs & REGF_Y)
                    cpu.regs[REG_Y] = v;
                if(Regs & REGF_C)
                {
                    cpu.regs[REG_C] = v;
                    assert(!v.is_num() || v.whole() <= 1);
                }
                if(Regs & REGF_Z)
                    cpu.regs[REG_Z] = v;
                cont(cpu, prev);
            }
            else
                assert(false);
        }
    };

    struct clear_conditional
    {
        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
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
            cont(cpu, prev);
        }
    };

    template<op_t Op, opt_t Opt>
    using set_regs_for = set_regs<op_output_regs(Op) & REGF_CPU, Opt>;

    template<op_t Op, opt_t Opt>
    constexpr bool can_set_regs_for = 
        ((Opt.can_set & op_output_regs(Op) & REGF_CPU) 
         == (op_output_regs(Op) & REGF_CPU));

    template<op_name_t OpName, opt_t Opt = opt_t{}>
    struct def_op
    {
        ssa_value_t def;
        ssa_value_t arg;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const;
    };

    template<opt_t Opt = opt_t{}>
    struct load_A
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(v);
            assert(is_orig_def(v));
            //assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            ssa_value_t def = orig_def(v);
            ssa_value_t mem = mem_arg(v);

            if(cpu.reg_eq(REG_A, def))
                cont(cpu, prev);
            else if((Opt.can_set & REGF_A) == 0)
            {
                assert(false);
                return; // Abort!
            }
            else if(cpu.reg_eq(REG_X, def))
                def_op<TXA, Opt>{def}(cpu, prev, cont);
            else if(cpu.reg_eq(REG_Y, def))
                def_op<TYA, Opt>{def}(cpu, prev, cont);
            else
            {
                def_op<LDA, Opt>{def, mem}(cpu, prev, cont);
                if((Opt.can_set & REGF_X) && !def.is_num())
                    def_op<LAX, Opt>{def, mem}(cpu, prev, cont);
                if((Opt.can_set & REGF_C) && def.is_num() && def.whole() == 0)
                    def_op<ANC, Opt>{0u, 0u}(cpu, prev, cont);

            }
        }
    };

    template<opt_t Opt = opt_t{}>
    struct load_X
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(v);
            assert(is_orig_def(v));
            //assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            ssa_value_t def = orig_def(v);
            ssa_value_t mem = mem_arg(v);

            if(cpu.reg_eq(REG_X, def))
                cont(cpu, prev);
            else if((Opt.can_set & REGF_X) == 0)
            {
                assert(false);
                return; // Abort!
            }
            else if(cpu.reg_eq(REG_A, def))
                def_op<TAX, Opt>{def}(cpu, prev, cont);
            else
            {
                def_op<LDX, Opt>{def, mem}(cpu, prev, cont);
                if((Opt.can_set & REGF_A) && !def.is_num())
                    def_op<LAX, Opt>{def, mem}(cpu, prev, cont);
            }
        }
    };

    template<opt_t Opt = opt_t{}>
    struct load_Y
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(v);
            assert(is_orig_def(v));
            //assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            ssa_value_t def = orig_def(v);
            ssa_value_t mem = mem_arg(v);

            if(cpu.reg_eq(REG_Y, def))
                cont(cpu, prev);
            else if((Opt.can_set & REGF_Y) == 0)
            {
                assert(false);
                return; // Abort!
            }
            else if(cpu.reg_eq(REG_A, def))
                def_op<TAY, Opt>{def}(cpu, prev, cont);
            else
                def_op<LDY, Opt>{def, mem}(cpu, prev, cont);
        }
    };

    template<opt_t Opt = opt_t{}>
    struct load_AX
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(v);
            assert(is_orig_def(v));
            //assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            ssa_value_t def = orig_def(v);
            ssa_value_t mem = mem_arg(v);

            if(cpu.reg_eq(REG_A, def) && cpu.reg_eq(REG_X, def))
                cont(cpu, prev);
            else if((Opt.can_set & REGF_AX) != REGF_AX)
            {
                assert(false);
                return; // Abort!
            }
            else if(cpu.reg_eq(REG_X, def))
                def_op<TXA, Opt>{def}(cpu, prev, cont);
            else if(cpu.reg_eq(REG_A, def))
                def_op<TAX, Opt>{def}(cpu, prev, cont);
            else if(v.is_num())
                (def_op<LDA, Opt>{def, mem}
                 >>= def_op<TAX, Opt>{def}
                 >>= cont)(cpu, prev);
            else
                def_op<LAX, Opt>{def, mem}(cpu, prev, cont);
        }
    };

    template<opt_t Opt = opt_t{}>
    struct load_Z
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(v);
            assert(is_orig_def(v));
            ////assert(!v.holds_ref() 
                   //|| v->type() == TYPE_BYTE
                   //|| v->type() == TYPE_BOOL);

            ssa_value_t def = orig_def(v);
            ssa_value_t mem = mem_arg(v);

            if(cpu.reg_eq(REG_Z, def))
                cont(cpu, prev);
            else if((Opt.can_set & REGF_Z) == 0)
            {
                assert(false);
                return; // Abort!
            }
            else
            {
                if(cpu.reg_eq(REG_A, def))
                {
                    def_op<EOR, Opt>{def, 0u}(cpu, prev, cont);
                    def_op<TAX, Opt>{def}(cpu, prev, cont);
                    def_op<TAY, Opt>{def}(cpu, prev, cont);
                }
                else
                    def_op<LDA, Opt>{def, mem}(cpu, prev, cont);

                if(cpu.reg_eq(REG_X, def))
                {
                    (def_op<INX, Opt>{}
                    >>= def_op<DEX, Opt>{}
                    >>= set_regs<REGF_X | REGF_Z, Opt>{def}
                    >>= cont)(cpu, prev);

                    (def_op<CPX, Opt>{{}, 0u}
                    >>= set_regs<REGF_X | REGF_Z, Opt>{def}
                    >>= cont)(cpu, prev);

                    def_op<TXA, Opt>{def}(cpu, prev, cont);
                }
                else
                    def_op<LDX, Opt>{def, mem}(cpu, prev, cont);

                if(cpu.reg_eq(REG_Y, def))
                {
                    (def_op<INY, Opt>{}
                    >>= def_op<DEY, Opt>{}
                    >>= set_regs<REGF_Y | REGF_Z, Opt>{def}
                    >>= cont)(cpu, prev);

                    (def_op<CPY, Opt>{{}, 0u}
                    >>= set_regs<REGF_Y | REGF_Z, Opt>{def}
                    >>= cont)(cpu, prev);

                    def_op<TYA, Opt>{def}(cpu, prev, cont);
                }
                else
                    def_op<LDY, Opt>{def, mem}(cpu, prev, cont);

                if(!cpu.reg_eq(REG_A, def) && !cpu.reg_eq(REG_X, def)
                   && !def.is_num())
                {
                    def_op<LAX, Opt>{def, mem}(cpu, prev, cont);
                }
            }
        }
    };

    template<opt_t Opt = opt_t{}>
    struct load_CA
    {
        ssa_value_t c;
        ssa_value_t a;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(a && c);

            if(!cpu.reg_eq(REG_C, c))
            {
                if((Opt.can_set & REGF_AC) != REGF_AC)
                    return;

                ssa_value_t cdef = orig_def(c);
                //ssa_value_t cmem = mem_arg(c);

                ssa_value_t adef = orig_def(a);

                if(c.is_handle())
                {
                    assert(c.handle());

                    (load_A<Opt>{ c }
                    >>= def_op<CMP>{ {}, 1u }
                    >>= set_regs<REGF_C, Opt>{ cdef }
                    >>= cont)(cpu, prev);

                    (load_X<Opt>{ c }
                    >>= def_op<CPX>{ {}, 1u }
                    >>= set_regs<REGF_C, Opt>{ cdef }
                    >>= cont)(cpu, prev);

                    (load_Y<Opt>{ c }
                    >>= def_op<CPY>{ {}, 1u }
                    >>= set_regs<REGF_C, Opt>{ cdef }
                    >>= cont)(cpu, prev);
                }
                else if(c.is_num())
                {
                    // TODO: use constraints instead
                    if(c.carry())
                    {
                        (def_op<SEC, Opt>{ cdef }
                        >>= load_A<Opt.mask(~REGF_C)>{ a }
                        >>= cont)(cpu, prev);

                        if((Opt.can_set & REGF_A) && a.is_num() 
                           && cpu.regs[REG_A].is_num())
                        {
                            unsigned const r = cpu.regs[REG_A].whole();
                            if((r & 1) == 1 && (r >> 1) == a.whole())
                                (def_op<LSR, Opt>{ adef }
                                >>= set_regs<REGF_C, Opt>{ 1u }
                                >>= cont)(cpu, prev);
                        }
                    }
                    else
                    {
                        (def_op<CLC, Opt>{ cdef }
                        >>= load_A<Opt.mask(~REGF_C)>{a}
                        >>= cont)(cpu, prev);

                        if((Opt.can_set & REGF_A) && a.is_num() 
                           && cpu.regs[REG_A].is_num())
                        {
                            unsigned const r = cpu.regs[REG_A].whole();
                            if((r >> 1) == a.whole())
                            {
                                if((r & 1) == 0)
                                    (def_op<LSR, Opt>{ adef }
                                     >>= set_regs<REGF_C, Opt>{ 0u }
                                     >>= cont)(cpu, prev);
                                else
                                    (def_op<ALR, Opt>{ adef, 0xFEu }
                                     >>= set_regs<REGF_C, Opt>{ 0u }
                                     >>= cont)(cpu, prev);
                            }

                            if(a.whole() == 0)
                                def_op<ANC, Opt>{0u, 0u}(cpu, prev, cont);
                        }
                    }
                }
                else
                    assert(false);
            }
            else if((Opt.can_set & REGF_C) == 0)
                return; // Abort!
            else
                return load_A<Opt>{a}(cpu, prev, cont);
        }
    };

    template<op_name_t OpName, opt_t Opt, 
        bool Enable = (Opt.can_set & REGF_X) 
        && get_op(OpName, MODE_ABSOLUTE_X) != BAD_OP>
    struct impl_addr_X
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, cpu_t cpu, 
                         sel_t const* prev, Cont cont)
        {
            constexpr op_t absolute_X = get_op(OpName, MODE_ABSOLUTE_X);
            static_assert(absolute_X);

            load_X<Opt.mask(REGF_X)>{h->input(1)}(cpu, prev, 
            [=](cpu_t cpu, sel_t const* prev)
            {
                set_regs_for<absolute_X, Opt>{def}(
                    cpu, &alloc_sel<absolute_X>(prev, h->input(0)), cont);
            });
        }
    };

    template<op_name_t OpName, opt_t Opt>
    struct impl_addr_X<OpName, Opt, false>
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, cpu_t cpu, 
                         sel_t const* prev, Cont cont)
        {}
    };

    template<op_name_t OpName, opt_t Opt, 
        bool Enable = (Opt.can_set & REGF_Y) 
        && get_op(OpName, MODE_ABSOLUTE_Y) != BAD_OP>
    struct impl_addr_Y
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, cpu_t cpu, 
                         sel_t const* prev, Cont cont)
        {
            constexpr op_t absolute_Y = get_op(OpName, MODE_ABSOLUTE_Y);
            static_assert(absolute_Y);

            load_Y<Opt.mask(REGF_Y)>{h->input(1)}(cpu, prev, 
            [=](cpu_t cpu, sel_t const* prev)
            {
                set_regs_for<absolute_Y, Opt>{def}(
                    cpu, &alloc_sel<absolute_Y>(prev, h->input(0)), cont);
            });
        }
    };

    template<op_name_t OpName, opt_t Opt>
    struct impl_addr_Y<OpName, Opt, false>
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, cpu_t cpu, 
                         sel_t const* prev, Cont cont)
        {}
    };

    template<op_name_t OpName, opt_t Opt>
    template<typename Cont>
    void def_op<OpName, Opt>
    ::operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
    {
        constexpr op_t implied    = get_op(OpName, MODE_IMPLIED);
        constexpr op_t relative   = get_op(OpName, MODE_RELATIVE);
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);

        if(implied && !arg)
        {
            if(can_set_regs_for<implied, Opt>)
                set_regs_for<implied, Opt>{def}
                    (cpu, &alloc_sel<implied>(prev, {}), cont);
            else
                assert(false);
        }
        else if(relative && arg.is_locator())
        {
            assert(arg.locator().is_label());
            cont(cpu, &alloc_sel<relative>(prev, arg));
        }
        else if(immediate && arg.is_num())
        {
            if(can_set_regs_for<immediate, Opt>)
                set_regs_for<immediate, Opt>{def}
                    (cpu, &alloc_sel<immediate>(prev, arg), cont);
            else
                assert(false);
        }
        else if(absolute && !arg.is_num())
        {
            ssa_value_t mem = mem_arg(arg);

            if(mem.is_const())
            {
                if(can_set_regs_for<absolute, Opt>)
                    set_regs_for<absolute, Opt>{def}
                        (cpu, &alloc_sel<absolute>(prev, mem), cont);
                else
                    assert(false);

                return;
            }

            assert(mem.is_handle());
            ssa_ht h = mem.handle();
            auto& d = cg_data(h);

            unsigned store_penalty = cost_fn<STA_ABSOLUTE> / 2;
            cpu_t new_cpu = cpu;
            if(h->cfg_node() == state.cfg_node && d.isel.store_mask)
            {
                // Nodes that belong to this CFG node are tracked more
                // precisely using the 'req_store' part of 'cpu_t'.

                unsigned const stores = 
                    builtin::popcount(~cpu.req_store & d.isel.store_mask);

                // This may equal 0 if 'req_store' already holds the bits.
                store_penalty = cost_fn<STA_ABSOLUTE> * stores;

                new_cpu.req_store |= d.isel.store_mask;
            }

            if(can_set_regs_for<absolute, Opt>)
            {
                (set_regs_for<absolute, Opt>{def} >>= cont)
                    (new_cpu, &alloc_sel<absolute>(prev, h, store_penalty));

                // If the node(s) were already stored, no point in checking
                // other methods of loading:
                if(store_penalty == 0)
                    return;
            }
            else
                assert(false);

            // If the node is an array read, try loading either X or Y
            // with the index, then using an indexed addressing mode.
            if(h->op() == SSA_read_array)
            {
                impl_addr_X<OpName, Opt>::func(
                    def, h, cpu, prev, cont);

                impl_addr_Y<OpName, Opt>::func(
                    def, h, cpu, prev, cont);
            }
        }
        else
            assert(false);
    }

    struct label
    {
        locator_t label;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            assert(label.is_label());
            cont(cpu, &alloc_sel<ASM_LABEL>(prev, label));
        }
    };

    void finish(cpu_t cpu, sel_t const* sel)
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

    template<op_name_t StoreOp, bool Maybe = true>
    struct store
    {
        ssa_value_t v;

        template<typename Cont>
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            // Store the node in whatever locator needs it:
            if(v.holds_ref())
                for(locator_t loc : cg_data(v.handle()).store_in_locs)
                    prev = &alloc_sel<get_op(StoreOp, MODE_ABSOLUTE)>(
                        prev, loc);

            ssa_value_t const mem = mem_arg(v);

            // Store the node, locally:
            if(Maybe && mem.holds_ref())
            {
                switch(StoreOp)
                {
                case STA: cont(cpu, &alloc_sel<MAYBE_STA>(prev, mem)); break;
                case STX: cont(cpu, &alloc_sel<MAYBE_STX>(prev, mem)); break;
                case STY: cont(cpu, &alloc_sel<MAYBE_STY>(prev, mem)); break;
                case SAX: cont(cpu, &alloc_sel<MAYBE_SAX>(prev, mem)); break;
                default: assert(false); break;
                }
            }
            else
                cont(cpu, 
                     &alloc_sel<get_op(StoreOp, MODE_ABSOLUTE)>(prev, mem));
        }

        void operator()(cpu_t cpu, sel_t const* prev) const
        {
            operator()(cpu, prev, finish);
        }
    };

    void store_carry(ssa_ht h)
    {
        ssa_value_t const mem = mem_arg(h);

        if(cg_data(h).store_in_locs.size() || h->output_size() > 1
           || (h->output_size() == 1 
               && h->output(0)->cfg_node() != h->cfg_node()))
        {
            locator_t const lbl = locator_t::minor_label(state.next_label++);

            select_step(
                def_op<ARR>{ {}, 0 }
                >>= store<STA, false>{h});

            constexpr opt_t opt = { .can_set = ~REGF_C, .conditional = true };

            select_step(
                load_X<opt>{ 0u }
                >>= def_op<BCC>{{}, lbl }
                >>= def_op<INX, opt>{ {}, lbl }
                >>= label{ lbl }
                >>= clear_conditional{}
                >>= set_regs<REGF_C>{ orig_def(h) }
                >>= store<STX, false>{mem});

            select_step(
                load_Y<opt>{ 0u }
                >>= def_op<BCC>{{}, lbl }
                >>= def_op<INY, opt>{ {}, lbl }
                >>= label{ lbl }
                >>= clear_conditional{}
                >>= set_regs<REGF_C>{ orig_def(h) }
                >>= store<STY, false>{mem});
        }
        else
        {
            select_step([h, mem](cpu_t cpu, sel_t const* prev)
            {
                cpu.regs[REG_C] = orig_def(h);
                assert(cpu.reg_eq(REG_C, orig_def(h)));
                finish(cpu, &alloc_sel<MAYBE_STORE_C>(prev, mem));
            });
        }
    }

    template<op_name_t BranchOp>
    void eq_branch(ssa_ht h, locator_t fail_label, locator_t success_label)
    {
        constexpr op_name_t InverseOp = invert_branch(BranchOp);
        for(unsigned i = 0; i < h->input_size(); i += 2)
        {
            select_step([&, i](cpu_t cpu, sel_t const* const prev)
            {
                constexpr opt_t opt = { .conditional = true };

                for(unsigned j = 0; j < 2; ++j)
                {
                    ssa_value_t lhs = h->input(i + j);
                    ssa_value_t rhs = h->input(i + 1-j);

                    bool const rhs_0 = rhs.is_const() && rhs.whole() == 0;

                    (load_A<opt>{ lhs }
                    >>= if_(!rhs_0, def_op<CMP, opt>{ {}, rhs })
                    >>= def_op<InverseOp, opt>{ {}, fail_label }
                    >>= finish)(cpu, prev);

                    (load_X<opt>{ lhs }
                    >>= if_(!rhs_0, def_op<CPX, opt>{ {}, rhs })
                    >>= def_op<InverseOp, opt>{ {}, fail_label }
                    >>= finish)(cpu, prev);

                    (load_Y<opt>{ lhs }
                    >>= if_(!rhs_0, def_op<CPY, opt>{ {}, rhs })
                    >>= def_op<InverseOp, opt>{ {}, fail_label }
                    >>= finish)(cpu, prev);
                }
            });
        }

        constexpr opt_t opt = { .conditional = true };

        select_step(
            def_op<BranchOp, opt>{ {}, success_label }
            >>= clear_conditional{}
            >>= finish);
    }

    template<op_name_t BranchOp>
    void eq_store(ssa_ht h)
    {
        locator_t fail     = locator_t::minor_label(state.next_label++);
        locator_t success  = locator_t::minor_label(state.next_label++);
        locator_t complete = locator_t::minor_label(state.next_label++);

        eq_branch<BranchOp>(h, fail, success);

        constexpr opt_t opt = { .conditional = true };

        select_step(
            label{ fail }
            >>= def_op<LDA, opt>{ {}, 0 }
            >>= def_op<JMP>{ {}, complete }
            >>= label{ success }
            >>= def_op<LDA, opt>{ {}, 1 }
            >>= label{ complete }
            >>= clear_conditional{}
            >>= store<STA>{ h });
    }

    /* TODO
    template<op_name_t BranchOp>
    void lt_comparison(ssa_ht h, locator_t fail_label, 
                       locator_t sucess_label, 
                       locator_t eq_label)
    {
        for(unsigned i = 0; i < h->input_size(); i += 2)
        {
            select_step([i, &](cpu_t cpu, sel_t const* const prev)
            {
                ssa_value_t lhs = h->input(i);
                ssa_value_t rhs = h->input(i + 1);
                
                bool const last_iter = (i + 2) > h->input_size();

                (load_A<>{ lhs }
                >>= def_op<CMP>{ {}, rhs }
                >>= if_{ !last_iter, def_op<BEQ>{ {}, next_label } }
                >>= def_op<BranchOp>{ {}, fail_label }
                >>= finish)(cpu, prev_sel);

                (load_X<>{ lhs }
                >>= def_op<CPX>{ {}, rhs }
                >>= if_{ !last_iter, def_op<BEQ>{ {}, next_label } }
                >>= def_op<BranchOp>{ {}, fail_label[0] }
                >>= finish)(cpu, prev_sel);

                (load_Y<>{ lhs }
                >>= def_op<CPY>{ {}, rhs }
                >>= if_{ !last_iter, def_op<BEQ>{ {}, next_label } }
                >>= def_op<BranchOp>{ {}, fail_label[0] }
                >>= finish)(cpu, prev_sel);
            });

            (load_A<>{ lhs }
            >>= def_op<CMP>{ {}, rhs }
            >>= def_op<BEQ>{ {}, next_label }
            >>= def_op<BranchOp>{ {}, fail_label }
            >>= finish)(cpu, prev_sel);

            (load_X<>{ lhs }
            >>= def_op<CPX>{ {}, rhs }
            >>= def_op<BEQ>{ {}, next_label }
            >>= def_op<BranchOp>{ {}, fail_label[0] }
            >>= finish)(cpu, prev_sel);

            (load_Y<>{ lhs }
            >>= def_op<CPY>{ {}, rhs }
            >>= def_op<BEQ>{ {}, next_label }
            >>= def_op<BranchOp>{ {}, fail_label[0] }
            >>= finish)(cpu, prev_sel);
        }

        (def_op<invert_branch(BranchOp)>{ {}, success_label }
        >>= finish)(cpu, prev_sel);
    }
    */

    void write_globals(ssa_ht h)
    {
        for_each_written_global(h, [h](ssa_value_t def, locator_t loc)
        {
            if(def.is_handle())
            {
                assert(def->op() == SSA_locator_store);

                if(def->test_flags(FLAG_COALESCED))
                    return;

                def = def->input(0);
            }

            select_step([loc, def](cpu_t cpu, sel_t const* prev)
            {
                (load_A<>{ def }
                 >>= def_op<STA>{ {}, loc }
                 >>= finish)(cpu, prev);

                (load_X<>{ def }
                 >>= def_op<STX>{ {}, loc }
                 >>= finish)(cpu, prev);

                (load_Y<>{ def }
                 >>= def_op<STY>{ {}, loc }
                 >>= finish)(cpu, prev);
            });
        });
    }

    /* TODO: remove

        unsigned const begin = write_globals_begin(h->op());
        unsigned const end = h->input_size();
        unsigned const size = (end - begin) / 2;

        if(size <= 64)
        {
            std::uint64_t inputs_left = 0;

            std::uint64_t bit = 1;
            for(unsigned i = begin; i < end; i += 2, bit <<= 1)
            {
                ssa_value_t input = h->input(i);
                if(input.holds_ref() && input->op() == SSA_locator_store)
                    continue;
                inputs_left |= bit;
            }

            bs_step(inputs_left, &cont, cpu, prev);
        }
        else
            it_step(begin, &cont, cpu, prev);
    }

    // TODO: opt!!!
    struct write_globals
    {
        ssa_ht h;

        template<typename Cont> 
        void operator()(cpu_t cpu, sel_t const* prev, Cont cont) const
        {
            unsigned const begin = write_globals_begin(h->op());
            unsigned const end = h->input_size();
            unsigned const size = (end - begin) / 2;

            if(size <= 64)
            {
                std::uint64_t inputs_left = 0;

                std::uint64_t bit = 1;
                for(unsigned i = begin; i < end; i += 2, bit <<= 1)
                {
                    ssa_value_t input = h->input(i);
                    if(input.holds_ref() && input->op() == SSA_locator_store)
                        continue;
                    inputs_left |= bit;
                }

                bs_step(inputs_left, &cont, cpu, prev);
            }
            else
                it_step(begin, &cont, cpu, prev);
        }

        template<typename Cont> 
        void bs_step(std::uint64_t inputs_left, Cont* cont, 
                     cpu_t cpu, sel_t const* prev) const
        {
            unsigned const begin = write_globals_begin(h->op());

            // Write anything currently in a register:
            bitset_for_each(inputs_left, [&](unsigned bit)
            {
                unsigned const input_i = begin + bit * 2;
                ssa_value_t const input = h->input(input_i);
                locator_t const loc = h->input(input_i + 1).locator();

                if(cpu.regs[REG_A] == input)
                    prev = &alloc_sel<STA_ABSOLUTE>(prev, loc);
                else if(cpu.regs[REG_X] == input)
                    prev = &alloc_sel<STX_ABSOLUTE>(prev, loc);
                else if(cpu.regs[REG_Y] == input)
                    prev = &alloc_sel<STY_ABSOLUTE>(prev, loc);
                else
                    return;

                assert(inputs_left & (1 << bit));
                inputs_left ^= 1 << bit;
            });

            // If there are writes remaining, load them first then try again:
            if(inputs_left)
            {
                unsigned const bit = builtin::ctz(inputs_left);
                unsigned const input_i = begin + 2 * bit;
                ssa_value_t const input = h->input(input_i);

                def_op<LDA, opt_t{ REGF_CPU }>{input, input}(cpu, prev,
                [this, inputs_left, cont](cpu_t cpu, sel_t const* prev) 
                { 
                    bs_step(inputs_left, cont, cpu, prev);
                });
            }
            else
                (*cont)(cpu, prev);

        }
        template<typename Cont> 
        void it_step(unsigned input_i, Cont* cont, 
                     cpu_t cpu, sel_t const* prev) const
        {
            if(input_i < h->input_size())
            {
                ssa_value_t const input = h->input(input_i);

                def_op<LDA, opt_t{ REGF_CPU }>{input, input}(cpu, prev,
                [this, input_i, cont](cpu_t cpu, sel_t const* prev) 
                { 
                    it_step(input_i + 2, cont, cpu, prev);
                });
            }
            else
                (*cont)(cpu, prev);
        }
    };
*/

    void isel_node_simple(ssa_ht h, cpu_t cpu, sel_t const* prev)
    {
        auto const commutative = [](ssa_ht h, auto fn)
        {
            for(unsigned i = 0; i < 2; ++i)
                fn(h->input(i), h->input(1-i));
        };

        auto const copy_impl = [&](ssa_ht def, ssa_value_t input)
        {
            ssa_value_t const orig = orig_def(def);

            (load_A<>{ input }
            >>= set_regs<REGF_A>{ orig }
            >>= store<STA>{ h })(cpu, prev);

            (load_X<>{ input }
            >>= set_regs<REGF_X>{ orig }
            >>= store<STX>{ h })(cpu, prev);

            (load_Y<>{ input }
            >>= set_regs<REGF_Y>{ orig }
            >>= store<STY>{ h })(cpu, prev);
        };

        cfg_ht const cfg_node = h->cfg_node();
        ssa_value_t const def = orig_def(h);

        switch(h->op())
        {
        case SSA_add:
            {
                ssa_value_t const carry = h->input(2);

                commutative(h, [&](ssa_value_t lhs, ssa_value_t rhs)
                {
                    (load_CA<>{ carry, lhs }
                    >>= def_op<ADC>{ def, rhs }
                    >>= store<STA>{h})(cpu, prev);
                });

                if(carry_used(*h) && carry.is_const())
                {
                    commutative(h, [&](ssa_value_t lhs, ssa_value_t rhs)
                    {
                        if(!rhs.is_const())
                            return;

                        unsigned const axs_arg =
                            (0x100 - rhs.whole() - carry.carry()) & 0xFF;

                        (load_AX<>{ lhs }
                        >>= def_op<AXS>{ def, axs_arg }
                        >>= set_regs<REGF_C>{}
                        >>= store<STX>{h})(cpu, prev);

                        (load_X<>{ lhs }
                        >>= load_A<opt_t{ ~REGF_X }>{ 0xFFu }
                        >>= def_op<AXS>{ def, axs_arg }
                        >>= set_regs<REGF_C>{}
                        >>= store<STX>{h})(cpu, prev);

                        (load_A<>{ 0xFFu }
                        >>= load_X<opt_t{ ~REGF_A }>{ lhs }
                        >>= def_op<AXS>{ def, axs_arg }
                        >>= set_regs<REGF_C>{}
                        >>= store<STX>{h})(cpu, prev);
                    });
                }
            }
            break;

        case SSA_and:
            commutative(h, [&](ssa_value_t lhs, ssa_value_t rhs)
            {
                (load_A<>{ lhs }
                >>= def_op<AND>{ def, rhs }
                >>= store<STA>{h})(cpu, prev);

                (load_A<>{ lhs }
                >>= load_X<opt_t{ ~REGF_A }>{ rhs }
                >>= def_op<AXS>{ def, 0xFFu }
                >>= set_regs<REGF_C>{}
                >>= store<STX>{h})(cpu, prev);

                (load_X<>{ lhs }
                >>= load_A<opt_t{ ~REGF_X }>{ rhs }
                >>= def_op<AXS>{ def, 0xFFu }
                >>= set_regs<REGF_C>{}
                >>= store<STX>{h})(cpu, prev);

                (load_A<>{ lhs }
                >>= load_X<opt_t{ ~REGF_A }>{ rhs }
                >>= store<SAX>{h})(cpu, prev);

                (load_X<>{ lhs }
                >>= load_A<opt_t{ ~REGF_X }>{ rhs }
                >>= store<SAX>{h})(cpu, prev);

                // TODO: consider using subtraction instructions,
                // checking constraints when it's applicable.
            });
            break;

        case SSA_or:
            commutative(h, [&](ssa_value_t lhs, ssa_value_t rhs)
            {
                (load_A<>{ lhs }
                >>= def_op<ORA>{ def, rhs }
                >>= store<STA>{h})(cpu, prev);

                // TODO: consider using add instructions, 
                // checking constraints when it's applicable.
            });
            break;

        case SSA_read_global:
            {
                if(h->test_flags(FLAG_COALESCED))
                    goto coalesced_op;

                locator_t loc = h->input(1).locator();
                assert(loc.lclass() != LCLASS_GLOBAL_SET);

                copy_impl(h, loc);
            }
            break;

        case SSA_phi_copy:
        case SSA_phi:
            {
                ssa_value_t input = h->input(0);
                if(input.holds_ref() 
                   && cset_head(h) == cset_head(input.handle()))
                {
                    goto coalesced_op;
                }
                copy_impl(h, h->input(0));
            }
            break;

        case SSA_fn_call:
            (def_op<JSR>{ {}, h->input(0) }
            >>= set_regs<REGF_CPU>{}
            >>= finish)(cpu, prev);
            break;

        case SSA_return:
            (def_op<RTS>{}
            >>= finish)(cpu, prev);
            break;

        case SSA_if:
            (load_Z<>{ h->input(0) }
            >>= def_op<BNE>{ {}, locator_t::cfg_label(cfg_node->output(0)) }
            >>= def_op<BEQ>{ {}, locator_t::cfg_label(cfg_node->output(1)) }
            >>= finish)(cpu, prev);
            break;

        case SSA_jump:
            assert(cfg_node->output_size() == 1);
            std::puts("JUMPIN");

            // TODO: use conditional jumps for efficiency.
            (def_op<JMP>{{}, locator_t::cfg_label(cfg_node->output(0)) }
            >>= finish)(cpu, prev);

            break;

        case SSA_entry:
        case SSA_locator_store:
        case SSA_eq:
        case SSA_not_eq:
        case SSA_branch_eq:
        case SSA_branch_not_eq:
        case SSA_carry:
        ignore_op:
            finish(cpu, prev);
            break;

        coalesced_op:
            if(cg_data(h).store_in_locs.size())
                copy_impl(h, h);
            else
                finish(cpu, prev);
            break;


        default:
            std::cout << "FAIL = " << h->op() << '\n';
            assert(false);
            break;
        }
    }

    void isel_node(ssa_ht h)
    {
        cfg_ht const cfg_node = h->cfg_node();

        std::cout << "doing " << h->op() << '\n';
        if(cg_data(h).store_in_locs.size())
            std::puts("has store in locs");

        switch(h->op())
        {
        case SSA_eq:     eq_store<BEQ>(h); break;
        case SSA_not_eq: eq_store<BNE>(h); break;

        // Branch ops jump directly:
        case SSA_branch_eq:
            eq_branch<BEQ>(h, locator_t::cfg_label(cfg_node->output(0)), 
                              locator_t::cfg_label(cfg_node->output(1)));
            break;
        case SSA_branch_not_eq:
            eq_branch<BNE>(h, locator_t::cfg_label(cfg_node->output(0)), 
                              locator_t::cfg_label(cfg_node->output(1)));
            break;

        case SSA_return:
        case SSA_fn_call:
            write_globals(h);
            break;

        case SSA_carry:
            store_carry(h);
            break;

        default: break;
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
                // (They may not be in final result.)
                // This is highly pessimistic, but simplifies the code gen.
                if(output->cfg_node() != h->cfg_node())
                    goto skip;

                if(cg_data(output).schedule.index 
                   > cg_data(last_use).schedule.index)
                {
                    last_use = output;
                }
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

