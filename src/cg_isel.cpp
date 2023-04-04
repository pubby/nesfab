#include "cg_isel.hpp"

#include <cstdint>
#include <functional>
#include <type_traits>
#include <vector>
#include <algorithm>
#include <numeric>

#include <boost/container/small_vector.hpp>

#include "robin/hash.hpp"
#include "robin/map.hpp"

#include "array_pool.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "cg_cset.hpp"
#include "options.hpp"
#include "ir_algo.hpp"
#include "worklist.hpp"
#include "debug_print.hpp"
#include "multi.hpp"
#include "switch.hpp"
#include "asm_graph.hpp"
#include "rom.hpp"

namespace bc = ::boost::container;

namespace isel
{
    TLS std::vector<cfg_d> _data_vec;

    // Backbone state of the instruction selection algorithm.
    struct state_t
    {
        // Holds all selection memory. 
        // Is reset at the start of the algorithm.
        array_pool_t<sel_t, 4098> sel_pool;

        using map_t = rh::batman_map<cpu_t, sel_pair_t>;

        // These track the in-flight selections:
        map_t map;
        map_t next_map;
        unsigned max_map_size = 0;

        // The current best selection has this cost:
        isel_cost_t best_cost = ~0;
        isel_cost_t next_best_cost = ~0;

        // Tracks what we're currently compiling:
        fn_ht fn = {};
        cfg_ht cfg_node = {};
        ssa_ht ssa_node = {};

        // If the isel needs to generate a new label or var, 
        // it uses these to get a unique ID:
        unsigned next_label = 0;
        unsigned next_var = 0;
        locator_t minor_label() { return locator_t::minor_label(next_label++); }
        locator_t minor_var() { return locator_t::minor_var(fn, next_var++); }

        // Scratchpad used for sorting stuff:
        std::vector<unsigned> indices;

        // Used for debug logging.
        log_t* log = nullptr;

#ifndef NDEBUG
        bool selecting = false;
#endif
    };

    // Main global state of the instruction selection algorithm.
    TLS state_t state;

    inline locator_t ssa_to_value(ssa_value_t v)
        { return locator_t::from_ssa_value(orig_def(v)); }

///////////////////////////////////////////////////////////////////////////////

    // The selection functions take global variables as template parameters.
    // To pass these globals through templates, they must be wrapped inside types.
    // Those types are below:

    template<typename Tag>
    struct param
    {
        static inline TLS ssa_value_t _node = {};
        static inline TLS locator_t _value = {};
        static inline TLS locator_t _trans = {};

        static void set(ssa_value_t v)
        {
            _node = v;
            _value = ssa_to_value(v);
            _trans = asm_arg(v);
        }

        static void set(ssa_value_t v, std::uint16_t offset)
        {
            _node = v;
            _value = ssa_to_value(v).with_advance_offset(offset);
            _trans = asm_arg(v).with_advance_offset(offset);
        }

        [[gnu::always_inline]] static ssa_value_t node() { return _node; }
        [[gnu::always_inline]] static locator_t value() { return _value; }
        [[gnu::always_inline]] static locator_t trans() { return _trans; }
        [[gnu::always_inline]] static locator_t trans_hi() { return {}; }
    };

    template<typename Param>
    struct array_index
    {
        [[gnu::always_inline]] static ssa_value_t node() 
        { 
            using namespace ssai::array;
            return Param::node()->input(INDEX); 
        }
        [[gnu::always_inline]] static locator_t value() { return ssa_to_value(node()); }
        [[gnu::always_inline]] static locator_t trans() { return asm_arg(node()); }
        [[gnu::always_inline]] static locator_t trans_hi() { return {}; }
    };

    template<typename Param>
    struct array_mem
    {
        [[gnu::always_inline]] static ssa_value_t node() 
        { 
            using namespace ssai::array;
            return Param::node()->input(ARRAY); 
        }
        
        [[gnu::always_inline]] static locator_t value() 
        { 
            using namespace ssai::array;
            return (ssa_to_value(node())
                    .with_advance_offset(Param::node()->input(OFFSET).whole())); 
        }

        [[gnu::always_inline]] static locator_t trans() 
        { 
            using namespace ssai::array;
            return (asm_arg(node())
                    .with_advance_offset(Param::node()->input(OFFSET).whole())); 
        }

        [[gnu::always_inline]] static locator_t trans_hi() { return {}; }
    };

    struct null_
    {
        [[gnu::always_inline]] static ssa_value_t node() { return {}; }
        [[gnu::always_inline]] static locator_t value() { return {}; }
        [[gnu::always_inline]] static locator_t trans() { return {}; }
        [[gnu::always_inline]] static locator_t trans_hi() { return {}; }

        // Pass-thru
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_pair_t sel) { Cont::run(cpu, sel); }
    };

    template<std::uint8_t I>
    struct const_
    {
        [[gnu::always_inline]] static ssa_value_t node() { return {}; }
        [[gnu::always_inline]] static locator_t value() { return locator_t::const_byte(I); }
        [[gnu::always_inline]] static locator_t trans() { return locator_t::const_byte(I); }
        [[gnu::always_inline]] static locator_t trans_hi() { return {}; }
    };

    template<typename Param, typename PtrHi>
    struct set_ptr_hi
    {
        [[gnu::always_inline]] static ssa_value_t node() { return Param::node(); }
        [[gnu::always_inline]] static locator_t value() { return Param::value(); }
        [[gnu::always_inline]] static locator_t trans() { return Param::trans(); }
        [[gnu::always_inline]] static locator_t trans_hi() { return PtrHi::trans(); }
    };

///////////////////////////////////////////////////////////////////////////////

    constexpr isel_cost_t cost_fn(op_t op) 
    { 
        isel_cost_t penalty = 0;

        switch(op_name(op))
        {
        default: 
            break;
        // Very slightly penalize ROL/ROR, to prefer LSR/ASL:
        case ROL:
        case ROR:
        // Very slightly penalize LAX, to prefer LDA or LDX:
        case LAX: 
        // Same with ALR and LSR:
        case ALR:
            penalty += 2;
            break;
        }

        return (op_cycles(op) * 256ull) + (op_size(op) * 4ull) + penalty;
    }

///////////////////////////////////////////////////////////////////////////////

    // Represents a list of functions.
    // This is used to write code in continuation-passing style (CPS).
    struct cons_t
    {
        std::type_identity_t<void(cpu_t const&, sel_pair_t, cons_t const*)>* fn;
        cons_t const* next;

        [[gnu::always_inline]]
        void call(cpu_t const& cpu, sel_pair_t sel) const { fn(cpu, sel, next); }
    };

    // The function pointer type of all our selection steps:
    using cont_t = std::type_identity_t<void(cpu_t const&, sel_pair_t, cons_t const*)>*;

    template<cont_t Head, cont_t... Conts>
    struct chain_t
    {
        [[gnu::flatten]]
        explicit chain_t(cons_t const* tail)
        : chain(tail)
        , cons{Head, &chain.cons}
        {}

        chain_t<Conts...> chain;
        cons_t cons;
    };

    template<cont_t Head>
    struct chain_t<Head>
    {   
        [[gnu::always_inline]]
        explicit chain_t(cons_t const* tail)
        : cons{Head, tail}
        {}
        
        cons_t cons;
    };

    // Combines multiple CPS functions into a single one.
    template<cont_t... Conts> [[gnu::noinline]]
    void chain(cpu_t const& cpu, sel_pair_t sel, cons_t const* cont)
    {
        chain_t<Conts...> c(cont);
        c.cons.call(cpu, sel);
    }

///////////////////////////////////////////////////////////////////////////////

    // These determine how extensive the search is.
    constexpr unsigned cost_cutoff(int size)
    {
        constexpr unsigned BASE = cost_fn(LDY_ABSOLUTE) * 2;
        return BASE;
        //return (BASE >> (size >> 4)) + cost_fn(TAY_IMPLIED);
        //return std::max<int>((BASE * (int(MAX_MAP_SIZE*2) - size)) / int(MAX_MAP_SIZE*2), cost_fn(TAY_IMPLIED) * 3 / 2);
    }

    // Finishes the selection step.
    // This adds it to the map and potentially updates the current best.
    template<bool FinishNode>
    void finish(cpu_t const& cpu, sel_pair_t sp, cons_t const*)
    {
        isel_cost_t const sel_cost = sp.cost;

        if(sel_cost > state.next_best_cost + cost_cutoff(state.next_map.size()))
            return;

        state_t::map_t::value_type insertion = { cpu, sp };

        // If this completes a node's operations, we'll release 'req_store'.
        if(FinishNode)
            insertion.first.req_store &= ~cg_data(state.ssa_node).isel.last_use;

        auto result = state.next_map.insert(std::move(insertion));

        if(!result.second && sel_cost < result.first->second.cost)
            result.first->second = sp;

        if(sel_cost < state.next_best_cost)
            state.next_best_cost = sel_cost;

        assert(!state.next_map.empty());
    }


    // Runs the function and adds the results to the state map.
    template<bool FinishNode, typename Fn>
    void select_step(Fn fn)
    {
        assert(!state.selecting);
        assert(state.selecting = true);
        assert(state.max_map_size > 0);

        dprint(state.log, "-SELECT_STEP", state.ssa_node);

        state.next_map.clear();
        state.next_best_cost = ~0 - cost_cutoff(0);

        assert(state.next_map.size() == 0);
        assert(state.next_map.begin() == state.next_map.end());
        assert(state.map.size());

        cons_t const cont = { finish<FinishNode> };

        isel_cost_t const cutoff = state.best_cost + cost_cutoff(state.map.size());

        std::size_t const prev_pool_size = state.sel_pool.size();

        // Run every selection step:
        if(state.map.size() > state.max_map_size)
        {
            state.indices.resize(state.map.size());

            auto const begin = state.indices.begin();
            auto end = state.indices.end();
            
            auto comp = [&](unsigned a, unsigned b)
            { 
                // TODO: optimize this
                unsigned ac = state.map.begin()[a].second.cost + heuristic_penalty(state.map.begin()[a].first.defs.data());
                unsigned bc = state.map.begin()[b].second.cost + heuristic_penalty(state.map.begin()[b].first.defs.data());
                return ac > bc;
            };

            std::iota(begin, end, 0);
            std::make_heap(begin, end, comp);

            for(unsigned i = 0; i < state.max_map_size; ++i)
            {
                std::pop_heap(begin, end, comp);
                auto const& pair = state.map.begin()[*(--end)];
                if(pair.second.cost > cutoff)
                    break;
                fn(pair.first, pair.second, &cont);
            }
        }
        else
        {
            for(auto const& pair : state.map)
                if(pair.second.cost <= cutoff)
                    fn(pair.first, pair.second, &cont);
        }

        if(state.next_map.empty())
            throw isel_no_progress_error_t{};

        dprint(state.log, "--SELECT_STEP_POOL_SIZE", state.sel_pool.size());
        dprint(state.log, "--SELECT_STEP_MAP_SIZE", state.next_map.size(), state.map.size());
        if(state.ssa_node)
        {
            dprint(state.log, "--SELECT_STEP_POOL_DIFF", state.sel_pool.size() - prev_pool_size, state.ssa_node->op());
            dprint(state.log, "--SELECT_STEP_MAP_DIFF", int(state.next_map.size()) - std::min<int>(state.max_map_size, state.map.size()), 
                                                        state.ssa_node->op());
        }

        state.map.swap(state.next_map);

        state.best_cost = state.next_best_cost;

        assert(state.map.size());
        assert(!(state.selecting = false));
    }

    template<op_t Op>
    sel_pair_t alloc_sel(cpu_t const& cpu, sel_pair_t sp, 
                         locator_t arg = {}, locator_t alt = {}, isel_cost_t extra_cost = 0)
    {
        assert(Op != BAD_OP);
        isel_cost_t total_cost = cost_fn(Op);
        if(cpu.conditional_regs & cpu_t::CONDITIONAL_EXEC)
            total_cost = (total_cost * 3) / 4; // Conditional ops are arbitrarily cheaper.
        total_cost += sp.cost + extra_cost;

        sel_t& sel =  state.sel_pool.emplace(sp.sel,
            asm_inst_t
            { 
                .op = Op, 
#ifndef NDEBUG
                .ssa_op = state.ssa_node ? state.ssa_node->op() : SSA_null, 
#endif
                .arg = arg, 
                .alt = alt,
#ifndef NDEBUG
            .cost = total_cost,
#endif
            });

        return { &sel, total_cost };
    }

    template<op_t Op, op_t NextOp, op_t... Ops, typename... Args>
    sel_pair_t alloc_sel(cpu_t const& cpu, sel_pair_t sp, locator_t arg, Args... args)
    {
        return alloc_sel<NextOp, Ops...>(cpu, alloc_sel<Op>(cpu, sp, arg), args...);
    }

    // Marks the node as stored without any cost.
    // This is used when a node has been aliased and doesn't need a MAYBE_STORE at all.
    template<typename Def>
    void ignore_req_store(cpu_t const& cpu, sel_pair_t sp, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;

        ssa_value_t const n = Def::node();
        if(n.holds_ref())
            cpu_copy.req_store |= cg_data(n.handle()).isel.store_mask;

        cont->call(cpu_copy, sp);
    }

    template<op_t Op>
    isel_cost_t handle_req_store_penalty(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
    {
        if(!arg.holds_ref())
            return 0;

        ssa_ht const arg_h = arg.handle();
        auto& d = cg_data(arg_h);

        // Nodes that belong to this CFG node are tracked more
        // precisely using the 'req_store' part of 'cpu_t'.
        if(arg_h->cfg_node() != state.cfg_node)
            return 0; 

        // Determine if this node has been stored yet.
        // If it has, there won't be a penalty.
        isel_cost_t const new_stores = builtin::popcount(~cpu.req_store & d.isel.store_mask);

        // Mark the node as stored.
        cpu.req_store |= d.isel.store_mask;
        assert(!(~cpu.req_store & d.isel.store_mask));

        // If this is a store, mark it immediately without cost.
        if(!xy_addr_mode(op_addr_mode(Op)) && (op_output_regs(Op) & REGF_M) 
           && !(op_flags(Op) & ASMF_MAYBE_STORE) && def.holds_ref() && def.handle() == state.ssa_node)
        {
            cpu.req_store |= cg_data(def.handle()).isel.store_mask;
        }

        return cost_fn(STA_ABSOLUTE) * new_stores;
    }

///////////////////////////////////////////////////////////////////////////////

    template<typename Label> [[gnu::noinline]]
    void label(cpu_t const& cpu, sel_pair_t sp, cons_t const* cont)
    {
        cont->call(cpu, alloc_sel<ASM_LABEL>(cpu, sp, Label::trans()));
    };

    template<typename Opt, regs_t Regs, bool KeepValue, typename Param>
    void set_defs(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_defs<Regs>(Opt::to_struct, Param::value(), KeepValue))
            cont->call(cpu_copy, prev);
    };

    template<typename Opt, op_t Op, typename Def, typename Arg = null_>
    void set_defs_for(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_defs_for<Op>(Opt::to_struct, Def::value(), Arg::trans()))
            cont->call(cpu_copy, prev);
    };

    constexpr bool can_set_defs_for(options_t opt, op_t op)
    {
        return (opt.can_set & op_output_regs(op) & REGF_ISEL) == (op_output_regs(op) & REGF_ISEL);
    }

    [[gnu::noinline]]
    void clear_conditional(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        if(!(cpu.conditional_regs & cpu_t::CONDITIONAL_EXEC))
        {
            cont->call(cpu, prev);
            return;
        }

        cpu_t cpu_copy = cpu;
        for(regs_t r = 0; r < NUM_ISEL_REGS; ++r)
        {
            if(cpu_copy.conditional_regs & (1 << r))
            {
                cpu_copy.defs[r] = {};
                cpu_copy.known[r] = 0;
            }
        }
        cpu_copy.known_mask &= ~cpu_copy.conditional_regs;
        cpu_copy.conditional_regs = 0;
        assert(cpu_copy.known_array_valid());
        cont->call(cpu_copy, prev);
    };

    // Generates an op, picking the addressing mode based on its parameters.
    template<typename Opt, op_name_t OpName, typename Def, typename Arg> [[gnu::noinline]]
    void pick_op(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont);

#ifndef NDEBUG
    template<op_t Op>
    bool valid_arg(locator_t arg)
    {
        switch(op_addr_mode(Op))
        {
        case MODE_IMPLIED: 
            return !arg;
        case MODE_RELATIVE:
        case MODE_LONG:
            return is_label(arg.lclass());
        case MODE_IMMEDIATE:
            return arg.is_immediate();
        case MODE_ZERO_PAGE:
        case MODE_ZERO_PAGE_X:
        case MODE_ZERO_PAGE_Y:
        case MODE_ABSOLUTE:
        case MODE_ABSOLUTE_X:
        case MODE_ABSOLUTE_Y:
        case MODE_INDIRECT:
        case MODE_INDIRECT_X:
        case MODE_INDIRECT_Y:
        case MODE_MAYBE:
        case MODE_LIKELY:
            return bool(arg);
        default:
            return true;
        }
    }
#endif

    // Spits out the op specified.
    // NOTE: You generally shouldn't use this for any op that uses
    // a memory argument, as that argument can't be a SSA_cg_read_array8_direct.
    // Thus, prefer pick_op.
    template<op_t Op> [[gnu::noinline]]
    void exact_op(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont,
                  options_t opt, locator_t def, locator_t arg, locator_t alt, ssa_value_t ssa_def, ssa_value_t ssa_arg)
    {
        constexpr auto mode = op_addr_mode(Op);

        passert(valid_arg<Op>(arg), arg, Op);

        cpu_t cpu_copy = cpu;

        if(cpu_copy.set_defs_for<Op>(opt, def, arg))
        {
            unsigned penalty = 0;
            if((op_input_regs(Op) & REGF_M)
               && (mode == MODE_ZERO_PAGE 
                   || mode == MODE_ABSOLUTE 
                   || indirect_addr_mode(mode)))
            {
                penalty = handle_req_store_penalty<Op>(cpu_copy, ssa_def, ssa_arg);
            }

            cont->call(cpu_copy, alloc_sel<Op>(cpu, prev, arg, alt, penalty));
        }
    }

    template<typename Opt, op_t Op, typename Def = null_, typename Arg = null_> [[gnu::noinline]]
    void exact_op(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        exact_op<Op>(cpu, prev, cont, Opt::to_struct, Def::value(), Arg::trans(), Arg::trans_hi(), Def::node(), Arg::node());
    }

    // Like exact_op, but with a simplified set of parameters.
    // Only supports a few addressing modes.
    template<op_t Op>
    void simple_op(options_t opt, locator_t def, locator_t arg, cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
#ifndef NDEBUG
        constexpr auto mode = op_addr_mode(Op);
        assert(Op >= NUM_NORMAL_OPS || mode == MODE_IMPLIED || mode == MODE_RELATIVE || mode == MODE_IMMEDIATE || mode == MODE_BAD);
        passert(valid_arg<Op>(arg), arg, Op);
#endif
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_defs_for<Op>(opt, def, arg))
            cont->call(cpu_copy, alloc_sel<Op>(cpu, prev, arg, {}, 0));
    }

    template<typename Opt, op_t Op, typename Def = null_, typename Arg = null_> [[gnu::noinline]]
    void simple_op(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        simple_op<Op>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
    }

    template<typename Opt, op_name_t Op, typename Arg> [[gnu::noinline]]
    void branch_op(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        switch(Op)
        {
        case BEQ:
        case BNE:
            if(cpu.def_eq(REG_Z, locator_t::const_byte(Op == BEQ)))
                cont->call(cpu, prev);
            else if(cpu.def_eq(REG_Z, locator_t::const_byte(Op != BEQ)))
                goto jmp;
            goto regular;

        case BCS:
        case BCC:
            if(cpu.def_eq(REG_C, locator_t::const_byte(Op == BCS)))
                cont->call(cpu, prev);
            else if(cpu.def_eq(REG_Z, locator_t::const_byte(Op != BCS)))
                goto jmp;
            goto regular;

        case BMI:
        case BPL:
            if(cpu.def_eq(REG_N, locator_t::const_byte(Op == BMI)))
                cont->call(cpu, prev);
            else if(cpu.def_eq(REG_Z, locator_t::const_byte(Op != BMI)))
                goto jmp;
            goto regular;

        default:
        regular:
            simple_op<Opt, get_op(Op, MODE_RELATIVE), null_, Arg>(cpu, prev, cont);
            break;
        jmp:
            exact_op<Opt, JMP_ABSOLUTE, null_, Arg>(cpu, prev, cont);
        }
    }

    // Generates an op using the 0..255 table.
    template<typename Opt, op_t Op, typename Def = null_> [[gnu::noinline]]
    void iota_op(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        static_assert(op_addr_mode(Op) == MODE_ABSOLUTE_X || op_addr_mode(Op) == MODE_ABSOLUTE_Y);
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_output_defs<Op>(Opt::to_struct, Def::value()))
            cont->call(cpu_copy, alloc_sel<Op>(cpu, prev, locator_t::runtime_rom(RTROM_iota), {}, /*-cost_fn(STA_MAYBE) / 2*/ 0));
    };

    template<typename Opt, typename Def>
    void load_NZ_for_impl(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.def_eq(REG_A, v))
        {
            simple_op<EOR_IMMEDIATE>(
                Opt::template unrestrict<REGF_A>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            simple_op<TAX_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);

            simple_op<TAY_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else if(cpu.def_eq(REG_X, v))
        {
            chain
            < simple_op<typename Opt::unrestrict<REGF_X>, INX_IMPLIED>
            , simple_op<typename Opt::unrestrict<REGF_X>, DEX_IMPLIED, Def>
            >(cpu, prev, cont);

            simple_op<CPX_IMMEDIATE>(
                Opt::template valid_for<REGF_NZ>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            simple_op<TXA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else if(cpu.def_eq(REG_Y, v))
        {
            chain
            < simple_op<typename Opt::template unrestrict<REGF_Y>, INY_IMPLIED>
            , simple_op<typename Opt::template unrestrict<REGF_Y>, DEY_IMPLIED, Def>
            >(cpu, prev, cont);

            simple_op<CPY_IMMEDIATE>(
                Opt::template valid_for<REGF_NZ>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            simple_op<TYA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else
        {
            pick_op<Opt, LDA, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LDX, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LDY, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Def>(cpu, prev, cont);
        }
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_NZ_for(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if((cpu.def_eq(REG_Z, v) || (v.is_const_num() && cpu.is_known(REG_Z) && cpu.known[REG_Z] == !v.data()))
        && (cpu.def_eq(REG_N, v) || (v.is_const_num() && cpu.is_known(REG_N) && cpu.known[REG_N] == !!(v.data() & 0x80))))
            cont->call(cpu, prev);
        else if((Opt::can_set & REGF_NZ) != REGF_NZ)
            return;
        else
            return load_NZ_for_impl<Opt, Def>(cpu, prev, cont);
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_Z_for(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.def_eq(REG_Z, v) || (v.is_const_num() && cpu.is_known(REG_Z) && cpu.known[REG_Z] == !v.data()))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_Z))
            return;
        else
            return load_NZ_for_impl<Opt, Def>(cpu, prev, cont);
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_N_for(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.def_eq(REG_N, v) || (v.is_const_num() && cpu.is_known(REG_N) && cpu.known[REG_N] == !!(v.data() & 0x80)))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_N))
            return;
        else
        {
            chain
            < pick_op<Opt, BIT, null_, Def>
            , set_defs<Opt, REGF_N, true, Def>
            >(cpu, prev, cont);

            return load_NZ_for_impl<Opt, Def>(cpu, prev, cont);
        }
    }
    
    [[gnu::noinline]]
    void load_A_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        assert(value.is_const_num());
        std::uint8_t const byte = value.data();

        cpu_t cpu_copy;

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<ANC_IMMEDIATE>(opt, {}, value) && cpu_copy.is_known(REG_A, byte))
            cont->call(cpu_copy, alloc_sel<ANC_IMMEDIATE>(cpu, prev, value));

        if(cpu.is_known(REG_A))
        {
            unsigned const mask = byte << 1;
            if((mask & cpu.known[REG_A] & 0xFF) == mask)
            {
                assert(mask < 0x100);
                // ALR can set the carry, or clear the carry.
                // We'll try to find both:
                for(unsigned i = 0; i < 2; ++i)
                {
                    locator_t const arg = locator_t::const_byte(mask | i);
                    cpu_copy = cpu;
                    if(cpu_copy.set_defs_for<ALR_IMMEDIATE>(opt, {}, arg) && cpu_copy.is_known(REG_A, byte))
                        cont->call(cpu_copy, alloc_sel<ALR_IMMEDIATE>(cpu, prev, arg));

                    // No point in doing the second iteration if it can't set the carry:
                    if(!(cpu.known[REG_A] & 1))
                        break;
                }
            }
        }

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<LSR_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
        {
            cont->call(cpu_copy, alloc_sel<LSR_IMPLIED>(cpu, prev));
            return;
        }

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<ASL_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
        {
            cont->call(cpu_copy, alloc_sel<ASL_IMPLIED>(cpu, prev));
            return;
        }

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<ROL_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
        {
            cont->call(cpu_copy, alloc_sel<ROL_IMPLIED>(cpu, prev));
            return;
        }

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<ROR_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
        {
            cont->call(cpu_copy, alloc_sel<ROR_IMPLIED>(cpu, prev));
            return;
        }
    }

    template<typename Opt, typename Load, typename Def = Load> [[gnu::noinline]]
    void load_A(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Load::value();
        assert(v);
        assert(Def::value());

        if(cpu.value_eq(REG_A, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_A))
            return;
        else if(cpu.value_eq(REG_X, v))
            simple_op<TXA_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else if(cpu.value_eq(REG_Y, v))
            simple_op<TYA_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else
        {
            pick_op<Opt, LDA, Def, Load>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Load>(cpu, prev, cont);

            if(v.is_const_num())
                load_A_impl(Opt::template valid_for<REGF_A | REGF_NZ>::to_struct, v, cpu, prev, cont);
        }
    }

    [[gnu::noinline]]
    void load_X_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        assert(value.is_const_num());
        std::uint8_t const byte = value.data();

        cpu_t cpu_copy;

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<INX_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_X, byte))
            cont->call(cpu_copy, alloc_sel<INX_IMPLIED>(cpu, prev));

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<DEX_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_X, byte))
            cont->call(cpu_copy, alloc_sel<DEX_IMPLIED>(cpu, prev));
    }

    template<typename Opt, typename Load, typename Def = Load> [[gnu::noinline]]
    void load_X(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Load::value();

        if(cpu.value_eq(REG_X, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_X))
            return;
        else if(cpu.value_eq(REG_A, v))
            simple_op<TAX_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else
        {
            if(cpu.value_eq(REG_Y, v))
            {
                chain
                < simple_op<Opt, TYA_IMPLIED>
                , simple_op<Opt, TAX_IMPLIED, Def>
                >(cpu, prev, cont);
            }

            pick_op<Opt, LDX, Def, Load>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Load>(cpu, prev, cont);

            if(v.is_const_num())
                load_X_impl(Opt::template valid_for<REGF_X | REGF_NZ>::to_struct, v, cpu, prev, cont);
        }
    }

    [[gnu::noinline]]
    void load_Y_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        assert(value.is_const_num());
        std::uint8_t const byte = value.data();

        cpu_t cpu_copy;

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<INY_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_Y, byte))
            cont->call(cpu_copy, alloc_sel<INY_IMPLIED>(cpu, prev));

        cpu_copy = cpu;
        if(cpu_copy.set_defs_for<DEY_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_Y, byte))
            cont->call(cpu_copy, alloc_sel<DEY_IMPLIED>(cpu, prev));
    }

    template<typename Opt, typename Load, typename Def = Load> [[gnu::noinline]]
    void load_Y(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Load::value();

        if(cpu.value_eq(REG_Y, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_Y))
            return;
        else if(cpu.value_eq(REG_A, v))
            simple_op<TAY_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else
        {
            if(cpu.value_eq(REG_X, v))
            {
                chain
                < simple_op<Opt, TXA_IMPLIED>
                , simple_op<Opt, TAY_IMPLIED, Def>
                >(cpu, prev, cont);
            }

            pick_op<Opt, LDY, Def, Load>(cpu, prev, cont);

            if(v.is_const_num())
                load_Y_impl(Opt::template valid_for<REGF_Y | REGF_NZ>::to_struct, v, cpu, prev, cont);
        }
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_C(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.value_eq(REG_C, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_C))
            return;
        else if(v.is_const_num())
        {
            if(v.data())
                simple_op<SEC_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
            else
                simple_op<CLC_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else if(cpu.def_eq(REG_Z, Def::value()))
        {
            using load_C_label = param<struct load_C_label_tag>;
            load_C_label::set(state.minor_label());

            if(cpu.is_known(REG_C, 0))
            {
                chain
                < branch_op<Opt, BNE, load_C_label>
                , simple_op<Opt, SEC_IMPLIED>
                , label<load_C_label>
                , clear_conditional
                , set_defs<Opt, REGF_C, true, Def>
                >(cpu, prev, cont);
            }
            else if(cpu.is_known(REG_C, 1))
            {
                chain
                < branch_op<Opt, BEQ, load_C_label>
                , simple_op<Opt, CLC_IMPLIED>
                , label<load_C_label>
                , clear_conditional
                , set_defs<Opt, REGF_C, true, Def>
                >(cpu, prev, cont);
            }
            else
            {
                chain
                < simple_op<Opt, CLC_IMPLIED>
                , branch_op<Opt, BNE, load_C_label>
                , simple_op<Opt, SEC_IMPLIED>
                , label<load_C_label>
                , clear_conditional
                , set_defs<Opt, REGF_C, true, Def>
                >(cpu, prev, cont);
            }
        }
        else
        {
            chain
            < load_A<Opt, Def>
            , simple_op<typename Opt::valid_for<REGF_C>, LSR_IMPLIED, Def>
            >(cpu, prev, cont);

            chain
            < load_A<Opt, Def>
            , simple_op<typename Opt::valid_for<REGF_C>, ALR_IMMEDIATE, Def, const_<1>>
            >(cpu, prev, cont);
        }
    }

    template<typename Opt, typename A, typename X> [[gnu::noinline]]
    void load_AX(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const a = A::value();
        locator_t const x = X::value();

        if(cpu.value_eq(REG_A, a))
        {
            if(cpu.value_eq(REG_X, x))
                cont->call(cpu, prev);
            load_X<typename Opt::restrict_to<~REGF_A>, X>(cpu, prev, cont);
        }
        else if(cpu.value_eq(REG_X, x))
            load_A<typename Opt::restrict_to<~REGF_X>, A>(cpu, prev, cont);
        else
        {
            chain
            < load_A<Opt, A>
            , load_X<typename Opt::restrict_to<~REGF_A>, X>
            >(cpu, prev, cont);

            chain
            < load_X<Opt, X>
            , load_A<typename Opt::restrict_to<~REGF_X>, A>
            >(cpu, prev, cont);
        }
    };

    template<typename Opt, typename A, typename Y> [[gnu::noinline]]
    void load_AY(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const a = A::value();
        locator_t const y = Y::value();

        if(cpu.value_eq(REG_A, a))
        {
            if(cpu.value_eq(REG_Y, y))
                cont->call(cpu, prev);
            load_Y<typename Opt::restrict_to<~REGF_A>, Y>(cpu, prev, cont);
        }
        else if(cpu.value_eq(REG_Y, y))
            load_A<typename Opt::restrict_to<~REGF_Y>, A>(cpu, prev, cont);
        else
        {
            chain
            < load_A<Opt, A>
            , load_Y<typename Opt::restrict_to<~REGF_A>, Y>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, Y>
            , load_A<typename Opt::restrict_to<~REGF_Y>, A>
            >(cpu, prev, cont);
        }
    };

    template<typename Opt, typename A, typename C> [[gnu::noinline]]
    void load_AC(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        chain
        < load_C<Opt, C>
        , load_A<typename Opt::restrict_to<~REGF_C>, A>
        >(cpu, prev, cont);
    }

    template<typename Opt, typename A> [[gnu::noinline]]
    void load_ANZ(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        chain
        < load_A<Opt, A>
        , load_NZ_for<typename Opt::restrict_to<~REGF_A>, A>
        >(cpu, prev, cont);
    }

    template<typename Opt, typename A> [[gnu::noinline]]
    void load_XNZ(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        chain
        < load_X<Opt, A>
        , load_NZ_for<typename Opt::restrict_to<~REGF_X>, A>
        >(cpu, prev, cont);
    }

    template<typename Opt, typename A> [[gnu::noinline]]
    void load_YNZ(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        chain
        < load_Y<Opt, A>
        , load_NZ_for<typename Opt::restrict_to<~REGF_Y>, A>
        >(cpu, prev, cont);
    }

    template<typename Opt, typename Def, typename Arg, op_t AbsoluteX, op_t AbsoluteY, op_t Absolute
            , bool Enable = (AbsoluteX || AbsoluteY) && (Opt::flags & OPT_NO_DIRECT) < OPT_NO_DIRECT>
    struct pick_op_xy
    {
        [[gnu::noinline]]
        static void call(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont, unsigned offset = 0)
        {
            using OptN = typename Opt::inc_no_direct;

            locator_t const index = array_index<Arg>::value();

            if(Absolute != BAD_OP && index.is_const_num())
            {
                locator_t mem = array_mem<Arg>::trans();
                mem.advance_offset(index.data());

                exact_op<Absolute>(cpu, prev, cont,
                    OptN::to_struct, Def::value(),  mem, locator_t{}, 
                    Def::node(), array_mem<Arg>::node());
            }
            else
            {
                if(AbsoluteX != BAD_OP && cpu.value_eq(REG_X, index))
                    exact_op<Opt, AbsoluteX, Def, array_mem<Arg>>(cpu, prev, cont);
                else if(AbsoluteY != BAD_OP && cpu.value_eq(REG_Y, index))
                    exact_op<Opt, AbsoluteY, Def, array_mem<Arg>>(cpu, prev, cont);
                else
                {
                    if(AbsoluteX != BAD_OP)
                    {
                        chain
                        < load_X<OptN, array_index<Arg>>
                        , exact_op<OptN, AbsoluteX, Def, array_mem<Arg>>
                        >(cpu, prev, cont);
                    }

                    if(AbsoluteY != BAD_OP)
                    {
                        chain
                        < load_Y<OptN, array_index<Arg>>
                        , exact_op<OptN, AbsoluteY, Def, array_mem<Arg>>
                        >(cpu, prev, cont);
                    }
                }
            }
        }
    };

    template<typename Opt, typename Def, typename Arg, op_t AbsoluteX, op_t AbsoluteY, op_t Absolute>
    struct pick_op_xy<Opt, Def, Arg, AbsoluteX, AbsoluteY, Absolute, false>
    {
        [[gnu::noinline]]
        static void call(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont) {}
    };


    // pick_op impl
    template<typename Opt, op_name_t OpName, typename Def, typename Arg> [[gnu::noinline]]
    void pick_op(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        constexpr op_t implied    = get_op(OpName, MODE_IMPLIED);
        constexpr op_t relative   = get_op(OpName, MODE_RELATIVE);
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);
        constexpr op_t absolute_X = get_op(OpName, MODE_ABSOLUTE_X);
        constexpr op_t absolute_Y = get_op(OpName, MODE_ABSOLUTE_Y);

        bool const read_direct = Arg::node().holds_ref() && Arg::node()->op() == SSA_cg_read_array8_direct;

        if(implied != BAD_OP && !Arg::trans())
            simple_op<implied>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else if(relative != BAD_OP )
            simple_op<relative>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if(immediate != BAD_OP && Arg::trans().is_immediate())
            simple_op<immediate>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if((absolute_X != BAD_OP || absolute_Y) && read_direct)
            pick_op_xy<Opt, Def, Arg, absolute_X, absolute_Y, absolute>::call(cpu, prev, cont);
        else if(absolute != BAD_OP && !Arg::trans().is_immediate() && !read_direct)
            exact_op<absolute>(cpu, prev, cont, Opt::to_struct, Def::value(), Arg::trans(), Arg::trans_hi(), Def::node(), Arg::node());
    }

    template<typename Opt, typename Label, bool Sec> [[gnu::noinline]]
    void maybe_carry_label_clear_conditional(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        carry_label_clear_conditional<Opt, Label, Sec>(cpu, prev, cont);

        chain<label<Label>, clear_conditional>(cpu, prev, cont);
    }

    template<typename Opt, typename Label, bool Sec> [[gnu::noinline]]
    void carry_label_clear_conditional(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        chain
        < load_C<Opt, const_<Sec>>
        , label<Label>
        , clear_conditional
        , set_defs_for<Opt, Sec ? SEC_IMPLIED : CLC_IMPLIED, null_>
        >(cpu, prev, cont);
    };


    // Adds a store operation.
    // 'Maybe' means the store may not be required in the final code;
    // such instructions can be pruned later.
    template<typename Opt, op_name_t StoreOp, typename Def, typename Param, bool Maybe = true, bool KeepValue = true> [[gnu::noinline]]
    void store(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        // Conditional stores break cg_liveness
        assert(!(cpu.conditional_regs & cpu_t::CONDITIONAL_EXEC));

        cpu_t cpu_copy = cpu;
        constexpr auto input_regs = op_input_regs(get_op(StoreOp, MODE_ABSOLUTE)) & REGF_ISEL;
        if(builtin::popcount(unsigned(input_regs)) == 1) // Don't modify cpu for SAX, etc.
        {
            if(!cpu_copy.set_defs<input_regs>(Opt::to_struct, Def::value(), KeepValue))
                return;
        }

        // Store the node, locally:
        if(Maybe)
        {
            ssa_value_t const n = Param::node();
            if(n.holds_ref() && n->cfg_node() == state.cfg_node && cg_data(n.handle()).isel.likely_store)
            {
                constexpr auto LikelyOp = get_op(StoreOp, MODE_LIKELY);
                static_assert(LikelyOp);
                cont->call(cpu_copy, alloc_sel<LikelyOp>(cpu_copy, prev, Param::trans(), Param::value()));
            }
            else
            {
                constexpr auto MaybeOp = get_op(StoreOp, MODE_MAYBE);
                static_assert(MaybeOp);
                cont->call(cpu_copy, alloc_sel<MaybeOp>(cpu_copy, prev, Param::trans(), Param::value()));
            }
        }
        else
        {
            if(Param::trans().lclass() == LOC_SSA)
            {
                ssa_ht h = Param::trans().ssa_node();
                auto& d = cg_data(h);

                if(h->cfg_node() == state.cfg_node)
                    cpu_copy.req_store |= d.isel.store_mask;
            }

            cont->call(cpu_copy, alloc_sel<get_op(StoreOp, MODE_ABSOLUTE)>(cpu_copy, prev, Param::trans()));
        }
    }

    template<typename Opt, typename Def, typename Load, typename Store, 
             bool Maybe = true, bool KeepValue = true> [[gnu::noinline]]
    void load_then_store(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        if(Load::trans() == Store::trans())
        {
            ignore_req_store<Def>(cpu, prev, cont);
            return;
        }

        locator_t const v = Load::value();

        if(cpu.value_eq(REG_A, v))
            store<Opt, STA, Def, Store, Maybe, KeepValue>(cpu, prev, cont);
        else if(cpu.value_eq(REG_X, v))
            store<Opt, STX, Def, Store, Maybe, KeepValue>(cpu, prev, cont);
        else if(cpu.value_eq(REG_Y, v))
            store<Opt, STY, Def, Store, Maybe, KeepValue>(cpu, prev, cont);
        else
        {
            chain
            < load_A<Opt, Load, Def>
            , store<Opt, STA, Def, Store, Maybe, KeepValue>
            >(cpu, prev, cont);

            chain
            < load_X<Opt, Load, Def>
            , store<Opt, STX, Def, Store, Maybe, KeepValue>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, Load, Def>
            , store<Opt, STY, Def, Store, Maybe, KeepValue>
            >(cpu, prev, cont);
        }
    };

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_B(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        std::uint16_t const bs_addr = bankswitch_addr(mapper().type);

        using mstate = param<struct load_B_state_tag>;
        using retry_label = param<struct load_B_retry_label_tag>;
        using done_label = param<struct load_B_done_label_tag>;
        using detail = param<struct load_B_detail_tag>;
        using reset_mapper = param<struct load_B_reset_mapper_tag>;
        using addr = param<struct load_B_addr_tag>;

        addr::set(locator_t::addr(bs_addr));

        if(!mapper().bankswitches())
        {
            cont->call(cpu, prev);
            return;
        }

        if(mapper().type == MAPPER_MMC1)
        {
            if(compiler_options().unsafe_bank_switch)
            {
                chain
                < load_A<Opt, Def>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                >(cpu, prev, cont);
            }
            else
            {
                detail::set(locator_t::runtime_ram(RTRAM_mapper_detail));
                retry_label::set(state.minor_label());
                done_label::set(state.minor_label());
                reset_mapper::set(locator_t::runtime_rom(RTROM_mapper_reset));

                chain
                < load_X<Opt, Def, Def>
                , exact_op<Opt, LDY_ABSOLUTE, null_, detail>
                , label<retry_label>
                , exact_op<Opt, TXA_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, LSR_IMPLIED, null_>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , exact_op<Opt, CPY_ABSOLUTE, null_, detail>
                , branch_op<Opt, BEQ, done_label>
                , exact_op<Opt, LDY_ABSOLUTE, null_, detail>
                , exact_op<Opt, JSR_ABSOLUTE, null_, reset_mapper>
                , simple_op<Opt, write_reg_op(REGF_ISEL)> // Clobbers most everything
                , exact_op<Opt, JMP_ABSOLUTE, null_, retry_label>
                , label<retry_label>
                , clear_conditional
                >(cpu, prev, cont);
            }
        }
        else if(mapper().bus_conflicts)
        {
            if(state_size())
            {
                mstate::set(locator_t::runtime_ram(RTRAM_mapper_state));

                if(compiler_options().unsafe_bank_switch)
                {
                    chain
                    < load_A<Opt, Def>
                    , exact_op<Opt, ORA_ABSOLUTE, null_, mstate>
                    , exact_op<Opt, TAX_IMPLIED>
                    , iota_op<Opt, STA_ABSOLUTE_X, null_>
                    >(cpu, prev, cont);

                    chain
                    < load_A<Opt, Def>
                    , exact_op<Opt, ORA_ABSOLUTE, null_, mstate>
                    , exact_op<Opt, TAY_IMPLIED>
                    , iota_op<Opt, STA_ABSOLUTE_Y, null_>
                    >(cpu, prev, cont);
                }
                else
                {
                    retry_label::set(state.minor_label());

                    chain
                    < label<retry_label>
                    , exact_op<Opt, LAX_ABSOLUTE, null_, mstate>
                    , exact_op<Opt, ORA_ABSOLUTE, null_, Def>
                    , exact_op<Opt, TAY_IMPLIED>
                    , iota_op<Opt, STA_ABSOLUTE_Y, null_>
                    , pick_op<Opt, CPX, null_, mstate>
                    , branch_op<Opt, BNE, retry_label>
                    , clear_conditional
                    >(cpu, prev, cont);
                }
            }
            else
            {
                chain
                < load_AX<Opt, Def, Def>
                , iota_op<Opt, STA_ABSOLUTE_X, null_>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, Def, Def>
                , iota_op<Opt, STA_ABSOLUTE_Y, null_>
                >(cpu, prev, cont);
            }
        }
        else if(state_size())
        {
            mstate::set(locator_t::runtime_ram(RTRAM_mapper_state));

            if(compiler_options().unsafe_bank_switch)
            {
                chain
                < load_A<Opt, Def>
                , exact_op<Opt, ORA_ABSOLUTE, null_, mstate>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                >(cpu, prev, cont);
            }
            else
            {
                retry_label::set(state.minor_label());

                chain
                < label<retry_label>
                , exact_op<Opt, LAX_ABSOLUTE, null_, mstate>
                , exact_op<Opt, ORA_ABSOLUTE, null_, Def>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , pick_op<Opt, CPX, null_, mstate>
                , branch_op<Opt, BNE, retry_label>
                , clear_conditional
                >(cpu, prev, cont);
            }
        }
        else
        {
            load_then_store<Opt, Def, Def, addr, false>(cpu, prev, cont);
        }
    }

    static void no_effect(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        cont->call(cpu, prev);
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waddress"
    template<typename Opt, typename Condition, cont_t Then, cont_t Else = nullptr> [[gnu::noinline]]
    static void if_(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        cons_t c = { nullptr, cont };
        
        if(Condition::value())
            c.fn = Then ? Then : no_effect;
        else
            c.fn = Else ? Else : no_effect;

        c.call(cpu, prev);
    }
#pragma GCC diagnostic pop

    template<typename Tag>
    struct condition
    {
        static inline TLS bool b;
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
    using p_carry_output = param<struct carry_output_tag>;
    template<unsigned I>
    using p_label = param<i_tag<struct label_tag, I>>;
    using p_condition = condition<struct condition_tag>;

    template<typename Opt, typename Def, typename Value> [[gnu::noinline]]
    void sign_extend(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        using this_label = param<i_tag<struct sign_extend_label_tag, 0>>;
        this_label::set(state.minor_label());

        chain
        < load_A<Opt, const_<0>>
        , load_N_for<typename Opt::restrict_to<~REGF_A>, Value>
        , branch_op<Opt, BMI, this_label>
        , simple_op<Opt, LDA_IMMEDIATE, null_, const_<0xFF>>
        , label<this_label>
        , clear_conditional
        , store<Opt, STA, Def, Def>
        >(cpu, prev, cont);

        chain
        < load_X<Opt, const_<0>>
        , load_N_for<typename Opt::restrict_to<~REGF_X>, Value>
        , branch_op<Opt, BMI, this_label>
        , simple_op<Opt, DEX_IMPLIED, null_, null_>
        , label<this_label>
        , clear_conditional
        , store<Opt, STX, Def, Def>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, const_<0>>
        , load_N_for<typename Opt::restrict_to<~REGF_Y>, Value>
        , branch_op<Opt, BMI, this_label>
        , simple_op<Opt, DEY_IMPLIED, null_, null_>
        , label<this_label>
        , clear_conditional
        , store<Opt, STY, Def, Def>
        >(cpu, prev, cont);

        chain
        < load_A<Opt, const_<0>>
        , simple_op<Opt, CMP_IMMEDIATE, null_, const_<0x80>>
        , branch_op<Opt, BCC, this_label>
        , simple_op<Opt, LDA_IMMEDIATE, null_, const_<0xFF>>
        , label<this_label>
        , clear_conditional
        , store<Opt, STA, Def, Def>
        >(cpu, prev, cont);

        chain
        < load_X<Opt, const_<0>>
        , load_N_for<typename Opt::restrict_to<~REGF_X>, Value>
        , branch_op<Opt, BMI, this_label>
        , simple_op<Opt, DEX_IMPLIED, null_, null_>
        , label<this_label>
        , clear_conditional
        , store<Opt, STX, Def, Def>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, const_<0>>
        , load_N_for<typename Opt::restrict_to<~REGF_Y>, Value>
        , branch_op<Opt, BMI, this_label>
        , simple_op<Opt, DEY_IMPLIED, null_, null_>
        , label<this_label>
        , clear_conditional
        , store<Opt, STY, Def, Def>
        >(cpu, prev, cont);
    }

    template<typename Opt, typename FailLabel, typename SuccessLabel>
    void eq_branch(ssa_ht h)
    {
        constexpr op_name_t BranchOp = BEQ;
        constexpr op_name_t InverseOp = BNE;
        using SignLabel = std::conditional_t<BranchOp == BEQ, FailLabel, SuccessLabel>;
        using sign_check = condition<struct eq_sign_check_tag>;
        using last_iter = condition<struct eq_last_iter_tag>;

        for(unsigned i = 0; i < h->input_size(); i += 2)
        {
            // The last comparison cares about sign.
            sign_check::set(i + 2 == h->input_size());

            select_step<false>([&, i](cpu_t const& cpu, sel_pair_t const prev, cons_t const* cont)
            {
                last_iter::set(i + 2 >= h->input_size());

                for(unsigned j = 0; j < 2; ++j)
                {
                    p_lhs::set(h->input(i + j));
                    p_rhs::set(h->input(i + 1-j));

                    assert(p_lhs::value());
                    assert(p_rhs::value());

                    if(p_lhs::value().eq_const_byte(0))
                    {
                        if(p_rhs::value().eq_const_byte(0))
                        {
                            cont->call(cpu, prev);
                            break;
                        }
                        else
                        {
                            chain
                            < load_Z_for<Opt, p_rhs>
                            , branch_op<Opt, InverseOp, FailLabel>
                            >(cpu, prev, cont);
                            break;
                        }
                    }
                    else if(p_rhs::value().eq_const_byte(0))
                    {
                        chain
                        < load_Z_for<Opt, p_lhs>
                        , branch_op<Opt, InverseOp, FailLabel>
                        >(cpu, prev, cont);
                        break;
                    }
                    else
                    {
                        chain
                        < load_A<Opt, p_lhs>
                        , if_<Opt, sign_check, 
                            chain<load_N_for<typename Opt::restrict_to<~REGF_X>, p_lhs>,
                            branch_op<Opt, BMI, SignLabel>>>
                        , pick_op<Opt, CMP, null_, p_rhs>
                        , branch_op<Opt, InverseOp, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_X<Opt, p_lhs>
                        , if_<Opt, sign_check, 
                            chain<load_N_for<typename Opt::restrict_to<~REGF_X>, p_lhs>,
                            branch_op<Opt, BMI, SignLabel>>>
                        , pick_op<Opt, CPX, null_, p_rhs>
                        , branch_op<Opt, InverseOp, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_Y<Opt, p_lhs>
                        , if_<Opt, sign_check, 
                            chain<load_N_for<typename Opt::restrict_to<~REGF_X>, p_lhs>,
                            branch_op<Opt, BMI, SignLabel>>>
                        , pick_op<Opt, CPY, null_, p_rhs>
                        , branch_op<Opt, InverseOp, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_A<Opt, p_lhs>
                        , if_<Opt, sign_check, 
                            chain<load_N_for<typename Opt::restrict_to<~REGF_X>, p_lhs>,
                            branch_op<Opt, BMI, SignLabel>>>
                        , load_X<typename Opt::restrict_to<~REGF_A>, p_rhs>
                        , iota_op<Opt, CMP_ABSOLUTE_X, null_>
                        , branch_op<Opt, InverseOp, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_A<Opt, p_lhs>
                        , if_<Opt, sign_check, 
                            chain<load_N_for<typename Opt::restrict_to<~REGF_X>, p_lhs>,
                            branch_op<Opt, BMI, SignLabel>>>
                        , load_Y<typename Opt::restrict_to<~REGF_A>, p_rhs>
                        , iota_op<Opt, CMP_ABSOLUTE_Y, null_>
                        , branch_op<Opt, InverseOp, FailLabel>
                        >(cpu, prev, cont);
                    }
                }
            });
        }

        select_step<false>(
            chain
            < simple_op<Opt, get_op(BranchOp, MODE_RELATIVE), null_, SuccessLabel>
            , clear_conditional
            >);
    }

    template<bool Eq>
    void eq_store(ssa_ht h)
    {
        using fail = p_label<0>;
        using success = p_label<1>;
        using O = options<>;

        // For now, this implementation only loads the result in register X.
        select_step<false>(load_X<O, const_<0>>);

        fail::set(state.minor_label());
        success::set(state.minor_label());

        p_def::set(h);

        if(Eq)
            eq_branch<O::restrict_to<~REGF_X>, fail, success>(h);
        else
            eq_branch<O::restrict_to<~REGF_X>, success, fail>(h);

        select_step<true>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
        {
            // Explicitly instantiate the labels.
            // (For some reason, GCC can't link without these lines. Likely a compiler bug.)
            (void)&label<success>;
            (void)&label<fail>;

            chain
            < label<success>
            , simple_op<O, INX_IMPLIED>
            , clear_conditional
            , label<fail>
            , store<O, STX, p_def, p_def>
            >(cpu, prev, cont);
        });
    }

    template<typename Opt, typename FailLabel, typename SuccessLabel, bool Flip = false>
    void lt_branch(ssa_ht h)
    {
        using last_comp = condition<struct lt_last_comp_tag>;

        multi_lt_info_t info(h, Flip);
        
        if(info.lwhole != info.rwhole)
        {
            // One number has more whole bytes than the other.
            // We'll compare these bytes to zero.

            // Signed < Signed comparisons should always have the same whole bytes. 
            assert(!(info.lsigned && info.rsigned));

            if(info.lwhole < info.rwhole)
            {
                for(int i = info.maxwhole - 1; i >= info.minwhole; --i)
                {
                    assert(info.validr(info.roffset() + i));
                    p_rhs::set(h->input(info.roffset() + i));
                    last_comp::set(i == info.minwhole && info.sbcwhole <= -info.maxfrac);

                    if(info.rsigned && i == info.maxwhole - 1) // If sign byte
                    {
                        assert(!info.lsigned);

                        if(p_rhs::value().eq_const_byte(0))
                            select_step<false>(if_<Opt, last_comp, exact_op<Opt, JMP_ABSOLUTE, null_, FailLabel>>);
                        else
                        {
                            select_step<false>(
                                chain
                                < load_NZ_for<Opt, p_rhs>
                                , branch_op<Opt, BMI, FailLabel>
                                , branch_op<Opt, BNE, SuccessLabel>
                                , if_<Opt, last_comp, simple_op<Opt, BEQ_RELATIVE, null_, FailLabel>>
                                >);
                        }
                    }
                    else
                    {
                        if(p_rhs::value().eq_const_byte(0))
                            select_step<false>(if_<Opt, last_comp, exact_op<Opt, JMP_ABSOLUTE, null_, FailLabel>>);
                        else
                        {
                            select_step<false>(
                                chain
                                < load_Z_for<Opt, p_rhs>
                                , branch_op<Opt, BNE, SuccessLabel>
                                , if_<Opt, last_comp, simple_op<Opt, BEQ_RELATIVE, null_, FailLabel>>
                                >);
                        }
                    }
                }
            }
            else
            {
                for(int i = info.maxwhole - 1; i >= info.minwhole; --i)
                {
                    assert(info.validl(info.loffset() + i));
                    p_lhs::set(h->input(info.loffset() + i));
                    last_comp::set(i == info.minwhole && info.sbcwhole <= -info.maxfrac);

                    if(info.lsigned && i == info.maxwhole - 1) // If sign byte
                    {
                        assert(!info.rsigned);

                        if(p_lhs::value().eq_const_byte(0))
                            select_step<false>(if_<Opt, last_comp, exact_op<Opt, JMP_ABSOLUTE, null_, SuccessLabel>>);
                        else
                        {
                            select_step<false>(
                                chain
                                < load_NZ_for<Opt, p_lhs>
                                , branch_op<Opt, BMI, SuccessLabel>
                                , branch_op<Opt, BNE, FailLabel>
                                , if_<Opt, last_comp, simple_op<Opt, BEQ_RELATIVE, null_, SuccessLabel>, nullptr>
                                >);
                        }
                    }
                    else
                    {
                        if(p_lhs::value().eq_const_byte(0))
                            select_step<false>(if_<Opt, last_comp, exact_op<Opt, JMP_ABSOLUTE, null_, SuccessLabel>>);
                        else
                        {
                            select_step<false>(
                                chain
                                < load_Z_for<Opt, p_lhs>
                                , branch_op<Opt, BNE, FailLabel>
                                , if_<Opt, last_comp, simple_op<Opt, BEQ_RELATIVE, null_, SuccessLabel>, nullptr>
                                >);
                        }
                    }
                }
            }
        }
        else if(info.lsigned != info.rsigned)
        {
            // The types have the same number whole bytes, but the signs differ.

            // The multi-byte subtraction won't use the highest bytes.
            info.sbcwhole -= 1;

            assert(info.validl(info.loffset() + info.lwhole - 1));
            assert(info.validr(info.roffset() + info.rwhole - 1));

            p_lhs::set(h->input(info.loffset() + info.lwhole - 1));
            p_rhs::set(h->input(info.roffset() + info.rwhole - 1));
            last_comp::set(info.sbcwhole <= -info.maxfrac);

            if(p_lhs::value().eq_const_byte(0))
            {
                select_step<false>(
                    chain
                    < load_Z_for<Opt, p_rhs>
                    , branch_op<Opt, BNE, SuccessLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BEQ_RELATIVE, null_, FailLabel>>
                    >);
            }
            else if(p_rhs::value().eq_const_byte(0))
            {
                select_step<false>(
                    chain
                    < load_Z_for<Opt, p_lhs>
                    , branch_op<Opt, BNE, FailLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BEQ_RELATIVE, null_, SuccessLabel>>
                    >);
            }
            else if(info.lsigned)
            {
                select_step<false>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                {
                    chain
                    < load_ANZ<Opt, p_lhs>
                    , branch_op<Opt, BMI, SuccessLabel>
                    , pick_op<Opt, CMP, null_, p_rhs>
                    , simple_op<Opt, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, FailLabel>,
                                           simple_op<Opt, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_XNZ<Opt, p_lhs>
                    , branch_op<Opt, BMI, SuccessLabel>
                    , pick_op<Opt, CPX, null_, p_rhs>
                    , simple_op<Opt, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, FailLabel>,
                                           simple_op<Opt, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_YNZ<Opt, p_lhs>
                    , branch_op<Opt, BMI, SuccessLabel>
                    , pick_op<Opt, CPY, null_, p_rhs>
                    , simple_op<Opt, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, FailLabel>,
                                           simple_op<Opt, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_ANZ<Opt, p_lhs>
                    , branch_op<Opt, BMI, SuccessLabel>
                    , load_X<typename Opt::restrict_to<~REGF_A>, p_rhs>
                    , iota_op<Opt, CMP_ABSOLUTE_X, null_>
                    , simple_op<Opt, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, FailLabel>,
                                           simple_op<Opt, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_ANZ<Opt, p_lhs>
                    , branch_op<Opt, BMI, SuccessLabel>
                    , load_Y<typename Opt::restrict_to<~REGF_A>, p_rhs>
                    , iota_op<Opt, CMP_ABSOLUTE_Y, null_>
                    , simple_op<Opt, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, FailLabel>,
                                           simple_op<Opt, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);
                });
            }
            else // if rsigned
            {
                assert(info.rsigned);

                select_step<false>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                {
                    chain
                    < load_ANZ<Opt, p_rhs>
                    , branch_op<Opt, BMI, FailLabel>
                    , pick_op<Opt, CMP, null_, p_lhs>
                    , simple_op<Opt, BCC_RELATIVE, null_, FailLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, SuccessLabel>,
                                          simple_op<Opt, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_XNZ<Opt, p_rhs>
                    , branch_op<Opt, BMI, FailLabel>
                    , pick_op<Opt, CPX, null_, p_lhs>
                    , simple_op<Opt, BCC_RELATIVE, null_, FailLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, SuccessLabel>,
                                          simple_op<Opt, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_YNZ<Opt, p_rhs>
                    , branch_op<Opt, BMI, FailLabel>
                    , pick_op<Opt, CPY, null_, p_lhs>
                    , simple_op<Opt, BCC_RELATIVE, null_, FailLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, SuccessLabel>,
                                          simple_op<Opt, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_ANZ<Opt, p_rhs>
                    , branch_op<Opt, BMI, FailLabel>
                    , load_X<typename Opt::restrict_to<~REGF_A>, p_lhs>
                    , iota_op<Opt, CMP_ABSOLUTE_X, null_>
                    , simple_op<Opt, BCC_RELATIVE, null_, FailLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, SuccessLabel>,
                                          simple_op<Opt, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_ANZ<Opt, p_rhs>
                    , branch_op<Opt, BMI, FailLabel>
                    , load_Y<typename Opt::restrict_to<~REGF_A>, p_lhs>
                    , iota_op<Opt, CMP_ABSOLUTE_Y, null_>
                    , simple_op<Opt, BCC_RELATIVE, null_, FailLabel>
                    , if_<Opt, last_comp, simple_op<Opt, BCS_RELATIVE, null_, SuccessLabel>,
                                          simple_op<Opt, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);
                });
            }
        }

        // Now do a multi-byte subtraction.

        int iteration = 0; // Tracks comparisons inserted.
        for(int i = -info.maxfrac; i < info.sbcwhole; ++i)
        {
            if(i < -info.lfrac)
            {
                if(info.validr(i) && h->input(info.roffset() + i).eq_whole(0))
                    continue; // No point to comparing
                else
                    p_lhs::set(ssa_value_t(0, TYPE_U));
            }
            else
                p_lhs::set(h->input(info.loffset() + i));

            if(i < -info.rfrac)
            {
                if(info.validl(i) && h->input(info.loffset() + i).eq_whole(0))
                    continue; // No point to comparing.
                else if(i + 1 == info.maxwhole)
                    p_rhs::set(ssa_value_t(0, TYPE_U));
                else
                    continue; // No point to comparing.
            }
            else
                p_rhs::set(h->input(info.roffset() + i));

            if(iteration == 0) // If this is the first iteration
            {
                if(i + 1 == info.maxwhole) // If this is the only iteration
                {
                    if(p_lhs::value().eq_const_byte(0))
                    {
                        select_step<false>(
                            chain
                            < load_Z_for<Opt, p_rhs>
                            , simple_op<Opt, BNE_RELATIVE, null_, SuccessLabel>
                            , simple_op<Opt, BEQ_RELATIVE, null_, FailLabel>
                            >);
                        goto done;
                    }
                    else if(info.lsigned && info.rsigned) // If we're signed.
                    {
                        select_step<false>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                        {
                            chain
                            < load_AC<Opt, p_lhs, const_<1>>
                            , pick_op<Opt, SBC, null_, p_rhs>
                            >(cpu, prev, cont);

                            chain
                            < load_C<Opt, const_<1>>
                            , load_AX<typename Opt::restrict_to<~REGF_C>, p_lhs, p_rhs>
                            , iota_op<typename Opt::restrict_to<~REGF_C>, SBC_ABSOLUTE_X, null_>
                            >(cpu, prev, cont);

                            chain
                            < load_C<Opt, const_<1>>
                            , load_AY<typename Opt::restrict_to<~REGF_C>, p_lhs, p_rhs>
                            , iota_op<typename Opt::restrict_to<~REGF_C>, SBC_ABSOLUTE_Y, null_>
                            >(cpu, prev, cont);
                        });
                    }
                    else 
                        goto cmp_first;
                }
                else
                {
                cmp_first:
                    // We can use CMP for the first iteration:
                    select_step<false>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                    {
                        chain
                        < load_A<Opt, p_lhs>
                        , pick_op<Opt, CMP, null_, p_rhs>
                        >(cpu, prev, cont);

                        chain
                        < load_X<Opt, p_lhs>
                        , pick_op<Opt, CPX, null_, p_rhs>
                        >(cpu, prev, cont);

                        chain
                        < load_Y<Opt, p_lhs>
                        , pick_op<Opt, CPY, null_, p_rhs>
                        >(cpu, prev, cont);

                        chain
                        < load_AX<Opt, p_lhs, p_rhs>
                        , iota_op<Opt, CMP_ABSOLUTE_X, null_>
                        >(cpu, prev, cont);

                        chain
                        < load_AY<Opt, p_lhs, p_rhs>
                        , iota_op<Opt, CMP_ABSOLUTE_Y, null_>
                        >(cpu, prev, cont);
                    });
                }
            }
            else // This isn't the first iteration
            {
                select_step<false>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                {
                    chain
                    < load_A<Opt, p_lhs>
                    , pick_op<Opt, SBC, null_, p_rhs>
                    >(cpu, prev, cont);

                    chain
                    < load_AX<Opt, p_lhs, p_rhs>
                    , iota_op<Opt, SBC_ABSOLUTE_X, null_>
                    >(cpu, prev, cont);

                    chain
                    < load_AY<Opt, p_lhs, p_rhs>
                    , iota_op<Opt, SBC_ABSOLUTE_Y, null_>
                    >(cpu, prev, cont);
                });
            }

            ++iteration;

        } // End for

        if(-info.maxfrac < info.sbcwhole)
        {
            assert(iteration > 0);

            using p_overflow_label = param<struct lt_overflow_label_tag>;
            p_overflow_label::set(state.minor_label());

            if(info.lsigned && info.rsigned)
            {
                select_step<false>(
                    chain
                    < simple_op<Opt, BVC_RELATIVE, null_, p_overflow_label>
                    , exact_op<Opt, EOR_ABSOLUTE, null_, const_<0x80>>
                    , label<p_overflow_label>
                    , clear_conditional
                    , simple_op<Opt, BMI_RELATIVE, null_, SuccessLabel>
                    , simple_op<Opt, BPL_RELATIVE, null_, FailLabel>
                    >);
            }
            else
            {
                select_step<false>(
                    chain
                    < simple_op<Opt, BCC_RELATIVE, null_, SuccessLabel>
                    , simple_op<Opt, BCS_RELATIVE, null_, FailLabel>
                    >);
            }
        }
        else
            assert(iteration == 0);

    done:
        select_step<false>(clear_conditional);
    }

    template<bool LTE = false>
    void lt_store(ssa_ht h)
    {
        using fail = p_label<0>;
        using success = p_label<1>;
        using O = options<>;

        // For now, this implementation only loads the result in register X.
        select_step<false>(load_X<O, const_<0>>);

        fail::set(state.minor_label());
        success::set(state.minor_label());

        p_def::set(h);

        lt_branch<O::restrict_to<~REGF_X>, fail, success, LTE>(h);

        select_step<true>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
        {
            // Explicitly instantiate the labels.
            // (For some reason, GCC can't link without these lines. Could be a compiler bug.)
            (void)&label<success>;
            (void)&label<fail>;

            chain
            < label<std::conditional_t<LTE, fail, success>>
            , simple_op<O, INX_IMPLIED>
            , clear_conditional
            , label<std::conditional_t<LTE, success, fail>>
            , store<O, STX, p_def, p_def>
            >(cpu, prev, cont);
        });
    }

    template<typename Opt>
    void fill_array(ssa_value_t def, ssa_value_t val, unsigned start, unsigned len = 0)
    {
        using loop_label = p_label<0>;

        p_arg<0>::set(val);

        if(len >= 256)
        {
            loop_label::set(state.minor_label());

            select_step<false>(
                chain
                < load_AX<Opt, p_arg<0>, const_<0>>
                , label<loop_label>
                >);

            for(unsigned page = 0; page < len; page += 256)
            {
                p_arg<1>::set(def,  start + page);
                select_step<false>(exact_op<Opt, STA_ABSOLUTE_X, null_, p_arg<1>>) ;
            }

            select_step<false>(
                chain
                < simple_op<Opt, INX_IMPLIED>
                , simple_op<Opt, BNE_RELATIVE, null_, loop_label>
                , clear_conditional
                , set_defs<Opt, REGF_X, true, const_<0>>
                >);
        }

        unsigned const left = len % 256;
        unsigned const iter = left / 2;
        if(iter)
        {
            loop_label::set(state.minor_label());
            p_arg<1>::set(locator_t::const_byte(iter - 1));
            p_arg<2>::set(def, (start + len - left));
            p_arg<3>::set(def,  (start + len - left) + iter);

            select_step<false>(
                chain
                < load_AX<Opt, p_arg<0>, p_arg<1>>
                , label<loop_label>
                , exact_op<Opt, STA_ABSOLUTE_X, null_, p_arg<2>>
                , exact_op<Opt, STA_ABSOLUTE_X, null_, p_arg<3>>
                , simple_op<Opt, DEX_IMPLIED>
                , simple_op<Opt, BPL_RELATIVE, null_, loop_label>
                , clear_conditional
                , set_defs<Opt, REGF_X, true, const_<0xFF>>
                >);
        }

        if(left % 2)
        {
            p_arg<1>::set(def,  start + len - 1);
            select_step<false>(
                chain
                < load_A<Opt, p_arg<0>>
                , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<1>>
                >);
        }
    }

    template<typename Opt>
    void copy_array(ssa_value_t from, ssa_value_t def, unsigned resize_to = 0)
    {
        using loop_label = p_label<0>;

        passert(is_tea(from.type().name()), from, from.type());

        if(asm_arg(def) == asm_arg(from))
            return;

        unsigned len = from.type().size();
        if(resize_to)
            len = std::min<unsigned>(len, resize_to);
        constexpr unsigned half_unroll = 2;

        if(len <= half_unroll * 2)
        {
            for(unsigned i = 0; i < len; ++i)
            {
                p_arg<0>::set(def, i);
                p_arg<1>::set(from, i);
                select_step<true>(load_then_store<Opt, p_arg<0>, p_arg<1>, p_arg<0>, false>);
            }
        }
        else
        {
            if(len >= 256)
            {
                loop_label::set(state.minor_label());

                select_step<false>(
                    chain
                    < load_X<Opt, const_<0>>
                    , label<loop_label>
                    >);

                for(unsigned page = 0; page < len; page += 256)
                {
                    p_arg<0>::set(from, page);
                    p_arg<1>::set(def,  page);
                    select_step<false>(
                        chain
                        < exact_op<Opt, LDA_ABSOLUTE_X, null_, p_arg<0>>
                        , exact_op<Opt, STA_ABSOLUTE_X, null_, p_arg<1>>
                        >);
                }

                select_step<false>(
                    chain
                    < simple_op<Opt, INX_IMPLIED>
                    , simple_op<Opt, BNE_RELATIVE, null_, loop_label>
                    , clear_conditional
                    , set_defs<Opt, REGF_X, true, const_<0>>
                    >);
            }

            unsigned const start = len - (len % 256);
            unsigned const iter = (len % 256) / half_unroll;
            assert(iter <= 0x80);

            p_arg<0>::set(locator_t::const_byte(iter - 1));
            p_arg<1>::set(from, 0 * iter + start);
            p_arg<2>::set(def,  0 * iter + start);
            p_arg<3>::set(from, 1 * iter + start);
            p_arg<4>::set(def,  1 * iter + start);
            loop_label::set(state.minor_label());
            
            select_step<false>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
            {
                chain
                < load_X<Opt, p_arg<0>>
                , label<loop_label>
                , exact_op<Opt, LDA_ABSOLUTE_X, null_, p_arg<1>>
                , exact_op<Opt, STA_ABSOLUTE_X, null_, p_arg<2>>
                , exact_op<Opt, LDA_ABSOLUTE_X, null_, p_arg<3>>
                , exact_op<Opt, STA_ABSOLUTE_X, null_, p_arg<4>>
                , simple_op<Opt, DEX_IMPLIED>
                , simple_op<Opt, BPL_RELATIVE, null_, loop_label>
                , clear_conditional
                , set_defs<Opt, REGF_X, true, const_<0xFF>>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, p_arg<0>>
                , label<loop_label>
                , exact_op<Opt, LDA_ABSOLUTE_Y, null_, p_arg<1>>
                , exact_op<Opt, STA_ABSOLUTE_Y, null_, p_arg<2>>
                , exact_op<Opt, LDA_ABSOLUTE_Y, null_, p_arg<3>>
                , exact_op<Opt, STA_ABSOLUTE_Y, null_, p_arg<4>>
                , simple_op<Opt, DEY_IMPLIED>
                , simple_op<Opt, BPL_RELATIVE, null_, loop_label>
                , clear_conditional
                , set_defs<Opt, REGF_Y, true, const_<0xFF>>
                >(cpu, prev, cont);
            });

            for(unsigned i = iter * half_unroll; i < (len % 256); ++i)
            {
                p_arg<0>::set(from, i + start);
                p_def::set(def, i + start);
                select_step<false>(load_then_store<Opt, p_arg<0>, p_arg<0>, p_def, false>);
            }
        }

        if(len < resize_to)
            fill_array<Opt>(def, ssa_value_t(0, TYPE_U), len, resize_to - len);
    }

    template<typename Opt>
    void write_globals(ssa_ht h)
    {
        // TODO: Create a sorted order of the globals before writing.
        for_each_written_global(h, [h](ssa_value_t def, locator_t loc)
        {
            if(def.is_handle() && cset_locator(def.handle()) == loc)
                return;

            if(is_tea(def.type().name()))
                copy_array<Opt>(def, loc);
            else
            {
                p_def::set(def);
                p_arg<0>::set(loc);

                select_step<true>(load_then_store<Opt, p_def, p_def, p_arg<0>, false>);
            }
        });
    }

    template<typename Opt, typename Def, typename Array, typename Index>
    void read_array(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const index = Index::value();

        if(index.is_const_num())
        {
            using temp = param<struct read_array_tag>;

            locator_t mem = Array::trans();
            mem.advance_offset(index.data());
            mem.set_is(IS_DEREF);
            temp::set(mem);

            chain
            < exact_op<Opt, LDA_ABSOLUTE, Def, temp>
            , store<Opt, STA, Def, Def>
            >(cpu, prev, cont);

            chain
            < exact_op<Opt, LAX_ABSOLUTE, Def, temp>
            , store<Opt, STA, Def, Def>
            >(cpu, prev, cont);

            chain
            < exact_op<Opt, LDX_ABSOLUTE, Def, temp>
            , store<Opt, STX, Def, Def>
            >(cpu, prev, cont);

            chain
            < exact_op<Opt, LDY_ABSOLUTE, Def, temp>
            , store<Opt, STY, Def, Def>
            >(cpu, prev, cont);
        }
        else
        {
            chain
            < load_X<Opt, Index>
            , exact_op<Opt, LDA_ABSOLUTE_X, Def, Array>
            , store<Opt, STA, Def, Def>
            >(cpu, prev, cont);

            chain
            < load_X<Opt, Index>
            , exact_op<Opt, LDY_ABSOLUTE_X, Def, Array>
            , store<Opt, STY, Def, Def>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, Index>
            , exact_op<Opt, LDA_ABSOLUTE_Y, Def, Array>
            , store<Opt, STA, Def, Def>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, Index>
            , exact_op<Opt, LDX_ABSOLUTE_Y, Def, Array>
            , store<Opt, STX, Def, Def>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, Index>
            , exact_op<Opt, LAX_ABSOLUTE_Y, Def, Array>
            , store<Opt, STA, Def, Def>
            >(cpu, prev, cont);
        }
    }

    template<typename Opt, typename Array, typename Index, typename Assignment>
    void write_array(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        locator_t const index = Index::value();

        if(index.is_const_num())
        {
            using temp = param<struct write_array_tag>;

            locator_t mem = Array::trans();
            mem.advance_offset(index.data());
            mem.set_is(IS_DEREF);
            temp::set(mem);

            load_then_store<Opt, Assignment, Assignment, temp, false>(cpu, prev, cont);
        }
        else
        {
            chain
            < load_AX<Opt, Assignment, Index>
            , exact_op<Opt, STA_ABSOLUTE_X, null_, Array>
            >(cpu, prev, cont);

            chain
            < load_AY<Opt, Assignment, Index>
            , exact_op<Opt, STA_ABSOLUTE_Y, null_, Array>
            >(cpu, prev, cont);
        }
    }

    template<typename Opt, typename Def, bool InvertZ>
    void store_Z_impl(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        ssa_value_t v = Def::node();

        if(v.holds_ref() && (v->output_size() > 1 || (v->output_size() == 1 && v->output(0)->cfg_node() != v->cfg_node())))
        {
            constexpr regs_t Z = InvertZ ? cpu_t::REGF_INVERTED_Z : REGF_Z;

            p_label<0>::set(state.minor_label());
            p_label<1>::set(state.minor_label());

            chain
            < simple_op<Opt, PHP_IMPLIED>
            , simple_op<Opt, PLA_IMPLIED>
            , simple_op<Opt, ALR_IMMEDIATE, null_, const_<0b10>>
            , exact_op<Opt, STA_ABSOLUTE, null_, p_def>
            , set_defs<Opt, REGF_A | Z, true, p_def>
            >(cpu, prev, cont);

            chain
            < simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>
            , simple_op<Opt, LDX_IMMEDIATE, null_, const_<0>>
            , exact_op<Opt, JMP_ABSOLUTE, null_, p_label<1>>
            , label<p_label<0>>
            , simple_op<Opt, LDX_IMMEDIATE, null_, const_<1>>
            , label<p_label<1>>
            , clear_conditional
            , exact_op<Opt, STX_ABSOLUTE, null_, p_def>
            , set_defs<Opt, REGF_X | Z, true, p_def>
            >(cpu, prev, cont);

            chain
            < simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>
            , simple_op<Opt, LDY_IMMEDIATE, null_, const_<0>>
            , exact_op<Opt, JMP_ABSOLUTE, null_, p_label<1>>
            , label<p_label<0>>
            , simple_op<Opt, LDY_IMMEDIATE, null_, const_<1>>
            , label<p_label<1>>
            , clear_conditional
            , exact_op<Opt, STY_ABSOLUTE, null_, p_def>
            , set_defs<Opt, REGF_Y | Z, true, p_def>
            >(cpu, prev, cont);
        }
        else
        {
            cpu_t new_cpu = cpu;
            if(new_cpu.set_def<InvertZ ? cpu_t::REG_INVERTED_Z : REG_Z>(Opt::to_struct, Def::value(), true))
                cont->call(new_cpu, alloc_sel<MAYBE_STORE_Z>(cpu, prev, Def::trans()));
        }
    }

    template<typename Opt, typename Def>
    void store_Z(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        if(cpu.inverted_z()) [[unlikely]]
            store_Z_impl<Opt, Def, true>(cpu, prev, cont);
        else
            store_Z_impl<Opt, Def, false>(cpu, prev, cont);
    }

    template<typename Opt, typename Def>
    void store_N(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        ssa_value_t v = Def::node();

        if(v.holds_ref() && (v->output_size() > 1 || (v->output_size() == 1 && v->output(0)->cfg_node() != v->cfg_node())))
        {
            p_label<0>::set(state.minor_label());
            p_label<1>::set(state.minor_label());

            chain
            < simple_op<Opt, PHP_IMPLIED>
            , simple_op<Opt, PLA_IMPLIED>
            , simple_op<Opt, ANC_IMMEDIATE, null_, const_<0x80>>
            , simple_op<Opt, ROL_IMPLIED>
            , exact_op<Opt, STA_ABSOLUTE, null_, p_def>
            , set_defs<Opt, REGF_A | REGF_N, true, p_def>
            >(cpu, prev, cont);

            chain
            < simple_op<Opt, BPL_RELATIVE, null_, p_label<0>>
            , simple_op<Opt, LDX_IMMEDIATE, null_, const_<0>>
            , exact_op<Opt, JMP_ABSOLUTE, null_, p_label<1>>
            , label<p_label<0>>
            , simple_op<Opt, LDX_IMMEDIATE, null_, const_<1>>
            , label<p_label<1>>
            , clear_conditional
            , exact_op<Opt, STX_ABSOLUTE, null_, p_def>
            , set_defs<Opt, REGF_X | REGF_N, true, p_def>
            >(cpu, prev, cont);

            chain
            < simple_op<Opt, BPL_RELATIVE, null_, p_label<0>>
            , simple_op<Opt, LDY_IMMEDIATE, null_, const_<0>>
            , exact_op<Opt, JMP_ABSOLUTE, null_, p_label<1>>
            , label<p_label<0>>
            , simple_op<Opt, LDY_IMMEDIATE, null_, const_<1>>
            , label<p_label<1>>
            , clear_conditional
            , exact_op<Opt, STY_ABSOLUTE, null_, p_def>
            , set_defs<Opt, REGF_Y | REGF_N, true, p_def>
            >(cpu, prev, cont);
        }
        else
        {
            cpu_t new_cpu = cpu;
            if(cpu.bool_in_n())
            {
                if(new_cpu.set_def<cpu_t::REG_BOOL_IN_N>(Opt::to_struct, Def::value(), true))
                    cont->call(new_cpu, alloc_sel<MAYBE_STORE_N>(cpu, prev, Def::trans()));
            }
            else
            {
                if(new_cpu.set_def<REG_N>(Opt::to_struct, Def::value(), true))
                    cont->call(new_cpu, alloc_sel<MAYBE_STORE_N>(cpu, prev, Def::trans()));
            }
        }
    }

    template<typename Opt, typename Def>
    void store_C(cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
    {
        ssa_value_t const h = Def::node();

        auto const used_as_carry = [&]() -> bool
        {
            assert(h->output_size() == 1);
            auto const oe = h->output_edge(0);
            return possible_carry_input_i(oe.handle->op()) == int(oe.index);
        };

        if(h.holds_ref() 
           && (h->output_size() > 1 
               || (h->output_size() == 1 
                   && (h->output(0)->cfg_node() != h->cfg_node()
                       || !used_as_carry()))))
        {
            p_label<0>::set(state.minor_label());

            chain
            < load_A<Opt, const_<0>>
            , simple_op<typename Opt::valid_for<REGF_A>, ROL_IMPLIED>
            , set_defs<Opt, REGF_A | REGF_N | REGF_Z, true, p_def>
            , exact_op<Opt, STA_LIKELY, null_, p_def>
            >(cpu, prev, cont);

            chain
            < load_X<Opt, const_<0>>
            , simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>
            , simple_op<Opt, INX_IMPLIED>
            , maybe_carry_label_clear_conditional<Opt, p_label<0>, false>
            , set_defs<Opt, REGF_X | REGF_N | REGF_Z, true, p_def>
            , exact_op<Opt, STX_LIKELY, null_, p_def>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, const_<0>>
            , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
            , exact_op<Opt, INY_IMPLIED>
            , maybe_carry_label_clear_conditional<Opt, p_label<0>, false>
            , set_defs<Opt, REGF_Y | REGF_N | REGF_Z, true, p_def>
            , exact_op<Opt, STY_LIKELY, null_, p_def>
            >(cpu, prev, cont);
        }
        else
        {
            cpu_t new_cpu = cpu;
            if(new_cpu.set_def<REG_C>(Opt::to_struct, Def::value(), true))
                cont->call(new_cpu, alloc_sel<MAYBE_STORE_C>(cpu, prev, Def::trans()));
        }
    }

    void isel_node_simple(ssa_ht h, cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
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

        using Opt = options<>;
        using carry_Z = condition<struct zero_outputs_tag>;

        switch(h->op())
        {
        case SSA_ready:
            p_arg<0>::set(locator_t::runtime_ram(RTRAM_nmi_ready));
            load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            break;

        case SSA_system:
            p_arg<0>::set(locator_t::runtime_ram(RTRAM_system));
            load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            break;

        case SSA_carry:
            {
                locator_t const v = p_def::value();

                if(cpu.value_eq(REG_A, v))
                    exact_op<Opt, STA_MAYBE, null_, p_def>(cpu, prev, cont);
                else if(cpu.value_eq(REG_X, v))
                    exact_op<Opt, STX_MAYBE, null_, p_def>(cpu, prev, cont);
                else if(cpu.value_eq(REG_Y, v))
                    exact_op<Opt, STY_MAYBE, null_, p_def>(cpu, prev, cont);
                else if(cpu.def_eq(REG_C, v))
                    store_C<Opt, p_def>(cpu, prev, cont);
                else if(cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, v))
                {
                    assert(!cpu.def_eq(REG_Z, v));
                    store_Z<Opt, p_def>(cpu, prev, cont);
                }
                else if(cpu.def_eq<cpu_t::REGF_BOOL_IN_N>(REG_N, v))
                    store_N<Opt, p_def>(cpu, prev, cont);
                else
                    passert(0, h, h->input(0), cpu.inverted_z()); // Input node must output carry.
            }
            break;

        case SSA_add:
            {
                ssa_ht const carry_output = ::carry_output(*h);
                p_carry::set(h->input(2));
                p_carry_output::set(carry_output ? carry_output : h);
                carry_Z::set(!!carry_output);

                // NOTE: Sometimes we track the carry using Z, 
                // which is problematic as it overrides the node's Z.
                // It would be better to either track both,
                // or to more accurately decide which should have priority.

                commutative(h, [&]()
                {
                    using in_IZ = condition<struct in_IZ_tag>;

                    chain
                    < load_AC<Opt, p_lhs, p_carry>
                    , pick_op<Opt::valid_for<REGF_A | REGF_NZ>, ADC, p_def, p_rhs>
                    , store<Opt, STA, p_def, p_def>
                    , set_defs<Opt, REGF_C, true, p_carry_output>
                    >(cpu, prev, cont);

                    chain
                    < load_AC<Opt, p_lhs, p_carry>
                    , load_X<Opt::restrict_to<~REGF_AC>, p_rhs>
                    , iota_op<Opt::valid_for<REGF_A | REGF_NZ>, ADC_ABSOLUTE_X, p_def>
                    , store<Opt, STA, p_def, p_def>
                    , set_defs<Opt, REGF_C, true, p_carry_output>
                    >(cpu, prev, cont);

                    chain
                    < load_AC<Opt, p_lhs, p_carry>
                    , load_Y<Opt::restrict_to<~REGF_AC>, p_rhs>
                    , iota_op<Opt::valid_for<REGF_A | REGF_NZ>, ADC_ABSOLUTE_Y, p_def>
                    , store<Opt, STA, p_def, p_def>
                    , set_defs<Opt, REGF_C, true, p_carry_output>
                    >(cpu, prev, cont);

                    if(p_rhs::value().is_const_num())
                    {
                        p_label<0>::set(state.minor_label());

                        if(p_rhs::value().data() == 0 && !carry_output)
                        {
                            if(cpu.def_eq(REG_C, p_carry::value()) || cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, p_carry::value()))
                            {
                                in_IZ::set(cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, p_carry::value()));

                                if(cpu.def_eq(REG_X, p_lhs::value()))
                                {
                                    chain
                                    < if_<Opt, in_IZ, simple_op<Opt, BNE_RELATIVE, null_, p_label<0>>,
                                                      simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>>
                                    , simple_op<Opt, INX_IMPLIED>
                                    , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                      maybe_carry_label_clear_conditional<Opt, p_label<0>, false>>
                                    , store<Opt, STX, p_def, p_def>
                                    >(cpu, prev, cont);
                                }

                                if(cpu.def_eq(REG_Y, p_lhs::value()))
                                {
                                    chain
                                    < if_<Opt, in_IZ, simple_op<Opt, BNE_RELATIVE, null_, p_label<0>>,
                                                      simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>>
                                    , simple_op<Opt, INY_IMPLIED>
                                    , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                      maybe_carry_label_clear_conditional<Opt, p_label<0>, false>>
                                    , store<Opt, STY, p_def, p_def>
                                    >(cpu, prev, cont);
                                }

                                if(p_def::trans() == p_lhs::trans())
                                {
                                    chain
                                    < if_<Opt, in_IZ, simple_op<Opt, BNE_RELATIVE, null_, p_label<0>>,
                                                      simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>>
                                    , pick_op<Opt, INC, p_def, p_lhs>
                                    , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                      maybe_carry_label_clear_conditional<Opt, p_label<0>, false>>
                                    >(cpu, prev, cont);
                                }
                            }
                        }

                        if(p_rhs::value().data() == 0xFF && !carry_output && cpu.def_eq(REG_C, p_carry::value()))
                        {
                            in_IZ::set(cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, p_carry::value()));

                            p_label<0>::set(state.minor_label());

                            if(cpu.def_eq(REG_X, p_lhs::value()))
                            {
                                chain
                                < if_<Opt, in_IZ, simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>,
                                                  simple_op<Opt, BCS_RELATIVE, null_, p_label<0>>>
                                , simple_op<Opt, DEX_IMPLIED>
                                , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                  maybe_carry_label_clear_conditional<Opt, p_label<0>, true>>
                                , store<Opt, STX, p_def, p_def>
                                >(cpu, prev, cont);
                            }

                            if(cpu.def_eq(REG_Y, p_lhs::value()))
                            {
                                chain
                                < if_<Opt, in_IZ, simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>,
                                                  simple_op<Opt, BCS_RELATIVE, null_, p_label<0>>>
                                , simple_op<Opt, DEY_IMPLIED>
                                , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                  maybe_carry_label_clear_conditional<Opt, p_label<0>, true>>
                                , store<Opt, STY, p_def, p_def>
                                >(cpu, prev, cont);
                            }

                            if(p_def::trans() == p_lhs::trans())
                            {
                                chain
                                < if_<Opt, in_IZ, simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>,
                                                  simple_op<Opt, BCS_RELATIVE, null_, p_label<0>>>
                                , pick_op<Opt, DEC, p_def, p_def>
                                , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                  maybe_carry_label_clear_conditional<Opt, p_label<0>, true>>
                                >(cpu, prev, cont);
                            }
                        }

                        if(p_carry::value().is_const_num())
                        {
                            p_arg<2>::set(ssa_value_t((0x100 - p_rhs::value().data() - !!p_carry::value().data()) & 0xFF, TYPE_U));

                            chain
                            < load_AX<Opt, p_lhs, p_lhs>
                            , simple_op<Opt::valid_for<REGF_X | REGF_NZ>, AXS_IMMEDIATE, p_def, p_arg<2>>
                            , store<Opt, STX, p_def, p_def>
                            , set_defs<Opt, REGF_C, true, p_carry_output>
                            >(cpu, prev, cont);

                            std::uint8_t const sum = p_rhs::value().data() + !!p_carry::value().data();

                            if(sum == 1)
                            {
                                chain
                                < load_X<Opt, p_lhs>
                                , simple_op<Opt, INX_IMPLIED, p_def>
                                , store<Opt, STX, p_def, p_def>
                                , if_<Opt, carry_Z, set_defs<Opt, cpu_t::REGF_INVERTED_Z, true, p_carry_output>>
                                >(cpu, prev, cont);

                                chain
                                < load_Y<Opt, p_lhs>
                                , simple_op<Opt, INY_IMPLIED, p_def>
                                , store<Opt, STY, p_def, p_def>
                                , if_<Opt, carry_Z, set_defs<Opt, cpu_t::REGF_INVERTED_Z, true, p_carry_output>>
                                >(cpu, prev, cont);

                                if(p_def::trans() == p_lhs::trans())
                                {
                                    chain
                                    < pick_op<Opt, INC, p_def, p_lhs>
                                    , if_<Opt, carry_Z, set_defs<Opt, cpu_t::REGF_INVERTED_Z, true, p_carry_output>>
                                    >(cpu, prev, cont);
                                }
                            }
                            else if(!carry_output)
                            {
                                if(sum == 2)
                                {
                                    chain
                                    < load_X<Opt, p_lhs>
                                    , simple_op<Opt, INX_IMPLIED, p_def>
                                    , simple_op<Opt, INX_IMPLIED, p_def>
                                    , store<Opt, STX, p_def, p_def>
                                    >(cpu, prev, cont);

                                    chain
                                    < load_Y<Opt, p_lhs>
                                    , simple_op<Opt, INY_IMPLIED, p_def>
                                    , simple_op<Opt, INY_IMPLIED, p_def>
                                    , store<Opt, STY, p_def, p_def>
                                    >(cpu, prev, cont);

                                    if(p_def::trans() == p_lhs::trans())
                                    {
                                        chain
                                        < pick_op<Opt, INC, p_def, p_lhs>
                                        , pick_op<Opt, INC, p_def, p_lhs>
                                        >(cpu, prev, cont);
                                    }
                                }
                                else if(sum == 0xFF)
                                {
                                    chain
                                    < load_X<Opt, p_lhs>
                                    , simple_op<Opt, DEX_IMPLIED, p_def>
                                    , store<Opt, STX, p_def, p_def>
                                    >(cpu, prev, cont);

                                    chain
                                    < load_Y<Opt, p_lhs>
                                    , simple_op<Opt, DEY_IMPLIED, p_def>
                                    , store<Opt, STY, p_def, p_def>
                                    >(cpu, prev, cont);

                                    if(p_def::trans() == p_lhs::trans())
                                        pick_op<Opt, DEC, p_def, p_lhs>(cpu, prev, cont);
                                }
                                else if(sum == 0xFE)
                                {
                                    chain
                                    < load_X<Opt, p_lhs>
                                    , simple_op<Opt, DEX_IMPLIED, p_def>
                                    , simple_op<Opt, DEX_IMPLIED, p_def>
                                    , store<Opt, STX, p_def, p_def>
                                    >(cpu, prev, cont);

                                    chain
                                    < load_Y<Opt, p_lhs>
                                    , simple_op<Opt, DEY_IMPLIED, p_def>
                                    , simple_op<Opt, DEY_IMPLIED, p_def>
                                    , store<Opt, STY, p_def, p_def>
                                    >(cpu, prev, cont);

                                    if(p_def::trans() == p_lhs::trans())
                                    {
                                        chain
                                        < pick_op<Opt, DEC, p_def, p_lhs>
                                        , pick_op<Opt, DEC, p_def, p_lhs>
                                        >(cpu, prev, cont);
                                    }
                                }
                            }
                        }
                    }
                });
            }
            break;

        case SSA_sub:
            {
                ssa_ht const carry_output = ::carry_output(*h);
                p_lhs::set(h->input(0));
                p_rhs::set(h->input(1));
                p_carry::set(h->input(2));
                p_carry_output::set(carry_output ? carry_output : h);
                carry_Z::set(!!carry_output);

                // NOTE: Sometimes we track the carry using Z, 
                // which is problematic as it overrides the node's Z.
                // It would be better to either track both,
                // or to more accurately decide which should have priority.

                using in_IZ = condition<struct in_IZ_tag>;

                chain
                < load_AC<Opt, p_lhs, p_carry>
                , pick_op<Opt::valid_for<REGF_A | REGF_NZ>, SBC, p_def, p_rhs>
                , store<Opt, STA, p_def, p_def>
                , set_defs<Opt, REGF_C, true, p_carry_output>
                >(cpu, prev, cont);

                chain
                < load_AC<Opt, p_lhs, p_carry>
                , load_X<Opt::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<Opt::valid_for<REGF_A | REGF_NZ>, SBC_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def, p_def>
                , set_defs<Opt, REGF_C, true, p_carry_output>
                >(cpu, prev, cont);

                chain
                < load_AC<Opt, p_lhs, p_carry>
                , load_Y<Opt::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<Opt::valid_for<REGF_A | REGF_NZ>, SBC_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def, p_def>
                , set_defs<Opt, REGF_C, true, p_carry_output>
                >(cpu, prev, cont);

                if(p_lhs::node().eq_whole(0))
                {
                    chain
                    < load_AC<Opt, p_rhs, p_carry>
                    , exact_op<Opt, EOR_IMMEDIATE, null_, const_<0xFF>>
                    , exact_op<Opt, ADC_IMMEDIATE, null_, const_<0>>
                    , store<Opt, STA, p_def, p_def>
                    , set_defs<Opt, REGF_C, true, p_carry_output>
                    >(cpu, prev, cont);

                    if(!carry_output && p_carry::node().eq_whole(1))
                    {
                        chain
                        < load_AC<Opt, p_rhs, const_<0>>
                        , exact_op<Opt, EOR_IMMEDIATE, null_, const_<0xFF>>
                        , exact_op<Opt, ADC_IMMEDIATE, null_, const_<1>>
                        , store<Opt, STA, p_def, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_A<Opt, p_rhs>
                        , exact_op<Opt, EOR_IMMEDIATE, null_, const_<0xFF>>
                        , exact_op<Opt, TAX_IMPLIED, null_>
                        , exact_op<Opt, INX_IMPLIED, null_>
                        , store<Opt, STX, p_def, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_A<Opt, p_rhs>
                        , exact_op<Opt, EOR_IMMEDIATE, null_, const_<0xFF>>
                        , exact_op<Opt, TAY_IMPLIED, null_>
                        , exact_op<Opt, INY_IMPLIED, null_>
                        , store<Opt, STY, p_def, p_def>
                        >(cpu, prev, cont);
                    }
                }

                if(p_rhs::value().is_const_num())
                {
                    p_label<0>::set(state.minor_label());

                    if(p_rhs::value().data() == 0 && !carry_output)
                    {
                        if(cpu.def_eq(REG_C, p_carry::value()) || cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, p_carry::value()))
                        {
                            in_IZ::set(cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, p_carry::value()));

                            if(cpu.def_eq(REG_X, p_lhs::value()))
                            {
                                chain
                                < if_<Opt, in_IZ, simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>,
                                                  simple_op<Opt, BCS_RELATIVE, null_, p_label<0>>>
                                , simple_op<Opt, DEX_IMPLIED>
                                , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                  maybe_carry_label_clear_conditional<Opt, p_label<0>, true>>
                                , store<Opt, STX, p_def, p_def>
                                >(cpu, prev, cont);
                            }

                            if(cpu.def_eq(REG_Y, p_lhs::value()))
                            {
                                chain
                                < if_<Opt, in_IZ, simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>,
                                                  simple_op<Opt, BCS_RELATIVE, null_, p_label<0>>>
                                , simple_op<Opt, DEY_IMPLIED>
                                , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                  maybe_carry_label_clear_conditional<Opt, p_label<0>, true>>
                                , store<Opt, STY, p_def, p_def>
                                >(cpu, prev, cont);
                            }

                            if(p_def::trans() == p_lhs::trans())
                            {
                                chain
                                < if_<Opt, in_IZ, simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>,
                                                  simple_op<Opt, BCS_RELATIVE, null_, p_label<0>>>
                                , pick_op<Opt, DEC, p_def, p_lhs>
                                , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                                  maybe_carry_label_clear_conditional<Opt, p_label<0>, true>>
                                >(cpu, prev, cont);
                            }
                        }
                    }

                    if(p_rhs::value().data() == 0xFF && cpu.def_eq(REG_C, p_carry::value()))
                    {
                        in_IZ::set(cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, p_carry::value()));

                        p_label<0>::set(state.minor_label());

                        if(cpu.def_eq(REG_X, p_lhs::value()))
                        {
                            chain
                            < if_<Opt, in_IZ, simple_op<Opt, BNE_RELATIVE, null_, p_label<0>>,
                                              simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>>
                            , simple_op<Opt, INX_IMPLIED>
                            , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                              maybe_carry_label_clear_conditional<Opt, p_label<0>, false>>
                            , store<Opt, STX, p_def, p_def>
                            >(cpu, prev, cont);
                        }

                        if(cpu.def_eq(REG_Y, p_lhs::value()))
                        {
                            chain
                            < if_<Opt, in_IZ, simple_op<Opt, BNE_RELATIVE, null_, p_label<0>>,
                                              simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>>
                            , simple_op<Opt, INY_IMPLIED>
                            , label<p_label<0>>
                            , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                              maybe_carry_label_clear_conditional<Opt, p_label<0>, false>>
                            , store<Opt, STY, p_def, p_def>
                            >(cpu, prev, cont);
                        }

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < if_<Opt, in_IZ, simple_op<Opt, BNE_RELATIVE, null_, p_label<0>>,
                                              simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>>
                            , pick_op<Opt, DEC, p_def, p_def>
                            , if_<Opt, in_IZ, chain<label<p_label<0>>, clear_conditional>,
                                              maybe_carry_label_clear_conditional<Opt, p_label<0>, false>>
                            >(cpu, prev, cont);
                        }
                    }

                    if(p_carry::value().is_const_num())
                    {
                        p_arg<2>::set(ssa_value_t((p_rhs::value().data() - (1 - !!p_carry::value().data())) & 0xFF, TYPE_U));

                        chain
                        < load_AX<Opt, p_lhs, p_lhs>
                        , simple_op<Opt::valid_for<REGF_X | REGF_NZ>, AXS_IMMEDIATE, p_def, p_arg<2>>
                        , store<Opt, STX, p_def, p_def>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        std::uint8_t const sum = p_rhs::value().data() - (1 - !!p_carry::value().data());

                        if(sum == 0xFF)
                        {
                            chain
                            < load_X<Opt, p_lhs>
                            , simple_op<Opt, INX_IMPLIED, p_def>
                            , store<Opt, STX, p_def, p_def>
                            , if_<Opt, carry_Z, set_defs<Opt, cpu_t::REGF_INVERTED_Z, true, p_carry_output>>
                            >(cpu, prev, cont);

                            chain
                            < load_Y<Opt, p_lhs>
                            , simple_op<Opt, INY_IMPLIED, p_def>
                            , store<Opt, STY, p_def, p_def>
                            , if_<Opt, carry_Z, set_defs<Opt, cpu_t::REGF_INVERTED_Z, true, p_carry_output>>
                            >(cpu, prev, cont);

                            if(p_def::trans() == p_lhs::trans())
                            {
                                chain
                                < pick_op<Opt, INC, p_def, p_lhs>
                                , if_<Opt, carry_Z, set_defs<Opt, cpu_t::REGF_INVERTED_Z, true, p_carry_output>>
                                >(cpu, prev, cont);
                            }
                        }
                        else if(!carry_output)
                        {
                            if(sum == 1 && !carry_output)
                            {
                                chain
                                < load_X<Opt, p_lhs>
                                , simple_op<Opt, DEX_IMPLIED, p_def>
                                , store<Opt, STX, p_def, p_def>
                                >(cpu, prev, cont);

                                chain
                                < load_Y<Opt, p_lhs>
                                , simple_op<Opt, DEY_IMPLIED, p_def>
                                , store<Opt, STY, p_def, p_def>
                                >(cpu, prev, cont);

                                if(p_def::trans() == p_lhs::trans())
                                {
                                    chain
                                    < pick_op<Opt, DEC, p_def, p_lhs>
                                    >(cpu, prev, cont);
                                }
                            }
                            else if(sum == 2)
                            {
                                chain
                                < load_X<Opt, p_lhs>
                                , simple_op<Opt, DEX_IMPLIED, p_def>
                                , simple_op<Opt, DEX_IMPLIED, p_def>
                                , store<Opt, STX, p_def, p_def>
                                >(cpu, prev, cont);

                                chain
                                < load_Y<Opt, p_lhs>
                                , simple_op<Opt, DEY_IMPLIED, p_def>
                                , simple_op<Opt, DEY_IMPLIED, p_def>
                                , store<Opt, STY, p_def, p_def>
                                >(cpu, prev, cont);

                                if(p_def::trans() == p_lhs::trans())
                                {
                                    chain
                                    < pick_op<Opt, DEC, p_def, p_lhs>
                                    , pick_op<Opt, DEC, p_def, p_lhs>
                                    >(cpu, prev, cont);
                                }
                            }
                            else if(sum == 0xFE)
                            {
                                chain
                                < load_X<Opt, p_lhs>
                                , simple_op<Opt, INX_IMPLIED, p_def>
                                , simple_op<Opt, INX_IMPLIED, p_def>
                                , store<Opt, STX, p_def, p_def>
                                >(cpu, prev, cont);

                                chain
                                < load_Y<Opt, p_lhs>
                                , simple_op<Opt, INY_IMPLIED, p_def>
                                , simple_op<Opt, INY_IMPLIED, p_def>
                                , store<Opt, STY, p_def, p_def>
                                >(cpu, prev, cont);

                                if(p_def::trans() == p_lhs::trans())
                                {
                                    chain
                                    < pick_op<Opt, INC, p_def, p_lhs>
                                    , pick_op<Opt, INC, p_def, p_lhs>
                                    >(cpu, prev, cont);
                                }
                            }
                        }
                    }
                }
            }
            break;

        case SSA_mul:
        case SSA_mul8_lo:
            p_arg<2>::set(locator_t::runtime_rom(RTROM_mul8));

            commutative(h, [&]()
            {
                chain
                < load_AY<Opt, p_lhs, p_rhs>
                , simple_op<Opt, read_reg_op(REGF_A | REGF_Y)>
                , exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<2>>
                , simple_op<Opt, write_reg_op(REGF_ISEL & ~REGF_X)>
                , store<Opt::template restrict_to<~REGF_X>, STA, p_def, p_def>
                >(cpu, prev, cont);
            });
            break;

        case SSA_mul8_hi:
            store<Opt, STY, p_def, p_def>(cpu, prev, cont);
            break;

        case SSA_and:
            commutative(h, [&]()
            {
                chain
                < load_A<Opt, p_lhs>
                , pick_op<Opt, AND, p_def, p_rhs>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , simple_op<Opt::valid_for<REGF_X | REGF_NZ>, AXS_IMMEDIATE, p_def, const_<0>>
                , store<Opt, STX, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , iota_op<Opt, AND_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, p_lhs, p_rhs>
                , iota_op<Opt, AND_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , store<Opt, SAX, p_def, p_def, false>
                >(cpu, prev, cont);
            });
            break;

        case SSA_or:
            commutative(h, [&]()
            {
                chain
                < load_A<Opt, p_lhs>
                , pick_op<Opt, ORA, p_def, p_rhs>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , iota_op<Opt, ORA_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, p_lhs, p_rhs>
                , iota_op<Opt, ORA_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);
            });
            break;

        case SSA_xor:
            commutative(h, [&]()
            {
                chain
                < load_A<Opt, p_lhs>
                , pick_op<Opt, EOR, p_def, p_rhs>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , iota_op<Opt, EOR_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, p_lhs, p_rhs>
                , iota_op<Opt, EOR_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def, p_def>
                >(cpu, prev, cont);
            });
            break;

        case SSA_rol:
            {
                ssa_ht const carry = carry_output(*h);
                p_lhs::set(h->input(0));
                p_rhs::set(h->input(1));
                p_carry_output::set(carry);
                if(h->input(1).eq_whole(0u))
                {
                    if(p_def::trans() == p_lhs::trans())
                    {
                        chain
                        < pick_op<Opt, ASL, p_def, p_lhs>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        chain
                        < load_A<Opt, const_<0>>
                        , pick_op<Opt, SLO, p_def, p_lhs>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        if(cpu.value_eq(REG_A, p_lhs::value()))
                            goto asl_implied;
                    }
                    else
                    {
                    asl_implied:

                        if(h->output_size() == 1 && carry)
                        {
                            assert(carry == h->output(0));

                            // If only the carry is used,
                            // and the carry isn't being used as a carry input,
                            // output the carry as N.
                            if(carry->output_size() > 1 || carry_input_i(orig_use(carry)->op()) < 0)
                            {
                                chain
                                < load_N_for<Opt, p_lhs>
                                , set_defs<Opt, cpu_t::REGF_BOOL_IN_N, true, p_carry_output>
                                >(cpu, prev, cont);
                            }
                        }

                        chain
                        < load_A<Opt, p_lhs>
                        , simple_op<Opt, ASL_IMPLIED, p_def>
                        , store<Opt, STA, p_def, p_def>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);
                    }
                }
                else
                {
                    if(p_def::trans() == p_lhs::trans())
                    {
                        chain
                        < load_C<Opt, p_rhs>
                        , pick_op<Opt, ROL, p_def, p_lhs>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        chain
                        < load_AC<Opt, const_<0xFF>, p_rhs>
                        , pick_op<Opt, RLA, p_def, p_lhs>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        if(cpu.value_eq(REG_A, p_lhs::value()))
                            goto rol_implied;
                    }
                    else
                    {
                    rol_implied:
                        chain
                        < load_AC<Opt, p_lhs, p_rhs>
                        , simple_op<Opt, ROL_IMPLIED, p_def>
                        , store<Opt, STA, p_def, p_def>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);
                    }
                }
            }
            break;

        case SSA_ror:
            {
                ssa_ht const carry = carry_output(*h);

                p_lhs::set(h->input(0));
                p_rhs::set(h->input(1));
                p_carry_output::set(carry);

                if(h->input(1).eq_whole(0u))
                {
                    if(p_def::trans() == p_lhs::trans())
                    {
                        chain
                        < pick_op<Opt, LSR, p_def, p_lhs>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        chain
                        < load_A<Opt, const_<0>>
                        , pick_op<Opt, SRE, p_def, p_lhs>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        if(cpu.value_eq(REG_A, p_lhs::value()))
                            goto lsr_implied;
                    }
                    else
                    {
                    lsr_implied:

                        if(h->output_size() == 1 && carry)
                        {
                            assert(carry == h->output(0));

                            // This version loads '0' into A:
                            chain
                            < load_A<Opt, p_lhs>
                            , simple_op<Opt, ALR_IMMEDIATE, null_, const_<1>>
                            , store<Opt, STA, p_def, p_def> // Unused; only used for cost.
                            , set_defs<Opt, REGF_C, true, p_carry_output>
                            >(cpu, prev, cont);

                            // If only the carry is used,
                            // and the carry isn't being used as a carry input,
                            // let's just use AND to shortcut things.
                            if(carry->output_size() > 1 || carry_input_i(orig_use(carry)->op()) < 0)
                            {
                                chain
                                < load_A<Opt, p_lhs>
                                , simple_op<Opt, AND_IMMEDIATE, p_carry_output, const_<1>>
                                >(cpu, prev, cont);
                            }
                        }

                        if(!carry)
                        {
                            // This version clears the carry:
                            chain
                            < load_A<Opt, p_lhs>
                            , simple_op<Opt, ALR_IMMEDIATE, p_def, const_<0b11111110>>
                            , store<Opt, STA, p_def, p_def>
                            , set_defs<Opt, REGF_C, true, const_<0>>
                            >(cpu, prev, cont);
                        }

                        chain
                        < load_A<Opt, p_lhs>
                        , simple_op<Opt, LSR_IMPLIED, p_def>
                        , store<Opt, STA, p_def, p_def>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);
                    }
                }
                else
                {
                    if(p_def::trans() == p_lhs::trans())
                    {
                        chain
                        < load_C<Opt, p_rhs>
                        , pick_op<Opt, ROR, p_def, p_lhs>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);

                        if(cpu.value_eq(REG_A, p_lhs::value()))
                            goto ror_implied;
                    }
                    else
                    {
                    ror_implied:
                        chain
                        < load_AC<Opt, p_lhs, p_rhs>
                        , simple_op<Opt, ROR_IMPLIED, p_lhs>
                        , store<Opt, STA, p_def, p_def>
                        , set_defs<Opt, REGF_C, true, p_carry_output>
                        >(cpu, prev, cont);
                    }
                }
            }
            break;
            
        case SSA_sign_extend:
            {
                p_arg<0>::set(h->input(0));
                sign_extend<Opt, p_def, p_arg<0>>(cpu, prev, cont);
            }
            break;

        case SSA_sign:
            {
                p_arg<0>::set(h->input(0));

                if(h->output_size() == 1 && int(h->output_edge(0).index) == carry_input_i(h->output(0)->op()))
                {
                    chain
                    < load_A<Opt, p_arg<0>>
                    , simple_op<Opt, CMP_IMMEDIATE, null_, const_<0x80>>
                    , store_C<Opt, p_def>
                    >(cpu, prev, cont);

                    chain
                    < load_X<Opt, p_arg<0>>
                    , simple_op<Opt, CPX_IMMEDIATE, null_, const_<0x80>>
                    , store_C<Opt, p_def>
                    >(cpu, prev, cont);

                    chain
                    < load_Y<Opt, p_arg<0>>
                    , simple_op<Opt, CPY_IMMEDIATE, null_, const_<0x80>>
                    , store_C<Opt, p_def>
                    >(cpu, prev, cont);
                }
                else
                {
                    chain
                    < load_A<Opt, p_arg<0>>
                    , simple_op<Opt, CMP_IMMEDIATE, null_, const_<0x80>>
                    , simple_op<Opt, LDA_IMMEDIATE, null_, const_<0>>
                    , simple_op<Opt, ADC_IMMEDIATE, null_, const_<0>>
                    , store<Opt, STA, p_def, p_def>
                    >(cpu, prev, cont);

                    chain
                    < load_X<Opt, p_arg<0>>
                    , simple_op<Opt, CPX_IMMEDIATE, null_, const_<0x80>>
                    , simple_op<Opt, LDA_IMMEDIATE, null_, const_<0>>
                    , simple_op<Opt, ADC_IMMEDIATE, null_, const_<0>>
                    , store<Opt, STA, p_def, p_def>
                    >(cpu, prev, cont);

                    chain
                    < load_Y<Opt, p_arg<0>>
                    , simple_op<Opt, CPY_IMMEDIATE, null_, const_<0x80>>
                    , simple_op<Opt, LDA_IMMEDIATE, null_, const_<0>>
                    , simple_op<Opt, ADC_IMMEDIATE, null_, const_<0>>
                    , store<Opt, STA, p_def, p_def>
                    >(cpu, prev, cont);
                }
            }
            break;

        case SSA_phi:
            {
                assert(h->input_size() > 0);
                locator_t const loc = locator_t::phi(h);

                if(h->output_size() > 0 && cset_head(h) != cset_head(h->input(0).handle()))
                {
                    // 'LOC_PHI' values are used to pass phi inputs from other CFG nodes.
                    // They are special-cased here:
                    if(cpu.def_eq(REG_A, loc))
                        store<Opt, STA, p_def, p_def>(cpu, prev, cont);
                    else if(cpu.def_eq(REG_X, loc))
                        store<Opt, STX, p_def, p_def>(cpu, prev, cont);
                    else if(cpu.def_eq(REG_Y, loc))
                        store<Opt, STY, p_def, p_def>(cpu, prev, cont);
                    else
                    {
                        // No LOC_PHI:
                        p_arg<0>::set(h->input(0));
                        load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
                    }
                }
                else
                {
                    if(cpu.def_eq(REG_A, loc))
                        chain<set_defs<Opt, REGF_A, true, p_def>, ignore_req_store<p_def>>(cpu, prev, cont);
                    else if(cpu.def_eq(REG_X, loc))
                        chain<set_defs<Opt, REGF_X, true, p_def>, ignore_req_store<p_def>>(cpu, prev, cont);
                    else if(cpu.def_eq(REG_Y, loc))
                        chain<set_defs<Opt, REGF_Y, true, p_def>, ignore_req_store<p_def>>(cpu, prev, cont);
                    else
                        ignore_req_store<p_def>(cpu, prev, cont);
                }
            }
            break;

        case SSA_aliased_store:
            // Aliased stores normally produce no code, however, 
            // we must implement an input 'cg_read_array_direct' as a store:
            if(h->output_size() > 0 && h->input(0).holds_ref() && h->input(0)->op() == SSA_cg_read_array8_direct)
            {
                h = h->input(0).handle();
                goto do_read_array_direct;
            }
            else
                ignore_req_store<p_def>(cpu, prev, cont);
            break;

        case SSA_write_array8:
            {
                using namespace ssai::array;
                using p_array = p_arg<0>;
                using p_index = p_arg<1>;
                using p_assignment = p_arg<2>;

                unsigned const offset = h->input(OFFSET).whole();

                p_array::set(h, offset);
                p_index::set(h->input(INDEX));
                p_assignment::set(h->input(ASSIGNMENT));

                write_array<Opt, p_array, p_index, p_assignment>(cpu, prev, cont);
            }
            break;

        case SSA_read_array8:
        do_read_array_direct:
            {
                using namespace ssai::array;
                using p_array = p_arg<0>;
                using p_index = p_arg<1>;

                unsigned const offset = h->input(OFFSET).whole();

                p_array::set(h->input(ARRAY), offset);
                p_index::set(h->input(INDEX));

                read_array<Opt, p_def, p_array, p_index>(cpu, prev, cont);
            }
            break;

        case SSA_read_ptr:
        case SSA_read_ptr_hw:
            {
                using p_ptr_lo = p_arg<0>;
                using p_ptr_hi = p_arg<1>;
                using p_ptr = set_ptr_hi<p_ptr_lo, p_ptr_hi>;
                using p_index = p_arg<2>;

                using namespace ssai::rw_ptr;

                p_ptr_lo::set(h->input(PTR));
                p_ptr_hi::set(h->input(PTR_HI));
                p_index::set(h->input(INDEX));

                if(h->input(PTR).is_const())
                {
                    assert(!h->input(PTR_HI) || h->input(PTR_HI).is_const());
                    if(h->output_size() == 0)
                    {
                        if(h->input(INDEX).eq_whole(0))
                            exact_op<Opt, IGN_ABSOLUTE, p_def, p_arg<0>>(cpu, prev, cont);
                        else
                        {
                            chain
                            < load_X<Opt, p_index>
                            , exact_op<Opt, IGN_ABSOLUTE_X, p_def, p_arg<0>>
                            >(cpu, prev, cont);
                        }
                    }
                    else
                        read_array<Opt, p_def, p_ptr, p_index>(cpu, prev, cont);
                }
                else
                {
                    if(h->input(INDEX).eq_whole(0))
                    {
                        chain
                        < load_X<Opt, const_<0>>
                        , exact_op<Opt, LDA_INDIRECT_X, p_def, p_ptr>
                        , store<Opt, STA, p_def, p_def>
                        >(cpu, prev, cont);
                    }

                    chain
                    < load_Y<Opt, p_index>
                    , exact_op<Opt, LDA_INDIRECT_Y, p_def, p_ptr>
                    , store<Opt, STA, p_def, p_def>
                    >(cpu, prev, cont);

                    chain
                    < load_Y<Opt, p_index>
                    , exact_op<Opt, LAX_INDIRECT_Y, p_def, p_ptr>
                    , store<Opt, STA, p_def, p_def>
                    >(cpu, prev, cont);
                }
            }
            break;

        case SSA_write_ptr:
        case SSA_write_ptr_hw:
            {
                using p_ptr_lo = p_arg<0>;
                using p_ptr_hi = p_arg<1>;
                using p_ptr = set_ptr_hi<p_ptr_lo, p_ptr_hi>;
                using p_index = p_arg<2>;
                using p_assignment = p_arg<3>;

                using namespace ssai::rw_ptr;

                p_ptr_lo::set(h->input(PTR));
                p_ptr_hi::set(h->input(PTR_HI));
                p_index::set(h->input(INDEX));
                p_assignment::set(h->input(ASSIGNMENT));

                if(h->input(PTR).is_const())
                {
                    assert(!h->input(PTR_HI) || h->input(PTR_HI).is_const());
                    write_array<Opt, p_ptr_lo, p_index, p_assignment>(cpu, prev, cont);
                }
                else
                {
                    if(h->input(INDEX).eq_whole(0))
                    {
                        chain
                        < load_AX<Opt, p_assignment, const_<0>>
                        , exact_op<Opt, STA_INDIRECT_X, null_, p_ptr>
                        >(cpu, prev, cont);
                    }

                    chain
                    < load_AY<Opt, p_assignment, p_index>
                    , exact_op<Opt, STA_INDIRECT_Y, null_, p_ptr>
                    >(cpu, prev, cont);
                }
            }

            break;

        case SSA_make_ptr_lo:
        case SSA_make_ptr_hi:
            p_arg<0>::set(h->input(1));
            load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            break;

        case SSA_read_mapper_state:
            p_arg<0>::set(locator_t::runtime_ram(RTRAM_mapper_state));

            chain
            < exact_op<Opt, LDA_ABSOLUTE, p_def, p_arg<0>>
            , store<Opt, STA, p_def, p_def>
            >(cpu, prev, cont);

            chain
            < exact_op<Opt, LAX_ABSOLUTE, p_def, p_arg<0>>
            , store<Opt, STA, p_def, p_def>
            >(cpu, prev, cont);

            chain
            < exact_op<Opt, LDX_ABSOLUTE, p_def, p_arg<0>>
            , store<Opt, STX, p_def, p_def>
            >(cpu, prev, cont);

            chain
            < exact_op<Opt, LDY_ABSOLUTE, p_def, p_arg<0>>
            , store<Opt, STY, p_def, p_def>

            >(cpu, prev, cont);
            break;

        case SSA_write_mapper_state:
            p_arg<0>::set(locator_t::runtime_ram(RTRAM_mapper_state));
            p_arg<1>::set(h->input(0));

            switch(mapper().type)
            {
            default:
                throw std::runtime_error(fmt("Undefined mapper state isel for %", mapper_name(mapper().type)));

            case MAPPER_MMC1:
                p_arg<2>::set(locator_t::runtime_rom(RTROM_mapper_reset));
                p_arg<3>::set(state.minor_label());
                p_arg<4>::set(locator_t::runtime_ram(RTRAM_mapper_detail));

                chain
                < label<p_arg<3>>
                , exact_op<Opt, LDY_ABSOLUTE, null_, p_arg<4>>
                , load_A<Opt, p_arg<1>>
                , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<0>>
                , exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<2>>
                , simple_op<Opt, write_reg_op(~(REGF_X | REGF_Y))> // Clobbers everything except X and Y
                , exact_op<Opt, CPY_ABSOLUTE, null_, p_arg<4>>
                , branch_op<Opt, BNE, p_arg<3>>
                , clear_conditional
                >(cpu, prev, cont);
                break;

            case MAPPER_ANROM: 
            case MAPPER_GNROM: 
            case MAPPER_GTROM:
                p_arg<2>::set(locator_t::this_bank());
                p_arg<3>::set(locator_t::addr(bankswitch_addr(mapper().type)));
                p_arg<4>::set(state.minor_label());

                if(mapper().bus_conflicts)
                {
                    if(compiler_options().unsafe_bank_switch)
                    {
                        chain
                        < load_A<Opt, p_arg<1>>
                        , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<0>>
                        , exact_op<Opt, ORA_IMMEDIATE, null_, p_arg<2>>
                        , exact_op<Opt, TAX_IMPLIED, null_>
                        , iota_op<Opt, STA_ABSOLUTE_X, null_>
                        >(cpu, prev, cont);
                    }
                    else
                    {
                        chain
                        < label<p_arg<4>>
                        , load_AX<Opt, p_arg<1>, p_arg<1>>
                        , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<0>>
                        , exact_op<Opt, ORA_IMMEDIATE, null_, p_arg<2>>
                        , exact_op<Opt, TAY_IMPLIED, null_>
                        , iota_op<Opt, STA_ABSOLUTE_Y, null_>
                        , exact_op<Opt, CPX_ABSOLUTE, null_, p_arg<0>>
                        , branch_op<Opt, BNE, p_arg<4>>
                        , clear_conditional
                        >(cpu, prev, cont);
                    }
                }
                else
                {
                    if(compiler_options().unsafe_bank_switch)
                    {
                        chain
                        < load_A<Opt, p_arg<1>>
                        , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<0>>
                        , exact_op<Opt, ORA_IMMEDIATE, null_, p_arg<2>>
                        , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<3>>
                        >(cpu, prev, cont);
                    }
                    else
                    {
                        chain
                        < label<p_arg<4>>
                        , load_AX<Opt, p_arg<1>, p_arg<1>>
                        , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<0>>
                        , exact_op<Opt, ORA_IMMEDIATE, null_, p_arg<2>>
                        , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<3>>
                        , exact_op<Opt, CPX_ABSOLUTE, null_, p_arg<0>>
                        , branch_op<Opt, BNE, p_arg<4>>
                        , clear_conditional
                        >(cpu, prev, cont);
                    }

                    break;
                }
            }

            break;

        case SSA_fn_call:
            {
                assert(h->input(0).is_locator());
                p_arg<0>::set(h->input(0));

                if(h->input(1)) // if we have an explicit bank
                    p_arg<1>::set(h->input(1));
                else
                    p_arg<1>::set(h->input(0).locator().with_is(IS_BANK));

                fn_ht const call = get_fn(*h);
                if(mapper().bankswitches() && !mod_test(call->mods(), MOD_static))
                {
                    chain
                    < load_Y<Opt, p_arg<1>>
                    , simple_op<Opt, read_reg_op(REGF_Y)>
                    , exact_op<Opt, BANKED_Y_JSR, null_, p_arg<0>>
                    , simple_op<Opt, write_reg_op(REGF_ISEL)> // Clobbers most everything
                    >(cpu, prev, cont);
                }
                else
                {
                    chain
                    < exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<0>>
                    , simple_op<Opt, write_reg_op(REGF_ISEL)> // Clobbers most everything
                    >(cpu, prev, cont);
                }
            }
            break;

        case SSA_return:
            switch(state.fn->fclass)
            {
            case FN_MODE:
                p_label<0>::set(locator_t::runtime_rom(RTROM_reset));
                exact_op<Opt, JMP_ABSOLUTE, null_, p_label<0>>(cpu, prev, cont);
                break;
            case FN_NMI:
                p_label<0>::set(locator_t::runtime_rom(RTROM_nmi_exit));
                exact_op<Opt, JMP_ABSOLUTE, null_, p_label<0>>(cpu, prev, cont);
                break;
            case FN_IRQ:
                p_label<0>::set(locator_t::runtime_rom(RTROM_irq_exit));
                exact_op<Opt, JMP_ABSOLUTE, null_, p_label<0>>(cpu, prev, cont);
                break;
            default:
                simple_op<Opt, RTS_IMPLIED>(cpu, prev, cont);
                break;
            }
            break;

        case SSA_jump:
            assert(cfg_node->output_size() == 1);
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            exact_op<Opt, JMP_ABSOLUTE, null_, p_label<0>>(cpu, prev, cont);
            break;

        case SSA_if:
            p_arg<0>::set(h->input(0));
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));

            if(cpu.def_eq(REG_C, p_arg<0>::value())) [[unlikely]]
            {
                chain
                < simple_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                , simple_op<Opt, BCS_RELATIVE, null_, p_label<1>>
                >(cpu, prev, cont);
            }
            else if(cpu.def_eq<cpu_t::REGF_BOOL_IN_N>(REG_N, p_arg<0>::value())) [[unlikely]]
            {
                chain
                < simple_op<Opt, BPL_RELATIVE, null_, p_label<0>>
                , simple_op<Opt, BMI_RELATIVE, null_, p_label<1>>
                >(cpu, prev, cont);
            }
            else if(cpu.def_eq<cpu_t::REGF_INVERTED_Z>(REG_Z, p_arg<0>::value())) [[unlikely]]
            {
                // Inverted Z
                chain
                < simple_op<Opt, BNE_RELATIVE, null_, p_label<0>>
                , simple_op<Opt, BEQ_RELATIVE, null_, p_label<1>>
                >(cpu, prev, cont);
            }
            else
            {
                chain
                < load_Z_for<Opt, p_arg<0>>
                , simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>
                , simple_op<Opt, BNE_RELATIVE, null_, p_label<1>>
                >(cpu, prev, cont);
            }

            break;

        case SSA_wait_nmi:
            p_arg<0>::set(locator_t::runtime_rom(RTROM_wait_nmi));

            chain
            < exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<0>>
            , simple_op<Opt, write_reg_op(REGF_ISEL & ~(REGF_X | REGF_Y))>
            >(cpu, prev, cont);

            break;

        case SSA_cli:
            if(h->input(0).is_const())
            {
                if(h->input(0).whole())
                    exact_op<Opt, CLI_IMPLIED, null_>(cpu, prev, cont);
                else
                    exact_op<Opt, SEI_IMPLIED, null_>(cpu, prev, cont);
            }
            else
            {
                p_arg<0>::set(h->input(0));
                p_label<0>::set(state.minor_label());

                chain
                < exact_op<Opt, SEI_IMPLIED, null_>
                , load_Z_for<Opt, p_arg<0>>
                , simple_op<Opt, BEQ_RELATIVE, null_, p_label<0>>
                , exact_op<Opt, CLI_IMPLIED, null_>
                , label<p_label<0>>
                , clear_conditional
                >(cpu, prev, cont);
            }

            break;

        case SSA_switch_full:
            {
                p_arg<0>::set(h->input(0));
                p_arg<1>::set(locator_t::switch_lo_table(h->cfg_node()));
                p_arg<2>::set(locator_t::switch_hi_table(h->cfg_node()));
                using p_ptr = set_ptr_hi<p_arg<1>, p_arg<2>>;

                chain
                < load_X<Opt, p_arg<0>>
                , exact_op<Opt, ASM_X_SWITCH, null_, p_ptr>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, p_arg<0>>
                , exact_op<Opt, ASM_Y_SWITCH, null_, p_ptr>
                >(cpu, prev, cont);
            }
            break;

        case SSA_uninitialized:
        case SSA_cg_read_array8_direct:
            ignore_req_store<p_def>(cpu, prev, cont);
            break;
        default:
            throw std::runtime_error(fmt("Unhandled SSA op in code gen: %", h->op()));
        }
    }

    preprep_flags_t isel_node_build_preprep(ssa_ht h)
    {
        assert(h);

        preprep_flags_t flags = 0;

        switch(h->op())
        {
        case SSA_multi_eq:
        case SSA_multi_not_eq:
        case SSA_multi_lt:
        case SSA_multi_lte:
            flags |= PREPREP_X_0 | PREPREP_Y_0;
            break;

        case SSA_sign_extend:
            flags |= PREPREP_A_0 | PREPREP_X_0 | PREPREP_Y_0;
            break;

        default:
            break;
        }

        return flags;
    }

    void isel_node(ssa_ht h)
    {
        p_def::set(h);

#ifndef NDEBUG
        p_arg<0>::set({});
        p_arg<1>::set({});
        p_arg<2>::set({});
        p_arg<3>::set({});
        p_arg<4>::set({});
        p_arg<6>::set({});
        p_arg<7>::set({});
#endif

        using Opt = options<>;

        cfg_ht const cfg_node = h->cfg_node();

        switch(h->op())
        {
        case SSA_multi_eq:
            eq_store<true>(h); 
            break;

        case SSA_multi_not_eq: 
            eq_store<false>(h); 
            break;

        case SSA_multi_lt:
            lt_store<false>(h);
            break;

        case SSA_multi_lte:
            lt_store<true>(h);
            break;

        // Branch ops jump directly:
        case SSA_branch_eq:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            eq_branch<Opt, p_label<0>, p_label<1>>(h);
            break;

        case SSA_branch_not_eq:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            eq_branch<Opt, p_label<1>, p_label<0>>(h);
            break;

        case SSA_branch_lt:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            lt_branch<Opt, p_label<0>, p_label<1>, false>(h);
            break;

        case SSA_branch_lte:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(1)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(0)));
            lt_branch<Opt, p_label<0>, p_label<1>, true>(h);
            break;

        case SSA_branch_sign:
            p_arg<0>::set(h->input(0));
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            select_step<true>([](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
            {
                chain
                < load_N_for<Opt, p_arg<0>>
                , branch_op<Opt, BPL, p_label<0>>
                , branch_op<Opt, BMI, p_label<1>>
                >(cpu, prev, cont);

                chain
                < load_A<Opt, p_arg<0>>
                , simple_op<Opt, CMP_IMMEDIATE, null_, const_<0x80>>
                , branch_op<Opt, BCC, p_label<0>>
                , branch_op<Opt, BCS, p_label<1>>
                >(cpu, prev, cont);

                chain
                < load_X<Opt, p_arg<0>>
                , simple_op<Opt, CPX_IMMEDIATE, null_, const_<0x80>>
                , branch_op<Opt, BCC, p_label<0>>
                , branch_op<Opt, BCS, p_label<1>>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, p_arg<0>>
                , simple_op<Opt, CPY_IMMEDIATE, null_, const_<0x80>>
                , branch_op<Opt, BCC, p_label<0>>
                , branch_op<Opt, BCS, p_label<1>>
                >(cpu, prev, cont);
            });
            break;

        case SSA_return:
        case SSA_fn_call:
        case SSA_wait_nmi:
        case SSA_cli:
            write_globals<Opt>(h);
            goto simple;

        case SSA_fence:
            write_globals<Opt>(h);
            select_step<true>(simple_op<Opt, ASM_FENCE>);
            break;

        case SSA_goto_mode:
            {
                write_globals<Opt>(h);

                assert(h->input(0).is_locator());
                assert(h->input(0).locator().lclass() == LOC_FN);

                fn_t const& call = *h->input(0).locator().fn();
                assert(call.fclass == FN_MODE);

                assert(h->input(1).is_locator());
                assert(h->input(1).locator().lclass() == LOC_STMT);

                mods_t const* mods = state.fn->def().mods_of(h->input(1).locator().stmt());

                bool did_reset_nmi = false;
                bool did_reset_irq = false;

                call.precheck_group_vars().for_each([&](group_vars_ht gv)
                {
                    if(!gv->has_init())
                        return;

                    if(mods && !mods->in_lists(MODL_PRESERVES, gv->group.handle()))
                    {
                        if(!did_reset_nmi && global_t::has_nmi())
                        {
                            // Reset the nmi handler until we've reset all group vars.
                            p_arg<0>::set(locator_t::runtime_ram(RTRAM_nmi_index));
                            select_step<false>(load_then_store<Opt, const_<0>, const_<0>, p_arg<0>, false>);
                            did_reset_nmi = true;
                        }

                        if(!did_reset_irq && global_t::has_irq())
                        {
                            // Reset the irq handler until we've reset all group vars.
                            p_arg<0>::set(locator_t::runtime_ram(RTRAM_irq_index));
                            select_step<false>(load_then_store<Opt, const_<0>, const_<0>, p_arg<0>, false>);
                            did_reset_irq = true;
                        }
                        p_arg<0>::set(locator_t::reset_group_vars(gv));
                        p_arg<1>::set(locator_t::reset_group_vars(gv).with_is(IS_BANK));

                        select_step<false>([](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                        {
                            if(mapper().bankswitches())
                            {
                                chain
                                < load_Y<Opt, p_arg<1>>
                                , simple_op<Opt, read_reg_op(REGF_Y)>
                                , exact_op<Opt, BANKED_Y_JSR, null_, p_arg<0>>
                                , simple_op<Opt, write_reg_op(REGF_ISEL)> // Clobbers everything
                                >(cpu, prev, cont);
                            }
                            else
                            {
                                chain
                                < exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<0>>
                                , simple_op<Opt, write_reg_op(REGF_ISEL)> // Clobbers everything
                                >(cpu, prev, cont);
                            }
                        });
                    }
                });

                bool same_nmi = true;
                bool same_irq = true;
                for(fn_ht mode : state.fn->precheck_parent_modes())
                {
                    same_nmi &= mode->mode_nmi() == call.mode_nmi();
                    same_irq &= mode->mode_irq() == call.mode_irq();
                }
                
                // Set the nmi handler to its proper value
                if(global_t::has_nmi() && (did_reset_nmi || !same_nmi))
                {
                    p_arg<0>::set(locator_t::runtime_ram(RTRAM_nmi_index));
                    p_arg<1>::set(locator_t::nmi_index(call.mode_nmi()));
                    select_step<false>(load_then_store<Opt, p_arg<1>, p_arg<1>, p_arg<0>, false>);
                }

                // Set the irq handler to its proper value
                if(global_t::has_irq() && (did_reset_irq || !same_irq))
                {
                    p_arg<0>::set(locator_t::runtime_ram(RTRAM_irq_index));
                    p_arg<1>::set(locator_t::irq_index(call.mode_irq()));
                    select_step<false>(load_then_store<Opt, p_arg<1>, p_arg<1>, p_arg<0>, false>);
                }

                // Do the jump:
                p_arg<0>::set(h->input(0));
                p_arg<1>::set(h->input(0).locator().with_is(IS_BANK));
                if(mapper().bankswitches())
                {
                    select_step<true>(
                        chain
                        < load_Y<Opt, p_arg<1>>
                        , simple_op<Opt, read_reg_op(REGF_Y)>
                        , exact_op<Opt, BANKED_Y_JMP, null_, p_arg<0>>
                        , set_defs<Opt, REGF_ISEL, false, null_>
                        >);
                }
                else
                {
                    select_step<true>(
                        chain
                        < exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<0>>
                        , set_defs<Opt, REGF_ISEL, false, null_>
                        >);
                }
            }
            break;

        case SSA_init_array:
            for(unsigned i = 0; i < h->input_size(); ++i)
            {
                p_arg<0>::set(h->input(i));
                p_arg<1>::set(h, i);

                select_step<false>([](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                {
                    chain
                    < load_A<Opt, p_arg<0>>
                    , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<1>>
                    >(cpu, prev, cont);

                    chain
                    < load_X<Opt, p_arg<0>>
                    , exact_op<Opt, STX_ABSOLUTE, null_, p_arg<1>>
                    >(cpu, prev, cont);

                    chain
                    < load_Y<Opt, p_arg<0>>
                    , exact_op<Opt, STY_ABSOLUTE, null_, p_arg<1>>
                    >(cpu, prev, cont);
                });

            }
            break;

        case SSA_fill_array:
            fill_array<Opt>(h, h->input(0), 0, h->type().array_length());
            break;

        case SSA_read_ptr:
        case SSA_read_ptr_hw:
        case SSA_write_ptr:
        case SSA_write_ptr_hw:
            {
                // Handle bankswitching here, then call to 'isel_node_simple'
                using namespace ssai::rw_ptr;
                if(h->input(BANK) && mapper().bankswitches() && !h->test_flags(FLAG_BANK_PRELOADED))
                {
                    p_arg<0>::set(h->input(BANK));
                    select_step<false>(load_B<Opt, p_arg<0>>);
                }
            }
            goto simple;

        case SSA_write_array8:
            if(!h->input(0).holds_ref() || cset_head(h->input(0).handle()) != cset_head(h))
                copy_array<Opt>(h->input(0), h);
            goto simple;

        case SSA_early_store:
            passert(h->type() == h->input(0).type(), h->type(), h->input(0).type());

            if(h->input(0).holds_ref() && cset_head(h) == cset_head(h->input(0).handle()))
                select_step<true>(ignore_req_store<p_def>);
            else if(is_tea(h->type().name()))
                copy_array<Opt>(h->input(0), h);
            else
            {
                p_arg<0>::set(h->input(0));
                select_step<true>(load_then_store<Opt, p_def, p_arg<0>, p_def, false>);
            }
            break;

        case SSA_phi_copy:
            if(!orig_def(h->input(0)).holds_ref() || cset_head(h) != cset_head(h->input(0).handle()))
            {
                if(is_tea(h->type().name()))
                    copy_array<Opt>(h->input(0), h);
                else
                {
                    p_arg<0>::set(h->input(0));

                    locator_t const loc = cset_locator(h);
                    if(loc.lclass() == LOC_SSA || loc.lclass() == LOC_PHI)
                        select_step<true>(load_then_store<Opt, p_def, p_arg<0>, p_def, true>);
                    else
                        select_step<true>(load_then_store<Opt, p_def, p_arg<0>, p_def, false>);
                }
            }
            else
                select_step<true>(ignore_req_store<p_def>);
            break;

        case SSA_read_global:
            if(h->output_size() > 0 && h->input(1).locator().mem_head() != cset_locator(h))
            {
                if(is_tea(h->type().name()))
                    copy_array<Opt>(h->input(1), h);
                else
                {
                    p_arg<0>::set(h->input(1));
                    select_step<true>(load_then_store<Opt, p_def, p_arg<0>, p_def>);
                }
            }
            else
                select_step<true>(ignore_req_store<p_def>);
            break;

        case SSA_resize_array:
            {
                assert(is_tea(h->type().name()));
                assert(is_tea(h->input(0).type().name()));

                unsigned const old_size = h->input(0).type().size();
                unsigned const new_size = h->type().size();

                if(!h->input(0).holds_ref() || cset_head(h->input(0).handle()) != cset_head(h))
                    copy_array<Opt>(h->input(0), h, new_size);
                else if(old_size < new_size)
                    fill_array<Opt>(h, ssa_value_t(0, TYPE_U), old_size, new_size - old_size);
            }
            break;

        case SSA_read_array16_b:
        case SSA_write_array16_b:
            {
                using namespace ssai::array;

                using p_array_lo = p_arg<0>;
                using p_array_hi = p_arg<1>;
                using p_index_lo = p_arg<2>;
                using p_index_hi = p_arg<3>;
                using p_assignment = p_arg<4>;
                using p_ptr_lo = p_arg<5>;
                using p_ptr_hi = p_arg<6>;
                using p_ptr = set_ptr_hi<p_ptr_lo, p_ptr_hi>;

                p_array_lo::set(asm_arg(h).with_is(IS_PTR));
                p_array_hi::set(asm_arg(h).with_is(IS_PTR_HI));

                p_index_lo::set(h->input(INDEX));
                p_index_hi::set(h->input(INDEX_HI));

                p_ptr_lo::set(locator_t::runtime_ram(RTRAM_ptr_temp));
                p_ptr_hi::set(locator_t::runtime_ram(RTRAM_ptr_temp), 1);

                select_step<false>(
                    chain
                    < load_AC<Opt, p_index_lo, const_<0>>
                    , pick_op<Opt, ADC, null_, p_array_lo>
                    , exact_op<Opt, STA_ABSOLUTE, null_, p_ptr_lo>
                    , load_A<Opt::restrict_to<~REGF_C>, p_index_hi>
                    , pick_op<Opt, ADC, null_, p_array_hi>
                    , exact_op<Opt, STA_ABSOLUTE, null_, p_ptr_hi>
                    >);

                if(h->op() == SSA_read_array16_b)
                {
                    select_step<true>([](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                    {
                        chain
                        < load_Y<Opt, const_<0>>
                        , exact_op<Opt, LDA_INDIRECT_Y, p_def, p_ptr>
                        , store<Opt, STA, p_def, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_X<Opt, const_<0>>
                        , exact_op<Opt, LDA_INDIRECT_X, p_def, p_ptr>
                        , store<Opt, STA, p_def, p_def>
                        >(cpu, prev, cont);
                    });
                }
                else
                {
                    assert(h->op() == SSA_write_array16_b);

                    p_assignment::set(h->input(ASSIGNMENT));

                    select_step<true>([](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                    {
                        chain
                        < load_AY<Opt, p_assignment, const_<0>>
                        , exact_op<Opt, STA_INDIRECT_Y, null_, p_ptr>
                        >(cpu, prev, cont);

                        chain
                        < load_AX<Opt, p_assignment, const_<0>>
                        , exact_op<Opt, STA_INDIRECT_X, null_, p_ptr>
                        >(cpu, prev, cont);
                    });
                }
            }
            break;

        case SSA_entry:
            break;

        default: 
        simple:
            select_step<true>([h](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
            {
                isel_node_simple(h, cpu, prev, cont);
            });
            break;
        }
    }

///////////////////////////////////////////////////////////////////////////////

    std::ostream& operator<<(std::ostream& o, cross_cpu_t const& cross)
    {
        o << "cross_cpu_t\n{\n";
        for(locator_t loc : cross.defs)
            o << "    " << loc << std::endl;
        o << "}" << std::endl;
        return o;
    }

    static void setup_rolling_window(cfg_ht cfg_node)
    {
        auto& cd = cg_data(cfg_node);

        for(ssa_ht h : cd.schedule)
            cg_data(h).isel = {};

        // A bitset is used to track which variables have been stored.
        // To shrink the bitset size down to 64 bits, a rolling window is used
        // based around the live ranges occurring within a single CFG node.

        // This code finds that rolling window and allocates each node to a bit:
        std::uint64_t free = ~0ull; // Set of available bits to use
        for(ssa_ht h : cd.schedule)
        {
            if(free)
            {
                // Allocate a single bit.
                std::uint64_t const allocated = 1ull << builtin::ctz(free);
                assert((free & allocated) == allocated);

                // Remove the allocated bit from 'free'.
                free ^= allocated;

                // Track the bit with this node:
                cg_data(h).isel.store_mask = allocated;

                // Determine which use occurs latest in the CFG node.
                // Also guess if the node is a 'likely_store'.
                ssa_ht last_use = h;
                for(unsigned j = 0; j < h->output_size(); ++j)
                {
                    auto const oe = h->output_edge(j);

                    if(oe.input_class() != INPUT_VALUE)
                        continue;

                    if(((ssa_flags(oe.handle->op()) & SSAF_WRITE_GLOBALS) 
                        && oe.index >= write_globals_begin(oe.handle->op()))
                       || oe.handle->op() == SSA_aliased_store)
                    {
                        cg_data(h).isel.likely_store = true;
                        goto skip;
                    }

                    // Assume that all stores used across basic block
                    // boundaries must be stored. 
                    // (They may not be in the final generated code.)
                    // This is highly pessimistic, but simplifies the code gen.
                    if(oe.handle->cfg_node() != h->cfg_node())
                    {
                        cg_data(h).isel.likely_store = true;
                        goto skip;
                    }

                    if(cg_data(oe.handle).schedule.index > cg_data(last_use).schedule.index)
                        last_use = oe.handle;
                }

                cg_data(last_use).isel.last_use |= allocated;
            }
        skip:
            // Reclaim bits after the last use has been seen:
            assert((free & cg_data(h).isel.last_use) == 0);
            free |= cg_data(h).isel.last_use;
        }
    }

    static bool value_unused(cfg_ht cfg, ssa_ht ssa, cfg_ht input_cfg)
    {
        return for_each_output_matching(ssa, INPUT_VALUE, [&](ssa_ht output) -> bool
        {
            if((ssa_flags(output->op()) & SSAF_COPY) && asm_arg(ssa) == asm_arg(output))
                return value_unused(cfg, output, input_cfg);
            if(output->op() == SSA_phi)
                return output->cfg_node() == input_cfg;
            return output->cfg_node() == cfg;
        });
    }

    memoized_input_t cross_cpu_transition(cross_cpu_t const& cross, cfg_ht cfg, unsigned input_i, regs_t reg)
    {
        auto& d = cg_data(cfg);
        auto& memoized_map = data(cfg).memoized_input_maps[input_i];

        locator_t l = cross.defs[reg];

        if(l.lclass() == LOC_NONE)
            return {};

        // Handle carry pairs, which translate to a set, clear, or unknown carry depending on the edge.
        if(l.lclass() == LOC_CARRY_PAIR)
        {
            assert(reg == REG_C);

            auto const from_carry = [&](carry_t carry) -> locator_t
            {
                switch(carry)
                {
                default:          return {};
                case CARRY_CLEAR: 
                    return locator_t::const_byte(0);
                case CARRY_SET:
                    return locator_t::const_byte(1);
                }
            };

            unsigned const output_i = cfg->input_edge(input_i).index;

            if(output_i == 0)
                l = from_carry(l.first_carry());
            else if(output_i == 1)
                l = from_carry(l.second_carry());
            else
                l = {};
        }

        if(memoized_input_t* memoized = memoized_map.mapped(l))
            return *memoized;

        memoized_input_t ret = { l };

        // Setup incoming phis.
        // Search to see if we have an input to the phi.
        // If we do, we'll change the locator to that phi and update 'next_phi'.
        // The point of 'next_phi' is to get a better phi variety;
        // we don't want to change all values to the same phi.

        for(ssa_ht phi : d.phi_order)
        {
            if(ssa_to_value(phi->input(input_i)) == l)
            {
                ret.phi = locator_t::phi(phi);
                break;
            }
        }

        if(l.lclass() == LOC_SSA)
        {
            ssa_ht const h = l.ssa_node();

            // Values can't loop:
            // Likewise, if 'h' has no output to another CFG, ignore it.
            if(h->cfg_node() == cfg || value_unused(h->cfg_node(), h, cfg))
                ret.main = LOC_NONE;
        }


        memoized_map.insert({ l, ret });
        return ret;
    }
    
    using cross_vec_t = bc::small_vector<cross_cpu_t, 8>;

    // Generates a list of cross_cpu_t inputs, given a cross_cpu_t output.
    static cross_vec_t cross_cpu_transitions(cross_cpu_t const& cross, cfg_ht cfg, unsigned input_i)
    {
        cross_vec_t vec = { cross_cpu_t{} };
        assert(vec.size() == 1);

        for(unsigned reg = 0; reg < NUM_CROSS_REGS; ++reg)
        {
            memoized_input_t const m = cross_cpu_transition(cross, cfg, input_i, reg);

            if(m.main)
            {
                for(cross_cpu_t& cpu : vec)
                    cpu.defs[reg] = m.main;
            }

            if(m.phi)
            {
                unsigned const size = vec.size();
                for(unsigned i = 0; i < size; ++i)
                {
                    cross_cpu_t& cpu = vec.emplace_back(vec[i]);
                    cpu.defs[reg] = m.phi;
                }
            }
        }

        assert(vec.size() > 0);
        return vec;
    }

    // For each register, checks if 'out' transitions to 'in'.
    // Returns all the copies that need to be made to implement the transition.
    static cross_cpu_t cross_loads(cross_cpu_t const& in, cross_cpu_t const& out, cfg_ht cfg, unsigned input_i)
    {
        cross_cpu_t ret = {};

        for(unsigned reg = 0; reg < NUM_CROSS_REGS; ++reg)
        {
            if(!in.defs[reg])
                continue;

            if(in.defs[reg].lclass() == LOC_PHI)
            {
                ssa_value_t const v = orig_def(in.defs[reg].ssa_node()->input(input_i));
                if(v.holds_ref() && v->op() == SSA_uninitialized)
                    continue;
            }

            memoized_input_t const m = cross_cpu_transition(out, cfg, input_i, reg);
            if(m.main == in.defs[reg] || m.phi == in.defs[reg])
                continue;

            ret.defs[reg] = in.defs[reg];
        }

        return ret;
    }
}

std::size_t select_instructions(log_t* log, fn_t& fn, ir_t& ir)
{
    using namespace isel;

    state.log = log;
    state.fn = fn.handle();
    state.ssa_node = {};

    build_loops_and_order(ir);
    build_dominators_from_order(ir);

    _data_vec.clear();
    _data_vec.resize(cfg_pool::array_size());

    ///////////////////////////
    // GENERATE PREPREP LIST //
    ///////////////////////////

    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);
        auto const& schedule = cg_data(cfg).schedule;

        d.preprep.resize(schedule.size());

        for(int i = 0; i < int(schedule.size()); ++i)
        {
            preprep_flags_t const flags = isel_node_build_preprep(schedule[i]);

            for(int j = i-1; j >= 0; --j)
            {
                if(ssa_input0_class(schedule[j]->op()) != INPUT_LINK)
                {
                    d.preprep[j] |= flags;
                    break;
                }
            }
        }

        assert(d.preprep.empty() || d.preprep.back() == 0);

        // Also prepare memoized map here:
        d.memoized_input_maps.resize(cfg->input_size());
    }

    ///////////////////////////////////////////////
    // GENERATE SELECTION LIST FOR EACH CFG NODE //
    ///////////////////////////////////////////////

    static TLS rh::batman_map<cross_transition_t, result_t> rebuilt;
    static TLS std::vector<rh::apair<cross_cpu_t, isel_cost_t>> new_out_states;

    constexpr unsigned BASE_SEL_SIZE = 32;
    constexpr unsigned BASE_MAP_SIZE = 128;
    constexpr auto SELS_COST_BOUND = cost_fn(LDA_ABSOLUTE) * 2;

    auto const shrink_sels = [&](cfg_ht cfg)
    {
        auto& d = data(cfg);

        unsigned const max_sels = std::min<unsigned>(1 + loop_depth(cfg), 4) * BASE_SEL_SIZE;

        if(d.sels.size() > max_sels)
        {
            // Reuse 'rebuilt':
            rebuilt.clear();
            rebuilt.reserve(max_sels);

            state.indices.resize(d.sels.size());

            auto const begin = state.indices.begin();
            auto end = state.indices.end();
            
            auto comp = [&](unsigned a, unsigned b)
                { return d.sels.begin()[a].second.cost > d.sels.begin()[b].second.cost; };

            std::iota(begin, end, 0);
            std::make_heap(begin, end, comp);

            for(unsigned i = 0; i < max_sels; ++i)
            {
                std::pop_heap(begin, end, comp);
                auto const& to_insert = d.sels.begin()[*(--end)];

                rebuilt.insert(to_insert);
            }

            d.sels.swap(rebuilt);
        }
    };

    // Create the initial worklist:
    assert(cfg_worklist.empty());
    for(cfg_ht cfg : postorder | std::views::reverse)
    {
        assert(!cfg->test_flags(FLAG_IN_WORKLIST));
        cfg_worklist.push(cfg);
    }

    // Setup initial 'in_state's:
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);
        d.in_states.insert({});
        d.to_compute.push_back(0);
    }

    // Run until completion:
    while(!cfg_worklist.empty())
    {
        cfg_ht const cfg = cfg_worklist.pop();
        auto& d = data(cfg);
        d.iter += 1;

        if(d.to_compute.empty())
            continue;

        state.cfg_node = cfg;
        setup_rolling_window(cfg);
        unsigned repairs = 0;
    do_selections:
        dprint(state.log, "-ISEL_CFG", cfg);

        // Init the state:
        state.sel_pool.clear();
        state.best_cost = ~0 - cost_cutoff(0);
        state.map.clear();
        for(unsigned index : d.to_compute)
        {
#ifndef NDEBUG
            for(locator_t loc : d.in_states.begin()[index].defs)
                if(loc.lclass() == LOC_SSA)
                    assert(loc.ssa_node()->cfg_node() != cfg);
#endif
            state.map.insert({ 
                d.in_states.begin()[index].to_cpu(),
                &state.sel_pool.emplace(nullptr,
                    asm_inst_t{ .op = ASM_PRUNED, .arg = locator_t::index(index) }) });
        }

        state.max_map_size = std::min<unsigned>(1 + loop_depth(cfg), 4) * BASE_MAP_SIZE;

        // Shrink the map size for large CFG nodes:
        if(cfg->ssa_size() > 64)
        {
            state.max_map_size *= 64;
            state.max_map_size /= cfg->ssa_size();
            state.max_map_size = std::max<unsigned>(BASE_MAP_SIZE / 2, state.max_map_size);
        }

        // Modes get stack instructions:
        if(cfg == ir.root && state.fn->fclass == FN_MODE)
        {
            using Opt = options<>;
            select_step<false>(
                chain
                < load_X<Opt, const_<0xFF>>
                , simple_op<Opt, TXS_IMPLIED>
                >);
        }

        assert(state.map.size() > 0);

        // Generate every selection:
        auto const& schedule = cg_data(cfg).schedule;
        for(unsigned i = 0; i < schedule.size(); ++i)
        {
            ssa_ht h = schedule[i];
            try
            {
                state.ssa_node = h;
                
                if(d.preprep[i])
                {
                    select_step<false>([&](cpu_t const& cpu, sel_pair_t prev, cons_t const* cont)
                    {
                        cont->call(cpu, prev);

                        if(d.preprep[i] & PREPREP_A_0)
                            load_A<options<>::restrict_to<~(REGF_X | REGF_Y)>, const_<0>>(cpu, prev, cont);

                        if(d.preprep[i] & PREPREP_X_0)
                            load_X<options<>::restrict_to<~(REGF_A | REGF_Y)>, const_<0>>(cpu, prev, cont);

                        if(d.preprep[i] & PREPREP_Y_0)
                            load_Y<options<>::restrict_to<~(REGF_A | REGF_X)>, const_<0>>(cpu, prev, cont);
                    });
                }

                isel_node(h); // This creates all the selections.
            }
            catch(isel_no_progress_error_t const&)
            {
                dprint(state.log, "-ISEL_NO_PROGRESS!");

                // We'll try and fix the error.

                ++repairs;
                bool repaired = false;
                constexpr unsigned REPAIR_LIMIT = 8;

                if(repairs < REPAIR_LIMIT)
                {
                    // Maybe the addressing mode was impossible,
                    // so let's make it simpler.
                    for_each_node_input(h, [&](ssa_ht input)
                    {
                        if(input->cfg_node() == cfg && input->op() == SSA_cg_read_array8_direct)
                        {
                            input->unsafe_set_op(SSA_read_array8);
                            repaired = true;
                        }
                    });
                }
                else if(repairs == REPAIR_LIMIT)
                {
                    for(ssa_node_t& node : *cfg)
                        if(node.op() == SSA_cg_read_array8_direct)
                            node.unsafe_set_op(SSA_read_array8);
                    repaired = true;
                }

                if(repaired)
                    goto do_selections;
                throw;
            }
            catch(...) { throw; }
        }

        // Clear after computing:
        d.to_compute.clear();

        // Assemble those selections:
        assert(state.map.size());
        new_out_states.clear();
        unsigned const bound = SELS_COST_BOUND >> d.iter;
        for(auto const& pair : state.map)
        {
            unsigned cost = pair.second.cost;

            unsigned out_reg_count = 0;
            for(unsigned i = 0; i < NUM_CROSS_REGS; ++i)
                if(pair.first.defs[i])
                    ++out_reg_count;

            if(cost > d.min_sel_cost + bound + (cost_fn(STA_MAYBE) * out_reg_count))
                continue;

            std::vector<asm_inst_t> code_temp;
            sel_t const* first_sel = pair.second.sel;

            // Create the 'code_temp' vector:
            {
                std::size_t size = 1;
                assert(first_sel);
                for(;first_sel->prev; first_sel = first_sel->prev)
                    ++size;
                code_temp.resize(size);
                for(sel_t const* sel = pair.second.sel; sel; sel = sel->prev)
                    code_temp[--size] = sel->inst;
                assert(size == 0);
                assert(code_temp[0].op == ASM_PRUNED);
                code_temp[0] = { .op = ASM_LABEL, .arg = locator_t::cfg_label(cfg), };
            }

            // For branches, determine if the carry varies per output.
            std::array<carry_t, 2> carry_outputs = {};
            if(cfg->output_size() == 2
               && cfg->last_daisy() 
               && !is_switch(cfg->last_daisy()->op()))
            {
                std::array<cfg_ht, 2> const outputs = { cfg->output(0), cfg->output(1) };

                for(asm_inst_t& inst : code_temp)
                {
                    if(inst.arg.lclass() == LOC_CFG_LABEL && (op_flags(inst.op) & (ASMF_JUMP | ASMF_BRANCH | ASMF_SWITCH)))
                    {
                        for(unsigned i = 0; i < 2; ++i)
                        {
                            if(inst.arg.cfg_node() != outputs[i])
                                continue;

                            if(inst.op == BCC_RELATIVE)
                                carry_outputs[i] = carry_union(carry_outputs[i], CARRY_CLEAR); 
                            else if(inst.op == BCS_RELATIVE)
                                carry_outputs[i] = carry_union(carry_outputs[i], CARRY_SET); 
                            else
                                carry_outputs[i] = CARRY_TOP;
                        }
                    }
                }
            }

            // Determine the final 'start state'.
            // This is the cpu state we started with, 
            // ignoring any input register not actually used.

            unsigned const in_i = first_sel->inst.arg.data();
            assert(in_i < d.in_states.size());

            cross_transition_t transition = 
            { 
                .in_state = d.in_states.begin()[in_i],
                .out_state = cross_cpu_t(pair.first, carry_outputs[0], carry_outputs[1], true) 
            };

            regs_t gen = 0;
            regs_t kill = 0;
            for(asm_inst_t& inst : code_temp)
            {
                gen |= op_input_regs(inst.op) & ~kill;
                kill |= op_output_regs(inst.op);

                if(op_flags(inst.op) & ASMF_MAYBE_STORE)
                {
                    // Reduce the cost of maybe stores when the register is output.
                    for(unsigned i = 0; i < NUM_CROSS_REGS; ++i)
                    {
                        if((op_input_regs(inst.op) & (1 << i)) && inst.alt == transition.out_state.defs[i]) [[unlikely]]
                        {
                            cost -= cost_fn(STA_MAYBE);
                            break;
                        }
                    }

                    inst.alt = LOC_NONE;
                }
            }

            assert(first_sel->inst.op == ASM_PRUNED);
            assert(first_sel->inst.arg.lclass() == LOC_INDEX);

#ifndef NDEBUG
            for(locator_t loc : transition.in_state.defs)
                if(loc.lclass() == LOC_SSA)
                    assert(loc.ssa_node()->cfg_node() != cfg);
#endif

            for(unsigned i = 0; i < NUM_CROSS_REGS; ++i)
                if(~gen & kill & (1 << i)) 
                    transition.in_state.defs[i] = LOC_NONE;

            // Some transitions have pass-through registers, 
            // meaning they don't read or write those registers,
            // they just pass their values along.
            // For each pass-through register, 
            // we'll also handle the case it's LOC_NONE,
            // and will generate all the possible combinations:

            bc::small_vector<cross_transition_t, 8> sub_transitions;
            sub_transitions.push_back(transition);

            for(unsigned i = 0; i < NUM_CROSS_REGS; ++i)
            {
                if((gen | kill) & (1 << i))
                    continue;

                if(!transition.in_state.defs[i] || transition.in_state.defs[i] != transition.out_state.defs[i])
                    continue;

                // If the cfg node passes through a register,
                // insert additional combinations:
                unsigned const size = sub_transitions.size();
                for(unsigned j = 0; j < size; ++j)
                {
                    auto& t = sub_transitions.emplace_back(sub_transitions[j]);
                    t.in_state.defs[i] = t.out_state.defs[i] = LOC_NONE;
                }
            }

            dprint(state.log, "ISEL_RESULT", cfg, cost);

            if(state.log)
                for(auto const& inst : code_temp)
                    dprint(state.log, inst);

            dprint(state.log, "ISEL_RESULT_IN", transition.in_state);
            dprint(state.log, "ISEL_RESULT_OUT", transition.out_state);

            auto code_ptr = std::make_shared<std::vector<asm_inst_t>>(std::move(code_temp));

            assert(!sub_transitions.empty());

            for(unsigned i = 0; i < sub_transitions.size(); ++i)
            {
                rh::apair<cross_transition_t, result_t> new_sel = 
                { 
                    transition, 
                    {
                        .cost = cost + sub_transitions[i].heuristic_penalty(),
                        .code = code_ptr
                    }
                };

                if(d.min_sel_cost > new_sel.second.cost)
                    d.min_sel_cost = new_sel.second.cost;

                // Insert the 'new_sel' into 'd':
                auto insert_result = d.sels.insert(new_sel);
                if(insert_result.second)
                    new_out_states.push_back({ new_sel.first.out_state, new_sel.second.cost });
                else
                {
                    // Keep the lowest cost:
                    if(insert_result.first->second.cost > new_sel.second.cost)
                    {
                        //new_sel.second.age = insert_result.first->second.age;
                        *insert_result.first = std::move(new_sel);
                    }
                }
            }
        }

        // Pass our output CPU states to our output CFG nodes.
        unsigned const output_size = cfg->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            auto const oe = cfg->output_edge(i);
            cfg_ht const output = oe.handle;
            auto& od = data(output);

            for(auto const& out_state : new_out_states)
            {
                if(out_state.second > d.min_sel_cost + bound)
                    continue;

                auto cpus = cross_cpu_transitions(out_state.first, output, oe.index);
                assert(!cpus.empty());
                for(cross_cpu_t const& cpu : cpus)
                {
                    auto result = od.in_states.insert(cpu);
                    if(result.second)
                    {
                        od.to_compute.push_back(result.first - od.in_states.begin());
                        cfg_worklist.push(output);
                        assert(output->test_flags(FLAG_IN_WORKLIST));
                    }
                    else
                        assert(output->test_flags(FLAG_IN_WORKLIST) || od.sels.size() > 0);
                }
            }
        }
    }

    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
        shrink_sels(cfg);

#ifndef NDEBUG
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);
        passert(!cfg->test_flags(FLAG_IN_WORKLIST), cfg);
        passert(d.sels.size() > 0, cfg, cfg->ssa_size());
    }
#endif

    /////////////////////////////////////////
    // PICK THE BEST SELECTION COMBINATION //
    /////////////////////////////////////////

    auto const gen_load = [&](cross_cpu_t const& cross, regs_t reg, locator_t loc) -> asm_inst_t
    {
        if(loc.lclass() == LOC_SSA || loc.lclass() == LOC_PHI)
            loc = asm_arg(loc.ssa_node());

        switch(reg)
        {
        case REG_A:
            if(cross.defs[REG_X] == loc)
                return { .op = TXA_IMPLIED, .ssa_op = SSA_gen_load };
            else if(cross.defs[REG_Y] == loc)
                return { .op = TYA_IMPLIED, .ssa_op = SSA_gen_load };
            else if(loc.is_immediate())
                return { .op = LDA_IMMEDIATE, .ssa_op = SSA_gen_load, .arg = loc };
            else
                return { .op = LDA_ABSOLUTE, .ssa_op = SSA_gen_load, .arg = loc };

        case REG_X:
            if(cross.defs[REG_A] == loc)
                return { .op = TAX_IMPLIED, .ssa_op = SSA_gen_load };
            else if(loc.is_immediate())
                return { .op = LDX_IMMEDIATE, .ssa_op = SSA_gen_load, .arg = loc };
            else
                return { .op = LDX_ABSOLUTE, .ssa_op = SSA_gen_load, .arg = loc };

        case REG_Y:
            if(cross.defs[REG_A] == loc)
                return { .op = TAY_IMPLIED, .ssa_op = SSA_gen_load };
            else if(loc.is_immediate())
                return { .op = LDY_IMMEDIATE, .ssa_op = SSA_gen_load, .arg = loc };
            else
                return { .op = LDY_ABSOLUTE, .ssa_op = SSA_gen_load, .arg = loc };

        case REG_C:
            passert(!loc || loc.lclass() == LOC_CONST_BYTE, loc);
            return { .op = loc.data() ? SEC_IMPLIED : CLC_IMPLIED, .ssa_op = SSA_gen_load };

        default:
            return { .op = ASM_PRUNED, .ssa_op = SSA_gen_load };
        }
    };

#ifndef NDEBUG
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
        assert(data(cfg).is_reset());
#endif

    {
        pbqp_t pbqp(state.log);

        for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
        {
            auto& d = data(cfg);

            isel_cost_t const multiplier = depth_exp(loop_depth(cfg));
            assert(multiplier > 0);
            assert(d.cost_vector.empty());

            d.cost_vector.resize(d.sels.size(), 0);
            for(unsigned i = 0; i < d.sels.size(); ++i)
                d.cost_vector[i] = d.sels.begin()[i].second.cost * multiplier;
        }

        for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
        {
            auto& d = data(cfg);

            unsigned const output_size = cfg->output_size();
            for(unsigned i = 0; i < output_size; ++i)
            {
                auto const oe = cfg->output_edge(i);
                auto& od = data(oe.handle);

                isel_cost_t const multiplier = depth_exp(edge_depth(cfg, oe.handle));

                std::vector<pbqp_cost_t> cost_matrix(d.sels.size() * od.sels.size());
                for(unsigned y = 0; y < od.sels.size(); ++y)
                for(unsigned x = 0; x < d.sels.size(); ++x)
                {
                    cross_cpu_t const& in  = od.sels.begin()[y].first.in_state;
                    cross_cpu_t const& out = d.sels.begin()[x].first.out_state;
                    cross_cpu_t const loads = cross_loads(in, out, oe.handle, oe.index);

                    pbqp_cost_t cost = 0;

                    for(regs_t reg = 0; reg < NUM_CROSS_REGS; ++reg)
                    {
                        if(!loads.defs[reg])
                            continue;

                        op_t const op = gen_load(loads, reg, LOC_NONE).op;
                        unsigned add_to_cost = cost_fn(op);
                        assert(add_to_cost);

                        // Make it arbitrarily worse than a normal load.
                        // (2 seems to be too small)
                        cost += add_to_cost * 3;
                    }

                    cost_matrix[x + y * d.sels.size()] = cost * multiplier;
                }

                pbqp.add_edge(d, od, std::move(cost_matrix));
            }
        }

        std::vector<pbqp_node_t*> pbqp_order;
        for(cfg_ht cfg : postorder)
            pbqp_order.push_back(&data(cfg));
        pbqp.solve(std::move(pbqp_order));
    }

    ///////////////////////////
    // PREPARE SWITCH TABLES //
    ///////////////////////////
    
    rh::batman_map<cfg_ht, switch_table_t> switch_tables;

    for(cfg_node_t const& cfg_node : ir)
    {
        ssa_ht const branch = cfg_node.last_daisy();

        assert(!branch || branch->op() != SSA_switch_partial);

        if(!branch || branch->op() != SSA_switch_full)
            continue;

        unsigned const output_size = cfg_node.output_size();
        switch_table_t table(output_size);
        for(unsigned i = 0; i < output_size; ++i)
            table[i] = locator_t::cfg_label(cfg_node.output(i));

        switch_tables.insert({ cfg_node.handle(), std::move(table) });
    }

    ////////////////////////////////////
    // INSERT ADDITIONAL INSTRUCTIONS //
    ////////////////////////////////////

    // Which register loads are needed for each CFG input:
    bc::small_vector<regs_t, 16> input_loads;

    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);

        dprint(state.log, "ISEL_GEN_COST", cfg, d.final_cost());
        dprint(state.log, "ISEL_GEN_IN", cfg, d.final_in_state());
        dprint(state.log, "ISEL_GEN_OUT", cfg, d.final_out_state());

        regs_t to_load = 0;
        unsigned const input_size = cfg->input_size();
        input_loads.resize(input_size);

        for(unsigned i = 0; i < input_size; ++i)
        {
            cfg_ht input = cfg->input(i);
            auto const& id = data(input);

            cross_cpu_t const loads = cross_loads(d.final_in_state(), id.final_out_state(), cfg, i);

            regs_t input_load = 0;
            for(unsigned reg = 0; reg < NUM_CROSS_REGS; ++reg)
            {
                if(!loads.defs[reg])
                    continue;

                to_load |= 1 << reg;
                input_load |= 1 << reg;
            }
            input_loads[i] = input_load;
        }

        auto const initial_cross = [&](regs_t loads)
        {
            cross_cpu_t cross = d.final_in_state();
            bitset_for_each(loads, [&](regs_t reg){ cross.defs[reg] = {}; });
            return cross;
        };

        auto const replace_labels = [&](regs_t loads, locator_t label)
        {
            unsigned depth = 0;

            for(unsigned i = 0; i < input_size; ++i)
            {
                if(!input_loads[i] || (input_loads[i] &= ~loads))
                    continue;

                cfg_ht input = cfg->input(i);
                auto& id = data(input);

                // Replace the labels of incoming jumps with the new label.
                for(asm_inst_t& inst : id.final_code())
                    if(inst.op != ASM_LABEL && inst.arg == locator_t::cfg_label(cfg))
                        inst.arg = label;

                // Handle switch:
                if(auto* table = switch_tables.mapped(cfg))
                    for(locator_t& loc : *table)
                        if(loc == locator_t::cfg_label(cfg))
                            loc = label;

                // Also track loop depth:
                depth = std::max<unsigned>(depth, loop_depth(cfg->input(i)));
            }

            return std::min<unsigned>({ loop_depth(cfg), depth, 0xFF });
        };

        // Find loads that all incoming paths (that need loads) will use.

        regs_t always_load = to_load;
        for(unsigned i = 0; i < input_size; ++i)
            if(input_loads[i])
                always_load &= input_loads[i];

        // Now find loads that can be appended onto an input's CFG.

        for(unsigned i = 0; i < input_size; ++i)
        {
            cfg_ht input = cfg->input(i);
            auto& id = data(input);

            // Single outputs only!
            if(input->output_size() != 1)
                continue;

            regs_t const load_now = input_loads[i] & ~always_load;

            if(!load_now)
                continue;

            input_loads[i] &= ~load_now;

            auto& code = id.final_code();
            assert(code.size());
            passert(op_flags(code.back().op) & ASMF_JUMP, to_string(code.back().op));
            asm_inst_t const jump = code.back();
            code.pop_back();

            cross_cpu_t const loads = cross_loads(d.final_in_state(), id.final_out_state(), cfg, i);
            bitset_for_each(load_now, [&](regs_t reg)
            {
                code.push_back(gen_load(loads, reg, d.final_in_state().defs[reg]));
            });

            code.push_back(jump);
        }

        // Generate code for 'always_load' now.

        bc::small_vector<asm_inst_t, NUM_CROSS_REGS * 2> always_load_code;
        unsigned next_label_index = 1;

        if(always_load)
        {
            locator_t const label = locator_t::cfg_label(cfg, next_label_index++);
            always_load_code.push_back({ .op = ASM_LABEL, .arg = label });

            cross_cpu_t cross = initial_cross(always_load);
            bitset_for_each(always_load, [&](regs_t reg)
            {
                always_load_code.push_back(gen_load(cross, reg, d.final_in_state().defs[reg]));
                cross.defs[reg] = d.final_in_state().defs[reg];
            });

            replace_labels(always_load, label);
        }

        // Now handle all the remaining loads.

        regs_t remaining_load = 0;
        for(unsigned i = 0; i < input_size; ++i)
            remaining_load |= input_loads[i];

        bc::small_vector<asm_inst_t, NUM_CROSS_REGS * 2> remaining_load_code;

        if(remaining_load)
        {
            // We'll try various permutations, searching for an optimal one.

            unsigned lowest_misses = ~0u;
            bc::small_vector<regs_t, NUM_CROSS_REGS> order, best_order;
            bitset_for_each(remaining_load, [&](regs_t r){ order.push_back(r); });
            assert(std::is_sorted(order.begin(), order.end()));

            do
            {
                unsigned misses = 0;

                for(unsigned i = 0; i < input_size; ++i)
                {
                    cfg_ht input = cfg->input(i);

                    unsigned const cost = depth_exp(edge_depth(input, cfg));
                    regs_t input_load = input_loads[i];

                    for(unsigned j = 0; j < order.size(); ++j)
                    {
                        regs_t const regf = 1 << order[j];

                        if(!(input_load & regf))
                            misses += cost;

                        input_load &= ~regf;

                        if(!input_load)
                            break;
                    }
                }

                if(misses < lowest_misses)
                {
                    lowest_misses = misses;
                    best_order = order;
                }
            }
            while(std::next_permutation(order.begin(), order.end()));

            // OK! 'best_order' is built. Now generate code:

            cross_cpu_t const cross = initial_cross(remaining_load);
            for(regs_t reg : best_order)
            {
                locator_t const label = locator_t::cfg_label(cfg, next_label_index++);
                remaining_load_code.push_back(gen_load(cross, reg, d.final_in_state().defs[reg]));
                remaining_load_code.push_back({ .op = ASM_LABEL, .arg = label });

                replace_labels(1 << reg, label);
            }
            std::reverse(remaining_load_code.begin(), remaining_load_code.end());
        }

        // Prepend to code:
        std::vector<asm_inst_t> new_code(d.final_code().size() + always_load_code.size() + remaining_load_code.size());
        std::copy(remaining_load_code.begin(), remaining_load_code.end(), 
                  new_code.begin());
        std::copy(always_load_code.begin(), always_load_code.end(), 
                  new_code.begin() + remaining_load_code.size());
        std::copy(d.final_code().begin(), d.final_code().end(), 
                  new_code.begin() + always_load_code.size() + remaining_load_code.size());
        d.final_code() = std::move(new_code);
    }

    ////////////////////////
    // GRAPH OPTIMIZATION //
    ////////////////////////

    asm_graph_t graph(log, locator_t::cfg_label(ir.root));

    if(std::ostream* os = fn.info_stream())
    {
        *os << "\nGRAPH " << fn.global.name << '\n';

        for(cfg_ht cfg : postorder | std::views::reverse)
        {
            auto& d = data(cfg);
            *os << "  GRAPH_CFG " << cfg.id << '\n';
            for(asm_inst_t const& inst : d.final_code())
                *os << "    " << inst << std::endl;
        }
    }

    for(cfg_ht cfg : postorder | std::views::reverse)
    {
        auto& d = data(cfg);
        graph.append_code(&*d.final_code().begin(), &*d.final_code().end(), switch_tables);
    }

    graph.finish_appending();
    graph.optimize();
    graph.optimize_live_registers();
    graph.remove_maybes(fn);
    graph.optimize_live_registers();

    lvars_manager_t lvars = graph.build_lvars(fn);

    asm_proc_t asm_proc(fn.handle(), graph.to_linear(graph.order()), graph.entry_label());

    if(std::ostream* os = fn.info_stream())
    {
        *os << "\nPROC " << fn.global.name << '\n';
        for(asm_inst_t const& inst : asm_proc.code)
            if(inst.op != ASM_PRUNED)
                *os << "    " << inst << std::endl;
    }

    asm_proc.initial_optimize();
    asm_proc.build_label_offsets();

    std::size_t const proc_size = asm_proc.size();

    if(std::ostream* os = fn.info_stream())
    {
        *os << "\nPROC_OPT " << fn.global.name << '\n';
        for(asm_inst_t const& inst : asm_proc.code)
            if(inst.op != ASM_PRUNED)
                *os << "    " << inst << std::endl;
    }

    // Add the lvars to the fn
    fn.assign_lvars(std::move(lvars));
    fn.rom_proc().safe().assign(std::move(asm_proc));

    return proc_size;
}

