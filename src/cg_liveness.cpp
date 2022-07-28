#include "cg_liveness.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "globals.hpp"
#include "ir.hpp"
#include "lvar.hpp"
#include "worklist.hpp"

//////////////////
// cfg liveness //
//////////////////

static inline cfg_liveness_d& live(cfg_ht h) 
{ 
    return cg_data(h).live;
}

static void _live_visit(ssa_ht def, cfg_ht cfg_node)
{
    if(def->cfg_node() == cfg_node)
        return;

    if(bitset_test(live(cfg_node).in, def.index))
        return;

    bitset_set(live(cfg_node).in, def.index);

    unsigned const input_size = cfg_node->input_size();
    assert(input_size > 0);
    for(unsigned i = 0; i < input_size; ++i)
    {
        cfg_ht input = cfg_node->input(i);
        bitset_set(live(input).out, def.index);
        _live_visit(def, input);
    }
}

void calc_ssa_liveness(ssa_ht node)
{
    unsigned const output_size = node->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto const oe = node->output_edge(i);
        cfg_ht const ocfg = oe.handle->cfg_node();

        // We only care about values
        if(oe.input_class() != INPUT_VALUE)
            continue;

        if(oe.handle->op() == SSA_phi)
        {
            assert(node->op() == SSA_phi_copy);
            assert(node->cfg_node() == ocfg->input(oe.index));

            //bitset_set(live(ocfg).in, node.index);
            bitset_set(live(node->cfg_node()).out, node.index);
            //_live_visit(node, node->cfg_node());
        }
        else
        {
            assert(node->op() != SSA_phi_copy);
            _live_visit(node, ocfg);
        }
    }
}

unsigned calc_ssa_liveness(ir_t const& ir)
{
    return calc_ssa_liveness(ir, ssa_pool::array_size());
}

unsigned calc_ssa_liveness(ir_t const& ir, unsigned pool_size)
{
    using namespace liveness_impl;
    cg_data_resize();
    bitset_pool.clear();
    set_size = ::bitset_size<>(pool_size);

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = live(cfg_it);
        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
        assert(d.in);
        assert(d.out);
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        calc_ssa_liveness(ssa_it);

    return set_size;
}

void clear_liveness_for(ir_t const& ir, ssa_ht node)
{
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = live(cfg_it);
        bitset_clear(d.in, node.index);
        bitset_clear(d.out, node.index);
    }
}

bool live_at_def(ssa_ht range, ssa_ht def)
{
    if(range == def)
        return false;

    bool const same_cfg = range->cfg_node() == def->cfg_node();
    auto const& def_live = live(def->cfg_node()); 

    // If 'range' begins before 'def':
    if((same_cfg && cg_data(range).schedule.index < cg_data(def).schedule.index)
       || bitset_test(def_live.in, range.index))
    {
        // Interfere if range is also live-out at def.
        if(bitset_test(def_live.out, range.index))
            return true;

        // Test to see if a use occurs after def:
        for(unsigned i = 0; i < range->output_size(); ++i)
        {
            auto const oe = range->output_edge(i);

            // We only care about values
            if(oe.input_class() != INPUT_VALUE)
                continue;

            ssa_ht const output = oe.handle;
            if(output->cfg_node() == def->cfg_node() && cg_data(def).schedule.index < cg_data(output).schedule.index)
                return true;
        }
    }

    return false;
}

bool live_at_any_def(ssa_ht range, ssa_ht const* defs_begin, 
                     ssa_ht const* defs_end)
{
    for(ssa_ht const* it = defs_begin; it < defs_end; ++it)
        if(live_at_def(range, *it))
            return true;
    return false;
}

bool live_range_overlap(ssa_ht a, ssa_ht b)
{
    return live_at_def(a, b) || live_at_def(b, a);
}

std::size_t live_range_busyness(ir_t& ir, ssa_ht h)
{
    using namespace liveness_impl;

    std::size_t total_size = 0;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& ld = live(cfg_it);

        assert(ld.in);
        assert(ld.out);

        if(bitset_test(ld.in, h.index))
            total_size += bitset_popcount(set_size, ld.in);

        if(bitset_test(ld.out, h.index))
            total_size += bitset_popcount(set_size, ld.out);
    }

    return total_size;
}

///////////////////////
// assembly liveness //
///////////////////////

template<typename ReadWrite>
static void do_inst_rw(fn_t const& fn, lvars_manager_t const& lvars, 
                       asm_inst_t const& inst, ReadWrite rw)
{
    if(inst.arg.lclass() == LOC_FN)
    {
        fn_ht const call_h = inst.arg.fn();
        fn_t const& call = *call_h;

        // Handle args and returns:
        lvars.for_each_lvar(false, [&](locator_t loc, unsigned i)
        {
            if(!has_fn(loc.lclass()) || loc.fn() != call_h)
                return;
            rw(i, loc.lclass() == LOC_ARG, 
                  loc.lclass() == LOC_RETURN);
        });

        bool const is_idep = fn.global.ideps().count(&call.global) > 0;

        if(is_idep)
        {
            assert(call.global.compiled());

            // Handle gmembers:
            lvars.for_each_non_lvar([&](locator_t loc, unsigned i)
            {
                if(loc.lclass() == LOC_GMEMBER)
                    rw(i, call.ir_reads().test(loc.gmember().value),
                          call.ir_writes().test(loc.gmember().value));
            });
        }
        else
        {
            // The fn may not be compiled yet.
            // We'll use 'lang_gvars' to implement this instead.

            assert(inst.op == JMP_ABSOLUTE);
            assert(call.fclass == FN_MODE);

            // Handle gmembers:
            lvars.for_each_non_lvar([&](locator_t loc, unsigned i)
            {
                if(loc.lclass() == LOC_GMEMBER)
                    rw(i, call.lang_gvars().test(loc.gmember()->gvar.handle().value), false);
            });
        }
    }
    else if(inst.op == RTS_IMPLIED)
    {
        // Every return will be "read" by the rts:
        lvars.for_each_lvar(true, [&](locator_t loc, unsigned i)
        {
            rw(i, loc.lclass() == LOC_RETURN, false);
        });

        // Handle gmembers:
        lvars.for_each_non_lvar([&](locator_t loc, unsigned i)
        {
            if(loc.lclass() == LOC_GMEMBER)
                rw(i, fn.ir_writes().test(loc.gmember().value), false);
        });
    }
    else
    {
        auto test_loc = [&](locator_t loc)
        {
            int const i = lvars.index(loc);
            if(i >= 0)
                rw(i, op_input_regs(inst.op) & REGF_M,
                      op_output_regs(inst.op) & REGF_M);
        };

        test_loc(inst.arg);

        // For indirect modes, also test the hi byte.
        if(indirect_addr_mode(op_addr_mode(inst.op)) && inst.ptr_hi)
            test_loc(inst.ptr_hi);
    }
}

// Once the assembly is (mostly) generated, 
// a second set of liveness checks is run to build an interference graph
// and then allocate memory.

void calc_asm_liveness(fn_t const& fn, ir_t const& ir, lvars_manager_t const& lvars)
{
    using namespace liveness_impl;
    cg_data_resize();
    bitset_pool.clear();
    set_size = lvars.bitset_size();

    // Call this before 'do_read'.
    auto const do_read = [](cfg_liveness_d& d, unsigned i)
    {
        if(bitset_test(d.out, i))
            bitset_set(d.in, i);
    };

    auto const do_write = [](cfg_liveness_d& d, unsigned i) { bitset_clear(d.out, i); };

    // Every arg will be "written" at root:
    lvars.for_each_lvar(true, [&](locator_t loc, unsigned i)
    {
        if(loc.lclass() == LOC_ARG)
            do_read(live(ir.root), i);
    });

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        auto& d = live(cfg_it);
        d.in  = bitset_pool.alloc(set_size);
        d.out = bitset_pool.alloc(set_size);
        bitset_set_all(set_size, d.out);

        // Set 'd.in's initial value to be the set of variables used in
        // this cfg node before an assignment.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'd.out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set is sometimes called 'KILL')
        for(asm_inst_t const& inst : cg_data(cfg_it).code)
        {
            do_inst_rw(fn, lvars, inst, [&](unsigned i, bool read, bool write)
            {
                // Order matters here. 'do_write' comes after 'do_read'.
                if(read)
                    do_read(d, i); 
                if(write)
                    do_write(d, i); 
            });
        }
    }

    // temp_set will hold a node's actual out-set while the algorithm is running.
    auto* temp_set = ALLOCA_T(bitset_uint_t, set_size);

    cfg_worklist.clear();

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        if(cfg_it->output_size() == 0)
            cfg_worklist.push(cfg_it);

    while(!cfg_worklist.empty())
    {
    reenter:
        cfg_ht cfg_node = cfg_worklist.pop();
        auto& d = live(cfg_node);

        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        bitset_clear_all(set_size, temp_set);
        unsigned const output_size = cfg_node->output_size();
        for(unsigned i = 0; i < output_size; ++i)
            bitset_or(set_size, temp_set, live(cfg_node->output(i)).in);

        // Now use that to calculate a new live-in set:
        bitset_and(set_size, temp_set, d.out); // (d.out holds KILL)
        bitset_or(set_size, temp_set, d.in);

        // If 'd.in' is changing, add all predecessors to the worklist.
        if(!cfg_node->test_flags(FLAG_PROCESSED)
           || !bitset_eq(set_size, temp_set, d.in))
        {
            cfg_node->set_flags(FLAG_PROCESSED);
            unsigned const input_size = cfg_node->input_size();
            for(unsigned i = 0; i < input_size; ++i)
                cfg_worklist.push(cfg_node->input(i));
        }

        // Assign 'd.in':
        bitset_copy(set_size, d.in, temp_set);
    }

    // Some nodes (infinite loops, etc) might not be reachable travelling
    // backwards from the exit. Handle those now:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        if(!cfg_it->test_flags(FLAG_PROCESSED))
           cfg_worklist.push(cfg_it);
           
    if(!cfg_worklist.empty())
        goto reenter;

    // Now properly set 'out' to be the union of all successor inputs:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        // Might as well clear flags.
        cfg_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);
        auto& d = live(cfg_it);

        bitset_clear_all(set_size, d.out);
        unsigned const output_size = cfg_it->output_size();
        for(unsigned i = 0; i < output_size; ++i)
            bitset_or(set_size, d.out, live(cfg_it->output(i)).in);
    }
}

void build_lvar_interferences(fn_t const& fn, ir_t const& ir, lvars_manager_t& lvars)
{
    using namespace liveness_impl;

    assert(set_size == lvars.bitset_size());

    // Step-through the code backwards, maintaining a current liveness set
    // and using that to build an interference graph.
    bitset_uint_t* const live = ALLOCA_T(bitset_uint_t, set_size);
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it);

        // Since we're going backwards, reset 'live' to the node's output state.
        bitset_copy(set_size, live, d.live.out);

        for(auto it = d.code.rbegin(); it != d.code.rend(); ++it)
        {
            asm_inst_t& inst = *it;

            if(inst.op == JSR_ABSOLUTE)
            {
                fn_ht const call_h = inst.arg.fn();

                // Every live lvar will interfere with this fn:
                bitset_for_each(set_size, live, [&](unsigned i)
                {
                    if(lvars.is_lvar(i))
                        lvars.add_fn_interference(i, call_h);
                });
            }
            else if(inst.arg)
            {
                int const lvar_i = lvars.index(inst.arg);
                if(lvar_i >= 0)
                {
                    if(op_flags(inst.op) & ASMF_MAYBE_STORE)
                    {
                        assert(op_output_regs(inst.op) & REGF_M);
                        if(bitset_test(live, lvar_i))
                        {
                            switch(inst.op)
                            {
                            case MAYBE_STA: inst.op = STA_ABSOLUTE; break;
                            case MAYBE_STX: inst.op = STX_ABSOLUTE; break;
                            case MAYBE_STY: inst.op = STY_ABSOLUTE; break;
                            case MAYBE_SAX: inst.op = SAX_ABSOLUTE; break;
                            case MAYBE_STORE_C: 
                                            assert(false); // TODO
                                            /*
                                temp_code.push_back({ PHP_IMPLIED, inst.ssa_op });
                                temp_code.push_back({ PHA_IMPLIED, inst.ssa_op });
                                temp_code.push_back({ LDA_IMMEDIATE, inst.ssa_op, locator_t::const_byte(0) });
                                temp_code.push_back({ ROL_IMPLIED, inst.ssa_op });
                                inst.op = STA_ABSOLUTE;
                                temp_code.push_back(std::move(inst));
                                temp_code.push_back({ PLA_IMPLIED, inst.ssa_op });
                                temp_code.push_back({ PLP_IMPLIED, inst.ssa_op });
                                continue;
                                */
                            default: assert(false);
                            }
                        }
                        else
                        {
                            std::cout << "PRUNE " << inst.arg << std::endl;
                            inst.op = ASM_PRUNED;
                        }
                    }
                }
            }

            do_inst_rw(fn, lvars, inst, [&](unsigned i, bool read, bool write)
            {
                std::cout << " POOP " << inst.arg << ' ' << read << ' ' << write << ' ' << i << std::endl;
                if(read)
                    bitset_set(live, i);
                else if(write) // Only occurs if 'read' is false.
                    bitset_clear(live, i);
            });


            /* TODO
            if(indirect_addr_mode(op_addr_mode(inst.op)))
            {
                // The argument is a pointer, 
                // meaning it must be allocated to zero-page.
                zp_only.push_back(i);
            }
            */

            // Variables that are live together interfere with each other.
            // Update the interference graph here:
            lvars.add_lvar_interferences(live);
        }
    }
}

