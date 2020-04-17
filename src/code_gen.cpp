#include "code_gen.hpp"

#include "ir.hpp"
#include "toposort.hpp"
#include "o.hpp" // for worklists: TODO

// Converts all operations with non-BYTE types to only use BYTE.
void byteify(ir_t& ir)
{
    assert(ssa_worklist::empty());

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;
            ssa_node.flags = 0;

            if(ssa_flags(ssa_node.op()) & SSAF_EFFECTFUL)
                ssa_worklist::push(ssa_it);
        }
    }

    while(!ssa_worklist::empty())
    {
        ssa_ht ssa_h = ssa_worklist::pop();
        ssa_node_t& ssa_node = *ssa_h;

        // Mark all inputs as used!
        for(unsigned i = 0; i < ssa_node.input_size(); ++i)
        {
            ssa_value_t input_v = ssa_node.input(i);
            if(input_v.is_const())
                continue;
            ssa_node_t& input = *input_v;

            auto const old_flags = input.flags; // TODO

            auto input_type_bits = arithmetic_bitpos(input.type().name);
            switch(ssa_node.op())
            {
            case SSA_and:
            case SSA_or:
            case SSA_xor:
                input.flags |= ssa_node.flags & input_type_bits;
                break;
            case SSA_add:
            case SSA_sub:
                input.flags |= ssa_node.flags & input_type_bits;


            }

            input.flags |= arithmetic_bitpos(ssa_node.type().name); // TODO!!

            if(old_flags != input.flags)
                ssa_worklist::push(input_v.handle());
        }
    }



    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;

            if(ssa_flags(ssa_node.op()) & SSAF_EFFECTFUL)
                ssa_node.flags = 
            else


            type_t const old_type = ssa_node.type();

            if(old_type.name == TYPE_BYTE)
                continue;

            if(is_arithmetic(old_type.name))
            {
               // - split node into N nodes
               // - 

            }
        }
    }
}

void make_conventional(ir_t& ir)
{
    // Deal with conditional nodes that have both edges going to the same node.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        assert(cfg_node.output_size() <= 2);
        if(cfg_node.output_size() == 2 &&
           cfg_node.output(0) == cfg_node.output(1))
        {
            // Introduce a new node as the fix:
            ir.split_edge(cfg_node.output_edge(1));
        }
    }

    // Insert copies around phi nodes to make the ir conventional.
    // - One copy per input in each predecessor block
    // - One copy of the phi node itself
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
        {
            ssa_node_t& phi_node = *phi_it;

            // Insert copies in predecessor nodes.
            unsigned const input_size = phi_node.input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = phi_node.input(i);
                type_t input_type = (input_v.is_const() ? type_t{TYPE_BYTE} 
                                                        : input_v->type());

                assert(!input_v.is_const() 
                       || input_v.whole() == (input_v.whole() & 0xFF));

                cfg_ht pred_h = cfg_node.input(i);
                cfg_node_t& pred_node = *pred_h;

                ssa_ht input_copy_h = pred_node.emplace_ssa(
                    SSA_exit_copy, input_type, input_v);

                phi_node.link_change_input(i, input_copy_h);
            }

            // Create a copy of the phi node.
            ssa_ht phi_copy_h = cfg_node.emplace_ssa(
                SSA_entry_copy, phi_node.type());
            phi_node.replace_with(phi_copy_h);
            phi_copy_h->link_append_input(phi_it);
        }
    }
}

void alloc_vars(ir_t& ir)
{
}

/*

struct schedule_t
{
    std::vector<ssa_ht> order;
};

static void _toposort_visit(schedule_t& schedule, ssa_ht node_h)
{
    ssa_node_t& node = *node_h;
    if(node.flags == MARK_PERMANENT)
        return;
    else if(node.flags == MARK_TEMPORARY)
        throw std::runtime_error("Circular SSA dependency.");
    node.flags = MARK_TEMPORARY;

    for(unsigned i = 0; i < node.input_size(); ++i)
    {
        ssa_value_t input = node.input(i);
        if(input.is_handle())
            _toposort_visit(todo, input.handle())
    }
    node.flags = MARK_PERMANENT;
    schedule.order.push_back(node_h);
}

schedule_t schedule_cfg(cfg_ht cfg_h)
{
    schedule_t schedule;
    cfg_node_t& cfg_node = *cfg_h;

    // Phi nodes first.
    for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
        schedule.order.push_back(phi_it);

    // Toposort the remaining.
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_node_t& ssa_node = ssa_it;
        ssa_node.flags = MARK_NONE;
    }
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_node_t& ssa_node = ssa_it;
        if(ssa_node.op() == SSA_phi)
            break;
        if(ssa_node.flags == MARK_NONE)
            _toposort_visit(schedule, ssa_it);
    }

    return schedule;
}

instr_t make_instr(asm_op_name_t op_name, ssa_value_t value)
{
    if(value.is_const())
    {
        assert((value.whole() & 0xFF) == value.whole());
        return { op_name, MODE_IMMEDIATE, value.whole() };
    }

    std::uint16_t const addr = TODO;
    return { op_name, is_zp(addr) ? MODE_ZERO_PAGE : MODE_ABSOLUTE, addr };
}

instr_t make_store(asm_op_name_t op_name, std::uint16_t addr)
{
    return { OP_NAME, addr < 256 ? MODE_IMMEDIATE : MODE_ABSOLUTE, addr };
}

void load_A(ssa_value_t value)
{
    instr_t instr = ssa_addr(value);
    instr.op_name = LDA;

    block.code.push_back({ LDA, mode });

    if(value.is_const())
        block.code.push_back({ LDA_IMMEDIATE, value.whole() & 0xFF });
    else
    {
        block.code.push_back({ LDA_IMMEDIATE, value.whole() & 0xFF });
    }
}

class code_gen_t
{

    void maybe_lda(ssa_ht ssa_h)
    void delay_sta(ssa_ht ssa_h)

    void maybe_clc();
    void maybe_sec();

};

void maybe_lda(ssa_ht ssa_h)
{
    append_asm(make_instr(LDA, ssa_h));
}

void maybe_clc()
{
    append_asm({ CLC });
}

void maybe_sec()
{
    append_asm({ SEC });
}

void delay_sta(TODO)
{
    append_asm(make_store(STA, todo));
}

void code_gen()
{

    rh::robin_map<ssa_ht, std::uint16_t> vars;


    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        schedule_t schedule = schedule_cfg(cfg_it);
        for(ssa_ht ssa_h : schedule.order)
        {
            ssa_node_t& ssa_node = *ssa_h;
            switch(ssa_node.op())
            {
            case SSA_add:
                maybe_lda(ssa_node.input(0));
                maybe_clc();
                append_asm(make_instr(ADC, ssa_node.input(1)));
                append_asm(make_store(STA, todo));
                break;
            }
        }

    }


    switch(TODO)
    {
    case SSA_add:
        block.code.push_back({ CLC });
        load_A(TODO);
        block.code.push_back({ ADC_IMMEDIATE, });
        store_A(TODO);
    }
}
*/
