#ifndef CG_HPP
#define CG_HPP

// Code-gen related code.

#include <list>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_set.hpp"

#include "asm.hpp"
#include "ir.hpp"

namespace bc = ::boost::container;

///////////
// aop_t //
///////////

// Instructions used for code generation.
struct cg_inst_t
{
    op_t op;
    ssa_op_t ssa_op;
    locator_t arg;
};

std::ostream& operator<<(std::ostream& o, cg_inst_t const& inst);

/* TODO
struct asm_inst_t
{
    op_t op;
    locator_t arg;
};

struct asm_bb_t
{
    std::vector<asm_inst_t> code;
    std::vector<unsigned> inputs;
    int branch = -1;
};

struct asm_fn_t
{
    std::vector<cg_bb_t> bbs;

    std::vector<ssa_value_t> vars;
    rh::batman_map<ssa_value_t, unsigned> var_map;
};
*/

/*
struct asm_inst_t
{
    op_t op;
    addr16_t position;
    locator_t arg;
};

struct asm_label_t
{
    addr16_t position;
};

struct assembler_t
{
    addr16_t next_position = 0;
    std::vector<asm_inst_t> instructions;


    void add_instruction(cg_inst_t cg_inst)
    {
        asm_inst_t inst = 
        {
            .op = cg_inst.op
            .postito
        };

        next_position += op_size(cg_inst.op);
    }

    fix()
    {
        for(asm_inst_t inst : instructions)
        {
            if(inst.arg

            op_t new_op = TODO;


        }
    }
};
*/

struct relocatable_t
{
    std::vector<cg_inst_t> code;
    rh::robin_map<locator_t, unsigned> labels;
    std::list<unsigned> relative_branch_indices;

    // Converts invalid relative branches into long branches.
    void expand_branch_ops()
    {
        bool progress; 
        do
        {
            progress = false;

            for(auto it = relative_branch_indices.begin(); it != relative_branch_indices.end();)
            {
                unsigned const branch_i = *it;
                cg_inst_t& inst = code[branch_i];
                assert(is_relative_branch(inst.op));

                unsigned const label_i = labels[inst.arg];
                int const dist = bytes_between(branch_i+1, label_i);

                if(dist > 127 || dist < -128)
                {
                    inst.op = get_op(op_name(inst.op), MODE_LONG);
                    it = relative_branch_indices.erase(it);
                    progress = true;
                }
                else
                    ++it;
            }
        }
        while(progress);
    }

    int bytes_between(unsigned ai, unsigned bi) const
    {
        if(bi < ai)
            return -bytes_between(bi, ai);

        int bytes = 0;
        for(unsigned i = ai; i < bi; ++i)
        {
            assert(i < code.size());
            bytes += op_size(code[i].op);
        }

        return bytes;
    }
};

// TODO


//////////
// data //
//////////

struct cfg_liveness_d
{
    bitset_uint_t* in;
    bitset_uint_t* out; // Also used to hold the 'KILL' set temporarily.
};

struct cfg_order_d
{
    std::vector<unsigned> pheramones;
    std::uint16_t bytes = 0;
    std::uint16_t offset = 0;
};

struct ssa_schedule_d
{
    ssa_ht carry_user = {};
    bitset_uint_t* deps = nullptr;
    int exit_distance = 0;

    unsigned index = 0;
};

struct ssa_isel_d
{
    std::uint64_t store_mask = 0;
    std::uint64_t last_use = 0;
};

//

struct cfg_cg_d
{
    cfg_liveness_d live;
    cfg_order_d order;

    std::vector<ssa_ht> schedule;
    std::vector<cg_inst_t> code;
};

struct ssa_cg_d
{
    ssa_value_t cset_head = {}; // Basically a union-find pointer.
    ssa_ht cset_next = {}; // A linked-list to the next node

    ssa_schedule_d schedule;
    ssa_isel_d isel;
};

//

inline cfg_cg_d& cg_data(cfg_ht h) { return h.data<cfg_cg_d>(); }
inline ssa_cg_d& cg_data(ssa_ht h) { return h.data<ssa_cg_d>(); }

inline void cg_data_resize()
{
    cfg_data_pool::resize<cfg_cg_d>(cfg_pool::array_size());
    ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());
}

void code_gen(ir_t& ir);

// cset functions: (declare as needed)
ssa_ht cset_head(ssa_ht h);
locator_t cset_locator(ssa_ht h);

#endif
