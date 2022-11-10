#ifndef ASM_GRAPH_HPP
#define ASM_GRAPH_HPP

#include <vector>

#include <boost/intrusive/list.hpp>

#include "flat/small_set.hpp"
#include "robin/map.hpp"
#include "robin/set.hpp"

#include "asm_proc.hpp"
#include "locator.hpp"
#include "debug_print.hpp"
#include "flags.hpp"
#include "array_pool.hpp"
#include "switch.hpp"
#include "worklist.hpp"

struct asm_inst_t;
class locator_t;
class fn_t;
class lvars_manager_t;

struct asm_node_t;
struct asm_path_t;

namespace bi = ::boost::intrusive;

struct asm_edge_t
{
    asm_node_t* node;
    int case_value = -1; // for switch nodes

    constexpr explicit operator bool() const { return node; }
    constexpr auto operator<=>(asm_edge_t const&) const = default;
};

class asm_node_t : public bi::list_base_hook<>, public flag_owner_t
{
friend class asm_graph_t;
public:
    asm_node_t(locator_t new_label, unsigned original_order)
    : label(new_label), original_order(original_order)
    {}

    void push_output(asm_edge_t o);
    void remove_output(unsigned i);
    void replace_output(unsigned i, asm_node_t* with);

    unsigned find_input(asm_node_t* h) const { return std::find(m_inputs.begin(), m_inputs.end(), h) - m_inputs.begin(); }
    unsigned find_output(asm_edge_t h) const { return std::find(m_outputs.begin(), m_outputs.end(), h) - m_outputs.begin(); }
    unsigned find_output(asm_node_t* h) const 
    { 
        return std::find_if(m_outputs.begin(), m_outputs.end(), [h](auto const& n){ return n.node == h; }) - m_outputs.begin(); 
    }

    void steal_outputs(asm_node_t& from);

    auto const& inputs() const { return m_inputs; }
    auto const& outputs() const { return m_outputs; }

    bool is_switch() const { return op_flags(output_inst.op) & ASMF_SWITCH; }

public:
    void remove_outputs_input(unsigned i);

    std::vector<asm_inst_t> code;
    asm_inst_t output_inst = {};
    locator_t label = {};

    union
    {
        unsigned vid;
        struct
        {
            int path_input;
            int path_output;
            asm_node_t* list_end;
        } vcover;
        struct
        {
            unsigned code_size;
            int offset;
            asm_path_t* path;
        } vorder;
        struct
        {
            regs_t in;
            regs_t out;
        } vregs;
        struct
        {
            bitset_uint_t* in;
            bitset_uint_t* out; // Also used to hold the 'KILL' set temporarily.
        } vlive;
    };

    cfg_ht cfg = {};
    unsigned original_order = 0;
private:

    bc::small_vector<asm_node_t*, 2> m_inputs;
    bc::small_vector<asm_edge_t, 2> m_outputs;
};

class asm_graph_t
{
public:
    asm_graph_t(log_t* log, locator_t entry_label);
    void append_code(asm_inst_t const* begin, asm_inst_t const* end, 
                     rh::batman_map<cfg_ht, switch_table_t> const& switch_tables);
    void finish_appending();

    std::vector<asm_node_t*> order();
    std::vector<asm_inst_t> to_linear(std::vector<asm_node_t*> order);
    void liveness(fn_t const& fn, lvars_manager_t& lvars);
    void optimize();

    void remove_maybes(fn_t const& fn);
    void optimize_live_registers();
    lvars_manager_t build_lvars(fn_t const& fn);

    template<typename Fn>
    void for_each_inst(Fn const& fn) const
    {
        for(asm_node_t const& node : list)
        for(asm_inst_t const& inst : node.code)
            fn(inst);
    }

    locator_t entry_label() const { return m_entry_label; }

    bool o_remove_stubs();
    bool o_remove_branches();
    bool o_merge();
    bool o_returns();
    bool o_peephole();
private:
    using list_t = bi::list<asm_node_t>;

    asm_node_t& push_back(locator_t label = LOC_NONE, bool succeed = false);
    list_t::iterator prune(asm_node_t& node);

    unsigned calc_liveness(fn_t const& fn, rh::batman_set<locator_t> const& map);

    template<typename Fn>
    void liveness_dataflow(Fn const& fn);

    array_pool_t<bitset_uint_t> bitset_pool;
    array_pool_t<asm_node_t> node_pool;
    list_t list;
    rh::batman_map<locator_t, asm_node_t*> label_map;
    locator_t m_entry_label = {};
    unsigned original_order = 0;

    struct delayed_lookup_t
    {
        asm_node_t* node;
        unsigned output;
        locator_t label;
    };

    std::vector<delayed_lookup_t> to_lookup;

    inline static thread_local worklist_t<asm_node_t*> worklist;

    log_t* log;
};


// TODO: 

// - remove useless labels (DONE)

// - reorder CFG (DONE)

// - liveness

// - move stores

// - peephole

#endif
