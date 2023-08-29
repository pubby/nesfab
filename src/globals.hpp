#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <cassert>
#include <ostream>
#include <sstream>

#include "robin/collection.hpp"
#include "robin/set.hpp"
#include "robin/map.hpp"

#include "flat/small_map.hpp"
#include "flat/flat_map.hpp"
#include "flat/flat_set.hpp"

#include "asm_proc.hpp"
#include "array_pool.hpp"
#include "bitset.hpp"
#include "file.hpp"
#include "decl.hpp"
#include "parser_decl.hpp"
#include "phase.hpp"
#include "ram.hpp"
#include "fn_def.hpp"
#include "type.hpp"
#include "lvar.hpp"
#include "rval.hpp"
#include "rom_decl.hpp"
#include "locator.hpp"
#include "mods.hpp"
#include "debug_print.hpp"
#include "byte_block.hpp"
#include "ident_map.hpp"

struct rom_array_t;
struct precheck_tracked_t;

namespace bc = boost::container;

std::string to_string(global_class_t gclass);

// A data member of a record.
struct field_t
{
    var_decl_t decl;

    type_t& type() { return decl.src_type.type; }
    type_t const& type() const { return decl.src_type.type; }
    pstring_t type_pstring() const { return decl.src_type.pstring; }
};

using field_map_t = rh::batman_map<std::uint64_t, field_t, std::identity>;

class global_t
{
friend class global_map_t;
public:
    std::string const name;
private:
    // These variables are set only by 'define', as soon
    // as the global's definition is parsed.
    std::mutex m_define_mutex;
    global_class_t m_gclass = GLOBAL_UNDEFINED;
    pstring_t m_pstring = {};

    // An index into some storage that holds the global's implementation data
    unsigned m_impl_id = ~0u;

    // An index referring to this global:
    unsigned m_this_id = ~0u;

    // 'ideps' means "immediate dependencies".
    // AKA any global name that appears in the definition of this global.
    // This is set by 'define'
    ideps_map_t m_ideps;

    // Likewise, 'iuses' holds the immediate users of this global.
    // This is built after all globals have been created, after parsing.
    fc::vector_set<global_t*> m_iuses;
    std::atomic<int> m_ideps_left = 0;

    // These are for debugging:
#ifndef NDEBUG
    std::atomic<bool> m_resolved = false;
    std::atomic<bool> m_prechecked = false;
    std::atomic<bool> m_compiled = false;
#endif
public:
    global_t() = delete;

    global_t(pstring_t pstring, std::string_view name, unsigned id)
    : name(name)
    , m_pstring(pstring)
    , m_this_id(id)
    {
        assert(m_pstring.size);
    }

    global_class_t gclass() const { assert(compiler_phase() > PHASE_PARSE); return m_gclass; }
    ideps_map_t const& ideps() const { assert(compiler_phase() > PHASE_PARSE); return m_ideps; }
    pstring_t pstring() const { return m_pstring; }
    unsigned impl_id() const { assert(compiler_phase() > PHASE_PARSE); return m_impl_id; }

#ifndef NDEBUG
    bool resolved() const { return m_resolved; }
    bool prechecked() const { return m_prechecked; }
    bool compiled() const { return m_compiled; }
#endif

    global_ht handle() const { assert(compiler_phase() < PHASE_COMPILE || &global_ht{m_this_id}.safe() == this); return { m_this_id }; }

    template<typename T>
    T handle() const
    {
        static_assert(is_handle<T>::value);
        assert(gclass() == T::value_type::global_class);
        assert(compiler_phase() > PHASE_PARSE);
        return { m_impl_id };
    }

    template<typename T>
    T& impl() const
    {
        assert(gclass() == T::global_class);
        assert(compiler_phase() > PHASE_PARSE);
        return *handle<typename T::handle_t>();
    }

    global_datum_t* datum() const;
    std::vector<local_const_t> const* local_consts() const;

    void add_idep(global_t& g, idep_pair_t pair)
    {
        std::lock_guard<std::mutex> guard(m_define_mutex);
        m_ideps[&g] = pair;
    }

    // Helpers that delegate to 'define':
    fn_ht define_fn(
        pstring_t pstring, ideps_map_t&& ideps,
        type_t type, fn_def_t&& fn_def, std::unique_ptr<mods_t> mods, fn_class_t fclass, bool iasm, fn_set_t* fn_set);
    gvar_ht define_var(
        pstring_t pstring, ideps_map_t&& ideps, 
        src_type_t src_type, defined_group_vars_t group, 
        ast_node_t const* expr, std::unique_ptr<paa_def_t> paa_def,
        std::unique_ptr<mods_t> mods);
    const_ht define_const(
        pstring_t pstring, ideps_map_t&& ideps, 
        src_type_t src_type, defined_group_data_t group, bool omni,
        ast_node_t const* expr, std::unique_ptr<paa_def_t> paa_def,
        std::unique_ptr<mods_t> mods);
    struct_ht define_struct(
        pstring_t pstring, ideps_map_t&& ideps, field_map_t&& map);
    charmap_ht define_charmap(
        pstring_t pstring, bool is_default, 
        string_literal_t const& characters, 
        string_literal_t const& sentinel,
        std::unique_ptr<mods_t> mods);
    fn_set_t& define_fn_set(pstring_t pstring);

    static void init();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(char const* source, pstring_t name)
        { return lookup_sourceless(name, name.view(source)); }
    static global_t& lookup_sourceless(pstring_t name, std::string_view key)
        { return global_pool_map.lookup(name, key); }
    static global_t* lookup_sourceless(std::string_view view)
        { return global_pool_map.lookup(view); }

    // Call after parsing
    static void parse_cleanup();

    // Implementation detail used in 'build_order'.
    // Sets 'm_ideps_left' with the idep calc required.
    static global_t* detect_cycle(global_t& global, idep_class_t pass, idep_class_t calc);
    inline static std::vector<std::string> detect_cycle_error_msgs;

    // This allocates 'gmember_t's.
    static void count_members(); 

    // Call after 'count_members' to build 'm_iuses' and 'm_ideps_left',
    // among other things.
    // This function isn't thread-safe.
    // Call from a single thread only.
    static void build_order();

    // Call after 'build_order'. Dethunkifies types.
    static void resolve_all();

    // Call after 'build_order'. Checks the code and gathers information with a pre-pass evaluation.
    static void precheck_all();

    // Call after 'build_order' to well... compile everything!
    static void compile_all();

    static std::vector<fn_t*> modes() { assert(compiler_phase() > PHASE_PARSE); return modes_vec; }
    static std::vector<fn_t*> nmis() { assert(compiler_phase() > PHASE_PARSE); return nmi_vec; }
    static std::vector<fn_t*> irqs() { assert(compiler_phase() > PHASE_PARSE); return irq_vec; }

    static bool has_nmi() { return nmis().size(); }
    static bool has_irq() { return irqs().size(); }

    static global_t& default_charmap(pstring_t at);
    static std::pair<global_t*, ast_node_t const*>& new_chrrom(pstring_t at);

    static bool has_chrrom();
    static void for_each_chrrom(std::function<void(global_t*, ast_node_t const*)> const& fn);
private:

    // Sets the variables of the global:
    unsigned define(pstring_t pstring, global_class_t gclass, 
                    ideps_map_t&& ideps, std::function<unsigned(global_t&)> create_impl);

    // Helper to implement 'compile_all', 'precheck_all', etc.
    template<typename Fn>
    static void do_all(Fn const& fn);

    global_t* resolve(log_t* log);
    global_t* precheck(log_t* log);
    global_t* compile(log_t* log);

    // Call on completion of compile or precheck.
    // Updates the ready list.
    global_t* completed();

    template<typename Fn>
    void delegate(Fn const& fn)
    {
        switch(gclass())
        {
        default: throw std::runtime_error("Invalid global.");
        case GLOBAL_FN:      fn(this->impl<fn_t>());      break;
        case GLOBAL_CONST:   fn(this->impl<const_t>());   break;
        case GLOBAL_VAR:     fn(this->impl<gvar_t>());    break;
        case GLOBAL_STRUCT:  fn(this->impl<struct_t>());  break;
        case GLOBAL_CHARMAP: fn(this->impl<charmap_t>()); break;
        case GLOBAL_FN_SET:  fn(this->impl<fn_set_t>()); break;
        }
    }

    // Returns and pops the next ready global from the ready list.
    static global_t* await_ready_global();

private:
    // Globals get allocated in these:
    inline static ident_map_t<global_ht> global_pool_map;

    // Tracks modes: 
    inline static std::mutex modes_vec_mutex;
    inline static std::vector<fn_t*> modes_vec;

    // Tracks nmis: 
    inline static std::mutex nmi_vec_mutex;
    inline static std::vector<fn_t*> nmi_vec;

    // Tracks irqs: 
    inline static std::mutex irq_vec_mutex;
    inline static std::vector<fn_t*> irq_vec;

    // Tracks chrroms: 
    inline static std::mutex chrrom_deque_mutex;
    inline static std::deque<std::pair<global_t*, ast_node_t const*>> chrrom_deque;

    // These represent a queue of globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    inline static std::vector<global_t*> ready;
    inline static unsigned globals_left;
};

class struct_t
{
public:
    static constexpr global_class_t global_class = GLOBAL_STRUCT;
    using handle_t = struct_ht;

    struct_t(global_t& global, field_map_t&& fields)
    : global(global)
    , m_fields(std::move(fields))
    {}

    global_t& global;

    field_map_t const& fields() const { return m_fields; }
    field_t const& field(unsigned field_i) const { return m_fields.begin()[field_i].second; }

    unsigned num_members() const { assert(m_num_members != UNCOUNTED); return m_num_members; }

    unsigned member(unsigned field_i) const
    {
        assert(field_i < fields().size());
        unsigned m = 0;
        for(unsigned k = 0; k < field_i; ++k)
            m += ::num_members(field(k).type());
        return m;
    }

    type_t member_type(unsigned member_i) const 
    {
        passert(global.resolved(), global.name);
        passert(member_i < m_member_types.size(), global.name, member_i, m_member_types.size());
        return m_member_types[member_i];
    }

    std::uint16_t member_offset(unsigned member_i) const
    {
        assert(global.resolved());
        assert(member_i < m_member_offsets.size());
        return m_member_offsets[member_i];
    }

    bool has_tea_member() const { return m_has_tea_member; }

    unsigned count_members(); 

    void resolve();
    void precheck();
    void compile();
private:
    void gen_member_types(struct_t const& s, unsigned tea_size);

    field_map_t m_fields;

    static constexpr unsigned UNCOUNTED = ~0u;
    unsigned m_num_members = UNCOUNTED;

    // Cached vectors, tracking expanded members
    std::vector<type_t> m_member_types;
    std::vector<std::uint16_t> m_member_offsets;
    bool m_has_tea_member = false;
};

// Holds state common to both fn_t and fn_set_t.
class callable_t
{
public:
    auto const& precheck_rw() const { assert(compiler_phase() > PHASE_PRECHECK); return m_precheck_rw; }
    auto const& precheck_calls() const { assert(compiler_phase() > PHASE_PRECHECK); return m_precheck_calls; }
    auto precheck_romv() const {  return m_precheck_romv; }
    bool precheck_fences() const { assert(compiler_phase() > PHASE_PRECHECK); return m_precheck_fences; }
    bool precheck_wait_nmi() const { assert(compiler_phase() > PHASE_PRECHECK); return m_precheck_wait_nmi; }

    auto const& ir_reads()  const { assert(m_ir_reads);  return m_ir_reads; }
    auto const& ir_writes() const { assert(m_ir_writes); return m_ir_writes; }
    auto const& ir_group_vars() const { assert(m_ir_group_vars); return m_ir_group_vars; }
    auto const& ir_calls() const { assert(m_ir_calls); return m_ir_calls; }
    auto const& ir_deref_groups() const { assert(m_ir_deref_groups); return m_ir_deref_groups; }
    bool ir_tests_ready() const { assert(m_ir_writes); return m_ir_tests_ready; }
    bool ir_io_pure() const { assert(m_ir_writes); return m_ir_io_pure; }
    bool ir_fences() const { assert(m_ir_writes); return m_ir_fences; }

    bool returns_in_different_bank() const { assert(m_ir_writes); return m_returns_in_different_bank; }

    virtual void for_each_fn(std::function<void(fn_ht)> const& fn) const = 0;
protected:
    xbitset_t<group_vars_ht> m_precheck_group_vars;
    xbitset_t<gmember_ht> m_precheck_rw; // TODO: replace with more accurate reads and writes
    xbitset_t<fn_ht> m_precheck_calls;
    romv_flags_t m_precheck_romv = 0;
    // If the function (or a called fn) waits on NMI
    bool m_precheck_wait_nmi = false;
    bool m_precheck_fences = false;

    xbitset_t<gmember_ht> m_ir_reads;
    xbitset_t<gmember_ht> m_ir_writes;
    xbitset_t<group_vars_ht> m_ir_group_vars;
    xbitset_t<group_ht> m_ir_deref_groups;
    xbitset_t<fn_ht> m_ir_calls;

    // If the function uses a 'SSA_ready' node:
    bool m_ir_tests_ready = false;

    // If the function (and called fns) doesn't do I/O.
    // (Using mutable memory state is OK.)
    // Gets set by 'calc_reads_writes_purity'.
    bool m_ir_io_pure = false;

    // If the function (or a called fn) waits on NMI
    bool m_ir_fences = false;

    // If the function bankswitches to a different bank:
    bool m_returns_in_different_bank = false;
};


struct fn_impl_base_t
{
    virtual ~fn_impl_base_t() {}
};

struct mode_impl_t : public fn_impl_base_t
{
    static constexpr fn_class_t fclass = FN_MODE;

    // This map is only used for modes.
    // It tracks which group are preserved in 'goto mode' statements to this mode. 
    // (This will be used to verify that all preserved groups are valid.)
    std::mutex incoming_preserved_groups_mutex;
    fc::small_map<group_ht, pstring_t, 16> incoming_preserved_groups;

    // Superset of 'm_precheck_group_vars',
    // defined as that union'd with the NMI and IRQ 'm_precheck_group_vars'.
    xbitset_t<group_vars_ht> m_precheck_mode_group_vars;
};

struct nmi_impl_t : public fn_impl_base_t
{
    static constexpr fn_class_t fclass = FN_NMI;
    unsigned index = ~0;
    xbitset_t<fn_ht> used_in_modes;
};

struct irq_impl_t : public fn_impl_base_t
{
    static constexpr fn_class_t fclass = FN_IRQ;
    unsigned index = ~0;
    xbitset_t<fn_ht> used_in_modes;
};

struct pstring_mods_t
{
    pstring_t pstring;
    mods_t const* mods;
};

struct precheck_tracked_t
{
    // This refers to a stmt index for regular functions,
    // OR an ast_t children index for iasm.
    using code_handle_t = unsigned;

    fc::vector_map<group_ht, src_type_t> deref_groups;
    rh::batman_set<type_t> deref_types;
    std::vector<pstring_mods_t> wait_nmis;
    std::vector<pstring_mods_t> fences;
    std::vector<std::pair<fn_ht, pstring_mods_t>> goto_modes;
    fc::vector_map<fn_ht, pstring_t> calls;
    fc::vector_map<fn_set_ht, pstring_t> calls_ptrs;
    fc::vector_map<gvar_ht, pstring_t> gvars_used;
};

class fn_t : public callable_t, public modded_t
{
friend class global_t;
friend class fn_set_t;
public:
    static constexpr global_class_t global_class = GLOBAL_FN;
    using handle_t = fn_ht;

    fn_t(global_t& global, type_t type, fn_def_t&& fn_def, std::unique_ptr<mods_t> mods, 
         fn_class_t fclass, bool iasm, fn_set_t* fn_set);

    fn_ht handle() const;

    type_t type() const { return m_type; }
    fn_def_t const& def() const { return m_def; }

    void resolve();
    void precheck();
    void compile();
    void compile_iasm();

    fn_ht mode_nmi() const; // Returns the NMI of this mode.
    unsigned nmi_index() const;
    xbitset_t<fn_ht> const& nmi_used_in_modes() const; 

    fn_ht mode_irq() const; // Returns the IRQ of this mode.
    unsigned irq_index() const;
    xbitset_t<fn_ht> const& irq_used_in_modes() const; 

    bool sloppy() const { return m_sloppy; }

    precheck_tracked_t const& precheck_tracked() const { assert(m_precheck_tracked); return *m_precheck_tracked; }
    auto const& precheck_group_vars() const { assert(m_precheck_group_vars); return m_precheck_group_vars; }
    auto const& precheck_parent_modes() const {assert(compiler_phase() > PHASE_PRECHECK); return m_precheck_parent_modes; }
    auto const& mode_group_vars() const { assert(fclass == FN_MODE); return pimpl<mode_impl_t>().m_precheck_mode_group_vars; }
    unsigned precheck_called() const { assert(compiler_phase() > PHASE_PRECHECK); return m_precheck_called; }

    // These are only valid after 'calc_ir_reads_writes_purity' has ran.
    bool ct_pure() const;

    bool always_inline() const { passert(global.compiled(), global.name); return m_always_inline; }

    locator_t first_bank_switch() const { assert(global.compiled()); return m_first_bank_switch; }
    void assign_first_bank_switch(locator_t loc) { assert(compiler_phase() == PHASE_COMPILE); m_first_bank_switch = loc; }

    rom_proc_ht rom_proc() const { return m_rom_proc; }

    void assign_lvars(lvars_manager_t&& lvars);
    lvars_manager_t const& lvars() const { assert(compiler_phase() >= PHASE_COMPILE); return m_lvars; }
    
    void assign_lvar_span(romv_t romv, unsigned lvar_i, span_t span);
    span_t lvar_span(romv_t romv, int lvar_i) const;
    span_t lvar_span(romv_t romv, locator_t loc) const;

    bool referenced() const { return m_referenced.load(); }

    void mark_referenced_return();
    bool referenced_return() const { return m_referenced.load() & 1; }

    void mark_referenced_param(unsigned param);
    std::uint64_t referenced_params() const { return m_referenced.load() >> 1; }
    std::uint64_t referenced_param(unsigned param) const { return referenced_params() & param; }
    void for_each_referenced_locator(std::function<void(locator_t)> const& fn) const;
    void for_each_referenced_param_locator(std::function<void(locator_t)> const& fn) const;

    // Iterates this function, and every inline function it calls, once each.
    template<typename Fn>
    void for_each_inlined(Fn const& fn) const
    {
        std::size_t const bs_size = fn_ht::bitset_size();
        bitset_uint_t* bs = ALLOCA_T(bitset_uint_t, bs_size);

        bitset_clear_all(bs_size, bs);

        for_each_inlined_impl(fn, bs);
    }

    locator_t new_asm_goto_mode(fn_ht fn, unsigned label, pstring_t pstring, mods_t const* mods); 
    rom_proc_ht asm_goto_mode_rom_proc(unsigned i) const; 
    static void implement_asm_goto_modes(); 

    std::stringstream const* info_stream() const { return m_info_stream.get(); }
    std::stringstream* info_stream() { return m_info_stream.get(); }

    fn_set_t* fn_set() const { return m_fn_set; }

    virtual void for_each_fn(std::function<void(fn_ht)> const& fn) const override;
    
private:
    template<typename Fn>
    void for_each_inlined_impl(Fn const& fn, bitset_uint_t* bs) const
    {
        if(bitset_test(bs, handle().id))
           return;

        fn(*this);
        bitset_set(bs, handle().id);

        precheck_calls().for_each([&](fn_ht h)
        {
            if(h->fclass == FN_FN && h->always_inline())
            {
                h->for_each_inlined_impl(fn, bs);
            }
        });
    }

public:
    global_t& global;
    fn_class_t const fclass;
    bool const iasm = false; // if the fn is inline assembly
private:
    void precheck_finish_mode() const;
    void precheck_finish_nmi_irq() const;

    void calc_precheck_bitsets();
    void calc_ir_bitsets(ir_t const* ir);

    template<typename P>
    P& pimpl() const { assert(P::fclass == fclass); return *static_cast<P*>(m_pimpl.get()); }

    type_t m_type;
    fn_def_t m_def;

    fn_set_t* m_fn_set;

    // This enables different fclasses to store different data.
    std::unique_ptr<fn_impl_base_t> m_pimpl;

    // Our precheck info:
    std::unique_ptr<precheck_tracked_t> m_precheck_tracked;
    fc::vector_set<fn_ht> m_precheck_parent_modes;

    // If we're using faster, but less accurate code generation:
    bool m_sloppy = false;

    // If the function should be inlined:
    bool m_always_inline = false;

    // The first, dominating bank switch in this function.
    // (This is the bank the fn should be called from.)
    locator_t m_first_bank_switch = {};

    // Holds the assembly code generated.
    rom_proc_ht m_rom_proc;

    // Aids in allocating RAM for local variables:
    lvars_manager_t m_lvars;
    std::array<std::vector<span_t>, NUM_ROMV> m_lvar_spans;

    struct asm_goto_mode_t
    {
        fn_ht fn;
        unsigned label;
        pstring_t pstring;
        fc::vector_set<group_vars_ht> preserves;
        rom_proc_ht rom_proc;
    };

    std::unique_ptr<std::vector<asm_goto_mode_t>> m_asm_goto_modes;


    // Used for debuggable output.
    std::unique_ptr<std::stringstream> m_info_stream;

    // TODO: Alter layout for less false sharing

    // Bitset tracking which parameters and return values have been referenced.
    // (i.e. used with unary operator '&')
    // The first bit tracks the return. 
    std::atomic<std::uint64_t> m_referenced = 0;

    std::atomic<unsigned> m_precheck_called = 0; // Counts how many times this has been called.
};

// Base class for vars and consts.
class global_datum_t : public modded_t
{
public:
    global_datum_t(global_t& global, src_type_t src_type, ast_node_t const* expr, std::unique_ptr<paa_def_t> paa_def, std::unique_ptr<mods_t> mods)
    : modded_t(std::move(mods))
    , global(global)
    , init_expr(expr)
    , m_src_type(src_type)
    , m_def(std::move(paa_def))
    {}
    
    global_t& global;
    ast_node_t const* const init_expr = nullptr;

    type_t type() const { return m_src_type.type; }
    rval_t const& rval() const { passert(global.resolved(), global.name); return m_rval; }

    void dethunkify(bool full);
    void resolve();
    void precheck();
    void compile();

    virtual group_ht group() const = 0;
    paa_def_t const* paa_def() const { return m_def.get(); }

    bool is_paa() const { return paa_def(); }

protected:
    virtual void paa_init(asm_proc_t&& proc) = 0;
    virtual void paa_init(loc_vec_t&& vec) = 0;
    virtual void rval_init(rval_t&& rval) = 0;

    src_type_t m_src_type = {};
    rval_t m_rval = {};
    std::unique_ptr<paa_def_t> m_def;
};
 
class gvar_t : public global_datum_t
{
friend class global_t;
public:
    static constexpr global_class_t global_class = GLOBAL_VAR;
    using handle_t = gvar_ht;

    inline gvar_ht handle() const { return global.handle<gvar_ht>(); }

    gvar_t(global_t& global, src_type_t src_type, group_vars_ht group_vars, ast_node_t const* expr, 
           std::unique_ptr<paa_def_t> paa_def, std::unique_ptr<mods_t> mods)
    : global_datum_t(global, src_type, expr, std::move(paa_def), std::move(mods))
    , group_vars(group_vars)
    {}

    group_vars_ht const group_vars = {};

    virtual group_ht group() const;

    auto handles() const { return std::ranges::iota_view(begin(), end()); }
    gmember_ht begin() const { assert(compiler_phase() > PHASE_COUNT_MEMBERS); return m_begin_gmember; }
    gmember_ht end() const { assert(compiler_phase() > PHASE_COUNT_MEMBERS); return m_end_gmember; }
    std::size_t num_members() { return end() - begin(); }

    void set_gmember_range(gmember_ht begin, gmember_ht end);

    byte_block_data_t const& init_data() const { assert(compiler_phase() > PHASE_COMPILE); return m_init_data; }
    void relocate_init_data(std::uint16_t addr);

    void for_each_locator(std::function<void(locator_t)> const& fn) const;

    static std::vector<gvar_ht> const& groupless_gvars() { assert(compiler_phase() > PHASE_PARSE); return m_groupless_gvars; }
private:
    virtual void paa_init(asm_proc_t&& proc);
    virtual void paa_init(loc_vec_t&& vec);
    virtual void rval_init(rval_t&& rval);

    byte_block_data_t m_init_data = {};

    gmember_ht m_begin_gmember = {};
    gmember_ht m_end_gmember = {};

    inline static std::mutex m_groupless_gvars_lock;
    inline static std::vector<gvar_ht> m_groupless_gvars;
};

class gmember_t
{
public:
    using handle_t = gmember_ht;

    gmember_t(gvar_t& parent, unsigned index)
    : gvar(parent)
    , index(index)
    {}

    gvar_t& gvar;
    unsigned const index;

    unsigned member() const { return index - gvar.begin().id; }
    type_t type() const { return member_type(gvar.type(), member()); }

    locator_t const* init_data(unsigned atom) const;
    std::size_t init_size() const;
    std::size_t init_span() const;

    void alloc_spans();
    span_t span(unsigned atom) const 
    { 
        assert(compiler_phase() >= PHASE_ALLOC_RAM); 
        passert(atom < m_spans.size(), atom, m_spans.size(), gvar.global.name); 
        return m_spans[atom]; 
    }
    void assign_span(unsigned atom, span_t span) { assert(compiler_phase() == PHASE_ALLOC_RAM); m_spans[atom] = span; }

    bool zero_init(unsigned atom) const;

private:
    locator_t const* init_data(unsigned atom, loc_vec_t const& vec) const;

    bc::small_vector<span_t, 2> m_spans = {};
};

class const_t : public global_datum_t
{
public:
    static constexpr global_class_t global_class = GLOBAL_CONST;
    using handle_t = const_ht;

    inline const_ht handle() const { return global.handle<const_ht>(); }

    const_t(global_t& global, src_type_t src_type, group_data_ht group_data, bool banked, ast_node_t const* expr, 
            std::unique_ptr<paa_def_t> paa_def, std::unique_ptr<mods_t> mods)
    : global_datum_t(global, src_type, expr, std::move(paa_def), std::move(mods))
    , group_data(group_data)
    , banked(banked)
    { assert(init_expr); }

    group_data_ht const group_data;
    bool const banked = false;

    virtual group_ht group() const;

    rom_array_ht rom_array() const { assert(global.compiled()); return m_rom_array; }

private:
    virtual void paa_init(asm_proc_t&& proc);
    virtual void paa_init(loc_vec_t&& vec);
    virtual void rval_init(rval_t&& rval);

    rom_array_ht m_rom_array = {};
};

class charmap_t : public modded_t
{
public:
    static constexpr global_class_t global_class = GLOBAL_CHARMAP;
    using handle_t = charmap_ht;

    charmap_t(global_t& global, bool is_default, 
              string_literal_t const& characters, 
              string_literal_t const& sentinel,
              std::unique_ptr<mods_t> mods);

    global_t& global;
    bool const is_default = false;

    unsigned size() const { return m_num_unique; }
    int convert(char32_t ch) const; // Returns negative on failure.
    int sentinel() const { return m_sentinel; }

    group_data_ht group_data() const { assert(compiler_phase() > PHASE_CHARMAP_GROUPS); return m_group_data; }
    bool stows_omni() const { assert(compiler_phase() > PHASE_CHARMAP_GROUPS); return m_stows_omni; }
    void set_group_data();
    static void set_all_group_data();

    void resolve() {}
    void precheck() {}
    void compile() {}
private:
    rh::batman_map<char32_t, unsigned> m_map;
    unsigned m_num_unique = 0;
    int m_sentinel = -1;
    group_data_ht m_group_data = {};
    bool m_stows_omni = false;
    rom_array_ht m_byte_pairs = {};
};

class fn_set_t : public callable_t
{
friend class global_t;
friend class fn_t;
public:
    static constexpr global_class_t global_class = GLOBAL_FN_SET;
    using handle_t = fn_set_ht;

    inline fn_set_ht handle() const { return global.handle<fn_set_ht>(); }

    fn_set_t(global_t& global)
    : global(global)
    {}

    void resolve();
    void precheck();
    void compile();

    global_t& lookup(char const* source, pstring_t pstring);
    global_t* lookup_hash(std::uint64_t hash) const;

    auto begin() const { assert(global.resolved()); return m_fns.cbegin(); }
    auto end()   const { assert(global.resolved()); return m_fns.cend(); }

    bool banked_ptrs() const { assert(compiler_phase() > PHASE_PARSE); return m_banked_ptrs; }
    unsigned ptr_size() const { return banked_ptrs() ? 3 : 2; }
    unsigned num_members() const { return banked_ptrs() ? 2 : 1; }
    void count_members();
    unsigned size_of() const { return ptr_size(); }
    bool all_static() const { return m_all_static; }

    romv_t romv() const 
    { 
        assert(builtin::popcount(m_precheck_romv) <= 1); 
        return m_precheck_romv ? romv_t(builtin::ctz(m_precheck_romv)) : ROMV_MODE; 
    }

    virtual void for_each_fn(std::function<void(fn_ht)> const& fn) const override;

    type_t type() const { assert(global.prechecked()); return m_type; }

    span_t lvar_span(romv_t romv, locator_t loc) const;

    global_t& global;
private:
    std::mutex m_fns_mutex;
    std::vector<fn_ht> m_fns;
    ident_map_t<global_ht> m_fns_map;
    rh::batman_map<std::uint64_t, global_t*> m_fn_hashes;
    type_t m_type = TYPE_VOID;

    bool m_banked_ptrs = false;
    bool m_all_static = false;
};

inline fn_ht fn_t::handle() const { return global.handle<fn_ht>(); }

fn_t const& get_main_mode();

charmap_t const& get_charmap(pstring_t from, global_t const& global);

#endif
