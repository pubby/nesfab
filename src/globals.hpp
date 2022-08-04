#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <cassert>
#include <ostream>

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
#include "stmt.hpp"
#include "type.hpp"
#include "lvar.hpp"
#include "sval.hpp"
#include "rom_decl.hpp"
#include "locator.hpp"
#include "mods.hpp"

struct rom_array_t;
struct eval_tracked_t;

namespace bc = boost::container;

std::string to_string(global_class_t gclass);

// A data member of a record.
struct field_t
{
    var_decl_t decl;
    token_t const* init_expr = nullptr;
    sval_t default_sval;

    type_t& type() { return decl.src_type.type; }
    type_t const& type() const { return decl.src_type.type; }
    pstring_t type_pstring() const { return decl.src_type.pstring; }
};

using field_map_t = rh::batman_map<std::uint64_t, field_t, std::identity>;

class global_t
{
public:
    using ideps_set_t = fc::vector_set<global_t*>;
    std::string const name;
private:
    // These variables are set only by 'define', as soon
    // as the global's definition is parsed.
    std::mutex m_define_mutex;
    global_class_t m_gclass = GLOBAL_UNDEFINED;
    pstring_t m_pstring = {};

    // An index into some storage that holds the global's implementation data
    unsigned m_impl_id = ~0;

    // 'ideps' means "immediate dependencies".
    // AKA any global name that appears in the definition of this global.
    // This is set by 'define'
    ideps_set_t m_ideps;

    // 'weak_ideps' exist to handle recursive mode gotos.
    // These can be converted into regular ideps, if no cycles will be created.
    // Otherwise, they're simply discarded.
    ideps_set_t m_weak_ideps;

    // Likewise, 'iuses' holds the immediate users of this global.
    // This is built after all globals have been created, after parsing.
    ideps_set_t m_iuses;
    std::atomic<unsigned> m_ideps_left = 0;

    std::atomic<bool> m_compiled = false; // Use for debugging only.
public:
    global_t() = delete;

    global_t(pstring_t pstring, char const* source)
    : name(pstring.view(source))
    , m_pstring(pstring)
    {
        assert(m_pstring.size);
    }

    global_class_t gclass() const { assert(compiler_phase() > PHASE_PARSE); return m_gclass; }
    ideps_set_t const& ideps() const { assert(compiler_phase() > PHASE_PARSE); return m_ideps; }
    pstring_t pstring() const { return m_pstring; }
    unsigned impl_id() const { assert(compiler_phase() > PHASE_PARSE); return m_impl_id; }
    bool compiled() const { return m_compiled; }

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

    // If this global has a dependency to 'other'
    bool has_dep(global_t& other);

    // Helpers that delegate to 'define':
    fn_t& define_fn(
        pstring_t pstring, global_t::ideps_set_t&& ideps, global_t::ideps_set_t&& weak_ideps, 
        type_t type, fn_def_t&& fn_def, mods_t&& mods, fn_class_t fclass);
    gvar_t& define_var(
        pstring_t pstring, global_t::ideps_set_t&& ideps, 
        src_type_t src_type, std::pair<group_vars_t*, group_vars_ht> group, token_t const* expr);
    const_t& define_const(
        pstring_t pstring, global_t::ideps_set_t&& ideps, 
        src_type_t src_type, std::pair<group_data_t*, group_data_ht> group, token_t const* expr);
    struct_t& define_struct(
        pstring_t pstring, global_t::ideps_set_t&& ideps, field_map_t&& map);

    static void init();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(char const* source, pstring_t name);

    static global_t const* lookup_sourceless(std::string_view view);
    //static group_ht universal_group() { return {0}; }

    // Call after parsing
    static void parse_cleanup();

    // Checks the code and gathers information with a pre-pass evaluation.
    static void precheck();

    // Implementation detail used in 'build_order'.
    static global_t* detect_cycle(global_t& global, std::vector<std::string>& error_msgs);

    // This allocates 'gmember_t's.
    static void count_members(); 

    // Call after 'count_members' to build 'm_iuses' and 'm_ideps_left',
    // among other things.
    // This function isn't thread-safe.
    // Call from a single thread only.
    static void build_order();

    // Call after 'build_order' to well... compile everything!
    static void compile_all();

    static std::vector<fn_t*> modes() { assert(compiler_phase() > PHASE_PARSE); return modes_vec; }
private:
    // Sets the variables of the global:
    unsigned define(pstring_t pstring, global_class_t gclass, 
                    ideps_set_t&& ideps, ideps_set_t&& weak_ideps,
                    std::function<unsigned(global_t&)> create_impl);

    void compile();

    // Returns and pops the next ready global from the ready list.
    static global_t* await_ready_global();

private:
    // Globals get allocated in these:
    inline static rh::robin_auto_table<global_t*> global_pool_map;

    // Tracks modes: 
    inline static std::mutex modes_vec_mutex;
    inline static std::vector<fn_t*> modes_vec;

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
        assert(global.compiled());
        assert(member_i < m_member_types.size());
        return m_member_types[member_i];
    }

    std::uint16_t member_offset(unsigned member_i) const
    {
        assert(global.compiled());
        assert(member_i < m_member_offsets.size());
        return m_member_offsets[member_i];
    }

    bool has_tea_member() const { return m_has_tea_member; }

    unsigned count_members(); 
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

using deref_groups_t = fc::vector_map<group_ht, src_type_t>;

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
};

struct eval_tracked_t
{
    fc::vector_map<group_ht, src_type_t> deref_groups;
    std::vector<std::pair<fn_ht, stmt_ht>> goto_modes;
    fc::vector_map<fn_ht, pstring_t> calls;
    fc::vector_map<gvar_ht, pstring_t> gvars_used;

    //bitset_t non_inlined_calls = bitset_t(fn_ht::bitset_size());
    //bitset_t group_vars_used = bitset_t(group_vars_ht::bitset_size());
    //bitset_t gmembers_used = bitset_t(gmember_ht::bitset_size());
    // TODO
    //bitset_t gvars_required = bitset_t(gvars_ht::bitset_size());

    //bool propagated = false;
};


class fn_t
{
public:
    static constexpr global_class_t global_class = GLOBAL_FN;
    using handle_t = fn_ht;

    fn_t(global_t& global, type_t type, fn_def_t&& fn_def, mods_t&& mods, fn_class_t fclass);

    fn_ht handle() const;

    type_t type() const { return m_type; }
    fn_def_t const& def() const { return m_def; }

    void compile();

    eval_tracked_t const& precheck_tracked() const { assert(m_eval_tracked); return *m_eval_tracked; }
    bitset_t const& precheck_group_vars() const { assert(m_precheck_group_vars); return m_precheck_group_vars; }
    /*
    bitset_t const& lang_preserves_group_vars() const 
    { 
        assert(m_lang_gvars); // as it's lazily allocated, check this instead.
        return m_lang_preserves_group_vars; 
    }
    */

    // These are only valid after 'calc_ir_reads_writes_purity' has ran.
    bitset_t const& ir_reads()  const { assert(m_ir_reads);  return m_ir_reads; }
    bitset_t const& ir_writes() const { assert(m_ir_writes); return m_ir_writes; }
    bitset_t const& ir_group_vars() const { assert(m_ir_group_vars); return m_ir_group_vars; }
    bitset_t const& ir_calls() const { assert(m_ir_calls); return m_ir_calls; }
    bitset_t const& ir_deref_groups() const { assert(m_ir_deref_groups); return m_ir_deref_groups; }
    bool ir_io_pure() const { assert(m_ir_writes); return m_ir_io_pure; }

    bool ir_reads(gmember_ht gmember)  const { return ir_reads().test(gmember.id); }
    bool ir_writes(gmember_ht gmember) const { return ir_writes().test(gmember.id); }

    rom_proc_ht rom_proc() const { return m_rom_proc; }

    void assign_lvars(lvars_manager_t&& lvars);
    lvars_manager_t const& lvars() const { assert(compiler_phase() >= PHASE_COMPILE); return m_lvars; }
    
    void assign_lvar_span(unsigned lvar_i, span_t span);
    span_t lvar_span(int lvar_i) const;
    span_t lvar_span(locator_t loc) const;

    void precheck_eval();
    void precheck_propagate();
    void precheck_verify() const;

public:
    global_t& global;
    fn_class_t const fclass;
    mods_t const mods;

private:
    void calc_ir_bitsets(ir_t const& ir);

    template<typename P>
    P& pimpl() const { assert(P::fclass == fclass); return *static_cast<P*>(m_pimpl.get()); }

    type_t m_type;
    fn_def_t m_def;

    // This enables different fclasses to store different data.
    std::unique_ptr<fn_impl_base_t> m_pimpl;

    // TODO
    std::unique_ptr<eval_tracked_t> m_eval_tracked;
    bitset_t m_precheck_group_vars;

    // 'lang_gvars' is calculated shortly after parsing.
    bitset_t m_lang_gvars;
    // Groups are calculated later on, in 'compile'.
    bitset_t m_lang_group_vars;
    // Subset of above. Group vars preserved in goto mode statements.
    // Calculated shortly after parsing.
    bitset_t m_lang_preserves_group_vars; // Lazily allocated. Can be null.

    // Bitsets of all global vars read/written in fn (deep)
    // These get assigned by 'calc_reads_writes_purity'.
    // The thread synchronization is implicit in the order of compilation.
    bitset_t m_ir_reads;
    bitset_t m_ir_writes;
    bitset_t m_ir_group_vars;
    bitset_t m_ir_deref_groups;
    bitset_t m_ir_calls;

    // If the function doesn't do I/O.
    // (Using mutable memory state is OK.)
    // Gets set by 'calc_reads_writes_purity'.
    bool m_ir_io_pure = false;

    // Holds the assembly code generated.
    rom_proc_ht m_rom_proc;

    // Aids in allocating RAM for local variables:
    lvars_manager_t m_lvars;
    std::vector<span_t> m_lvar_spans;
};

// Base class for vars and consts.
class global_datum_t
{
public:
    global_datum_t(global_t& global, src_type_t src_type, token_t const* expr)
    : global(global)
    , init_expr(expr)
    , is_paa(::is_paa(src_type.type.name()))
    , m_src_type(src_type)
    {}
    
    global_t& global;
    token_t const* const init_expr = nullptr;
    bool const is_paa = false; // Cache this so it can be read even before 'type()' is ready.

    type_t type() const { assert(!is_thunk(m_src_type.type.name())); return m_src_type.type; }
    sval_t const& sval() const { assert(global.compiled()); return m_sval; }

    void dethunkify(bool full);
    void compile();

    virtual group_ht group() const = 0;

protected:
    virtual void paa_init(loc_vec_t&& paa) = 0;
    virtual void sval_init(sval_t&& sval) = 0;

    src_type_t m_src_type = {};
    sval_t m_sval;
};
 
class gvar_t : public global_datum_t
{
public:
    static constexpr global_class_t global_class = GLOBAL_VAR;
    using handle_t = gvar_ht;

    inline gvar_ht handle() const { return global.handle<gvar_ht>(); }

    gvar_t(global_t& global, src_type_t src_type, group_vars_ht group_vars, token_t const* expr)
    : global_datum_t(global, src_type, expr)
    , group_vars(group_vars)
    {}

    group_vars_ht const group_vars = {};

    virtual group_ht group() const;

    auto handles() const { return std::ranges::iota_view(begin(), end()); }
    gmember_ht begin() const { assert(compiler_phase() > PHASE_COUNT_MEMBERS); return m_begin_gmember; }
    gmember_ht end() const { assert(compiler_phase() > PHASE_COUNT_MEMBERS); return m_end_gmember; }
    std::size_t num_members() { return end() - begin(); }

    void set_gmember_range(gmember_ht begin, gmember_ht end);

    loc_vec_t const& init_data() const { assert(compiler_phase() > PHASE_COMPILE); return m_init_data; }

    void for_each_locator(std::function<void(locator_t)> const& fn) const;

private:
    virtual void paa_init(loc_vec_t&& paa);
    virtual void sval_init(sval_t&& sval);

    loc_vec_t m_init_data;

    gmember_ht m_begin_gmember = {};
    gmember_ht m_end_gmember = {};
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

    void alloc_spans();
    span_t span(unsigned atom) const { assert(compiler_phase() >= PHASE_ALLOC_RAM); return m_spans[atom]; }
    void assign_span(unsigned atom, span_t span) { assert(compiler_phase() == PHASE_ALLOC_RAM); m_spans[atom] = span; }

    bool zero_init(unsigned atom) const;

private:
    bc::small_vector<span_t, 2> m_spans = {};
};

class const_t : public global_datum_t
{
public:
    static constexpr global_class_t global_class = GLOBAL_CONST;
    using handle_t = const_ht;

    inline const_ht handle() const { return global.handle<const_ht>(); }

    const_t(global_t& global, src_type_t src_type, group_data_ht group_data, token_t const* expr)
    : global_datum_t(global, src_type, expr)
    , group_data(group_data)
    { assert(init_expr); }

    group_data_ht const group_data;

    virtual group_ht group() const;

    rom_array_ht rom_array() const { assert(global.compiled()); return m_rom_array; }

private:
    virtual void paa_init(loc_vec_t&& paa);
    virtual void sval_init(sval_t&& sval);

    rom_array_ht m_rom_array = {};
};

inline fn_ht fn_t::handle() const { return global.handle<fn_ht>(); }

fn_t const& get_main_entry();

#endif
