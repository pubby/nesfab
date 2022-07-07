#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <cassert>
#include <ostream>

#include "robin/collection.hpp"
#include "robin/set.hpp"
#include "robin/map.hpp"

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
    unsigned m_impl_index = ~0;

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
    explicit global_t(std::string_view name)
    : name(name)
    {}

    global_t(pstring_t pstring, char const* source)
    : name(pstring.view(source))
    , m_pstring(pstring)
    {}

    global_class_t gclass() const 
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_gclass;
    }

    ideps_set_t const& ideps() const
    {
        assert(compiler_phase() > PHASE_PARSE);
        return m_ideps;
    }

    pstring_t pstring() const { return m_pstring; }

    unsigned index() const
    { 
        assert(compiler_phase() > PHASE_PARSE);
        return m_impl_index; 
    }

    template<typename T>
    T handle() const
    {
        static_assert(is_global_handle<T>::value);
        assert(gclass() == T::gclass);
        assert(compiler_phase() > PHASE_PARSE);
        return { m_impl_index };
    }

    template<typename T>
    T& impl() const
    {
        static_assert(is_global_impl<T>::value);
        assert(gclass() == T::gclass);
        assert(compiler_phase() > PHASE_PARSE);
        return impl_deque<T>[m_impl_index];
    }

    bool compiled() const { return m_compiled; }

    // If this global has a dependency to 'other'
    bool has_dep(global_t& other);

    // Helpers that delegate to 'define':
    fn_t& define_fn(pstring_t pstring, 
                    global_t::ideps_set_t&& ideps, global_t::ideps_set_t&& weak_ideps, 
                    type_t type, fn_def_t&& fn_def, fclass_t fclass);
    gvar_t& define_var(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                      src_type_t src_type, std::pair<group_vars_t*, group_vars_ht> group,
                      token_t const* expr);
    const_t& define_const(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                          src_type_t src_type, std::pair<group_data_t*, group_data_ht> group,
                          token_t const* expr);
    struct_t& define_struct(pstring_t pstring, global_t::ideps_set_t&& ideps, 
                            field_map_t&& map);

    static void init();

    // Creates a global if it doesn't exist,
    // otherwise returns the existing global with name.
    static global_t& lookup(char const* source, pstring_t name);

    // TODO
    //static global_t& lookup_sourceless(std::string_view view);
    //static group_ht universal_group() { return {0}; }

    // Call after parsing
    static void parse_cleanup();

    // Implementation detail used in 'build_order'.
    static global_t* detect_cycle(global_t& global, std::vector<std::string>& error_msgs);

    // Call after 'parse_cleanup' to properly handle struct members.
    // This allocates 'gmember_t's.
    static void count_members(); 

    // Call after 'count_members' to build 'm_iuses' and 'm_ideps_left',
    // among other things.
    // This function isn't thread-safe.
    // Call from a single thread only.
    static void build_order();

    // Call after 'build_order' to well... compile everything!
    static void compile_all();

    // Call after 'compile_all'.
    // Assigns variables to ram addresses.
    static void alloc_ram();

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
    inline static std::mutex global_pool_mutex;
    inline static rh::robin_auto_table<global_t*> global_pool_map;
    static std::deque<global_t> global_pool;

    // Tracks modes: 
    inline static std::mutex modes_vec_mutex;
    inline static std::vector<fn_t*> modes_vec;

    // These represent a queue of globals ready to be compiled.
    inline static std::condition_variable ready_cv;
    inline static std::mutex ready_mutex;
    static std::vector<global_t*> ready;
    inline static unsigned globals_left;
};

class struct_t
{
public:
    static constexpr compiler_phase_t impl_deque_phase = PHASE_PARSE;
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_STRUCT;

    struct_t(global_t& global, field_map_t&& fields)
    : global(global)
    , m_fields(std::move(fields))
    {}

    global_t& global;

    field_map_t const& fields() const { return m_fields; }
    field_t const& field(unsigned i) const { return m_fields.begin()[i].second; }

    unsigned num_members() const { assert(m_num_members != UNCOUNTED); return m_num_members; }

    unsigned member(unsigned field_i) const
    {
        assert(field_i < fields().size());
        unsigned m = 0;
        for(unsigned k = 0; k < field_i; ++k)
            m += ::num_members(field(k).type());
        return m;
    }

    type_t member_type(unsigned i) const 
    {
        assert(global.compiled());
        assert(i < m_member_types.size());
        return m_member_types[i];
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
    bool m_has_tea_member = false;
};

class fn_t
{
public:
    static constexpr compiler_phase_t impl_deque_phase = PHASE_PARSE;
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_FN;

    fn_t(global_t& global, type_t type, fn_def_t fn_def, fclass_t fclass) 
    : global(global)
    , fclass(fclass)
    , m_type(std::move(type))
    , m_def(std::move(fn_def)) 
    /* TODO
    , m_param_record([this]()
    {
        field_vector_t fields;
        for(unsigned i = 0; i < m_def.num_params; ++i)
            fields.push_back({field_t{ .decl = m_def.local_vars[i] }});
        return fields;
    }())
    */
    {}

    fn_ht handle() const;

    type_t type() const { return m_type; }
    fn_def_t const& def() const { return m_def; }

    // TODO
    //record_t<field_vector_t> const& param_record() const { return m_param_record; }

    void compile();

    void calc_lang_gvars_groups();
    void calc_ir_bitsets(ir_t const& ir);

    bitset_t const& lang_gvars()  const { assert(m_lang_gvars);  return m_lang_gvars; }
    bitset_t const& lang_group_vars() const { assert(m_lang_group_vars); return m_lang_group_vars; }

    // These are only valid after 'calc_ir_reads_writes_purity' has ran.
    bitset_t const& ir_reads()  const { assert(m_ir_reads);  return m_ir_reads; }
    bitset_t const& ir_writes() const { assert(m_ir_writes); return m_ir_writes; }
    bitset_t const& ir_group_vars() const { assert(m_ir_group_vars); return m_ir_group_vars; }
    bitset_t const& ir_immediate_group_data() const { assert(m_ir_immediate_group_data); return m_ir_immediate_group_data; }
    bitset_t const& ir_calls() const { assert(m_ir_calls); return m_ir_calls; }
    bool ir_io_pure() const { assert(m_ir_writes); return m_ir_io_pure; }

    bool ir_reads(gmember_ht gmember)  const { return ir_reads().test(gmember.value); }
    bool ir_writes(gmember_ht gmember) const { return ir_writes().test(gmember.value); }

    // Be careful to call this from a single thread only.
    void assign_proc(asm_proc_t&& proc)
    {
        assert(compiler_phase() == PHASE_COMPILE);
        m_proc = std::move(proc);
    }

    asm_proc_t const& proc() const { assert(compiler_phase() > PHASE_COMPILE); return m_proc; }

    void assign_lvars(lvars_manager_t&& lvars);
    lvars_manager_t const& lvars() const { assert(compiler_phase() >= PHASE_COMPILE); return m_lvars; }
    
    void mask_usable_ram(ram_bitset_t const& mask);
    ram_bitset_t const& usable_ram() const { return m_usable_ram; }

    ram_bitset_t const& lvar_ram() const { return m_lvar_ram; }
    void assign_lvar_span(unsigned lvar_i, span_t span);
    span_t lvar_span(unsigned lvar_i) const;

    bool emits_code() const { return true; } // TODO: implement

public:
    global_t& global;
    fclass_t const fclass;

    //rom_alloc_ht rom_alloc; TODO
private:
    type_t m_type;
    fn_def_t m_def;

    // TODO
    //ct_manager_t m_ct_manager;

    // TODO
    //record_t<field_vector_t> m_param_record;

    bitset_t m_lang_gvars;
    bitset_t m_lang_group_vars;

    // Bitsets of all global vars read/written in fn (deep)
    // These get assigned by 'calc_reads_writes_purity'.
    // The thread synchronization is implicit in the order of compilation.
    bitset_t m_ir_reads;
    bitset_t m_ir_writes;
    bitset_t m_ir_group_vars;
    bitset_t m_ir_immediate_group_data;
    bitset_t m_ir_calls;

    // If the function doesn't do I/O.
    // (Using mutable memory state is OK.)
    // Gets set by 'calc_reads_writes_purity'.
    bool m_ir_io_pure = false;

    // Holds the assembly code generated.
    asm_proc_t m_proc;

    ram_bitset_t m_usable_ram = ram_bitset_t::filled(); // Tracks unallocated RAM addresses this fn can use
    ram_bitset_t m_lvar_ram = ram_bitset_t::filled();// Tracks which addresses are used by this fn's lvars.

    // Aids in allocating RAM for local variables:
    lvars_manager_t m_lvars;
    std::vector<span_t> m_lvar_spans;
};
 
class gvar_t
{
public:
    static constexpr compiler_phase_t impl_deque_phase = PHASE_PARSE;
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_VAR;

    inline gvar_ht handle() const { return global.handle<gvar_ht>(); }

    gvar_t(global_t& global, src_type_t src_type, group_vars_ht group_vars, token_t const* expr)
    : global(global)
    , group_vars(group_vars)
    , init_expr(expr)
    , m_src_type(src_type)
    {}

    global_t& global;
    group_vars_ht const group_vars = {};
    token_t const* const init_expr = nullptr;

    type_t type() const { assert(!is_thunk(m_src_type.type.name())); return m_src_type.type; }

    gmember_ht begin_gmember() const { assert(compiler_phase() > PHASE_COUNT_MEMBERS); return m_begin_gmember; }
    gmember_ht end_gmember() const { assert(compiler_phase() > PHASE_COUNT_MEMBERS); return m_end_gmember; }
    void set_gmember_range(gmember_ht begin, gmember_ht end);

    //group_bitset_t group_bitset() const { return 1ull << group.value; }

    void dethunkify(bool full);
    void compile();

    void for_each_locator(std::function<void(locator_t)> const& fn) const;

private:
    src_type_t m_src_type = {};
    sval_t m_sval; // TODO?

    gmember_ht m_begin_gmember = {};
    gmember_ht m_end_gmember = {};
};

class gmember_t
{
public:
    static constexpr compiler_phase_t impl_vector_phase = PHASE_COUNT_MEMBERS;

    gmember_t(gvar_t& parent, unsigned index)
    : gvar(parent)
    , index(index)
    {}

    gvar_t& gvar;
    unsigned const index;

    unsigned member() const { return gvar.begin_gmember().value - index; }
    type_t type() const { return member_type(gvar.type(), member()); }

    void alloc_spans();
    span_t span(unsigned atom) const { assert(compiler_phase() >= PHASE_ALLOC_RAM); return m_spans[atom]; }
    void assign_span(unsigned atom, span_t span) { assert(compiler_phase() == PHASE_ALLOC_RAM); m_spans[atom] = span; }

private:
    bc::small_vector<span_t, 2> m_spans = {};
};

class const_t
{
public:
    static constexpr compiler_phase_t impl_deque_phase = PHASE_PARSE;
    using global_impl_tag = void;
    static constexpr global_class_t gclass = GLOBAL_CONST;

    inline const_ht handle() const { return global.handle<const_ht>(); }

    const_t(global_t& global, src_type_t src_type, group_data_ht group_data, token_t const* expr)
    : global(global)
    , group_data(group_data)
    , init_expr(expr)
    , m_src_type(src_type)
    {
        assert(init_expr);
    }

    global_t& global;
    group_data_ht const group_data;
    token_t const* const init_expr = nullptr;

    type_t type() const { assert(global.compiled()); return m_src_type.type; }
    group_ht group();

    void compile();
    sval_t const& sval() const { assert(global.compiled()); return m_sval; }

private:
    src_type_t m_src_type = {};
    sval_t m_sval;
};

inline fn_ht fn_t::handle() const { return global.handle<fn_ht>(); }

#endif
