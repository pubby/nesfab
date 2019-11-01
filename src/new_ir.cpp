#include "new_ir.hpp"

// Allocates the specified amount, using small buffer optimization 
// whenever possible.
template<typename T, typename SizeT, std::size_t StorageSize> 
[[gnu::always_inline]]
static void sbo_resize(T*& ptr, SizeT& size, SizeT& capacity,
                       std::array<T, StorageSize>& storage, SizeT new_size)
{
    if(new_size < capacity)
    {
        size = new_size;
        return;
    }

    T* old_ptr = ptr;
    SizeT old_capacity = capacity;

    if(new_size > StorageSize)
    {
        capacity = 1 << (builtin::rclz(new_size + 2));
        assert(builtin::popcount(capacity) == 1);
        assert(capacity >= new_size);
        ptr = new T[capacity]
    }
    else
        ptr = storage.data();

    std::copy_n(old_ptr, size, ptr);
    size = new_size;

    if(old_capacity > StorageSize)
        delete[] old_ptr;
}

// This is like 'sbo_resize', except only used for the first allocation.
// (i.e. when 'capacity == 0')
// It's slightly more efficient than 'sbo_resize'.
template<typename T, typename SizeT, std::size_t StorageSize> 
[[gnu::always_inline]]
static void sbo_alloc(T*& ptr, SizeT& size, SizeT& capacity,
                      std::array<T, StorageSize>& storage, SizeT new_size)
{
    assert(capacity == 0);

    if(new_capacity > StorageSize)
    {
        capacity = 1 << (builtin::rclz(new_size + 2));
        assert(builtin::popcount(capacity) == 1);
        assert(capacity >= new_size);
        ptr = new T[capacity]
    }
    else
        ptr = storage.data();
    size = new_size;
    assert(capacity >= size);

#ifndef NDEBUG
    // This is useful for running asserts on.
    std::fill_n(ptr, size, T{});
#endif
}

// Self-explanatory convenience function..
template<typename T, typename SizeT, std::size_t StorageSize> 
[[gnu::always_inline]]
static void sbo_free(T* ptr, SizeT size, std::array<T, StorageSize>& storage)
{
    if(size > StorageSize)
        delete[] ptr;
}

// Call while moving to properly move the small buffer.
template<typename T, typename SizeT, std::size_t StorageSize> 
[[gnu::always_inline]]
static void sbo_moved(T*& ptr, SizeT capacity, std::array<T, StorageSize>& sbo,
                      std::array<T, StorageSize>& o_sbo)
{
    if(capacity <= StorageSize)
    {
        ptr = sbo.data();
        sbo = o_sbo;
    }
}

////////////////////////////////////////
// ssa_node_t                         //
////////////////////////////////////////

ssa_node_t::ssa_node_t(ssa_node_t&& o) { operator=(std::move(o)); }
ssa_node_t& ssa_node_t::operator=(ssa_node_t&& o)
{
    *this = o;
    sbo_moved(m_input, m_input_capacity, m_input_sbo, o.m_input_sbo);
    sbo_moved(m_output, m_output_capacity, m_output_sbo, o.m_output_sbo);
    o = {};
}

~ssa_node_t::ssa_node_t()
{
    sbo_free(m_input, m_input_size, m_input_sbo);
    sbo_free(m_output, m_output_size, m_output_sbo);
}

void ssa_node_t::create(ssa_op_t op, type_t type)
{
    assert(m_input == nullptr);
    assert(m_output == nullptr);
    assert(m_input_size == 0);
    assert(m_output_size == 0);
    assert(m_input_capacity == 0);
    assert(m_output_capacity == 0);

    m_op = op;
    m_type = type;
    flags = 0;

    //sbo_alloc(m_input, m_input_size, m_input_sbo, input_size, input_size);
}

void ssa_node_t::destroy()
{
    sbo_free(m_input, m_input_size, m_input_sbo);
    sbo_free(m_output, m_output_size, m_output_sbo);
    m_input = nullptr;
    m_output = nullptr;
    m_input_size = 0;
    m_output_size = 0;
    m_input_capacity = 0;
    m_output_capacity = 0;
}

void ssa_node_t::resize_input(unsigned size)
{
    sbo_resize(m_input, m_input_size, m_input_sbo, size);
}

void ssa_node_t::resize_output(unsigned size)
{
    sbo_resize(m_output, m_output_size, m_output_sbo, size);
}

void ssa_node_t::alloc_input(unsigned size)
{
    assert(m_input_capacity = 0);
    sbo_alloc(m_input, m_input_size, m_input_sbo, size);
}

void ssa_node_t::alloc_output(unsigned size)
{
    assert(m_output_capacity = 0);
    sbo_alloc(m_output, m_output_size, m_output_sbo, size);
}

void ssa_node_t::build_set_input(unsigned i, ssa_ht new_node_h)
{
    assert(i < input_vec.size());
    assert(!m_input[i].ht());

    ssa_node_t& new_node = *new_node_h;
    m_input[i] = { new_node_h, new_node.m_output_size };
    new_node.append_output({ ht(), i });
}

void ssa_node_t::link_append_input(ssa_fwd_edge_t input)
{
    unsigned const i = m_input_size;
    if(input.is_ht())
    {
        ssa_node_t& node = *input.ht();
        unsigned const o = node.m_output_size;
        input.set_index(o);
        node.resize_output(o + 1);
        node.m_output[o].set(ht(), i);
    }
    resize_input(i + 1);
    m_input[i] = input;
}

void ssa_node_t::remove_inputs_output(unsigned i)
{
    assert(i < m_input_size);

    ssa_fwd_edge_t input = m_input[i];
    if(input.is_ht())
    {
        assert(m_input[i].ht());

        ssa_node_t& from_node = *input.ht();
        unsigned const from_i = input.index();

        // Remove the output edge that leads to our input on 'i'.
        from_node.last_output().input().set_index(from_i);
        std::swap(from_node.m_output[from_i], from_node.last_output());
        from_node.m_output_size -= 1;
    }
}

void ssa_node_t::link_remove_input(unsigned i)
{
    assert(i < m_input_size);

    // The back of 'input_vec' will move to position 'i'.
    // We have to adjust the edge's index too.
    // Do that first, before calling 'remove_inputs_output'.
    if(ssa_bck_edge_t* o = last_input().output())
        o->index = i;

    // Deal with the node we're receiving input along 'i' from.
    remove_inputs_output(i);

    // Remove the input edge on 'i'.
    std::swap(m_input[i], last_input());
    --m_input_size;
}

// Returns true if changed.
bool ssa_node_t::link_change_input(unsigned i, ssa_fwd_edge_t new_value)
{
    assert(i < m_input_size);

    if(new_value.target() == m_input[i].target())
        return false;

    // First deal with the node we're receiving input along 'i' from.
    remove_inputs_output(i);

    // Now change our input.
    if(new_value.is_ht())
    {
        assert(new_value.ht());
        ssa_node_t& from_node = *new_value;
        new_value.set_index(from_node.m_output_size);

        // Add the new output entry.
        auto append_i = from_node.m_output_size;
        from_node.resize_output(append_i + 1);
        from_node.m_output[append_i] = { ht(), i };
    }
    m_input[i] = new_value;
    return true;
}

void ssa_node_t::link_clear_inputs()
{
    for(std::size_t i = 0; i < input_vec.size(); ++i)
        remove_inputs_output(i);
    m_input_size.size = 0;
}

void ssa_node_t::replace_with_const(fixed_t const_val)
{
    for(unsigned i = 0; i < m_output_size; ++i)
        m_output[i].input() = const_val;
    m_output_size = 0;
}

void ssa_node_t::replace_with(ssa_fwd_edge_t value)
{
    if(value.is_const())
        return replace_with_const(value.fixed());

    assert(value.ht());
    ssa_node_t& node = *value.ht();

    if(&node == this)
        return;

    // All of this node's outputs will get appended onto 'node's outputs.
    unsigned index = node.m_output_size;
    node.resize_output(node.m_output_size + m_output_size);

    for(unsigned i = 0; i < m_output_size; ++i)
    {
        m_output[i].input().set(value.ht(), index);
        node.m_output[index] = m_output[i];
        ++index;
    }

    m_output_size = 0;
}

////////////////////////////////////////
// cfg_node_t                         //
////////////////////////////////////////

cfg_node_t::cfg_node_t(cfg_node_t&& o) { operator=(std::move(o)); }
cfg_node_t& cfg_node_t::operator=(cfg_node_t&& o)
{
    *this = o;
    sbo_moved(m_input, m_input_capacity, m_input_sbo, o.m_input_sbo);
    sbo_moved(m_output, m_output_capacity, m_output_sbo, o.m_output_sbo);
    o = {};
    return *this;
}

~cfg_node_t::cfg_node_t()
{
    sbo_free(m_input, m_input_size, m_input_sbo);
    sbo_free(m_output, m_output_size, m_output_sbo);

    // Unlink all members.
    // TODO
}


void cfg_node_t::alloc_input(unsigned size)
{
    assert(m_input_capacity = 0);
    sbo_alloc(m_input, m_input_size, m_input_sbo, size, size);
#ifndef NDEBUG
    std::fill_n(m_input, m_input_size, 0);
#endif
}

void cfg_node_t::alloc_output(unsigned size)
{
    assert(m_output_capacity = 0);
    sbo_alloc(m_output, m_output_size, m_output_sbo, size, size);
#ifndef NDEBUG
    std::fill_n(m_output, m_output_size, 0);
#endif
}

void cfg_node_t::build_set_output(unsigned i, cfg_ht new_node_h)
{
    assert(i < output_vec.size());
    assert(!m_output[i].ht());

    cfg_node_t& new_node = *new_node_h;
    m_output[i] = { new_node, new_node.m_input_size };
    new_node.append_input({ ht(), i });
}

ssa_ht cfg_node_t::emplace_ssa(ssa_op_t op, type_t type)
{
    // Alloc and initialize it.
    ssa_ht h = ssa_pool::alloc();
    ssa_node_t& node = *h;
    node.create(op, type);

    // Add it to our list.
    if(op == SSA_phi)
    {
        if(!m_ssa_begin)
            m_ssa_begin = h;
        if(m_last_non_phi)
            m_last_non_phi->next = h;
        if(m_phi_begin)
            m_phi_begin->prev = h;
        node.next = m_phi_begin;
        node.prev = m_last_non_phi;
        m_phi_begin = h;
    }
    else
    {
        if(m_ssa_begin)
            m_ssa_begin->prev = h;
        if(!m_last_non_phi)
            m_last_non_phi = h;
        node.next = m_ssa_begin;
        node.prev = {};
        m_ssa_begin = h;
    }

    return h;
}

ssa_ht cfg_t::unsafe_prune_ssa(ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;
    ssa_ht ret = ssa_node.next;

    // Remove it from our list.
    if(ssa_h == m_last_non_phi)
        m_last_non_phi = ssa_node.prev;
    if(ssa_h == m_phi_begin)
        m_phi_begin = ssa_node.next;
    if(ssa_node.prev)
        ssa_node.prev->next = ssa_node.next;
    else
    {
        assert(ssa_h == m_ssa_begin);
        m_ssa_begin = ssa_node.next;
    }
    if(ssa_node.next)
        ssa_node.next->prev = ssa_node.prev;

    // Free it
    ssa_node.destroy();
    ssa_pool::free(ssa_h);

    return ret;
}

////////////////////////////////////////
// ir_t                               //
////////////////////////////////////////

cfg_ht ir_t::emplace_cfg()
{
    // Alloc and initialize it.
    cfg_ht h = cfg_pool::alloc();
    cfg_node_t& node = *h;
    node.create();

    // Add it to our list.
    if(m_cfg_begin)
        m_cfg_begin->prev = h;
    node.next = m_cfg_begin;
    node.prev = {};
    m_cfg_begin = h;

    return h;
}

cfg_ht unsafe_prune_cfg(cfg_ht cfg_h)
{
    assert(cfg_h != m_root);
    assert(cfg_h != m_exit);

    cfg_node_t& cfg_node = *cfg_h;
    assert(!cfg_node.ssa_begin());

    cfg_ht ret = cfg_node.next;

    // Remove it from our list.
    if(cfg_node.prev)
        cfg_node.prev->next = cfg_node.next;
    else
    {
        assert(cfg_h == m_cfg_begin);
        m_cfg_begin = cfg_node.next;
    }
    if(cfg_node.next)
        cfg_node.next->prev = cfg_node.prev;

    // Free it
    cfg_node.destroy();
    cfg_pool::free(cfg_h);

    return ret;
}

cfg_ht ir_t::split_edge(cfg_bck_edge_t edge)
{
    cfg_ht split_h = emplace_cfg();

    edge.input().output() = { split_h, 0 };

    split.alloc_input(1);
    split.m_input[0] = edge.input();

    split.alloc_output(1);
    split.m_output[0] = edge;

    edge.input() = { split_h, 0 };

    return split;
}

cfg_ht ir_t::merge_edge(cfg_ht cfg_h)
{
    cfg_node_t& cfg_node = *cfg_h;
    assert(cfg_node.input_size() == 1);
    assert(cfg_node.output_size() == 1);

    node.input(0).output() = node.output(0);
    node.output(0).input() = node.input(0);

    return unsafe_prune_cfg(cfg_h);
}

