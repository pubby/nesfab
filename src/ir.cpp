#include "ir.hpp"

#include "builtin.hpp"
#include "globals.hpp"
#include "multi.hpp"

// Allocates the specified amount, using small buffer optimization 
// whenever possible.
template<typename T, std::size_t StorageSize> 
[[gnu::always_inline]] static inline 
void sbo_resize(T*& ptr, std::uint16_t& size, std::uint16_t& capacity,
                std::array<T, StorageSize>& storage, std::uint16_t new_size)
{
    if(new_size < capacity)
    {
        size = new_size;
        return;
    }

    T* old_ptr = ptr;
    std::uint16_t old_capacity = capacity;

    if(new_size > StorageSize)
    {
        capacity = 1 << (builtin::rclz(new_size + 2u));
        passert(capacity >= new_size, capacity);
        passert(builtin::popcount((unsigned)capacity) == 1, capacity);
        ptr = new T[capacity];
    }
    else
    {
        capacity = StorageSize;
        ptr = storage.data();
    }

    std::copy_n(old_ptr, size, ptr);
    size = new_size;

    if(old_capacity > StorageSize)
        delete[] old_ptr;
}

// This is like 'sbo_resize', except only used for the first allocation.
// (i.e. when 'capacity == 0')
// It's slightly more efficient than 'sbo_resize'.
template<typename T, std::size_t StorageSize> 
[[gnu::always_inline]] static inline 
void sbo_alloc(T*& ptr, std::uint16_t& size, std::uint16_t& capacity,
               std::array<T, StorageSize>& storage, std::uint16_t new_size)
{
    assert(capacity == 0);

    if(new_size > StorageSize)
    {
        capacity = 1 << (builtin::rclz(new_size + 2u));
        assert(builtin::popcount((unsigned)capacity) == 1);
        assert(capacity >= new_size);
        ptr = new T[capacity];
    }
    else
    {
        capacity = StorageSize;
        ptr = storage.data();
    }
    size = new_size;
    assert(capacity >= size);
    assert(size == new_size);

#ifndef NDEBUG
    // This is useful for running asserts on.
    std::fill_n(ptr, size, T{});
#endif
}

// Self-explanatory convenience function..
template<typename T, std::size_t StorageSize> 
[[gnu::always_inline]] static inline
void sbo_free(T* ptr, std::uint16_t capacity, 
              std::array<T, StorageSize>& storage)
{
    if(capacity > StorageSize)
        delete[] ptr;
}

// Call while moving to properly move the small buffer.
template<typename T, std::size_t StorageSize> 
[[gnu::always_inline]] static inline
void sbo_moved(T*& ptr, std::uint16_t capacity, 
               std::array<T, StorageSize>& sbo,
               std::array<T, StorageSize>& o_sbo)
{
    if(capacity <= StorageSize)
    {
        ptr = sbo.data();
        sbo = o_sbo;
    }
}

////////////////////////////////////////
// node_io_buffers_t                  //
////////////////////////////////////////

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
auto node_io_buffers_t<I, O, ISize, OSize>::operator=(node_io_buffers_t&& o) 
-> node_io_buffers_t& 
{
    m_input = o.m_input;
    m_output = o.m_output;

    m_input_size = o.m_input_size;
    m_output_size = o.m_output_size;

    m_input_capacity = o.m_input_capacity;
    m_output_capacity = o.m_output_capacity;

    sbo_moved(m_input, m_input_capacity, m_input_sbo, o.m_input_sbo);
    sbo_moved(m_output, m_output_capacity, m_output_sbo, o.m_output_sbo);

    o.m_input_capacity = 0;
    o.m_output_capacity = 0;

    return *this;
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
node_io_buffers_t<I, O, ISize, OSize>::~node_io_buffers_t()
{
    sbo_free(m_input, m_input_capacity, m_input_sbo);
    sbo_free(m_output, m_output_capacity, m_output_sbo);
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::alloc_input(unsigned size)
{
    passert(m_input_capacity == 0, m_input_capacity);
    sbo_alloc(m_input, m_input_size, m_input_capacity, m_input_sbo, size);
    assert(input_size() == size);
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::alloc_output(unsigned size)
{
    passert(m_output_capacity == 0, m_output_capacity);
    sbo_alloc(m_output, m_output_size, m_output_capacity, m_output_sbo, size);
    assert(output_size() == size);
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::resize_input(unsigned size)
{
    sbo_resize(m_input, m_input_size, m_input_capacity, m_input_sbo, size);
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::resize_output(unsigned size)
{
    sbo_resize(m_output, m_output_size, m_output_capacity, m_output_sbo, size);
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::shrink_input(unsigned size)
{
    assert(size <= m_input_size);
    m_input_size = size;
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::shrink_output(unsigned size)
{
    assert(size <= m_output_size);
    m_output_size = size;
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::clear_input()
{
    m_input_size = 0;
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::clear_output()
{
    m_output_size = 0;
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::reset()
{
    sbo_free(m_input, m_input_capacity, m_input_sbo);
    sbo_free(m_output, m_output_capacity, m_output_sbo);
    m_input = nullptr;
    m_output = nullptr;
    m_input_size = m_output_size = m_input_capacity = m_output_capacity = 0;
}

////////////////////////////////////////
// ssa_node_t                         //
////////////////////////////////////////

void ssa_node_t::create(cfg_ht cfg_h, ssa_op_t op, type_t type)
{
    passert(m_io.empty(), input_size(), output_size(), cfg_h, handle(), (int)test_flags(FLAG_PRUNED), output(0));
    m_cfg_h = cfg_h;
    m_op = op;
    m_type = type;
    m_flags = 0;
#ifndef NDEBUG
    clear_flags(FLAG_PRUNED);
#endif
}

void ssa_node_t::destroy()
{
#ifndef NDEBUG
    assert(!test_flags(FLAG_PRUNED));
    set_flags(FLAG_PRUNED);
#endif
    m_op = SSA_null;
    m_io.reset();
    assert(m_io.empty());
}

void ssa_node_t::alloc_input(unsigned size) { assert(!test_flags(FLAG_PRUNED)); m_io.alloc_input(size); }
void ssa_node_t::alloc_output(unsigned size) { assert(!test_flags(FLAG_PRUNED)); m_io.alloc_output(size); }

void ssa_node_t::build_set_input(unsigned i, ssa_value_t value)
{
    assert(i < input_size());
    assert(!input(i));

    if(value.holds_ref())
    {
        ssa_node_t& new_node = *value;
        assert(!test_flags(FLAG_PRUNED));
        assert(!new_node.test_flags(FLAG_PRUNED));
        value.set_index(new_node.output_size());
        m_io.input(i) = value;
        new_node.append_output({ handle(), i });
    }
    else
        m_io.input(i) = value;
}

unsigned ssa_node_t::append_output(ssa_bck_edge_t edge)
{
    assert(!test_flags(FLAG_PRUNED));
    assert(!edge.handle->test_flags(FLAG_PRUNED));
    unsigned const i = output_size();
    m_io.resize_output(i + 1);
    m_io.output(i) = edge;
    return i;
}

void ssa_node_t::link_append_input(ssa_value_t value)
{
    assert(!test_flags(FLAG_PRUNED));
    unsigned const i = input_size();
    m_io.resize_input(i + 1);
    if(value.holds_ref())
    {
        ssa_node_t& node = *value;
        assert(!node.test_flags(FLAG_PRUNED));
        value.set_index(node.append_output({ handle(), i }));
    }
    m_io.input(i) = value;
}

void ssa_node_t::link_append_input(ssa_value_t* begin, ssa_value_t* end)
{
    assert(!test_flags(FLAG_PRUNED));
    unsigned const dist = end - begin;
    unsigned i = input_size();

    m_io.resize_input(i + dist);

    for(ssa_value_t* it = begin; it < end; ++it)
    {
        if(it->holds_ref())
        {
            ssa_node_t& node = **it;
            assert(!node.test_flags(FLAG_PRUNED));
            it->set_index(node.append_output({ handle(), i }));
        }
        m_io.input(i) = *it;
        ++i;
    }
}

void ssa_node_t::remove_inputs_output(unsigned i)
{
    assert(i < input_size());

    ssa_fwd_edge_t input = m_io.input(i);
    if(input.holds_ref())
    {
        assert(input.handle());

        ssa_node_t& from_node = *input.handle();
        assert(!from_node.test_flags(FLAG_PRUNED));
        unsigned const from_i = input.index();

        // Remove the output edge that leads to our input on 'i'.
        from_node.m_io.last_output().input().set_index(from_i);
        std::swap(from_node.m_io.output(from_i), from_node.m_io.last_output());
        from_node.m_io.shrink_output(from_node.output_size() - 1);
        
#ifndef NDEBUG
        if(from_i < from_node.m_io.output_size())
            assert(from_node.m_io.output(from_i).input().index() == from_i);
        for(unsigned i = 0; i < from_node.output_size(); ++i)
            assert(from_node.output_edge(i).input().index() == i);
#endif
    }
}

void ssa_node_t::link_remove_input(unsigned i)
{
    link_change_input(i, m_io.last_input());
    link_change_input(input_size() - 1, ssa_value_t());
    m_io.shrink_input(input_size() - 1);
#ifndef NDEBUG
    for(unsigned j = 0; j < input_size(); ++j)
        if(ssa_bck_edge_t* edge = input_edge(j).output())
            assert(!edge || edge->index == j);
#endif
}

// Returns true if changed.
bool ssa_node_t::link_change_input(unsigned i, ssa_value_t new_value)
{
    assert(!test_flags(FLAG_PRUNED));
    assert(i < input_size());

    if(new_value == input(i))
        return false;

    // First deal with the node we're receiving input along 'i' from.
    remove_inputs_output(i);

    // Now change our input.
    if(new_value.holds_ref())
    {
        ssa_node_t& from_node = *new_value;
        assert(!from_node.test_flags(FLAG_PRUNED));

        // Add the new output entry.
        std::size_t const append_i = from_node.output_size();
        from_node.m_io.resize_output(append_i + 1);
        from_node.m_io.output(append_i) = { handle(), i };

        new_value.set_index(append_i);
    }
    m_io.input(i) = new_value;

    assert(i < input_size());
    assert(input(i) == new_value);

    return true;
}

void ssa_node_t::link_clear_inputs()
{
    link_shrink_inputs(0);
}

void ssa_node_t::link_shrink_inputs(unsigned new_size)
{
    std::size_t const size = input_size();
    assert(new_size <= size);
    for(std::size_t i = new_size; i < size; ++i)
        remove_inputs_output(i);
    m_io.shrink_input(new_size);
}

void ssa_node_t::link_swap_inputs(unsigned ai, unsigned bi)
{
    assert(!test_flags(FLAG_PRUNED));
    assert(ai < input_size());
    assert(bi < input_size());

    if(ai == bi)
        return;

    ssa_fwd_edge_t& ae = m_io.input(ai);
    ssa_fwd_edge_t& be = m_io.input(bi);

    if(ssa_bck_edge_t* ao = ae.output())
    {
        passert(ao->index == ai, ai, bi, ao->index);
        ao->index = bi;
    }

    if(ssa_bck_edge_t* bo = be.output())
    {
        assert(bo->index == bi);
        bo->index = ai;
    }

    std::swap(ae, be);
}

void ssa_node_t::replace_with(ssa_value_t value)
{
    assert(!test_flags(FLAG_PRUNED));
    unsigned const this_size = output_size();

    if(value.holds_ref())
    {
        ssa_node_t& node = *value;
        assert(!node.test_flags(FLAG_PRUNED));

        if(&node == this)
            return;

        // All of this node's outputs will get appended onto 'node's outputs.
        unsigned index = node.output_size();
        node.m_io.resize_output(this_size + index);

        for(unsigned i = 0; i < this_size; ++i)
        {
            m_io.output(i).input().set(value.handle(), index);
            node.m_io.output(index) = m_io.output(i);
            ++index;
        }
    }
    else
    {
        for(unsigned i = 0; i < this_size; ++i)
            m_io.output(i).input() = value;
    }

    m_io.clear_output();
}

unsigned ssa_node_t::replace_with(input_class_t input_class, ssa_value_t value)
{
    assert(!test_flags(FLAG_PRUNED));
    unsigned changed = 0;
    for(unsigned i = 0; i < output_size();)
    {
        auto oe = output_edge(i);
        if(oe.input_class() == input_class)
        {
            oe.handle->link_change_input(oe.index, value);
            ++changed;
        }
        else
            ++i;
    }
    return changed;
}

ssa_ht ssa_node_t::prune()
{
    return cfg_node()->prune_ssa(handle());
}

////////////////////////////////////////
// cfg_node_t                         //
////////////////////////////////////////

void cfg_node_t::create()
{
    assert(m_io.empty());
#ifndef NDEBUG
    clear_flags(FLAG_PRUNED);
#endif
    m_first_phi = {};
    m_last_daisy = {};
    m_flags = 0;
}

void cfg_node_t::destroy()
{
    assert(!ssa_begin());
    assert(!m_first_phi);
    assert(!m_last_daisy);
#ifndef NDEBUG
    set_flags(FLAG_PRUNED);
#endif
    m_io.reset();
}

void cfg_node_t::alloc_input(unsigned size) { m_io.alloc_input(size); }
void cfg_node_t::alloc_output(unsigned size) { m_io.alloc_output(size); }

// Returns the input index.
unsigned cfg_node_t::build_set_output(unsigned i, cfg_ht new_node_h)
{
    assert(i < output_size());
    assert(!output(i));

    cfg_node_t& new_node = *new_node_h;
    unsigned const input_i = new_node.input_size();
    m_io.output(i) = { new_node_h, input_i };
    new_node.append_input({ handle(), i });
    return input_i;
}

unsigned cfg_node_t::build_append_output(cfg_ht new_node_h)
{
    return link_append_output(new_node_h, [](ssa_ht){ return ssa_value_t(); });
}

unsigned cfg_node_t::append_input(cfg_fwd_edge_t edge)
{
    unsigned const i = input_size();
    m_io.resize_input(i + 1);
    m_io.input(i) = edge;
    return i;
}

void cfg_node_t::list_insert(ssa_ht it, ssa_node_t& node)
{
    assert(node.cfg_node() == handle());

    node.next = it;

    if(it)
    {
        assert(it->cfg_node() == handle());
        node.prev = it->prev;
        it->prev = node.handle();
    }
    else
    {
        node.prev = m_last_ssa;
        m_last_ssa = node.handle();
    }

    if(node.prev)
        node.prev->next = node.handle();

    if(it == m_first_ssa)
        m_first_ssa = node.handle();

    assert(node.next == it);
}

void cfg_node_t::list_insert(ssa_node_t& node)
{
    list_insert(m_first_phi, node);
    if(node.op() == SSA_phi)
        m_first_phi = node.handle();
}

ssa_ht cfg_node_t::list_erase(ssa_node_t& node)
{
    assert(node.next != node.handle());
    assert(node.prev != node.handle());

    if(node.next)
        node.next->prev = node.prev;
    else
        m_last_ssa = node.prev;

    if(node.prev)
        node.prev->next = node.next;
    else
        m_first_ssa = node.next;

    if(node.op() == SSA_phi && node.handle() == m_first_phi)
        m_first_phi = node.next;

    if(node.test_flags(FLAG_DAISY) && node.handle() == m_last_daisy)
        m_last_daisy = node.prev;
    node.clear_flags(FLAG_DAISY);

    ssa_ht ret = node.next;

    node.next = {};
    node.prev = {};

    return ret;
}

void cfg_node_t::list_insert_daisy(ssa_ht it, ssa_node_t& node)
{
    assert(node.op() != SSA_phi);
    assert(node.cfg_node() == handle());
    assert(!node.test_flags(FLAG_DAISY));

    list_erase(node);
    if(it)
    {
        assert(it->cfg_node() == handle());
        assert(it->test_flags(FLAG_DAISY));
        list_insert(it, node);
    }
    else
    {
        if(m_last_daisy)
            list_insert(m_last_daisy->next, node);
        else
            list_insert(m_first_ssa, node);
        m_last_daisy = node.handle();
    }

    node.set_flags(FLAG_DAISY);
}

void cfg_node_t::list_append_daisy(ssa_node_t& node)
{
    assert(node.op() != SSA_phi);
    assert(node.cfg_node() == handle());
    list_insert_daisy(ssa_ht{}, node);
    assert(last_daisy());
    assert(last_daisy() == node.handle());
}

void cfg_node_t::list_erase_daisy(ssa_node_t& node)
{
    assert(node.op() != SSA_phi);
    assert(node.cfg_node() == handle());
    assert(node.test_flags(FLAG_DAISY));
    list_erase(node);
    list_insert(node);
    assert(!node.test_flags(FLAG_DAISY));
}

ssa_ht cfg_node_t::emplace_ssa(ssa_op_t op, type_t type)
{
    assert(!test_flags(FLAG_PRUNED));
    passert(!is_ct(type), type);

    // Alloc and initialize it.
    ssa_ht h = ssa_pool::alloc();
    ssa_node_t& node = *h;
    node.create(handle(), op, type);

    // Add it to our list.
    list_insert(node);

    // Up the size
    ++m_ssa_size;

    return h;
}

void cfg_node_t::prune_ssa()
{
    while(ssa_ht it = ssa_begin())
        prune_ssa(it);
}

ssa_ht cfg_node_t::prune_ssa(ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;
    ssa_ht ret = ssa_node.next;

    assert(!test_flags(FLAG_PRUNED));
    assert(!ssa_node.test_flags(FLAG_PRUNED));

    assert(ssa_node.cfg_node() == handle());

    // Unlink all our shit
    ssa_node.link_clear_inputs();
    ssa_node.replace_with(ssa_ht{});

    assert(ssa_node.input_size() == 0);
    assert(ssa_node.output_size() == 0);

    // Remove it from our list.
    list_erase(ssa_node);

    assert(ssa_h->m_io.empty());

    // Free it
    ssa_node.destroy();
    assert(ssa_node.test_flags(FLAG_PRUNED));
    ssa_pool::free(ssa_h);
    --m_ssa_size;

    return ret;
}

void cfg_node_t::steal_outputs(cfg_node_t& cfg)
{
    assert(output_size() == 0);

    cfg_ht const this_handle = handle();
    unsigned const output_size = cfg.output_size();

    m_io.resize_output(output_size);
    for(unsigned i = 0; i < output_size; ++i)
    {
        m_io.output(i) = cfg.m_io.output(i);
        m_io.output(i).input().handle = this_handle;
    }
    cfg.m_io.clear_output();
}

void cfg_node_t::steal_ssa_nodes(cfg_ht cfg)
{
    if(&*cfg == this)
        return;

    while(ssa_ht it = cfg->ssa_begin())
    {
        ssa_node_t& node = *it;
        bool const in_daisy = node.in_daisy();

        assert(node.op() != SSA_phi);

        cfg->list_erase(node);
        assert(!node.in_daisy());
        node.m_cfg_h = handle();
        list_insert(node);
        if(in_daisy)
            list_append_daisy(node);
    }

    m_ssa_size += cfg->m_ssa_size;
    cfg->m_ssa_size = 0;
}

ssa_ht cfg_node_t::steal_ssa(ssa_ht ssa, bool steal_linked)
{
    ssa_ht ret = ssa.next();

    assert(ssa->op() != SSA_phi);
    cfg_node_t& old_cfg = *ssa->cfg_node();

    if(&old_cfg == this)
        return ret;

    bool const in_daisy = ssa->in_daisy();

    old_cfg.list_erase(*ssa);
    old_cfg.m_ssa_size -= 1;

    ssa->m_cfg_h = handle();
    list_insert(*ssa);
    if(in_daisy)
        list_append_daisy(*ssa);
    m_ssa_size += 1;

    if(steal_linked)
    {
        for_each_output_matching(ssa, INPUT_LINK, [&](ssa_ht linked)
        {
            // Currently unimplemented: links with daisy.
            assert(!linked->in_daisy());

            if(ret == linked)
            {
                assert(ret);
                ++ret;
            }

            steal_ssa(linked, true);
        });
    }

    assert(!ret || ret->cfg_node() == old_cfg.handle());
    return ret;
}

void cfg_node_t::link_remove_output(unsigned i)
{
    link_swap_outputs(i, output_size() - 1);
    link_shrink_outputs(output_size() - 1);
}

void cfg_node_t::link_clear_inputs()
{
    unsigned const size = input_size();
    for(std::size_t i = 0; i < size; ++i)
        remove_inputs_output(i);
    m_io.clear_input();

    // Clear phi inputs
    for(ssa_ht phi_it = phi_begin(); phi_it; ++phi_it)
        phi_it->link_clear_inputs();
}

void cfg_node_t::link_shrink_outputs(unsigned new_size)
{
    std::size_t const size = output_size();
    assert(new_size <= size);
    for(std::size_t i = new_size; i < size; ++i)
        remove_outputs_input(i);
    m_io.shrink_output(new_size);
}

void cfg_node_t::link_clear_outputs()
{
    link_shrink_outputs(0);
}

void cfg_node_t::link_swap_inputs(unsigned ai, unsigned bi)
{
    if(ai == bi)
        return;

    cfg_fwd_edge_t& ae = m_io.input(ai);
    cfg_fwd_edge_t& be = m_io.input(bi);

    for(ssa_ht phi_it = phi_begin(); phi_it; ++phi_it)
        phi_it->link_swap_inputs(ai, bi);

    std::swap(ae, be);
    std::swap(ae.output().index, be.output().index);
}

void cfg_node_t::link_swap_outputs(unsigned ai, unsigned bi)
{
    if(ai == bi)
        return;

    cfg_bck_edge_t& ae = m_io.output(ai);
    cfg_bck_edge_t& be = m_io.output(bi);

    std::swap(ae, be);
    std::swap(ae.input().index, be.input().index);
}

void cfg_node_t::remove_inputs_output(unsigned i)
{
    assert(i < input_size());
    assert(m_io.input(i).handle);

    // We'll be removing this node eventually:
    cfg_fwd_edge_t edge = m_io.input(i);
    cfg_node_t& edge_node = *edge.handle;
    unsigned const from_i = edge.index;

    assert(edge_node.output_size() > 0);

    // Remove the input edge that leads to our input on 'i'.
    edge_node.m_io.last_output().input().index = from_i;

    std::swap(edge.output(), edge_node.m_io.last_output());
    edge_node.m_io.shrink_output(edge_node.output_size() - 1);

#ifndef NDEBUG
        if(from_i < edge_node.m_io.output_size())
            assert(edge_node.m_io.output(from_i).input().index == from_i);
        for(unsigned i = 0; i < edge_node.output_size(); ++i)
            assert(edge_node.output_edge(i).input().index == i);
#endif
}

void cfg_node_t::remove_outputs_input(unsigned i)
{
    assert(i < output_size());
    assert(m_io.output(i).handle);

    // We'll be removing this node eventually:
    cfg_bck_edge_t edge = m_io.output(i);
    cfg_node_t& edge_node = *edge.handle;
    unsigned const from_i = edge.index;

    assert(edge_node.input_size() > 0);

    // Remove the output edge that leads to our input on 'i'.
    edge_node.m_io.last_input().output().index = from_i;
    edge.input().handle = {};

    // Update all phi nodes
    for(ssa_ht phi_it = edge_node.phi_begin(); phi_it; ++phi_it)
    {
        assert(phi_it->op() == SSA_phi);
        assert(phi_it->cfg_node() == edge.handle);
        assert(phi_it->input_size() == edge_node.input_size());
        phi_it->link_remove_input(edge.index);
    }

    std::swap(edge.input(), edge_node.m_io.last_input());
    edge_node.m_io.shrink_input(edge_node.input_size() - 1);

#ifndef NDEBUG
        if(from_i < edge_node.m_io.input_size())
            assert(edge_node.m_io.input(from_i).output().index == from_i);
        for(unsigned i = 0; i < edge_node.input_size(); ++i)
            assert(edge_node.input_edge(i).output().index == i);
#endif
}

////////////////////////////////////////
// ir_t                               //
////////////////////////////////////////

ir_t::ir_t()
{
    assert(cfg_pool::size() == 0);
    assert(ssa_pool::size() == 0);
}

ir_t::~ir_t()
{
    cfg_pool::clear();
    ssa_pool::clear();
}

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

    // Bump the size
    ++m_size;

    return h;
}

cfg_ht ir_t::prune_cfg(cfg_ht cfg_node)
{
    cfg_node->prune_ssa();
    assert(cfg_node->ssa_size() == 0);

    cfg_node->link_clear_inputs();
    cfg_node->link_clear_outputs();

    assert(cfg_node->input_size() == 0);
    assert(cfg_node->output_size() == 0);

    cfg_ht ret = cfg_node->next;

    // Remove it from our list.
    if(cfg_node->prev)
        cfg_node->prev->next = cfg_node->next;
    else
    {
        assert(cfg_node == m_cfg_begin);
        m_cfg_begin = cfg_node->next;
    }
    if(cfg_node->next)
        cfg_node->next->prev = cfg_node->prev;

    // Dealt with root handle.
    if(cfg_node == root)
        root = {};

    // Free it
    cfg_node->destroy();
    cfg_pool::free(cfg_node);
    --m_size;

    return ret;
}

cfg_ht ir_t::split_edge(cfg_bck_edge_t edge)
{
    cfg_ht split_h = emplace_cfg();
    cfg_node_t& split = *split_h;

    edge.input().output() = { split_h, 0 };

    split.alloc_input(1);
    split.m_io.input(0) = edge.input();

    split.alloc_output(1);
    split.m_io.output(0) = edge;

    edge.input() = { split_h, 0 };

    return split_h;
}

cfg_ht ir_t::merge_edge(cfg_ht cfg_h)
{
    cfg_node_t& cfg_node = *cfg_h;
    assert(cfg_node.input_size() == 1);
    assert(cfg_node.output_size() == 1);

    cfg_node.input_edge(0).output() = cfg_node.output_edge(0);
    cfg_node.output_edge(0).input() = cfg_node.input_edge(0);

    cfg_node.m_io.clear_input();
    cfg_node.m_io.clear_output();

    return prune_cfg(cfg_h);
}

std::size_t ir_t::ssa_size() const
{
    std::size_t size = 0;
    for(auto const& cfg : *this)
        size += cfg.ssa_size();
    return size;
}

#ifndef NDEBUG
void ir_t::assert_valid(bool cg) const
{
    assert(root);

    if(!compiler_options().assert_valid)
        return;

    for(cfg_ht cfg_it = cfg_begin(); cfg_it; ++cfg_it)
    { 
        cfg_node_t& cfg_node = *cfg_it;

        passert(cfg_node.input_size() <= MAX_CFG_INPUT, cfg_node.input_size());
        passert(cfg_node.output_size() <= MAX_CFG_OUTPUT, cfg_node.output_size());

        for(unsigned i = 0; i < cfg_node.input_size(); ++i)
        {
            assert((bool)cfg_node.input_edge(i).handle);
            assert((cfg_node.input_edge(i).output().input().edges_eq(
                cfg_node.input_edge(i))));
        }

        for(unsigned i = 0; i < cfg_node.output_size(); ++i)
        {
            assert((bool)cfg_node.output_edge(i).handle);
            assert((cfg_node.output_edge(i).input().output().edges_eq(
                cfg_node.output_edge(i))));
        }

        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;

            assert(!is_banked_ptr(ssa_it->type().name()));

            assert(ssa_node.cfg_node() == cfg_it);
            if(ssa_node.op() == SSA_phi)
                assert(ssa_node.input_size() == cfg_node.input_size());

            for(unsigned i = 0; i < ssa_node.input_size(); ++i)
            {
                if((ssa_flags(ssa_node.op()) & SSAF_NULL_INPUT_VALID) && !ssa_node.input(i))
                    continue;
                passert(ssa_node.input(i), ssa_node.op(), ssa_it, i, ssa_node.input_size());
                if(!ssa_node.input(i).holds_ref())
                    continue;
                assert(ssa_node.input_edge(i).output()->input().handle()
                       == ssa_node.input_edge(i).handle());
            }

            for(unsigned i = 0; i < ssa_node.output_size(); ++i)
            {
                assert((bool)ssa_node.output_edge(i).handle);
                assert(ssa_node.output_edge(i).input().output()->edges_eq(
                    ssa_node.output_edge(i)));
            }

            for(ssa_ht daisy = ssa_node.prev_daisy(); daisy; --daisy)
                assert(daisy->cfg_node() == cfg_it);
            for(ssa_ht daisy = ssa_node.next_daisy(); daisy; ++daisy)
                assert(daisy->cfg_node() == cfg_it);

            if(ssa_flags(ssa_it->op()) & SSAF_CONDITIONAL)
                assert(ssa_it == cfg_node.last_daisy());

            if(ssa_it->op() == SSA_if)
            {
                assert(ssa_it == cfg_node.last_daisy());
                assert(ssa_it->cfg_node() == cfg_node.handle());
                passert(cfg_node.output_size() == 2, cfg_node.output_size(), cfg_node.output_size(), ssa_it->cfg_node(), ssa_it);
            }

            if(ssa_it->in_daisy())
            {
                // Currently unimplemented: links with daisy.
                assert(ssa_input0_class(ssa_it->op()) != INPUT_LINK);
            }

            // phi checks
            if(ssa_it->op() == SSA_phi)
            {
                /* TODO
                for(unsigned i = 0; i < ssa_it->input_size(); ++i)
                    passert(ssa_it->input(i).type() == ssa_it->type(), ssa_it->input(i).type(), ssa_it->type());
                    */
            }

            // Multi checks
            if(ssa_it->op() == SSA_multi_eq)
            {
                assert(ssa_it->input_size() % 2 == 0);
                for(unsigned i = 0; i < ssa_it->input_size(); i += 2)
                {
                    if(!cg && i + 2 != ssa_it->input_size())
                        passert(ssa_it->input(i).type() == ssa_it->input(i+1).type(),
                                ssa_it->input(i).type(), ssa_it->input(i+1).type(),
                                ssa_it->input(i), ssa_it->input(i+1));
                }
            }

            // Add / sub checks
#if 0
            switch(ssa_it->op())
            {
            case SSA_add:
            case SSA_sub:
            case SSA_or:
            case SSA_and:
            case SSA_xor:
                if(!cg)
                {
                    passert(ssa_it->input(0).type() == ssa_it->type(), ssa_it->op(), ssa_it->input(0).type(), ssa_it->type(), ssa_it->input(0)->op());
                    passert(ssa_it->input(1).type() == ssa_it->type(), ssa_it->op(), ssa_it->input(1).type(), ssa_it->type(), ssa_it->input(1)->op());
                }
            default:
                break;
            }
#endif

            // Array checks
            if(ssa_flags(ssa_it->op()) & SSAF_INDEXES_ARRAY8)
            {
                using namespace ssai::array;
                assert(ssa_it->input(OFFSET).type() == TYPE_U20);
            }

            // Mul checks
            if(ssa_it->op() == SSA_mul8_hi)
            {
                assert(ssa_it->input(0).holds_ref());
                assert(ssa_it->input(0)->op() == SSA_mul8_lo);
            }

            // Cast Checks
            if(ssa_it->op() == SSA_cast)
            {
                assert(ssa_it->type() != TYPE_BOOL);
            }

            // Early Store Checks
            if(ssa_it->op() == SSA_early_store)
            {
                passert(ssa_it->output_size() <= 1, ssa_it->op(), ssa_it->output_size());
            }
        }
    }
}
#endif

////////////////////////////////////////
// Utility functions                  //
////////////////////////////////////////

ssa_ht split_output_edge(ssa_ht ssa_node, bool this_cfg, unsigned output_i, ssa_op_t op)
{
    assert(!ssa_node->test_flags(FLAG_PRUNED));

    // Create a copy and set its input to this.
    cfg_ht const cfg = this_cfg ? ssa_node->cfg_node() : ssa_node->output(output_i)->cfg_node();
    ssa_ht const copy = cfg->emplace_ssa(op, ssa_node->type());

    copy->alloc_input(1); 
    copy->m_io.input(0) = ssa_fwd_edge_t(ssa_node, output_i);

    // Create this reference after emplace_ssa.
    ssa_bck_edge_t& oe = ssa_node->m_io.output(output_i);

    // Update our (original) output's input to be copy,
    // and also update copy's output.
    oe.input() = ssa_fwd_edge_t(copy, copy->append_output(oe));

    // Finally, update our own output:
    oe = { copy, 0 };

    passert(copy->type() == copy->input(0).type(), copy->type(), copy->input(0).type());
    return copy;
}

callable_t const* get_callable(ssa_node_t const& node, bool allow_goto)
{
    if(!is_fn_call(node.op(), allow_goto))
        return nullptr;
    if(fn_ht fn = get_fn(node))
        return &*fn;
    if(fn_set_ht fn_set = get_fn_set(node))
        return &*fn_set;
    return nullptr;
}

template class node_io_buffers_t<ssa_fwd_edge_t, ssa_bck_edge_t, 3, 1>;
template class node_io_buffers_t<cfg_fwd_edge_t, cfg_bck_edge_t, 3, 2>;

