#include "ir.hpp"

#include "builtin.hpp"

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
        assert(builtin::popcount((unsigned)capacity) == 1);
        assert(capacity >= new_size);
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
    assert(m_input_capacity == 0);
    sbo_alloc(m_input, m_input_size, m_input_capacity, m_input_sbo, size);
    assert(input_size() == size);
}

template<typename I, typename O, std::size_t ISize, std::size_t OSize>
void node_io_buffers_t<I, O, ISize, OSize>::alloc_output(unsigned size)
{
    assert(m_output_capacity == 0);
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
    assert(m_io.empty());
    m_cfg_h = cfg_h;
    m_op = op;
    m_type = type;
    flags = 0;
}

void ssa_node_t::destroy()
{
    m_io.reset();
}

void ssa_node_t::alloc_input(unsigned size) { m_io.alloc_input(size); }
void ssa_node_t::alloc_output(unsigned size) { m_io.alloc_output(size); }

void ssa_node_t::build_set_input(unsigned i, ssa_value_t value)
{
    assert(i < input_size());
    assert(!input(i));

    if(value.is_handle())
    {
        ssa_node_t& new_node = *value;
        value.set_index(new_node.output_size());
        m_io.input(i) = value;
        new_node.append_output({ handle(), i });
    }
    else
        m_io.input(i) = value;
}

unsigned ssa_node_t::append_output(ssa_bck_edge_t edge)
{
    unsigned const i = output_size();
    m_io.resize_output(i + 1);
    m_io.output(i) = edge;
    return i;
}

void ssa_node_t::link_append_input(ssa_value_t value)
{
    unsigned const i = input_size();
    if(value.is_handle())
    {
        ssa_node_t& node = *value;
        value.set_index(node.append_output({ handle(), i }));
    }
    m_io.resize_input(i + 1);
    m_io.input(i) = value;
}

void ssa_node_t::remove_inputs_output(unsigned i)
{
    assert(i < input_size());

    ssa_fwd_edge_t input = m_io.input(i);
    if(input.is_handle())
    {
        assert(input.handle());

        ssa_node_t& from_node = *input.handle();
        unsigned const from_i = input.index();

        // Remove the output edge that leads to our input on 'i'.
        from_node.m_io.last_output().input().set_index(from_i);
        std::swap(from_node.m_io.output(from_i), from_node.m_io.last_output());
        from_node.m_io.shrink_output(from_node.output_size() - 1);
        
#ifndef NDEBUG
        for(unsigned i = 0; i < from_node.output_size(); ++i)
            assert(from_node.output_edge(i).input().index() == i);
#endif
    }
}

void ssa_node_t::link_remove_input(unsigned i)
{
    assert(i < input_size());

    // The last input will move to position 'i'.
    // We have to adjust the edge's index too.
    // Do that first, before calling 'remove_inputs_output'.
    if(ssa_bck_edge_t* o = m_io.last_input().output())
        o->index = i;

    // Deal with the node we're receiving input along 'i' from.
    remove_inputs_output(i);

    // Remove the input edge on 'i'.
    std::swap(m_io.input(i), m_io.last_input());
    m_io.shrink_input(input_size() - 1);

#ifndef NDEBUG
    for(unsigned i = 0; i < input_size(); ++i)
        if(ssa_bck_edge_t* edge = input_edge(i).output())
            assert(edge->index == i);
#endif
}

// Returns true if changed.
bool ssa_node_t::link_change_input(unsigned i, ssa_value_t new_value)
{
    assert(i < input_size());

    if(new_value.target() == input(i).target())
        return false;

    // First deal with the node we're receiving input along 'i' from.
    remove_inputs_output(i);

    // Now change our input.
    if(new_value.is_handle())
    {
        assert(new_value.handle());
        ssa_node_t& from_node = *new_value;
        new_value.set_index(from_node.output_size());

        // Add the new output entry.
        std::size_t const append_i = from_node.output_size();
        from_node.m_io.resize_output(append_i + 1);
        from_node.m_io.output(append_i) = { handle(), i };
    }
    m_io.input(i) = new_value;
    return true;
}

void ssa_node_t::link_clear_inputs()
{
    std::size_t const size = input_size();
    for(std::size_t i = 0; i < size; ++i)
        remove_inputs_output(i);
    m_io.clear_input();
}

void ssa_node_t::replace_with_const(fixed_t const_val)
{
    std::size_t const size = output_size();
    for(unsigned i = 0; i < size; ++i)
        m_io.output(i).input() = const_val;
    m_io.clear_output();
}

void ssa_node_t::replace_with(ssa_value_t value)
{
    if(value.is_const())
        return replace_with_const(value.fixed());

    assert(value.handle());
    ssa_node_t& node = *value;

    if(&node == this)
        return;

    // All of this node's outputs will get appended onto 'node's outputs.
    unsigned index = node.output_size();
    unsigned const this_size = output_size();
    node.m_io.resize_output(this_size + index);

    for(unsigned i = 0; i < this_size; ++i)
    {
        m_io.output(i).input().set(value.handle(), index);
        node.m_io.output(index) = m_io.output(i);
        ++index;
    }

    m_io.clear_output();
}

ssa_ht ssa_node_t::unsafe_prune()
{
    return cfg_node()->unsafe_prune_ssa(handle());
}

////////////////////////////////////////
// cfg_node_t                         //
////////////////////////////////////////

void cfg_node_t::create()
{
    assert(m_io.empty());
    exit = {};
    flags = 0;
}

void cfg_node_t::destroy()
{
    assert(!ssa_begin());
    assert(!exit);
    m_io.reset();
}

void cfg_node_t::alloc_input(unsigned size) { m_io.alloc_input(size); }
void cfg_node_t::alloc_output(unsigned size) { m_io.alloc_output(size); }

void cfg_node_t::build_set_output(unsigned i, cfg_ht new_node_h)
{
    assert(i < output_size());
    assert(!output(i));

    cfg_node_t& new_node = *new_node_h;
    m_io.output(i) = { new_node_h, new_node.input_size() };
    new_node.append_input({ handle(), i });
}

unsigned cfg_node_t::append_input(cfg_fwd_edge_t edge)
{
    unsigned const i = input_size();
    m_io.resize_input(i + 1);
    m_io.input(i) = edge;
    return i;
}

ssa_ht cfg_node_t::emplace_ssa(ssa_op_t op, type_t type)
{
    // Alloc and initialize it.
    ssa_ht h = ssa_pool::alloc();
    ssa_node_t& node = *h;
    node.create(handle(), op, type);

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

void cfg_node_t::unsafe_prune_ssa()
{
    for(ssa_ht it = ssa_begin(); it; ++it)
    {
        it->link_clear_inputs();
        it->replace_with_const({});
    }

    while(ssa_ht it = ssa_begin())
        unsafe_prune_ssa(it);
}

ssa_ht cfg_node_t::unsafe_prune_ssa(ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;
    ssa_ht ret = ssa_node.next;

    assert(ssa_node.cfg_node() == handle());
    assert(ssa_node.input_size() == 0);

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

    // Dealt with exit handle.
    if(ssa_h == exit)
        exit = {};

    // Free it
    ssa_node.destroy();
    ssa_pool::free(ssa_h);

    return ret;
}

void cfg_node_t::link_remove_output(unsigned i)
{
    assert(i < output_size());

    // The back of 'output_vec' will move to position 'i'.
    // We have to adjust the edge's index too.
    assert(m_io.last_output().handle);
    assert(m_io.last_output().input().index == output_size() - 1);
    m_io.last_output().input().index = i;

    // Deal with the node we're passing outputs along 'i' from.
    remove_outputs_input(i);

    // Remove the output.
    std::swap(m_io.output(i), m_io.last_output());
    m_io.shrink_output(output_size() - 1);
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

void cfg_node_t::link_clear_outputs()
{
    unsigned const size = output_size();
    for(std::size_t i = 0; i < size; ++i)
        remove_outputs_input(i);
    m_io.clear_output();
}

void cfg_node_t::remove_inputs_output(unsigned i)
{
    assert(i < input_size());
    assert(m_io.input(i).handle);

    // We'll be removing this node eventually:
    cfg_fwd_edge_t edge = m_io.input(i);
    cfg_node_t& edge_node = *edge.handle;

    assert(edge_node.output_size() > 0);

    // Remove the input edge that leads to our input on 'i'.
    edge_node.m_io.last_output().input().index = edge.index;

    std::swap(edge.output(), edge_node.m_io.last_output());
    edge_node.m_io.shrink_output(edge_node.output_size() - 1);
}

void cfg_node_t::remove_outputs_input(unsigned i)
{
    assert(i < output_size());
    assert(m_io.output(i).handle);

    // We'll be removing this node eventually:
    cfg_bck_edge_t edge = m_io.output(i);
    cfg_node_t& edge_node = *edge.handle;

    assert(edge_node.input_size() > 0);

    // Remove the output edge that leads to our input on 'i'.
    edge_node.m_io.last_input().output().index = edge.index;

    // Update all phi nodes
    for(ssa_ht phi_it = edge_node.phi_begin(); phi_it; ++phi_it)
    {
        std::puts("updating phi");
        assert(phi_it->op() == SSA_phi);
        assert(phi_it->cfg_node() == edge.handle);
        std::printf("%i %i\n", input_size(), phi_it->input_size());
        assert(phi_it->input_size() == edge_node.input_size());
        phi_it->link_remove_input(edge.index);
    }

    std::swap(edge.input(), edge_node.m_io.last_input());
    edge_node.m_io.shrink_input(edge_node.input_size() - 1);
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

cfg_ht ir_t::unsafe_prune_cfg(cfg_ht cfg_h)
{
    std::printf("pruning cfg %i\n", cfg_h.index);
    assert(cfg_h != root);
    assert(cfg_h != exit);

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

    // Dealt with root/exit handle.
    if(cfg_h == root)
        root = {};
    if(cfg_h == exit)
        exit = {};

    // Free it
    cfg_node.destroy();
    cfg_pool::free(cfg_h);

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

    return unsafe_prune_cfg(cfg_h);
}

bool ir_t::valid() const
{
#ifdef NDEBUG
    return true;
#else
    bool valid = true;
    for(cfg_ht cfg_it = cfg_begin(); cfg_it; ++cfg_it)
    { 
        cfg_node_t& cfg_node = *cfg_it;

        for(unsigned i = 0; i < cfg_node.input_size(); ++i)
        {
            valid &= (bool)cfg_node.input_edge(i).handle;
            valid &= (cfg_node.input_edge(i).output().input()
                      == cfg_node.input_edge(i));
        }

        for(unsigned i = 0; i < cfg_node.output_size(); ++i)
        {
            valid &= (bool)cfg_node.output_edge(i).handle;
            valid &= (cfg_node.output_edge(i).input().output()
                      == cfg_node.output_edge(i));
        }

        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;

            valid &= ssa_node.cfg_node() == cfg_it;
            if(ssa_node.op() == SSA_phi)
                valid &= ssa_node.input_size() == cfg_node.input_size();

            for(unsigned i = 0; i < ssa_node.input_size(); ++i)
            {
                if(!ssa_node.input(i).is_handle())
                    continue;
                valid &= (ssa_node.input_edge(i).output()->input().handle()
                          == ssa_node.input_edge(i).handle());
            }

            for(unsigned i = 0; i < ssa_node.output_size(); ++i)
            {
                valid &= (bool)ssa_node.output_edge(i).handle;
                valid &= (*ssa_node.output_edge(i).input().output()
                          == ssa_node.output_edge(i));
            }
        }
    }
    return valid;
#endif
}
