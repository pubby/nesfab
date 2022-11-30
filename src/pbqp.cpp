#include "pbqp.hpp"

#include "debug_print.hpp"

template<typename Fn> [[gnu::flatten]]
void handle_cases(bool node_a, Fn const& fn)
{
    if(node_a)
        fn(true);
    else
        fn(false);
}

template<typename Fn> [[gnu::flatten]]
void handle_cases(bool node_a, bool node_b, Fn const& fn)
{
    if(node_a)
    {
        if(node_b)
            fn(true, true);
        else
            fn(true, false);
    }
    else
    {
        if(node_b)
            fn(false, true);
        else
            fn(false, false);
    }
}

void pbqp_t::solve(std::vector<pbqp_node_t*> order)
{
    if(order.empty())
        return;

#ifndef NDEBUG
    for(auto* node : order)
        assert(node->num_sels() > 0);
#endif

    std::vector<pbqp_node_t*> next_order;
    next_order.reserve(order.size());

    // Continuously reduce nodes until no remain.
    while(true)
    {
        unsigned optimal_reductions = 0;

        next_order.clear();
        for(pbqp_node_t* node : order)
        {
            if(!node)
                continue;

            if(optimal_reduction(*node))
                optimal_reductions += 1;
            else
                next_order.push_back(node);
        }

        if(next_order.empty())
            break;

        std::swap(order, next_order);

        assert(!order.empty());

        // If nothing was reduced optimally, reduce 1 node suboptimally.
        if(optimal_reductions == 0)
        {
            unsigned best_i = 0;

            // We'll reduce the node with the highest degree.
            // Find it here:
            for(unsigned i = 1; i < order.size(); ++i)
                if(order[i]->degree > order[best_i]->degree)
                    best_i = i;

            heuristic_reduction(*order[best_i]);
            order[best_i] = nullptr;
        }
        else
            passert(order.size() < next_order.size(), order.size(), next_order.size());
    }

    // Back-propagate
    while(!bp_stack.empty())
    {
        pbqp_node_t& node = *bp_stack.back();
        bp_stack.pop_back();

        if(node.degree == 1)
        {
            pbqp_edge_t* edge = node.edges[0];
            pbqp_node_t& other = *edge->nodes[!edge->index(node)];

            assert(other.sel >= 0);
            assert(other.sel < int(node.bp_proof.size()));

            node.sel = node.bp_proof[other.sel];
        }
        else
        {
            passert(node.degree == 2, node.degree);

            pbqp_edge_t* edge_a = node.edges[0];
            pbqp_edge_t* edge_b = node.edges[1];
            pbqp_node_t& other_a = *edge_a->nodes[!edge_a->index(node)];
            pbqp_node_t& other_b = *edge_b->nodes[!edge_b->index(node)];

            passert(other_a.sel >= 0, other_a.sel, other_b.sel, other_a.degree, other_b.degree);
            passert(other_b.sel >= 0, other_b.sel, other_a.sel, other_b.degree, other_a.degree);

            unsigned const index = other_a.sel + (other_b.sel * other_a.num_sels());
            passert(index < node.bp_proof.size(), index, node.bp_proof.size());
            node.sel = node.bp_proof[index];
        }
    }
}

void pbqp_t::add_edge(pbqp_node_t& from, pbqp_node_t& to, std::vector<pbqp_cost_t> cost_matrix)
{
    assert(from.degree >= 0 && to.degree >= 0);
    assert(cost_matrix.size() == from.num_sels() * to.num_sels());

    // Handle loops:
    if(&from == &to)
    {
        for(unsigned i = 0; i < from.num_sels(); ++i)
            from.cost_vector[i] += cost_matrix[i + i * from.num_sels()];
        return;
    }

    // Handle duplicate edges:
    for(int i = 0; i < from.degree; ++i)
    {
        auto& prev_matrix = from.edges[i]->cost_matrix;

        if(from.edges[i]->eq(from, to))
        {
            assert(prev_matrix.size() == cost_matrix.size());
            for(unsigned j = 0; j < cost_matrix.size(); ++j)
                prev_matrix[j] += cost_matrix[j];
            return;
        }
        else if(from.edges[i]->eq_flipped(from, to))
        {
            assert(prev_matrix.size() == cost_matrix.size());

            // Transpose the cost matrix while adding:
            for(unsigned x = 0; x < from.num_sels(); ++x)
            for(unsigned y = 0; y < to.num_sels(); ++y)
                prev_matrix[y + x * to.num_sels()] += cost_matrix[x + y * from.num_sels()];
            return;
        }
    }

    // Otherwise, create the edge:
    auto& edge = edge_pool.emplace_back(pbqp_edge_t{ { &from, &to }, std::move(cost_matrix) });

    from.edges.push_back(&edge);
    std::swap(from.edges.back(), from.edges[from.degree++]);

    to.edges.push_back(&edge);
    std::swap(to.edges.back(), to.edges[to.degree++]);
}

bool pbqp_t::optimal_reduction(pbqp_node_t& node)
{
    if(node.degree == 0) // R0
    {
        dprint(log, "-PBQP R0", &node);
        pbqp_cost_t min_cost = ~0ull;
        for(unsigned i = 0; i < node.num_sels(); ++i)
        {
            if(node.cost_vector[i] < min_cost)
            {
                min_cost = node.cost_vector[i];
                node.sel = i;
            }
        }

        return true;
    }
    else if(node.degree == 1) // R1
    {
        dprint(log, "-PBQP R1", &node);
        pbqp_edge_t* edge = node.edges[0];
        bool const node_i = edge->index(node);
        pbqp_node_t& other = *edge->nodes[!node_i];

        node.bp_proof.resize(other.num_sels());

        handle_cases(node_i, [&](bool node_i) __attribute__((always_inline))
        {
            for(unsigned j = 0; j < other.num_sels(); ++j)
            {
                pbqp_cost_t min_cost = ~0ull;
                auto& bp_proof = node.bp_proof[j];

                for(unsigned i = 0; i < node.num_sels(); ++i)
                {
                    pbqp_cost_t const cost = (edge->cost(i, j, node_i)
                        + node.cost_vector[i] 
                        + other.cost_vector[j]);

                    if(cost < min_cost)
                    {
                        min_cost = cost;
                        bp_proof = i;
                    }
                }

                other.cost_vector[j] = min_cost;
            }
        });

        bp_stack.push_back(&node);
        other.dec_degree(edge);
        assert(node.degree == 1);
        return true;
    }
    else if(node.degree == 2) // R2
    {
        dprint(log, "-PBQP R2", &node);
        pbqp_edge_t* edge_a = node.edges[0];
        pbqp_edge_t* edge_b = node.edges[1];
        assert(edge_a != edge_b);

        bool const node_a = edge_a->index(node);
        bool const node_b = edge_b->index(node);

        pbqp_node_t& other_a = *edge_a->nodes[!node_a];
        pbqp_node_t& other_b = *edge_b->nodes[!node_b];

        assert(&node != &other_a);
        assert(&node != &other_b);

        unsigned const matrix_size = other_a.num_sels() * other_b.num_sels();
        node.bp_proof.resize(matrix_size);
        std::vector<pbqp_cost_t> new_matrix(matrix_size);

        handle_cases(node_a, node_b, [&](bool node_a, bool node_b) __attribute__((always_inline))
        {
            for(unsigned a = 0 ; a < other_a.num_sels(); ++a)
            for(unsigned b = 0 ; b < other_b.num_sels(); ++b)
            {
                pbqp_cost_t min_cost = ~0ull;
                auto& bp_proof = node.bp_proof[a + (other_a.num_sels() * b)];

                for(unsigned i = 0; i < node.num_sels(); ++i)
                {
                    pbqp_cost_t const cost = (node.cost_vector[i] 
                        + edge_a->cost(i, a, node_a) 
                        + edge_b->cost(i, b, node_b));

                    if(cost < min_cost) [[unlikely]]
                    {
                        min_cost = cost;
                        bp_proof = i;
                        node.bp_proof[a + (other_a.num_sels() * b)] = i;
                    }
                }

                new_matrix[a + (other_a.num_sels() * b)] = min_cost;
            }
        });

        bp_stack.push_back(&node);
        other_a.dec_degree(edge_a);
        other_b.dec_degree(edge_b);
        add_edge(other_a, other_b, std::move(new_matrix));

        assert(node.degree == 2);

        return true;
    }

    return false;
}

void pbqp_t::heuristic_reduction(pbqp_node_t& node)
{
    assert(node.degree > 2);
    dprint(log, "-PBQP RN", &node);

    pbqp_cost_t min_i_cost = ~0ull;
    unsigned best_i = ~0u;

    for(unsigned i = 0; i < node.num_sels(); ++i)
    {
        unsigned i_cost = node.cost_vector[i];

        for(unsigned n = 0; n < node.degree; ++n)
        {
            pbqp_edge_t* edge = node.edges[n];
            bool const node_i = edge->index(node);
            pbqp_node_t& other = *edge->nodes[!node_i];

            pbqp_cost_t min_cost = ~0ull;

            handle_cases(node_i, [&](bool node_i) __attribute__((always_inline))
            {
                for(unsigned j = 0; j < other.num_sels(); ++j)
                {
                    pbqp_cost_t const cost = other.cost_vector[j] + edge->cost(i, j, node_i);
                    if(cost < min_cost)
                        min_cost = cost;
                }
            });

            i_cost += min_cost;
        }

        if(i_cost < min_i_cost)
        {
            min_i_cost = i_cost;
            best_i = i;
        }
    }

    node.sel = best_i;
    assert(node.sel >= 0);
}

