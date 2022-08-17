#ifndef PBQP_HPP
#define PBQP_HPP

// Solver for Partial Boolean Quadratic Problem (PBQP)
//
// References:
//  https://pp.ipd.kit.edu/uploads/publikationen/mbaumstark19bachelorarbeit.pdf
//  https://pp.ipd.kit.edu/uploads/publikationen/buchwald11cc.pdf
//  Software and Compilers for Embedded Systems: 7th International Workshop

#include <cassert>
#include <cstdint>
#include <array>
#include <algorithm>
#include <vector>
#include <deque>

#include "debug_print.hpp"

using pbqp_cost_t = std::uint64_t;

struct pbqp_edge_t;
class pbqp_node_t;
class pbqp_t;

class pbqp_node_t
{
friend class pbqp_t;
private:
    std::uint16_t degree = 0;
public:
    std::int16_t sel = -1;

    unsigned num_sels() const { return cost_vector.size(); }
    std::vector<pbqp_cost_t> cost_vector;

    pbqp_node_t() = default;
    pbqp_node_t(pbqp_node_t const&) = delete;
    pbqp_node_t(pbqp_node_t&&) = default;
    pbqp_node_t& operator=(pbqp_node_t const&) = delete;
    pbqp_node_t& operator=(pbqp_node_t&&) = delete;
private:
    std::vector<pbqp_edge_t*> edges;
    std::vector<unsigned> bp_proof; // 'sel' can be determined from this during backpropagation.

    void dec_degree(pbqp_edge_t* edge)
    {
        assert(degree > 0);
        --degree;
        auto it = std::find(edges.begin(), edges.begin() + degree, edge);
        assert(*it == edge);
        std::swap(*it, edges[degree]);
    }
};

struct pbqp_edge_t
{
    static constexpr unsigned FROM = 0;
    static constexpr unsigned TO = 1;

    bool index(pbqp_node_t& node)
    {
        assert(nodes[0] == &node || nodes[1] == &node);
        return nodes[1] == &node;
    }

    pbqp_cost_t& cost(unsigned from_sel, unsigned to_sel, bool node_i = false)
    {
        if(node_i)
            std::swap(from_sel, to_sel);

        assert(from_sel < nodes[FROM]->num_sels());
        assert(to_sel < nodes[TO]->num_sels());
            
        unsigned const index = from_sel + (to_sel * nodes[FROM]->num_sels());
        assert(index < cost_matrix.size());

        return cost_matrix[index];
    }

    bool eq(pbqp_node_t const& from, pbqp_node_t const& to) const
        { return nodes[FROM] == &from && nodes[TO] == &to; }
    bool eq_flipped(pbqp_node_t const& from, pbqp_node_t const& to) const
        { return nodes[FROM] == &to && nodes[TO] == &from; }

    std::array<pbqp_node_t*, 2> nodes; // from, to
    std::vector<pbqp_cost_t> cost_matrix;
};

class pbqp_t
{
public:
    explicit pbqp_t(log_t* log) : log(log) {}

    void add_edge(pbqp_node_t& from, pbqp_node_t& to, std::vector<pbqp_cost_t> cost_matrix);
    void solve(std::vector<pbqp_node_t*> order);

private:
    void reduce(pbqp_node_t& node);
    bool optimal_reduction(pbqp_node_t& node);
    void heuristic_reduction(pbqp_node_t& node);

    std::deque<pbqp_edge_t> edge_pool;
    std::vector<pbqp_node_t*> bp_stack; // back propagation stack
    log_t* log;
};

#endif
