// A quick-n-dirty lexer generator; an alternative to 'lex' and 'flex'.

#include <ctype.h>
#include <cstdio>
#include <deque>
#include <map>
#include <memory>
#include <numeric>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "flat/flat_map.hpp"
#include "flat/flat_multimap.hpp"
#include "flat/flat_set.hpp"

constexpr unsigned ACCEPT = 256;
constexpr unsigned EMPTY = 257;
constexpr unsigned CONCAT = 258;
constexpr unsigned UNION = 259;
constexpr unsigned KLEENE = 260;

struct regex_t
{
    unsigned type;
    std::unique_ptr<regex_t> l;
    std::unique_ptr<regex_t> r;
    char const* name = nullptr;
    char const* string = nullptr;
    int precedence = 0;
};

struct nfa_edge_t { unsigned chr; struct nfa_node_t* ptr; };

struct nfa_node_t 
{
    std::vector<nfa_edge_t> edges;
    char const* name = nullptr;
    char const* string = nullptr;
    int precedence = 0;
    unsigned prio = 0;
};

struct nfa_t
{
    nfa_node_t* first;
    nfa_node_t* last;
};

using rptr = std::unique_ptr<regex_t>;

rptr clone(rptr const& a) 
{
    if(!a)
        return nullptr;
    return rptr(new regex_t{ a->type, clone(a->l), clone(a->r), a->name, a->string, a->precedence  });
}

template<typename F>
rptr pred(F f)
{
    rptr base;
    for(unsigned char c = 0; c < 128; ++c)
    {
        if(!f(c)) 
            continue;
        if(!base)
            base.reset(new regex_t{c});
        else
            base.reset(new regex_t{ UNION, rptr(new regex_t{c}), std::move(base) });
    }
    return base;
}

rptr word(std::string word)
{
    rptr base;
    while(word.size())
    {
        unsigned c = word.back();
        if(!base)
            base.reset(new regex_t{c});
        else
            base.reset(new regex_t{ CONCAT, rptr(new regex_t{c}), std::move(base) });
        word.pop_back();
    }
    return base;
}

template<typename... T>
rptr cat(rptr a, rptr b, rptr c, T... t) 
    { return rptr(new regex_t{ CONCAT, std::move(a), cat(std::move(b), std::move(c), std::move(t)...) }); }
rptr cat(rptr a, rptr b) 
    { return rptr(new regex_t{ CONCAT, std::move(a), std::move(b) }); }
template<typename... T>
rptr uor(rptr a, rptr b, rptr c, T... t) 
    { return rptr(new regex_t{ UNION, std::move(a), uor(std::move(b), std::move(c), std::move(t)...) }); }
rptr uor(rptr a, rptr b) 
    { return rptr(new regex_t{ UNION, std::move(a), std::move(b) }); }
rptr accept(char const* name, char const* str, rptr a, int precedence = 0) 
    { return cat(std::move(a), rptr(new regex_t{ ACCEPT, nullptr, nullptr, name, str, precedence })); }
rptr accept(int precedence, char const* name, char const* str, rptr a) 
    { return cat(std::move(a), rptr(new regex_t{ ACCEPT, nullptr, nullptr, name, str, precedence })); }
rptr kleene(rptr a) 
    { return rptr(new regex_t{ KLEENE, std::move(a), nullptr }); }
rptr word(char const* a, std::string w)
    { return accept(a, a, word(std::move(w))); }
rptr keyword(char const* str, int precedence = 1)
    { return accept(str, str, word(str), precedence); }
rptr keyword(int precedence, char const* a, std::string w)
    { return accept(a, a, word(std::move(w)), precedence); }
rptr keyword(char const* a, std::string w)
    { return accept(a, a, word(std::move(w)), 1); }
rptr many1(rptr r)
    { return cat(clone(r), kleene(clone(r))); }

nfa_t gen_nfa(regex_t const& regex, 
              std::deque<nfa_node_t>& nodes, 
              nfa_node_t* start = nullptr)
{
    static unsigned prio = 0;
    switch(regex.type)
    {
    default:
    case ACCEPT:
    case EMPTY:
        {
            nfa_t nfa;
            nfa.first = start ? start : &nodes.emplace_back();
            nfa.last  = &nodes.emplace_back();
            nfa.first->edges.push_back({ regex.type, nfa.last });
            nfa.last->name = regex.name;
            nfa.last->string = regex.string;
            nfa.last->precedence = regex.precedence;
            if(regex.type == ACCEPT)
                nfa.last->prio = prio++;
            return nfa;
        }
    case CONCAT:
        {
            nfa_t l = gen_nfa(*regex.l, nodes, start);
            nfa_t r = gen_nfa(*regex.r, nodes, l.last);
            return { l.first, r.last };
        }
    case UNION:
        {
            nfa_t l = gen_nfa(*regex.l, nodes);
            nfa_t r = gen_nfa(*regex.r, nodes);
            nfa_t nfa;
            nfa.first = start ? start : &nodes.emplace_back();
            nfa.last  = &nodes.emplace_back();
            nfa.first->edges.push_back({ EMPTY, l.first });
            nfa.first->edges.push_back({ EMPTY, r.first });
            l.last->edges.push_back({ EMPTY, nfa.last });
            r.last->edges.push_back({ EMPTY, nfa.last });
            return nfa;
        }
    case KLEENE:
        {
            nfa_t nfa = gen_nfa(*regex.l, nodes);
            nfa_node_t* old_first = nfa.first;
            nfa_node_t* old_last = nfa.last;
            nfa.first = start ? start : &nodes.emplace_back();
            nfa.last  = &nodes.emplace_back();
            nfa.first->edges.push_back({ EMPTY, old_first });
            nfa.first->edges.push_back({ EMPTY, nfa.last });
            old_last->edges.push_back({ EMPTY, old_first });
            old_last->edges.push_back({ EMPTY, nfa.last });
            return nfa;
        }
    }
}

using nfa_set_t = fc::vector_set<nfa_node_t*>;
using dfa_node_t = std::pair<nfa_set_t const, struct dfa_edges_t>;
using dfa_set_t = fc::vector_set<dfa_node_t*>;
struct dfa_edges_t
{
    fc::vector_map<unsigned, dfa_node_t*> map;
    dfa_set_t* dfa_set;
};

namespace fc {
template<typename T>
bool operator==(fc::vector_set<T> const& a, fc::vector_set<T> const& b) noexcept
{
    return std::equal(a.begin(), a.end(),
                      b.begin(), b.end());
}

template<typename T>
bool operator<(fc::vector_set<T> const& a, fc::vector_set<T> const& b) noexcept
{
    return std::lexicographical_compare(a.begin(), a.end(),
                                        b.begin(), b.end());
}
}

struct dfa_t
{
    using map_t = std::map<nfa_set_t, dfa_edges_t>;
    dfa_node_t* first;
    std::map<nfa_set_t, dfa_edges_t> nodes;
};

nfa_set_t gen_eclosure(nfa_set_t todo)
{
    nfa_set_t ret(todo);
    while(todo.size())
    {
        nfa_node_t* n = todo.container.back();
        todo.container.pop_back();
        for(nfa_edge_t e : n->edges)
            if(e.chr == EMPTY && ret.insert(e.ptr).second)
                todo.insert(e.ptr);
    }
    return ret;
}

dfa_t nfa_to_dfa(nfa_t const& nfa)
{
    dfa_t dfa;
    dfa.nodes[gen_eclosure({ nfa.first })];
    dfa.first = &*dfa.nodes.begin();
    fc::vector_set<dfa_node_t*> todo = { &*dfa.nodes.begin() };

    fc::vector_map<std::pair<unsigned, unsigned>, nfa_set_t> edges;
    while(todo.size())
    {
        dfa_node_t* node = todo.container.back();
        todo.container.pop_back();

        edges.clear();
        for(nfa_node_t* np : node->first)
            for(nfa_edge_t edge : np->edges)
                if(edge.chr <= ACCEPT)
                    edges[std::make_pair(edge.chr, edge.ptr->prio)].insert(edge.ptr);

        for(auto const& edge : edges)
        {
            auto p = dfa.nodes.emplace(gen_eclosure(std::move(edge.second)), dfa_edges_t());
            if(p.second)
                todo.insert(&*p.first);
            node->second.map.emplace(edge.first.first, &*p.first);
        }
    }
    return dfa;
}

fc::vector_set<dfa_set_t> minimize_dfa(dfa_t& dfa)
{
    fc::vector_set<dfa_set_t> P;
    dfa_set_t not_final;
    for(auto& p : dfa.nodes)
        if(p.second.map.empty())
            P.insert({ &p });
        else
            not_final.insert(&p);
    P.insert(std::move(not_final));

    fc::vector_set<dfa_set_t> P2;
    fc::vector_set<unsigned> letters;
    fc::vector_map<dfa_set_t*, dfa_set_t> C;
    while(true)
    {
        for(dfa_set_t& s : P.container)
            for(dfa_node_t* np : s)
                np->second.dfa_set = &s;

        for(dfa_set_t const& s : P)
        {
            if(s.size() == 1)
            {
                P2.insert(s);
                continue;
            }

            letters.clear();
            for(dfa_node_t const* np : s)
                for(auto const& pair : np->second.map)
                    if(pair.first < 256)
                        letters.emplace(pair.first);

            if(letters.empty())
            {
                C.clear();
                for(dfa_node_t* np : s)
                {
                    for(auto const& pair : np->second.map)
                    {
                        if(pair.first == ACCEPT)
                        {
                            C[pair.second->second.dfa_set].insert(np);
                            goto insertedC;
                        }
                    }
                    C[nullptr].insert(np);
                insertedC:;
                }
                for(auto& pair : C)
                    P2.insert(std::move(pair.second));
            }
            else 
            {
                for(unsigned l : letters)
                {
                    C.clear();
                    for(dfa_node_t* np : s)
                    {
                        auto& map = np->second.map;
                        auto it = map.find(l);
                        if(it == np->second.map.end())
                            it = map.find(ACCEPT);
                        if(it == np->second.map.end())
                            C[nullptr].insert(np);
                        else
                            C[it->second->second.dfa_set].insert(np);
                    }
                    if(C.size() > 1)
                    {
                        for(auto& pair : C)
                            P2.insert(std::move(pair.second));
                        goto done_insert;
                    }
                }
                P2.insert(s);
            done_insert:;
            }
        }
        if(P == P2)
            return P;
        P = std::move(P2);
    }
}

void print_nfa(nfa_t const& nfa, std::deque<nfa_node_t> const& nodes)
{
    for(nfa_node_t const& n : nodes)
    {
        if(&n == nfa.first)
            std::printf("first:\n");
        std::printf("%p\n", &n);
        for(nfa_edge_t const& p : n.edges)
            std::printf(" '%c' (%i) -> %p\n", p.chr, p.chr, p.ptr);
    }
}

void print_dfa(dfa_t const& dfa)
{
    for(auto const& p : dfa.nodes)
    {
        std::printf("%p\n", &p.first);
        for(auto const& p : p.second.map)
            std::printf(" '%c' (%i) -> %p\n", p.first, p.first, p.second);
    }
}

void print_output(dfa_t const& dfa, fc::vector_set<dfa_set_t> const& mini)
{
    std::unordered_map<dfa_set_t const*, unsigned> nodes;
    std::vector<dfa_set_t const*> node_vector;
    std::printf("using token_type_t = unsigned;\n");
    std::printf("constexpr token_type_t TOK_ERROR = %u;\n", (unsigned)nodes.size());
    nodes.emplace(nullptr, nodes.size());
    node_vector.push_back(nullptr);
    
    fc::vector_map<unsigned, std::pair<dfa_set_t const*, nfa_node_t const*>> names;
    for(dfa_set_t const& s : mini)
    {
        if(s.size() != 1)
            continue;
        dfa_edges_t const& e = s.container.front()->second;
        if(e.map.size())
            continue;
        for(nfa_node_t const* np : s.container.front()->first)
            if(np->name)
                names.emplace(np->prio, std::make_pair(&s, np));
    }

    for(auto const& p : names)
    {
        std::printf("constexpr token_type_t TOK_%s = %u;\n", 
                    p.second.second->name, (unsigned)node_vector.size());
        nodes.emplace(p.second.first, nodes.size());
        node_vector.push_back(p.second.first);
    }
    std::printf("constexpr token_type_t TOK_END = %u;\n", (unsigned)node_vector.size());

    std::printf("#include <string_view>\n");
    std::printf("inline std::string_view token_name(token_type_t type)\n{\n");
    std::printf("    using namespace std::literals;\n");
    std::printf("    switch(type)\n    {\n");
    std::printf("    default: return \"?BAD?\"sv;\n");
    for(auto const& p : names)
        std::printf("    case TOK_%s: return \"%s\"sv;\n", 
                    p.second.second->name, p.second.second->name);
    std::printf("    }\n}\n");

    std::printf("inline std::string_view token_string(token_type_t type)\n{\n");
    std::printf("    using namespace std::literals;\n");
    std::printf("    switch(type)\n    {\n");
    std::printf("    default: return \"?BAD?\"sv;\n");
    for(auto const& p : names)
        std::printf("    case TOK_%s: return \"%s\"sv;\n", 
                    p.second.second->name, p.second.second->string);
    std::printf("    }\n}\n");

    std::printf("constexpr int token_precedence_table[] =\n{\n");
    for(auto const& p : names)
        std::printf("    %i,\n", p.second.second->precedence);
    std::printf("};\n");

    std::printf("#define TOK_KEY_CASES \\\n");
    for(auto const& p : names)
        if(p.second.second->precedence)
            std::printf("    case TOK_%s:\\\n", p.second.second->name);
    std::printf("\n");

    std::printf("constexpr token_type_t TOK_LAST_STATE = %u;\n", 
                (unsigned)nodes.size() - 1);

    for(dfa_set_t const& s : mini)
        if(nodes.emplace(&s, nodes.size()).second)
            node_vector.push_back(&s);

    for(dfa_set_t const& s : mini)
    for(dfa_node_t const* np : s)
    {
        if(np == dfa.first)
        {
            std::printf("constexpr token_type_t TOK_START = %u;\n", nodes[&s]);
            goto exit_loop;
        }
    }
exit_loop:

    fc::vector_map<dfa_set_t const*, fc::vector_set<unsigned>> outgoing; 
    fc::vector_set<fc::vector_set<unsigned>> char_sets; 
    fc::vector_set<fc::vector_set<unsigned>> char_ec; 
    fc::vector_set<fc::vector_set<unsigned>> char_ec_swap; 
    for(dfa_set_t const& s : mini)
    {
        outgoing.clear();
        fc::vector_set<unsigned> total;
        fc::vector_set<unsigned> complement;
        for(dfa_node_t const* np : s)
            for(auto pair : np->second.map)
                if(pair.first < 256)
                    outgoing[pair.second->second.dfa_set].insert(pair.first),
                    total.insert(pair.first);
        for(auto& pair : outgoing.container)
            char_sets.insert(std::move(pair.second));
        for(unsigned i = 0 ; i != 256; ++i)
            if(total.count(i) == 0)
                complement.insert(i);
        char_sets.insert(std::move(complement));
    }

    for(fc::vector_set<unsigned> cs : char_sets)
    {
        char_ec_swap.clear();
        for(auto& ec : char_ec)
        {
            fc::vector_set<unsigned> intersect;
            fc::vector_set<unsigned> difference;
            fc::vector_set<unsigned> remainder;
            std::set_intersection(cs.begin(), cs.end(), ec.begin(), ec.end(),
                                  std::back_inserter(intersect.container));
            std::set_difference(ec.begin(), ec.end(), intersect.begin(), intersect.end(),
                                std::back_inserter(remainder.container));
            std::set_difference(cs.begin(), cs.end(), ec.begin(), ec.end(),
                                std::back_inserter(difference.container));
            cs = std::move(difference);
            if(remainder.size())
                char_ec_swap.insert(std::move(remainder));
            if(intersect.size())
                char_ec_swap.insert(std::move(intersect));
        }
        if(cs.size())
            char_ec_swap.insert(std::move(cs));
        char_ec = std::move(char_ec_swap);
    }

    std::array<unsigned, 256> ec_table;
    for(unsigned i = 0 ; i != 256; ++i)
    for(unsigned j = 0; j < char_ec.size(); ++j)
    {
        if(char_ec.container[j].count(i))
        {
            ec_table[i] = j * node_vector.size();
            break;
        }
    }

    std::printf("constexpr unsigned ec_table[] = {");
    for(unsigned i = 0 ; i != 256; ++i)
        std::printf("%s%i,", i % 16 == 0 ? "\n    " : " ", ec_table[i]);
    std::printf("\n};\n");

    std::vector<unsigned> ttable(char_ec.size() * node_vector.size(), 0);
    for(unsigned i = 0; i < char_ec.size(); ++i)
    {
        for(unsigned j = 0; j < node_vector.size(); ++j)
        {
            if(node_vector[j] == nullptr)
                continue;
            unsigned c = char_ec.container[i].container.front();
            unsigned best_index = -1;
            for(dfa_node_t const* np : *node_vector[j])
            {
                auto it = np->second.map.find(c);
                if(it != np->second.map.end())
                {
                    ttable[i*node_vector.size()+j] = nodes[it->second->second.dfa_set];
                    goto assigned;
                }
            }
            for(dfa_node_t const* np : *node_vector[j])
            {
                auto it = np->second.map.find(ACCEPT);
                if(it != np->second.map.end())
                    best_index = std::min(best_index, nodes[it->second->second.dfa_set]);
            }
            if(best_index != -1u)
                ttable[i*node_vector.size()+j] = best_index;
        assigned:;
        }
    }

    std::printf("constexpr token_type_t transition_table[] = {");
    for(unsigned i = 0; i < ttable.size(); ++i)
        std::printf("%s%i,", i % 16 == 0 ? "\n    " : " ", ttable[i]);
    std::printf("\n};\n");
}

bool is_idchar(unsigned char c) 
{ 
    return c == '_' || std::isalnum(c) || std::isdigit(c);
}

bool is_lower_or_digit(unsigned char c) 
{ 
    return std::islower(c) || std::isdigit(c);
}


rptr idchar() { return pred(is_idchar); }
rptr upper() { return pred(&isupper); }
rptr lower() { return pred(&islower); }
rptr newline() { return uor(word("\n"), word("\r"), word("\n\r"), word("\r\n")); }
rptr comchar() { return pred([](unsigned char c) { return c != '\n' && c != 'r' && c; }); }
rptr eof() { return pred([](unsigned char c) { return c == '\0'; }); }
rptr whitespace() { return pred([](unsigned char c) { return c == ' ' || c == '\t'; }); }
rptr type() { return many1(cat(upper(), many1(pred(is_lower_or_digit)))); }

int main()
{
    std::deque<nfa_node_t> nfa_nodes;
    nfa_t nfa = gen_nfa(*
        uor(
            accept("eof", "file ending", eof()),
            accept("comment", "comment", cat(word("//"), kleene(comchar()), uor(eof(), newline()))),
            accept("eol", "line ending", newline()),
            accept("whitespace", "space", many1(whitespace())),

            // Keywords
            keyword("if"),
            keyword("else"),
            keyword("for"),
            keyword("while"),
            keyword("do"),
            keyword("break"),
            keyword("continue"),
            keyword("then"),
            keyword("return"),
            keyword("fn"),

            // Symbols
            keyword(1, "lbrace", "{"),
            keyword(1, "rbrace", "}"),

            keyword(1, "lbracket", "{"),
            keyword(1, "rbracket", "}"),

            //keyword(3, "colon", ":"),
            //keyword(3, "double_colon", "::"),
            keyword(4, "pointer", "%"),
            //keyword(4, "rarrow", "->"),
            keyword(6, "semicolon", ";"),
            keyword(6, "comma", ","),
            keyword(1, "lparen", "("),


            //keyword(5, "period", "."),
            accept(7, "apply", "apply", eof()), // dummy

            keyword(10, "asterisk", "*"),
            keyword(10, "fslash", "/"),

            keyword(11, "plus", "+"),
            keyword(11, "minus", "-"),

            keyword(12, "lshift", "<<"),
            keyword(12, "rshift", ">>"),

            keyword(13, "lt", "<"),
            keyword(13, "lte", "<="),
            keyword(13, "gt", ">"),
            keyword(13, "gte", ">="),

            keyword(14, "equal", "=="),
            keyword(14, "not_equal", "!="),

            keyword(15, "bitwise_and", "&"),
            keyword(16, "bitwise_xor", "^"),
            keyword(17, "bitwise_or", "|"),
            keyword(18, "logical_and", "&&"),
            keyword(19, "logical_or", "||"),

            keyword("bitwise_not", "~"),
            keyword("logical_not", "!"),
            keyword("plus_plus", "++"),
            keyword("minus_minus", "--"),

            keyword(30, "assign", "="),
            keyword(30, "plus_assign", "+="),
            keyword(30, "minus_assign", "-="),
            keyword(30, "times_assign", "*="),
            keyword(30, "div_assign", "/="),
            keyword(30, "bitwise_and_assign", "&="),
            keyword(30, "logical_and_assign", "&&="),
            keyword(30, "bitwise_or_assign", "|="),
            keyword(30, "logical_or_assign", "||="),
            keyword(30, "bitwise_not_assign", "~="),
            keyword(30, "bitwise_xor_assign", "^="),
            keyword(30, "lshift_assign", "<<="),
            keyword(30, "rshift_assign", ">>="),

            keyword(1, "rparen", ")"),

            accept("void", "void type", word("void")),
            accept("bool", "bool type", word("bool")),
            accept("byte", "byte type", word("byte")),
            accept("short", "short type", word("short")),
            accept("ident", "identifier", cat(lower(), kleene(idchar()))),
            accept("decimal", "number", many1(pred(isdigit))),

            accept("number", "number", eof()),
            accept("global_ident", "global identifier", eof())
            ),
        nfa_nodes);
    dfa_t dfa = nfa_to_dfa(nfa);

    print_output(dfa, minimize_dfa(dfa));

    //nfa_t nfa = gen_nfa(*pred(&isdigit), nfa_nodes);
    //print_nfa(nfa, nfa_nodes);
    //print_dfa(dfa);
}

