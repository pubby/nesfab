// A quick-n-dirty lexer generator; an alternative to 'lex' and 'flex'.

#include <ctype.h>
#include <cstdio>
#include <cstdint>
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

constexpr std::uint8_t RIGHT_ASSOC = 0x80;

struct regex_t
{
    unsigned type;
    std::unique_ptr<regex_t> l;
    std::unique_ptr<regex_t> r;
    char const* name = nullptr;
    char const* string = nullptr;
    std::uint8_t precedence = 0;
};

struct nfa_edge_t { unsigned chr; struct nfa_node_t* ptr; };

struct nfa_node_t 
{
    std::vector<nfa_edge_t> edges;
    char const* name = nullptr;
    char const* string = nullptr;
    std::uint8_t precedence = 0;
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
rptr accept(char const* name, char const* str, rptr a, std::uint8_t precedence = 0) 
    { return cat(std::move(a), rptr(new regex_t{ ACCEPT, nullptr, nullptr, name, str, precedence })); }
rptr accept(std::uint8_t precedence, char const* name, char const* str, rptr a) 
    { return cat(std::move(a), rptr(new regex_t{ ACCEPT, nullptr, nullptr, name, str, precedence })); }
rptr kleene(rptr a) 
    { return rptr(new regex_t{ KLEENE, std::move(a), nullptr }); }
rptr word(char const* a, std::string w)
    { return accept(a, a, word(std::move(w))); }
rptr keyword(char const* str, std::uint8_t precedence = 1)
    { return accept(str, str, word(str), precedence); }
rptr keyword(std::uint8_t precedence, char const* a, std::string w)
    { return accept(a, a, word(std::move(w)), precedence); }
rptr keyword(char const* a, std::string w)
    { return accept(a, a, word(std::move(w)), 1); }
rptr op(std::uint8_t precedence, char const* a, char const* w)
    { return accept(a, w, word(w), precedence); }
rptr op(char const* a, char const* w)
    { return accept(a, w, word(w), 1); }
rptr many1(rptr r)
    { return cat(clone(r), kleene(clone(r))); }
rptr maybe(rptr r)
    { return cat(clone(r), nullptr); }

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
    std::FILE* hpp = std::fopen("lex_tables.hpp", "w");
    std::FILE* cpp = std::fopen("lex_tables.cpp", "w");

    std::unordered_map<dfa_set_t const*, unsigned> nodes;
    std::vector<dfa_set_t const*> node_vector;
    std::fprintf(hpp, "#include <cstdint>\n");
    std::fprintf(hpp, "#include <string_view>\n");
    std::fprintf(hpp, "using token_type_t = std::uint32_t;\n");
    std::fprintf(hpp, "constexpr token_type_t TOK_ERROR = 0;\n");
    nodes.emplace(nullptr, nodes.size()); // Reserve for TOK_ERROR.
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
        std::fprintf(hpp, "constexpr token_type_t TOK_%s = %u;\n", 
                     p.second.second->name, (unsigned)node_vector.size());
        nodes.emplace(p.second.first, nodes.size());
        node_vector.push_back(p.second.first);
    }
    std::fprintf(hpp, "constexpr token_type_t TOK_END = %u;\n", (unsigned)node_vector.size());

    std::fprintf(hpp, "inline std::string_view token_name(token_type_t type)\n{\n");
    std::fprintf(hpp, "    using namespace std::literals;\n");
    std::fprintf(hpp, "    switch(type)\n    {\n");
    std::fprintf(hpp, "    default: return \"?BAD?\"sv;\n");
    for(auto const& p : names)
        std::fprintf(hpp, "    case TOK_%s: return \"%s\"sv;\n", 
                    p.second.second->name, p.second.second->name);
    std::fprintf(hpp, "    }\n}\n");

    std::fprintf(hpp, "inline std::string_view token_string(token_type_t type)\n{\n");
    std::fprintf(hpp, "    using namespace std::literals;\n");
    std::fprintf(hpp, "    switch(type)\n    {\n");
    std::fprintf(hpp, "    default: return \"?BAD?\"sv;\n");
    for(auto const& p : names)
        std::fprintf(hpp, "    case TOK_%s: return \"%s\"sv;\n", 
                    p.second.second->name, p.second.second->string);
    std::fprintf(hpp, "    }\n}\n");

    std::fprintf(hpp, "constexpr unsigned char token_precedence_table[] =\n{\n");
        std::fprintf(hpp, "    0,\n"); // TOK_ERROR
    for(auto const& p : names)
        std::fprintf(hpp, "    %i,\n", p.second.second->precedence & 0xFF);
    std::fprintf(hpp, "};\n");

    std::fprintf(hpp, "constexpr bool token_right_assoc_table[] =\n{\n");
        std::fprintf(hpp, "    0,\n"); // TOK_ERROR
    for(auto const& p : names)
        std::fprintf(hpp, "    %i,\n", bool(p.second.second->precedence & RIGHT_ASSOC));
    std::fprintf(hpp, "};\n");

    std::fprintf(hpp, "#define TOK_KEY_CASES \\\n");
    for(auto const& p : names)
        if(p.second.second->precedence)
            std::fprintf(hpp, "    case TOK_%s:\\\n", p.second.second->name);
    std::fprintf(hpp, "\n");

    std::fprintf(hpp, "constexpr token_type_t TOK_LAST_STATE = %u;\n", 
                (unsigned)nodes.size() - 1);

    for(dfa_set_t const& s : mini)
        if(nodes.emplace(&s, nodes.size()).second)
            node_vector.push_back(&s);

    for(dfa_set_t const& s : mini)
    for(dfa_node_t const* np : s)
    {
        if(np == dfa.first)
        {
            std::fprintf(hpp, "constexpr token_type_t TOK_START = %u;\n", nodes[&s]);
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

    std::fprintf(cpp, "#include \"lex_tables.hpp\"\n");
    std::fprintf(cpp, "extern unsigned const lexer_ec_table[256] = {");
    for(unsigned i = 0 ; i != 256; ++i)
        std::fprintf(cpp, "%s%i,", i % 16 == 0 ? "\n    " : " ", ec_table[i]);
    std::fprintf(cpp, "\n};\n");

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

    std::fprintf(cpp, "extern token_type_t const lexer_transition_table[%u] = {\n", (unsigned)ttable.size());
    for(unsigned i = 0; i < ttable.size(); ++i)
        std::fprintf(cpp, "%s%i,", i % 16 == 0 ? "\n    " : " ", ttable[i]);
    std::fprintf(cpp, "\n};\n");

    std::fprintf(hpp, "extern unsigned const lexer_ec_table[256];\n");
    std::fprintf(hpp, "extern token_type_t const lexer_transition_table[%u];\n", (unsigned)ttable.size());

    std::fclose(hpp);
    std::fclose(cpp);
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
rptr comchar() { return pred([](unsigned char c) { return c != '\n' && c != '\r' && c; }); }
rptr eof() { return pred([](unsigned char c) { return c == '\0'; }); }
rptr whitespace() { return pred([](unsigned char c) { return c == ' ' || c == '\t'; }); }
rptr type() { return many1(cat(upper(), many1(pred(is_lower_or_digit)))); }
rptr digit() { return pred(isdigit); }
rptr hex_digit() { return pred(isxdigit); }
rptr bin_digit() { return pred([](unsigned char c) { return c == '0' || c == '1'; }); }

rptr asm_op_keyword(char const* a) 
{ 
    std::string lower(a);
    std::string upper(a);

    for(char& c : lower)
        c = std::tolower(c);
    for(char& c : upper)
        c = std::toupper(c);

    return uor(word(lower.c_str()), word(upper.c_str()));
}

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
            keyword("return"),
            keyword("fn"),
            keyword("ct"),
            keyword("mode"),
            keyword("nmi"),
            keyword("goto"),
            keyword("label"),
            keyword("using"),
            keyword("file"),
            keyword("struct"),
            keyword("vars"),
            keyword("data"),
            keyword("omni"),
            keyword("ready"),
            keyword("fence"),
            keyword("switch"), // TODO
            keyword("case"), // TODO
            keyword("default"), // TODO

            keyword("PPUCTRL"),
            keyword("PPUMASK"),
            keyword("PPUSTATUS"),
            keyword("PPUSCROLL"),
            keyword("PPUADDR"),
            keyword("PPUDATA"),
            keyword("OAMADDR"),
            keyword("OAMDATA"),
            keyword("OAMDMA"),

            // Symbols
            keyword("lbrace", "{"),
            keyword("rbrace", "}"),

            keyword("lbracket", "["),
            keyword("rbracket", "]"),

            keyword("colon", ":"),

            keyword(1, "dquote", "\""),
            keyword(1, "quote", "'"),

            //keyword(3, "double_colon", "::"),
            //keyword(4, "pointer", "%"),
            //keyword(4, "rarrow", "->"),
            keyword(6, "semicolon", ";"),
            keyword(6, "comma", ","),
            keyword(1, "lparen", "("),



            keyword(4, "at", "@"),
            keyword(5, "period", "."),
            keyword(7, "sizeof", "sizeof"),
            accept(7, "sizeof_expr", "sizeof", eof()), // dummy
            keyword(7, "len", "len"),
            accept(7, "len_expr", "len", eof()), // dummy
            accept(7, "apply", "apply", eof()), // dummy
            accept(7, "cast_argn", "cast", eof()), // dummy
            accept(7, "cast_type", "cast type", eof()), // dummy
            accept(7, "index", "index", eof()), // dummy
            accept(8, "unary_minus", "unary -", eof()), // dummy
            op(8, "unary_xor", "~"),
            op(8, "unary_negate", "!"),

            op(10, "asterisk", "*"),
            op(10, "fslash", "/"),

            op(11, "plus", "+"),
            op(11, "minus", "-"),

            op(12, "lshift", "<<"),
            op(12, "rshift", ">>"),

            op(13, "bitwise_and", "&"),
            op(14, "bitwise_xor", "^"),
            op(15, "bitwise_or", "|"),

            op(16, "lt", "<"),
            op(16, "lte", "<="),
            op(16, "gt", ">"),
            op(16, "gte", ">="),

            op(17, "eq", "=="),
            op(17, "not_eq", "!="),

            op(18, "logical_and", "&&"),
            accept(18, "end_logical_and", "end_logical_and", eof()),
            op(19, "logical_or", "||"),
            accept(19, "end_logical_or", "end_logical_or", eof()),

            op(30 | RIGHT_ASSOC, "assign", "="),
            op(30 | RIGHT_ASSOC, "plus_assign", "+="),
            op(30 | RIGHT_ASSOC, "minus_assign", "-="),
            op(30 | RIGHT_ASSOC, "times_assign", "*="),
            op(30 | RIGHT_ASSOC, "div_assign", "/="),
            op(30 | RIGHT_ASSOC, "bitwise_and_assign", "&="),
            op(30 | RIGHT_ASSOC, "logical_and_assign", "&&="),
            op(30 | RIGHT_ASSOC, "bitwise_or_assign", "|="),
            op(30 | RIGHT_ASSOC, "logical_or_assign", "||="),
            op(30 | RIGHT_ASSOC, "bitwise_xor_assign", "^="),
            op(30 | RIGHT_ASSOC, "lshift_assign", "<<="),
            op(30 | RIGHT_ASSOC, "rshift_assign", ">>="),

            keyword(1, "rparen", ")"),

            accept("Void", "void type", word("Void")), // First type
            accept("F", "F type", word("F")),
            accept("FF", "FF type", word("FF")),
            accept("FFF", "FFF type", word("FFF")),
            accept("U", "U type", word("U")),
            accept("UU", "UU type", word("UU")),
            accept("UUU", "UUU type", word("UUU")),
            accept("UF", "UF type", word("UF")),
            accept("UUF", "UUF type", word("UUF")),
            accept("UUUF", "UUUF type", word("UUUF")),
            accept("UFF", "UFF type", word("UFF")),
            accept("UUFF", "UUFF type", word("UUFF")),
            accept("UUUFF", "UUUFF type", word("UUUFF")),
            accept("UFFF", "UFFF type", word("UFFF")),
            accept("UUFFF", "UUFFF type", word("UUFFF")),
            accept("UUUFFF", "UUUFFF type", word("UUUFFF")),
            accept("S", "S type", word("S")),
            accept("SS", "SS type", word("SS")),
            accept("SSS", "SSS type", word("SSS")),
            accept("SF", "SF type", word("SF")),
            accept("SSF", "SSF type", word("SSF")),
            accept("SSSF", "SSSF type", word("SSSF")),
            accept("SFF", "SFF type", word("SFF")),
            accept("SSFF", "SSFF type", word("SSFF")),
            accept("SSSFF", "SSSFF type", word("SSSFF")),
            accept("SFFF", "SFFF type", word("SFFF")),
            accept("SSFFF", "SSFFF type", word("SSFFF")),
            accept("SSSFFF", "SSSFFF type", word("SSSFFF")),
            accept("PP", "PP type", word("PP")),
            accept("PPP", "PPP type", word("PPP")),
            accept("MM", "MM type", word("MM")),
            accept("MMM", "MMM type", word("MMM")),
            accept("Int", "Int type", word("Int")),
            accept("Real", "Real type", word("Real")),
            accept("Bool", "Bool type", word("Bool")), // Last type
            //accept("group_ident", "group identifier", cat(word("@"), kleene(idchar()))),
            accept("ident", "identifier", cat(lower(), kleene(idchar()))),
            accept("type_ident", "type identifier", cat(upper(), kleene(idchar()))),
            accept("decimal", "number", uor(many1(digit()), cat(many1(digit()), word("."), many1(digit())))),
            accept("hex", "number", cat(word("$"), uor(many1(hex_digit()), cat(many1(hex_digit()), word("."), many1(hex_digit()))))),
            accept("binary", "number", cat(word("%"), uor(many1(bin_digit()), cat(many1(bin_digit()), word("."), many1(bin_digit()))))),

            // dummy:
            accept("int", "int", eof()),
            accept("real", "real", eof()),
            accept("global_ident", "global identifier", eof()),
            accept("weak_ident", "weak identifier", eof()),
            accept("hw_expr", "hardware expression", eof()),
            accept("hw_addr", "hardware address", eof()),
            accept("read_hw", "read hardware", eof()),
            accept("write_hw", "write hardware", eof()),
            accept("push_paa_byte_array", "byte array", eof()),
            accept("push_paa", "push paa", eof()),
            accept("begin_paa", "begin paa", eof()),
            accept("end_paa", "end paa", eof()),
            accept("group_set", "group set", eof()),
            accept("spair", "spair", eof())
            ),
        nfa_nodes);
    dfa_t dfa = nfa_to_dfa(nfa);
    print_output(dfa, minimize_dfa(dfa));

    std::deque<nfa_node_t> asm_nfa_nodes;
    nfa_t asm_nfa = gen_nfa(*
        uor(
            accept("eof", "file ending", eof()),
#define OP_NAME(name, b) asm_op_keyword(#name),
#include "lexed_op_name.inc"
#undef OP_NAME
            accept("reg_x", "register x", uor(word("x"), word("X"))),
            accept("reg_y", "register y", uor(word("y"), word("Y")))
            ),
        asm_nfa_nodes);
    dfa_t asm_dfa = nfa_to_dfa(asm_nfa);
    //print_output(asm_dfa, minimize_dfa(asm_dfa));
}

