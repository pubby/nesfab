// A quick-n-dirty lexer generator; an alternative to 'lex' and 'flex'.

#include <ctype.h>
#include <cassert>
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
    unsigned char c = 0;
    do
    {
        if(f(c)) 
        {
            if(!base)
                base.reset(new regex_t{c});
            else
                base.reset(new regex_t{ UNION, rptr(new regex_t{c}), std::move(base) });
        }
        ++c;
    }
    while(c != 0);
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
    { return uor(clone(r), rptr(new regex_t{ EMPTY })); }

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

void print_output(dfa_t const& dfa, fc::vector_set<dfa_set_t> const& mini, char const* name_space, bool extra)
{
    std::string hpp_name = name_space;
    std::string cpp_name = name_space;
    hpp_name += "_tables.hpp";
    cpp_name += "_tables.cpp";

    std::FILE* hpp = std::fopen(hpp_name.c_str(), "w");
    std::FILE* cpp = std::fopen(cpp_name.c_str(), "w");

    std::unordered_map<dfa_set_t const*, unsigned> nodes;
    std::vector<dfa_set_t const*> node_vector;
    std::fprintf(hpp, "#include <cstdint>\n");
    std::fprintf(hpp, "#include <string_view>\n");
    std::fprintf(hpp, "namespace %s\n{\n", name_space);
    std::fprintf(hpp, "using token_type_t = std::uint16_t;\n");
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

    if(extra)
    {
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
    }

    std::fprintf(hpp, "#define %s_TOK_KEY_CASES \\\n", name_space);
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

    std::fprintf(cpp, "#include \"%s_tables.hpp\"\n", name_space);
    std::fprintf(cpp, "namespace %s\n{\n", name_space);
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
    assert(ttable.size() < 65536);

    std::fprintf(cpp, "extern token_type_t const lexer_transition_table[%u] = {\n", (unsigned)ttable.size());
    for(unsigned i = 0; i < ttable.size(); ++i)
        std::fprintf(cpp, "%s%i,", i % 16 == 0 ? "\n    " : " ", ttable[i]);
    std::fprintf(cpp, "\n};\n");

    std::fprintf(hpp, "extern unsigned const lexer_ec_table[256];\n");
    std::fprintf(hpp, "extern token_type_t const lexer_transition_table[%u];\n", (unsigned)ttable.size());

    std::fprintf(hpp, "} // namespace %s\n", name_space);
    std::fprintf(cpp, "} // namespace %s\n", name_space);

    std::fclose(hpp);
    std::fclose(cpp);
}

bool is_idchar(unsigned char c) 
    { return c == '_' || std::isalnum(c); }
bool is_idlower(unsigned char c) 
    { return c == '_' || std::islower(c) || std::isdigit(c); }
bool is_idupper(unsigned char c) 
    { return c == '_' || std::isupper(c) || std::isdigit(c); }
bool is_underscore(unsigned char c) 
    { return c == '_'; }

rptr underscore() { return pred(is_underscore); }
rptr idchar() { return pred(is_idchar); }
rptr idlower() { return pred(is_idlower); }
rptr idupper() { return pred(is_idupper); }
rptr upper() { return pred(&isupper); }
rptr lower() { return pred(&islower); }
rptr newline() { return uor(word("\n"), word("\r"), word("\n\r"), word("\r\n")); }
rptr comchar() { return pred([](unsigned char c) { return c != '\n' && c != '\r' && c; }); }
rptr nonnull() { return pred([](unsigned char c) { return c; }); }
rptr notchar(char n) { return pred([n](unsigned char c) { return c != n; }); }
rptr eof() { return pred([](unsigned char c) { return c == '\0'; }); }
rptr whitespace() { return pred([](unsigned char c) { return c == ' '; }); }
rptr digit() { return pred(isdigit); }
rptr hex_digit() { return pred(isxdigit); }
rptr bin_digit() { return pred([](unsigned char c) { return c == '0' || c == '1'; }); }
rptr ident() { return uor(cat(lower(), kleene(idlower())), cat(upper(), kleene(idupper()))); }

rptr dummy(char const* name) 
    { return cat(eof(), rptr(new regex_t{ ACCEPT, nullptr, nullptr, name, name, 0 })); }

rptr either_case_keyword(char const* a) 
{ 
    std::string lower(a);
    std::string upper(a);

    for(char& c : lower)
        c = std::tolower(c);
    for(char& c : upper)
        c = std::toupper(c);

    return accept(a, a, uor(word(lower.c_str()), word(upper.c_str())));
}

int main()
{
    std::deque<nfa_node_t> nfa_nodes;
    nfa_t nfa = gen_nfa(*uor(
        accept("eof", "file ending", eof()),
        accept("comment", "single-line comment", cat(word("//"), kleene(comchar()), uor(eof(), newline()))),
        accept("ml_comment_begin", "multi-line comment", word("/*")),
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
        keyword("irq"),
        keyword("goto"),
        keyword("label"),
        keyword("file"),
        keyword("struct"),
        keyword("vars"),
        keyword("data"),
        keyword("omni"),
        keyword("asm"),
        keyword("ready"),
        keyword("fence"),
        keyword("switch"),
        keyword("case"),
        keyword("default"),
        keyword("charmap"),
        keyword("chrrom"),
        keyword("employs"),
        keyword("preserves"),
        keyword("stows"),
        keyword("audio"),
        keyword("system"),
        keyword("state"),
        keyword("abs"),
        keyword("min"),
        keyword("max"),
        keyword("swap"),
        keyword("macro"),
        keyword("__mapper_detail"),
        keyword("__mapper_reset"),
        keyword("__mapper"),
        keyword("__illegal"),
        keyword("__controllers"),
        keyword("__expansion_audio"),
        keyword("__sector_size"),
        keyword("__fixed"),
        keyword("nmi_counter"),
        keyword("read"),
        keyword("write"),
        keyword("mapfab"),
        keyword("push"),
        keyword("pop"),

        keyword("true"),
        keyword("false"),

        keyword("PPUCTRL"),
        keyword("PPUMASK"),
        keyword("PPUSTATUS"),
        keyword("PPUSCROLL"),
        keyword("PPUADDR"),
        keyword("PPUDATA"),
        keyword("OAMADDR"),
        keyword("OAMDATA"),
        keyword("OAMDMA"),

        keyword("SYSTEM_NTSC"),
        keyword("SYSTEM_PAL"),
        keyword("SYSTEM_DENDY"),
        keyword("SYSTEM_UNKNOWN"),

        // Symbols

        keyword("colon", ":"),
        keyword("hash", "#"),

        keyword("backtick", "`"),
        keyword("dquote", "\""),
        keyword("quote", "'"),

        //keyword(3, "double_colon", "::"),
        //keyword(4, "pointer", "%"),
        //keyword(4, "rarrow", "->"),
        keyword("semicolon", ";"),
        keyword("comma", ","),

        keyword("sizeof"),
        accept("sizeof_expr", "sizeof", eof()), // dummy
        keyword("len", "len"),
        accept("len_expr", "len", eof()), // dummy

        accept(8, "unary_plus", "unary +", eof()), // dummy
        accept(8, "unary_minus", "unary -", eof()), // dummy
        op(8, "unary_xor", "~"),
        op(8, "unary_negate", "!"),
        op(8, "unary_ref", "unary &"),

        keyword(4, "at", "@"),

        keyword(5, "period", "."),
        accept(7, "apply", "apply", eof()), // dummy
        accept(7, "mode_apply", "mode_apply", eof()), // dummy
        accept(7, "cast", "cast", eof()), // dummy
        accept(7, "cast_type", "cast_type", eof()), // dummy
        accept(7, "index8", "index []", eof()), // dummy
        accept(7, "index16", "index {}", eof()), // dummy

        keyword(6, "lbrace", "{"),
        keyword(6, "rbrace", "}"),
        keyword(6, "lbracket", "["),
        keyword(6, "rbracket", "]"),
        keyword(6, "lparen", "("), // begin infix ops

        op(8, "increment", "++"),
        op(8, "decrement", "--"),

        op(10, "asterisk", "*"),
        op(10, "fslash", "/"),

        op(11, "plus", "+"),
        op(11, "minus", "-"),

        op(12, "rol", "<-<"),
        op(13 | RIGHT_ASSOC, "ror", ">->"),
        accept(13, "ror_flip", "ror_flip", eof()), // dummy

        op(14, "lshift", "<<"),
        op(14, "rshift", ">>"),

        op(15, "bitwise_and", "&"),
        op(16, "bitwise_xor", "^"),
        op(17, "bitwise_or", "|"),

        op(18, "lt", "<"),
        op(18, "lte", "<="),
        op(18, "gt", ">"),
        op(18, "gte", ">="),

        op(19, "eq", "=="),
        op(19, "not_eq", "!="),

        op(20, "logical_and", "&&"),
        accept(20, "end_logical_and", "end_logical_and", eof()),
        op(21, "logical_or", "||"),
        accept(21, "end_logical_or", "end_logical_or", eof()),

        op(28 | RIGHT_ASSOC, "rol_assign", "<=<"),
        op(29, "ror_assign", ">=>"),
        accept(29, "ror_assign_flip", "ror_assign_flip", eof()), // dummy
        op(30 | RIGHT_ASSOC, "assign", "="),
        op(30 | RIGHT_ASSOC, "plus_assign", "+="),
        op(30 | RIGHT_ASSOC, "minus_assign", "-="),
        op(30 | RIGHT_ASSOC, "times_assign", "*="),
        op(30 | RIGHT_ASSOC, "div_assign", "/="),
        op(30 | RIGHT_ASSOC, "bitwise_and_assign", "&="),
        //op(30 | RIGHT_ASSOC, "logical_and_assign", "&&="),
        op(30 | RIGHT_ASSOC, "bitwise_or_assign", "|="),
        //op(30 | RIGHT_ASSOC, "logical_or_assign", "||="),
        op(30 | RIGHT_ASSOC, "bitwise_xor_assign", "^="),
        op(30 | RIGHT_ASSOC, "lshift_assign", "<<="),
        op(30 | RIGHT_ASSOC, "rshift_assign", ">>="),

        keyword(1, "rparen", ")"), // end infix ops

        accept("Void", "Void type", word("Void")), // First type
        accept("Fn", "Fn type", word("Fn")), // First type
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
        accept("AA", "AA type", word("AA")),
        accept("AAA", "AAA type", word("AAA")),
        accept("PP", "PP type", word("PP")),
        accept("PPP", "PPP type", word("PPP")),
        accept("CC", "CC type", word("CC")),
        accept("CCC", "CCC type", word("CCC")),
        accept("MM", "MM type", word("MM")),
        accept("MMM", "MMM type", word("MMM")),
        accept("Int", "Int type", word("Int")),
        accept("Real", "Real type", word("Real")),
        accept("Bool", "Bool type", word("Bool")), // Last type
        //accept("group_ident", "group identifier", cat(word("@"), kleene(idchar()))),
        accept("ident", "identifier", cat(maybe(underscore()), ident())),
        accept("type_ident", "type identifier", cat(maybe(underscore()), upper(), kleene(idchar()))),
        accept("decimal", "number", uor(many1(digit()), cat(many1(digit()), word("."), many1(digit())))),
        accept("hex", "number", cat(word("$"), uor(many1(hex_digit()), cat(many1(hex_digit()), word("."), many1(hex_digit()))))),
        accept("binary", "number", cat(word("%"), uor(many1(bin_digit()), cat(many1(bin_digit()), word("."), many1(bin_digit()))))),

        // dummy:
        accept("int", "int", eof()),
        accept("real", "real", eof()),
        accept("global_ident", "global identifier", eof()),
        accept("weak_ident", "weak identifier", eof()),
        accept("hw_addr", "hardware address", eof()),
        accept("read_hw", "read hardware", eof()),
        accept("write_hw", "write hardware", eof()),
        //accept("paa_byte_array", "byte array", eof()),
        //accept("paa_locator_array", "locator array", eof()),
        //accept("push_paa", "push paa", eof()),
        //accept("begin_paa", "begin paa", eof()),
        //accept("end_paa", "end paa", eof()),
        accept("group_set", "group set", eof()),
        accept("rpair", "rpair", eof()),
        accept("ssa", "ssa", eof()),

        /*
        dummy("force_truncate"),
        dummy("force_promote"),
        dummy("force_intify_ptr"),
        dummy("force_round_real"),
        dummy("force_boolify"),
        */
        dummy("implicit_cast"),
        dummy("shift_atom"),
        dummy("replace_atom"),
        dummy("write_state"),
        dummy("byte_vec"),
        dummy("locator_vec"),

        // string/char literals:
        accept("character", "character literal", eof()),
        accept("string_uncompressed", "uncompressed string literal", eof()),
        accept("string_compressed", "compressed string literal", eof()),

        // byte block:
        accept("byte_block_proc", "byte block", eof()),
        accept("byte_block_data", "byte block", eof()),
        accept("byte_block_asm_op", "assembly instruction", eof()),
        accept("byte_block_label", "assembly label", eof()),
        accept("byte_block_call", "assembly fn call", eof()),
        accept("byte_block_goto", "assembly goto", eof()),
        accept("byte_block_goto_mode", "assembly goto mode", eof()),
        accept("byte_block_wait_nmi", "assembly wait nmi", eof()),
        accept("byte_block_bank_switch_a", "assembly bank switch A", eof()),
        accept("byte_block_bank_switch_x", "assembly bank switch X", eof()),
        accept("byte_block_bank_switch_y", "assembly bank switch Y", eof()),
        accept("byte_block_bank_switch_ax", "assembly bank switch AX", eof()),
        accept("byte_block_byte_array", "byte block array", eof()),
        accept("byte_block_locator_array", "byte block locator array", eof()),
        accept("byte_block_sub_proc", "byte block sub proc", eof()),
        accept("byte_block_if", "byte block if", eof()),
        accept("byte_block_push", "byte block push", eof()),
        accept("byte_block_pop", "byte block pop", eof())
        ),
        nfa_nodes);
    dfa_t dfa = nfa_to_dfa(nfa);
    print_output(dfa, minimize_dfa(dfa), "lex", true);

    std::deque<nfa_node_t> asm_nfa_nodes;
    nfa_t asm_nfa = gen_nfa(*
        uor(
#define OP_NAME(name) either_case_keyword(#name),
#include "lex_op_name.inc"
#undef OP_NAME
            accept("eof", "file ending", eof())
            //keyword("bank")
            ),
        asm_nfa_nodes);
    dfa_t asm_dfa = nfa_to_dfa(asm_nfa);
    print_output(asm_dfa, minimize_dfa(asm_dfa), "asm_lex", false);

    std::deque<nfa_node_t> ext_nfa_nodes;
    nfa_t ext_nfa = gen_nfa(*uor(
        either_case_keyword("bin"),
        either_case_keyword("chr"),
        either_case_keyword("nam"),
        either_case_keyword("png"),
        either_case_keyword("txt"),
        either_case_keyword("pal"),
        either_case_keyword("map")
        ),
        ext_nfa_nodes);
    dfa_t ext_dfa = nfa_to_dfa(ext_nfa);
    print_output(ext_dfa, minimize_dfa(ext_dfa), "ext_lex", false);

    std::deque<nfa_node_t> macro_nfa_nodes;
    nfa_t macro_nfa = gen_nfa(*uor(
        accept("eof", "file ending", eof()),
        keyword("backtick", "`"),
        keyword("dquote", "\""),
        keyword("quote", "'"),
        accept("comment", "single-line comment", cat(word("//"), kleene(comchar()), uor(eof(), newline()))),
        accept("ml_comment", "multi-line comment", cat(word("/*"), kleene(uor(notchar('*'), cat(word("*"), notchar('/')))), word("*/"))),
        accept("ident", "macro identifier", cat(word("#"), ident(), word("#"))),
        accept("dquote_ident", "quoted macro identifier", cat(word("#\""), ident(), word("\"#"))),
        accept("quote_ident", "quoted macro identifier", cat(word("#'"), ident(), word("'#"))),
        accept("backtick_ident", "quoted macro identifier", cat(word("#`"), ident(), word("`#"))),
        accept("dash_ident", "quoted macro identifier", cat(word("#-"), ident(), word("-#"))),
        accept("eq_ident", "quoted macro identifier", cat(word("#="), ident(), word("=#"))),
        accept("colon_ident", "macro identifier declaration", cat(word("#:"), ident(), word(":#")))
        ),
        macro_nfa_nodes);
    dfa_t macro_dfa = nfa_to_dfa(macro_nfa);
    print_output(macro_dfa, minimize_dfa(macro_dfa), "macro_lex", false);
}

