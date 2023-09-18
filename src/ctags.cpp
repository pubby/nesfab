#include "ctags.hpp"

#include <algorithm>
#include <string>
#include <vector>

#include "file.hpp"
#include "globals.hpp"
#include "pstring.hpp"

void write_ctags(FILE* fp, fs::path ctags_path)
{
    struct ctag_t
    {
        std::string ident;
        lpstring_t lpstring;
    };

    ctags_path = fs::absolute(ctags_path);
    ctags_path.remove_filename();

    std::vector<std::string> file_paths(compiler_options().source_names.size());
    for(unsigned i = 0; i < file_paths.size(); ++i)
        file_paths[i] = fs::proximate(source_path(i), ctags_path).string();

    std::vector<ctag_t> ctags;
    ctags.reserve(global_ht::pool().size());

    for(global_t const& g : global_ht::values())
    {
        if(!g.name.empty() && g.name[0] == '_')
            continue;
        if(g.lpstring().file_i >= file_paths.size())
            continue;
        if(g.lpstring().line == BAD_LINE_NUMBER)
            continue;
        if(global_t::lookup_sourceless(g.name) != &g)
            continue;
        ctags.push_back({ g.name, g.lpstring() });
    }

    std::sort(ctags.begin(), ctags.end(), [&](ctag_t const& a, ctag_t const& b)
    {
        return a.ident < b.ident;
    });

    for(ctag_t const& ctag : ctags)
    {
        std::fprintf(
            fp, "%s\t%s\t%i\n", 
            ctag.ident.c_str(), 
            file_paths[ctag.lpstring.file_i].c_str(),
            ctag.lpstring.line - 1);
    }
}
