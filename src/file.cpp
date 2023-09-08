#include "file.hpp"

#include <cassert>
#include <cstdio>
#include <stdexcept>
#include <deque>
#include <mutex>

#include "robin/set.hpp"

#include "platform.hpp"

#ifdef PLATFORM_UNIX
#  include <sys/mman.h>
#  include <sys/stat.h>
#  include <sys/types.h>
#  include <fcntl.h>
#  include <unistd.h>
#endif

#include "guard.hpp"
#include "format.hpp"
#include "compiler_error.hpp"
#include "macro.hpp"
#include "ident_map.hpp"
#include "decl.hpp"

struct macro_result_t
{
    fs::path path;
    macro_invocation_t invoke;
    std::string contents;
    ident_map_t<global_ht> private_globals;
    ident_map_t<group_ht> private_groups;
};

static std::mutex invoke_mutex;
static rh::batman_set<macro_invocation_t> invoke_set;
static std::deque<macro_result_t> macro_results;
static std::deque<macro_result_t> new_macro_results;

void invoke_macro(macro_invocation_t invoke)
{
    invoke_macro(std::move(invoke), {}, {});
}

void invoke_macro(
    macro_invocation_t invoke,
    ident_map_t<global_ht> private_globals,
    ident_map_t<group_ht> private_groups,
    std::string const& append)
{
    // Ignore macros with empty names:
    if(invoke.name.empty())
        return;

    auto* pair = compiler_options().macro_names.lookup(invoke.name);

    if(!pair)
        throw std::runtime_error(fmt("Unknown macro: %", invoke.name));

    unsigned const file_i = (pair - compiler_options().macro_names.begin()) + compiler_options().num_fab;
    std::string str = invoke_macro(file_i, invoke.args);
    str += append;
    str.push_back('\0');

    {
        std::lock_guard<std::mutex> lock(invoke_mutex);
        if(invoke_set.insert(invoke).second)
        {
            new_macro_results.push_back({ pair->second.dir / pair->second.file, std::move(invoke), std::move(str), 
                                          std::move(private_globals), std::move(private_groups) });
        }
    }
}

std::pair<unsigned, unsigned> finalize_macros()
{
    unsigned const first = macro_results.size() + compiler_options().source_names.size();
    macro_results.insert(macro_results.end(), new_macro_results.begin(), new_macro_results.end());
    unsigned const second = macro_results.size() + compiler_options().source_names.size();
    new_macro_results.clear();
    return { first, second };
}

bool resource_path(fs::path preferred_dir, fs::path name, fs::path& result)
{
    result = preferred_dir / name;

    if(fs::exists(result))
        return true;

    for(fs::path const& dir : compiler_options().resource_dirs)
    {
        result = dir / name;
        if(fs::exists(result))
            return true;
    }

    for(fs::path const& dir : compiler_options().nesfab_dirs)
    {
        result = dir / name;
        if(fs::exists(result))
            return true;
    }

    return false;
}

bool read_binary_file(char const* filename, std::function<void*(std::size_t)> const& alloc)
{
#ifdef PLATFORM_UNIX
    int fd = open(filename, O_RDONLY);
    auto scope_guard = make_scope_guard([&]{ close(fd); });

    struct stat sb;
    if(fstat(fd, &sb) == -1)
        return false;

    void* data = alloc(std::size_t(sb.st_size));

    if(!data || read(fd, data, sb.st_size) != sb.st_size)
        return false;

    return data;
#else
    FILE* fp = std::fopen(filename, "rb");
    if(!fp)
        return false;
    auto scope_guard = make_scope_guard([&]{ std::fclose(fp); });

    // Get the file size
    std::fseek(fp, 0, SEEK_END);
    std::size_t const file_size = ftell(fp);
    std::fseek(fp, 0, SEEK_SET);

    auto* data = alloc(file_size);

    if(!data || std::fread(data, file_size, 1, fp) != 1)
        return false;

    return data;
#endif
}

std::vector<std::uint8_t> read_binary_file(std::string filename, pstring_t at)
{
    try
    {
        return read_binary_file(std::move(filename));
    }
    catch(std::exception const& e)
    {
        compiler_error(at, e.what());
    }
    catch(...)
    {
        throw;
    }
}

std::vector<std::uint8_t> read_binary_file(std::string filename)
{
    std::vector<std::uint8_t> vec;

    if(!read_binary_file(filename.c_str(), [&](std::size_t size)
    {
        vec.resize(size);
        return vec.data();
    }))
    {
        throw std::runtime_error("Unable to read: %" + filename);
    }

    return vec;
}

fs::path source_path(unsigned file_i)
{
    assert(file_i < compiler_options().source_names.size());
    fs::path path;
    auto const& source = compiler_options().source_names.at(file_i);

    auto const iter = [&](fs::path const& dir) -> bool
    {
        path = dir / source.file;
        return fs::exists(path);
    };

    if(iter(source.dir))
        return path;

    for(fs::path const& dir : compiler_options().code_dirs)
        if(iter(dir))
            return path;

    for(fs::path const& dir : compiler_options().nesfab_dirs)
        if(iter(dir))
            return path;

    throw std::runtime_error("Unable to stat file: " + source.file.string());
}

void file_contents_t::reset(unsigned file_i)
{
    m_size = 0;
    m_alloc.reset();
    m_source = nullptr;
    m_path = fs::path();
    m_private_globals = nullptr;
    m_private_groups = nullptr;

    // Set this first so that 'input()' can be used.
    m_file_i = file_i;

    if(file_i < compiler_options().source_names.size())
    {
        m_path = source_path(file_i);

        if(!read_binary_file(m_path.string().c_str(), [this](std::size_t size)
        {
            m_size = size + 2;
            m_alloc.reset(new char[m_size]);
            return reinterpret_cast<void*>(m_alloc.get());
        }))
        {
            throw std::runtime_error("Unable to open file: " + input().file.string());
        }

        m_alloc[m_size-1] = m_alloc[m_size-2] = '\0';
        m_source = m_alloc.get();
    }
    else
    {
        // Load a macro-generated file:

        unsigned const index = file_i - compiler_options().source_names.size();
        passert(index < macro_results.size(), index, macro_results.size());
        auto const& macro = macro_results[index];

        m_path = macro.path;
        m_size = macro.contents.size()+1;
        m_source = macro.contents.data();
        m_private_globals = &macro.private_globals;
        m_private_groups = &macro.private_groups;
        m_invoke = &macro.invoke;
    }
}
