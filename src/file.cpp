#include "file.hpp"

#include <cassert>
#include <cstdio>
#include <stdexcept>

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
    std::vector<std::uint8_t> vec;

    if(!read_binary_file(filename.c_str(), [&](std::size_t size)
    {
        vec.resize(size);
        return vec.data();
    }))
    {
        compiler_error(at, fmt("Unable to read: %", filename));
    }

    return vec;
}

void file_contents_t::reset(unsigned file_i)
{
    assert(file_i < compiler_options().source_names.size());

    // Set this first so that 'input_path()' can be used.
    m_file_i = file_i;

    for(fs::path const& dir : compiler_options().code_dirs)
    {
        m_path = (dir / input_path());

        if(!read_binary_file(m_path.c_str(), [this](std::size_t size)
        {
            m_size = size + 2;
            m_source.reset(new char[m_size]);
            return reinterpret_cast<void*>(m_source.get());
        }))
        {
            continue;
        }

        m_source[m_size-1] = m_source[m_size-2] = '\0';
        return;
    }

    m_size = 0;
    m_source.reset();
    m_path = fs::path();
    throw std::runtime_error("Unable to open file: " + input_path().string());
}
