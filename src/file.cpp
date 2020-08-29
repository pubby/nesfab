#include "file.hpp"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

std::vector<std::string> source_file_names;

file_contents_t::file_contents_t(unsigned file_i)
: m_file_i(file_i)
{
    std::string const& name = source_file_names[file_i];
    int fd = open(name.c_str(), O_RDONLY);
    struct stat sb;
    if(fstat(fd, &sb) == -1)
        throw std::runtime_error("Unable to stat file: " + name);
    m_source.reset(new char[sb.st_size + 1]);
    if(read(fd, reinterpret_cast<void*>(m_source.get()), sb.st_size) == -1)
        throw std::runtime_error("Unable to read file: " + name);
    m_source[sb.st_size] = '\0';
}

