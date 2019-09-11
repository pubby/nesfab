#include "file.hpp"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

file_contents_t::file_contents_t(std::string filename)
: m_filename(std::move(filename))
{
    int fd = open(m_filename.c_str(), O_RDONLY);
    struct stat sb;
    if(fstat(fd, &sb) == -1)
        throw std::runtime_error("Unable to stat file.");
    m_source.reset(new char[sb.st_size + 1]);
    if(read(fd, reinterpret_cast<void*>(m_source.get()), sb.st_size) == -1)
        throw std::runtime_error("Unable to read file.");
    m_source[sb.st_size] = '\0';
}

unsigned open_file(std::string name)
{
    files.emplace_back(std::move(name));
    return files.size() - 1;
}

std::vector<file_contents_t> files;
