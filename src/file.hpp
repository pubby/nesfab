#ifndef FILE_HPP
#define FILE_HPP

#include <memory>
#include <string>
#include <vector>
#include <cassert>
#include <cstdio>
#include <filesystem>
#include <functional>
#include <cstdint>

#include "options.hpp"

namespace fs = ::std::filesystem;

struct pstring_t;

bool resource_path(fs::path preferred_dir, fs::path name, fs::path& result);
bool read_binary_file(char const* filename, std::function<void*(std::size_t)> const& alloc);
std::vector<std::uint8_t> read_binary_file(std::string filename, pstring_t at);


// Holds the contents of a file in a buffer and its filename.
struct file_contents_t
{
public:
    file_contents_t() = default;

    // Reads the file from disk.
    explicit file_contents_t(unsigned file_i) { reset(file_i); }

    file_contents_t(file_contents_t&&) = default;
    file_contents_t& operator=(file_contents_t&&) = default;

    fs::path const& input_path() const { return compiler_options().source_names[m_file_i]; }
    fs::path const& path() const { assert(m_source); return m_path; }
    std::string name() const { return fs::relative(path()).string(); }
    unsigned index() const { return m_file_i; }
    char const* source() const { return m_source.get(); }
    std::size_t size() const { return m_size; }

    void clear() { m_source.reset(); m_size = 0; }
    void reset(unsigned file_i);
private:
    unsigned m_file_i = 0;
    int m_size = 0;
    fs::path m_path;
    std::unique_ptr<char[]> m_source;
};

#endif
