#ifndef FILE_HPP
#define FILE_HPP

#include <memory>
#include <string>
#include <vector>
#include <cassert>

extern std::vector<std::string> source_file_names;

// Holds the contents of a file in a buffer and its filename.
struct file_contents_t
{
public:
    file_contents_t() = default;

    // Reads the file from disk.
    explicit file_contents_t(unsigned file_i) { reset(file_i); }

    file_contents_t(file_contents_t&&) = default;
    file_contents_t& operator=(file_contents_t&&) = default;

    std::string const& name() const { assert(m_source); return source_file_names[m_file_i]; }
    unsigned index() const { assert(m_source); return m_file_i; }
    char const* source() const { return m_source.get(); }
    std::size_t size() const { return m_size; }

    void clear() { m_source.reset(); m_size = 0; }
    void reset(unsigned file_i);
private:
    unsigned m_file_i = 0;
    unsigned m_size = 0;
    std::unique_ptr<char[]> m_source;
};

#endif
