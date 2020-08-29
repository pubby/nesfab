#ifndef FILE_HPP
#define FILE_HPP

#include <memory>
#include <string>
#include <vector>

extern std::vector<std::string> source_file_names;

// Holds the contents of a file in a buffer and its filename.
struct file_contents_t
{
public:
    file_contents_t() = delete;
    // Reads the file from disk.
    explicit file_contents_t(unsigned file_i);
    file_contents_t(file_contents_t&&) = default;
    file_contents_t& operator=(file_contents_t&&) = default;

    std::string const& name() const { return source_file_names[m_file_i]; }
    unsigned index() const { return m_file_i; }
    char const* source() const { return m_source.get(); }
private:
    unsigned m_file_i;
    std::unique_ptr<char[]> m_source;
};

#endif
