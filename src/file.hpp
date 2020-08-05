#ifndef FILE_HPP
#define FILE_HPP

#include <memory>
#include <string>
#include <vector>

// Holds the contents of a file in a buffer and its filename.
struct file_contents_t
{
public:
    file_contents_t() = delete;
    // Reads the file from disk.
    file_contents_t(std::string filename);
    file_contents_t(file_contents_t&&) = default;
    file_contents_t& operator=(file_contents_t&&) = default;

    std::string const& filename() const { return m_filename; }
    char const* source() const { return m_source.get(); }
private:
    std::unique_ptr<char[]> m_source;
    std::string m_filename;
};

// Files don't have any thread-synchronization, so they must be loaded
// before multi-threading occurs.

extern std::vector<file_contents_t> _files;

void load_files(std::string* begin, std::string* end);
inline file_contents_t const& get_file(unsigned i) { return _files[i]; }
inline std::size_t num_files() { return _files.size(); }

#endif
