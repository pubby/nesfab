#ifndef CTAGS_HPP
#define CTAGS_HPP

#include <ostream>
#include <filesystem>

namespace fs = ::std::filesystem;

void write_ctags(FILE* fp, fs::path ctags_path);

#endif
