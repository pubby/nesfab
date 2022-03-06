#ifndef OPTIONS_HPP
#define OPTIONS_HPP

// Compiler options.

struct options_t
{
    int num_threads = 1;
    int time_limit = 1000;
    bool optimize = false;
    bool graphviz = false;
};

extern options_t _options;
inline options_t const& compiler_options() { return _options; }

#endif
