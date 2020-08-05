#ifndef OPTIONS_HPP
#define OPTIONS_HPP

// Compiler options.

struct options_t
{
    unsigned num_threads = 5;
    bool graphviz = true;
};

extern options_t _options;
inline options_t const& compiler_options() { return _options; }

#endif
