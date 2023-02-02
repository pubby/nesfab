#ifndef STRING_HPP
#define STRING_HPP

#include <algorithm>
#include <cctype>
#include <string>

// Utility functions for working with strings.

inline std::string to_lower(std::string str)
{ 
    std::transform(str.begin(), str.end(), str.begin(),
        [](unsigned char c){ return std::tolower(c); });
    return str;
}

inline std::string to_upper(std::string str)
{ 
    std::transform(str.begin(), str.end(), str.begin(),
        [](unsigned char c){ return std::toupper(c); });
    return str;
}

#endif
