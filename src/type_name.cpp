#include "type_name.hpp"

#include <exception>
#include <stdexcept>

#include "format.hpp"

std::string to_string(type_name_t type_name) 
{ 
    using namespace std::literals;
    switch(type_name)
    {
    default: 
        throw std::runtime_error(fmt("bad type name %", (int)type_name));
    case TYPE_TEA_THUNK:  return "TEA_THUNK"s; 
    case TYPE_PAA_THUNK:  return "PAA_THUNK"s; 
    case TYPE_STRUCT_THUNK: return "STRUCT_THUNK"s; 
    case TYPE_VOID:  return "Void"s;  
    case TYPE_BOOL:  return "Bool"s;  
    case TYPE_INT:   return "Int"s;  
    case TYPE_REAL:  return "Real"s;  
    case TYPE_F1:    return "F"s;  
    case TYPE_F2:    return "FF"s;  
    case TYPE_F3:    return "FFF"s;  
    case TYPE_U10:   return "U"s;  
    case TYPE_U20:   return "UU"s;  
    case TYPE_U30:   return "UUU"s;  
    case TYPE_U11:   return "UF"s;  
    case TYPE_U21:   return "UUF"s;  
    case TYPE_U31:   return "UUUF"s;  
    case TYPE_U12:   return "UFF"s;  
    case TYPE_U22:   return "UUFF"s;  
    case TYPE_U32:   return "UUUFF"s;  
    case TYPE_U13:   return "UFFF"s;  
    case TYPE_U23:   return "UUFFF"s;  
    case TYPE_U33:   return "UUUFFF"s;  
    case TYPE_S10:   return "S"s;  
    case TYPE_S20:   return "SS"s;  
    case TYPE_S30:   return "SSS"s;  
    case TYPE_S11:   return "SF"s;  
    case TYPE_S21:   return "SSF"s;  
    case TYPE_S31:   return "SSSF"s;  
    case TYPE_S12:   return "SFF"s;  
    case TYPE_S22:   return "SSFF"s;  
    case TYPE_S32:   return "SSSFF"s;  
    case TYPE_S13:   return "SFFF"s;  
    case TYPE_S23:   return "SSFFF"s;  
    case TYPE_S33:   return "SSSFFF"s;  
    case TYPE_TEA:  return "TEA"s; 
    case TYPE_PAA:  return "PAA"s; 
    case TYPE_STRUCT: return "STRUCT"s; 
    case TYPE_BANKED_APTR: return "BANKED_APTR"s;
    case TYPE_APTR: return "PTR"s;
    case TYPE_BANKED_CPTR: return "BANKED_CPTR"s;
    case TYPE_CPTR: return "CPTR"s;
    case TYPE_BANKED_MPTR: return "BANKED_MPTR"s;
    case TYPE_MPTR: return "MPTR"s;
    case TYPE_FN: return "FN"s;
    }
}

std::ostream& operator<<(std::ostream& o, type_name_t type_name)
{
    o << to_string(type_name);
    return o;
}
