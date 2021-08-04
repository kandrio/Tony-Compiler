#ifndef __TYPE_HPP___
#define __TYPE_HPP___
#pragma once

#include <iostream>

enum TypeBlock {TYPE_int, TYPE_bool, TYPE_char, TYPE_array, TYPE_list};

class Type {
public:
    Type(TypeBlock current, Type *nested): current_type(current), nested_type(nested) {}
    ~Type() {delete nested_type;};
    TypeBlock get_current_type() {
        return current_type;
    }
    Type* get_nested_type() {
        return nested_type;
    }
private:
    TypeBlock current_type;
    Type* nested_type;
};

bool check_type_equality(Type* type1, Type* type2);

inline std::ostream& operator<< (std::ostream &out, Type* t) {
  Type* curr = t;
  while (curr != nullptr) {
    switch (curr->get_current_type())
    {
      case TYPE_int: out << "\"int\""; break;
      case TYPE_bool: out << "\"bool\""; break;
      case TYPE_char: out << "\"char\""; break;
      case TYPE_array: out << "array: "; break;
      case TYPE_list: out << "list: "; break;
      default: out << "\"invalid\""; break;
    }
    curr = curr->get_nested_type();
  }
  return out;
}

#endif