#ifndef __TYPE_HPP___
#define __TYPE_HPP___
#pragma once

#include <iostream>
#include <vector>

enum TypeBlock {TYPE_int, TYPE_bool, TYPE_char, TYPE_array, TYPE_list, TYPE_function, TYPE_void, TYPE_any};
void yyerror(const char *msg, ...);

class TonyType {
public:
    TonyType(TypeBlock current, TonyType *nested): current_type(current), nested_type(nested) {}
    TonyType(TypeBlock current, TonyType *nested, TonyType *ret, std::vector<TonyType *> args, bool dec): 
    current_type(current), nested_type(nested), returnType(ret), function_args(args), declDef(dec){    }
    ~TonyType() {delete nested_type;};
    TypeBlock get_current_type() {
        return current_type;
    }
    TonyType* get_nested_type() {
        return nested_type;
    }

    TonyType *get_return_type() {
        if (current_type != TYPE_function) yyerror("No return type for a non function type.");
        return returnType;
    }

    int get_array_size() {
        if (current_type != TYPE_array) yyerror("No array size for non-array type.");
        return size;
    }

    void set_array_size(int n) {
      if (current_type != TYPE_array) yyerror("No array size for non-array type.");
      size = n;
    }

    // TODO: Implement this for all types
    int get_data_size_of_type() {
      /* Return the size (in Bytes) of an object that has this type. */
      switch(current_type) {
        case TYPE_int: return 4;
        case TYPE_char: return 1;
        case TYPE_bool: return 1;
        case TYPE_array: return size*(nested_type->get_data_size_of_type());
        case TYPE_list: return 8;
        case TYPE_any: return 4;
        default: return 0;
      }
    }

    /*
     * Types have their own hash string key so that they can be found in the
     * `llvm_list_types` catalog (if they are created first).
     * 
     * Example: list [list [char]] : "list_list_char"
     */
    std::string createHashKeyForType() {
      switch(get_current_type()) {
        case TYPE_int: return std::string("int");
        case TYPE_bool: return std::string("bool");
        case TYPE_char: return std::string("char");
        case TYPE_any: return std::string("int");
        case TYPE_list: {
          return std::string("list_") + nested_type->createHashKeyForType();
        }
        default: yyerror("Cannot have that type in a list.");
      }
      return std::string("error");
    }

    std::vector<TonyType *> get_function_args (){
      return function_args;
    }
    bool isDeclared() {
      return declDef;
    }
    void toggleDeclDef(){
      declDef = !declDef;
    }

protected:
    TypeBlock current_type;
    TonyType* nested_type;
    
    //For functions only
    TonyType *returnType;
    std::vector<TonyType *> function_args;
    bool declDef;

    //For arrays only
    int size;
};

bool check_type_equality(TonyType* type1, TonyType* type2);

inline std::ostream& operator<< (std::ostream &out, TonyType* t) {
  TonyType* curr = t;
  while (curr != nullptr) {
    switch (curr->get_current_type())
    {
      case TYPE_int: out << "\"int\""; break;
      case TYPE_bool: out << "\"bool\""; break;
      case TYPE_char: out << "\"char\""; break;
      case TYPE_array: out << "array: "; break;
      case TYPE_list: out << "list: "; break;
      case TYPE_function: {
        out << "function ->" << curr->get_return_type() << ", parameters (";
        for (auto i: curr->get_function_args()) out << i << ", ";
        out << ")";} 
      break;
      case TYPE_void: out << "void "; break;
      default: out << "\"invalid\""; break;
    }
    curr = curr->get_nested_type();
  }
  return out;
}

#endif