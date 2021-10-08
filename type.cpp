#include <iostream>
#include "type.hpp"

bool check_type_equality(TonyType* type1, TonyType* type2) {
    if (type1->get_current_type() == TYPE_any || type2->get_current_type() == TYPE_any) {
        return true;
    }
    if (type1->get_current_type() != type2->get_current_type()) {
        return false;
    }
    if ((type1->get_nested_type() == nullptr && type2->get_nested_type() != nullptr) || 
        (type1->get_nested_type() != nullptr && type2->get_nested_type() == nullptr)) {
        return false;
    }

    // function type checks
    if(type1->get_current_type()==TYPE_function){
        if(!check_type_equality(type1->get_return_type(), type2->get_return_type()))
            return false;
        std::vector<TonyType *> args1 = type1->get_function_args();
        std::vector<TonyType *> args2 = type2->get_function_args();
        if(args1.size() != args2.size())
            return false;
        for(int i=0; i< (int) args1.size(); i++){
            if (!check_type_equality(args1[i], args2[i]))
                return false;
        }
        // all good until here, rest should be empty
    }

    if (type1->get_nested_type() == nullptr && type2->get_nested_type() == nullptr) {
        return true;
    }
    return check_type_equality(type1->get_nested_type(), type2->get_nested_type());
}

bool is_nil_constant(TonyType *type) {
    if (type->get_current_type() != TYPE_list) {
        return false;
    }
    if (type->get_nested_type() == nullptr) {
        return false;
    }
    if (type->get_nested_type()->get_current_type() == TYPE_any) {
        return true;
    }
    return false;
} 
