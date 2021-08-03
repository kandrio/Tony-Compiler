#include <iostream>
#include "type.hpp"

using namespace std;

bool check_type_equality(Type* type1, Type* type2) {
    if (type1->get_current_type() != type2->get_current_type()) {
        return false;
    }
    if ((type1->get_nested_type() == nullptr && type2->get_nested_type() != nullptr) || 
        (type1->get_nested_type() != nullptr && type2->get_nested_type() == nullptr)) {
        return false;
    }
    if (type1->get_nested_type() == nullptr && type2->get_nested_type() == nullptr) {
        return true;
    }
    return check_type_equality(type1->get_nested_type(), type2->get_nested_type());
}
