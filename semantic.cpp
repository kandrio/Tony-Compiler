#include "ast.hpp"

void Id::sem() {
    SymbolEntry *e = st.lookup(var, T_BOTH);
    if(e == nullptr) {
        error(lineno, "Unknown variable \"%s\".", var.c_str());
    } 
    type = e->type;

    // Checking if var not in current scope, in order to save it for later;

    if(type->get_current_type() != TYPE_function && st.lookupCurentScope(var, T_VAR) == nullptr){
        TonyType *fun = st.getScopeFunction();
        fun->addPreviousScopeArg(var, type);
    }
}

void ArrayElement::sem(){
    atom->sem();
    if(atom->get_type()->get_current_type()!=TYPE_array){
        error(lineno, "Accessing array value of non-array object of type \"%s\".", atom->get_type()->toString().c_str());
    }
    expr->sem();
    if(expr->get_type()->get_current_type() != TYPE_int){
        error(lineno, "Index of an array must be an integer, received a \"%s\" instead.", expr->get_type()->toString().c_str());
    }

    type = atom->get_type()->get_nested_type();
}

void StringLiteral::sem(){
    type = new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr));
}

void CharConst::sem(){
    type = new TonyType(TYPE_char, nullptr);
}

void IntConst::sem(){
    type = new TonyType(TYPE_int, nullptr);
} 

void New::sem(){
    expr->sem();
    if(expr->get_type()->get_current_type() != TYPE_int){
        error(lineno, "Array size must be an integer, received \"%s\" instead", expr->get_type()->toString().c_str());
    }
    type = new TonyType(TYPE_array, type_of_elems);
}

void Nil::sem() {
    type = new TonyType(TYPE_list, new TonyType(TYPE_any, nullptr));
}

void Boolean::sem(){
    type = new TonyType(TYPE_bool, nullptr);
}

void BinOp::sem(){
    if (op == "+" || op == "-" || op == "*" || op == "/" || op == "mod") {
        left->sem();
        right->sem();
        if (left->get_type()->get_current_type() != (TYPE_int) || right->get_type()->get_current_type() != (TYPE_int)) {
        // TODO: We must be more specific in our errors. This is temporary.
        error(lineno, "Type mismatch: Expected \"int\" and \"int\", received \"%s\" and \"%s\" instead.",
              left->get_type()->toString().c_str(), right->get_type()->toString().c_str()  );
        }
        type = new TonyType(TYPE_int, nullptr);
    } else if (op == "=" || op == "<>" || op == "<" || op == ">" || op == "<=" || op == ">=") {
        left->sem();
        right->sem();
        if (!check_type_equality(left->get_type(), right->get_type())) {
        error(lineno, "Type mismatch: Expected same types, received \"%s\" and \"%s\" instead.",
               left->get_type()->toString().c_str(), right->get_type()->toString().c_str());
        }
        type = new TonyType(TYPE_bool, nullptr);
    } else if (op == "and" || op == "or") {
        left->sem();
        right->sem();
        if (left->get_type()->get_current_type() != (TYPE_bool) || right->get_type()->get_current_type() != (TYPE_bool)) {
        error(lineno, "Type mismatch: Expected \"bool\" and \"bool\", received \"%s\" and \"%s\" instead.",
              left->get_type()->toString().c_str(), right->get_type()->toString().c_str()  );
        }
        type = new TonyType(TYPE_bool, nullptr);
    } else if (op == "#") {
        left->sem();
        right->sem();

        if (right->get_type()->get_current_type() != TYPE_list) {
        error(lineno, "Type mismatch: Expected type \"list\", received \"%s\" instead.", right->get_type()->toString().c_str());
        }

        if (right->get_type()->get_nested_type() != nullptr && 
            !check_type_equality(left->get_type(), right->get_type()->get_nested_type())) {
        error(lineno, "Type mismatch: Expression on the left of '#' operator must be have the same type as the elements of the list on the right of the operator.");
        }
        type = new TonyType(TYPE_list, left->get_type());
    } else {
        error(lineno, "Wrong binary operator.");
    }
}

void UnOp::sem(){
    if (op == "+" || op == "-") {
        right->sem();
        if (right->get_type()->get_current_type() != TYPE_int) {
        error(lineno, "Type mismatch: Expected type \"int\", received \"%s\" instead.", right->get_type()->toString().c_str());
        }
        type = new TonyType(TYPE_int, nullptr);
    } else if (op == "not") { 
        right->sem();
        if (right->get_type()->get_current_type() != TYPE_bool){
        error(lineno, "Type mismatch: Expected type \"bool\", received \"%s\" instead.", right->get_type()->toString().c_str());
        }
        type = new TonyType(TYPE_bool, nullptr);
    } else if (op == "head") {
        // Compute the type of the expression.
        right->sem();
        TonyType *operand_type = right->get_type();
        // Check that the expression is a list.
        if (operand_type->get_current_type() != TYPE_list) {
            error(lineno, "Type mismatch: Expected type \"list\", received \"%s\" instead.", right->get_type()->toString().c_str());
        }
        // Check that the expression is not the 'nil' constant (empty list).
        if (is_nil_constant(operand_type)) {
            error(lineno, "Type mismatch: Expression after 'head' cannot be a 'nil' list.");
        }
        // The nested type of the expression is actually the type of the list's elements.
        // NOTE: Maybe we should create a new type here, that is a copy of: 
        // `operand->get_nested_type()`. This is because, if we add a destructor in class
        // `Expr`, then the value of `type` may be deleted by multiple nodes.
        type = operand_type->get_nested_type(); 
    } else if (op == "tail") {
        // Compute the type of the expression.
        right->sem();
        TonyType *operand_type = right->get_type();
        // Check that the expression is a list.
        if (operand_type->get_current_type() != TYPE_list) {
            error(lineno, "Type mismatch: Expected type \"list\", received \"%s\" instead.", right->get_type()->toString().c_str());
        }
        // Check that the expression is not the 'nil' constant (empty list).
        if (is_nil_constant(operand_type)) {
            error(lineno, "Type mismatch: Expression after 'tail' cannot be a 'nil' list.");
        }
        // The type of the expression is the type of the tail.
        type = operand_type; 
    } else if (op == "nil?") {
        // Compute the type of the expression.
        right->sem();
        TonyType *operand_type = right->get_type();
        if(operand_type->get_current_type() != TYPE_list) {
            error(lineno, "Type mismatch: Expected type \"list\", received \"%s\" instead.", right->get_type()->toString().c_str());
        }
        type = new TonyType(TYPE_bool, nullptr); 
    }
}

void VarList::sem(){
    if(isRef) type->setPassMode(REF);
    for (Id * i : ids) {i->set_type(type); i->insertIntoScope(T_VAR);}
}       

void Formal::sem(){
    var_list->setIsRef(is_ref);
    var_list->sem();
}

void FormalList::sem(){
    for (Formal *f: formals) f->sem();
}

void Header::sem() {}

void Header::semHeaderDecl(){
    // Get arguments if any
    std::vector<TonyType *> args;
    if (formals){
        args = formals->getArgs();
    }
    TonyType *fun;
    if (!isTyped){
        fun = new TonyType(TYPE_function, nullptr, new TonyType(TYPE_void, nullptr), args, true);
    }else{
        fun = new TonyType(TYPE_function, nullptr, type, args, true); 
    }
    id->set_type(fun);
    id->insertIntoScope(T_FUNC);  
}

void Header::semHeaderDef(){
    // Get arguments if any
    if (formals) formals->sem();

    std::vector<TonyType *> args;
    if (formals){
        args = formals->getArgs();
    }

    TonyType *fun;
    if (!isTyped){
        fun = new TonyType(TYPE_function, nullptr, new TonyType(TYPE_void, nullptr), args, false);
    }else{
        fun = new TonyType(TYPE_function, nullptr, type, args, false); 
    }
        
    // Check if function is previously defined
    SymbolEntry *e = st.lookupParentScope(id->getName(), T_FUNC);


    if(e != nullptr) {
        //Function either declared or defined
        TonyType *t = e->type;
        if(t->get_current_type()!= TYPE_function){
            error(lineno, "Found declared function that is not type function.");
        }

        if(!t->isDeclared()){
        //this means function is redefined
            error(lineno, "Multiple definitions of function \"%s()\" in the same scope.", getName().c_str());
        }

        // This means function was previously declared
        //TODO: TonyType check if the vars in declaration match the definition
        t->toggleDeclDef();
        if(!check_type_equality(t, fun)){
            error(lineno, "Definition of function \"%s\" different from declaration.");
        }
        id->set_type(t);
        st.setScopeFunction(t);
        return;
    }
    id->set_type(fun);
    st.setScopeFunction(fun);


    if(st.hasParentScope()){
        id->insertIntoParentScope(T_FUNC);
    }
}

void Return::sem(){
    ret_expr->sem();
    if(!check_type_equality(ret_expr->get_type(), st.getCurrentScopeReturnType())){
        error(lineno, "Return type mismatch: Expected type \"%s\", received \"%s\" instead.",
        ret_expr->get_type()->toString().c_str(), st.getCurrentScopeReturnType()->toString().c_str());
    }
    st.setScopeHasReturn();
}

void Exit::sem(){
    TonyType *t = st.getCurrentScopeReturnType();
    if(t->get_current_type() != TYPE_void){
        error(lineno, "Found 'exit' statement in a typed function.");
    }
}

void StmtBody::sem(){
    for (Stmt *s : stmts) {
        s->sem();
    }
}

void Assign::sem() {
    atom->sem();
    expr->sem();
    if (!check_type_equality(expr->get_type(), atom->get_type())) {
        error(lineno, "Assignment type mismatch: Cannot assign type \"%s\" to type \"%s\".",
        expr->get_type()->toString().c_str(), atom->get_type()->toString().c_str());
    }
    if(!atom->isLvalue()){
        error(lineno, "Atom is not a valid l-value.");
    }   
}

void Skip::sem(){
    // Intentionally left empty
}

void If::sem(){

    if(condition != nullptr) {
        condition->sem();
        if(condition->get_type()->get_current_type() != TYPE_bool)
            error(lineno, "Type mismatch : Expected condition type \"bool\", received \"%s\" instead.", condition->get_type()->toString().c_str());
        }
    stmt_body->sem();
    if(next_if != nullptr) next_if->sem();
}

void SimpleList::sem() {
    for (Simple *s : simples) {
        s->sem();
    }
}

void For::sem(){
    initializations->sem();
    condition->sem();
    if(condition->get_type()->get_current_type() != TYPE_bool){
        if(condition->get_type()->get_current_type() != TYPE_bool)
            error(lineno, "Type mismatch : Expected condition type \"bool\", received \"%s\" instead.", condition->get_type()->toString().c_str());
    }
    steps->sem();
    stmt_body->sem();
}

void ExprList::sem(){
    for (auto i : expressions) i->sem();
}

void FunctionCall::sem(){
    name->sem();

    if (name->get_type()->get_current_type() != TYPE_function)
        error(lineno, "Function call, expected a function for name \"%s\", received a \"%s\" instead.",
        name->getName().c_str(), name->get_type()->toString().c_str());
    std::vector<TonyType *> args = name->get_type()->get_function_args();
    std::vector<Expr*> expressions;
    if (hasParams){
        expressions = params->get_expr_list();
    }

    if(expressions.size() != args.size()){
        error(lineno, "Function call: Exptected %d arguments, received %d", args.size(), expressions.size() );
    }

    for (int i=0; i<(int) args.size();++i){
        expressions[i]->sem();
        if(!check_type_equality(args[i],expressions[i]->get_type())){
            error(lineno, "Function call: Expected type \"%s\" for positional argument %d, received \"%s\" instead",
            args[i]->toString().c_str(), i+1, expressions[i]->get_type()->toString().c_str());
        }      
    }
    type = name->get_type()->get_return_type();

    std::map<std::string, TonyType*> previous = name->get_type()->getPreviousScopeArgs();
    

    TonyType *fun = st.getScopeFunction();
    for(auto it:previous){
        if(st.lookupCurentScope(it.first, T_VAR) == nullptr)
            fun->addPreviousScopeArg(it.first, it.second);
    }

}

void FunctionDeclaration::sem(){
    header->semHeaderDecl();
    SymbolEntry *e = st.lookupCurentScope(header->getName(), T_FUNC);
    if(e != nullptr){
        functionType = e->type;
    }else{
        error(lineno, "Couldn't find function declaration for function \"%s\"", header->getName().c_str());
    }
}

void FunctionDefinition::sem(){
    // Global Scope including functions first
    bool isFirstScope = false;
    if(!st.hasParentScope()){
        isFirstScope = true;
        st.openScope(new TonyType(TYPE_void, nullptr));
        initFunctions();
        st.openScope(new TonyType(TYPE_void, nullptr));
    }
    TonyType *prevFunctionType = st.getScopeFunction();
    st.openScope(header->getType());

    header->semHeaderDef();

    functionType = st.getScopeFunction();

    for (AST *a : local_definitions) a->sem();
    body->sem();
    if(header->getIsTyped() && !st.getScopeHasReturn()){
        error(lineno, "No return value on typed function.");
    }

    std::map<std::string, TonyType*> previous = functionType->getPreviousScopeArgs();

    // Transfering previous scope variables
    st.closeScope();

    for(auto it:previous){
        if(st.lookupCurentScope(it.first, T_VAR) == nullptr)
        prevFunctionType->addPreviousScopeArg(it.first, it.second);
    }
/* 
    std::cout << "\n=============================================\nI am function << \"" << header->getName() << "()\" and these are my previous scope variables : ";
    for(auto it:previous){
        std::cout << it.first << ", ";
    }

    std::cout << "\n=============================================\n";
 */

    //Closing Global Scope
    if(isFirstScope) {
        st.closeScope();
    st.closeScope();
    }
}

void FunctionDefinition::initFunctions(){
    //puti
    std::vector<TonyType*> v {new TonyType(TYPE_int, nullptr)};
    st.insert(std::string("puti"), new TonyType(TYPE_function, nullptr,new TonyType(TYPE_void, nullptr), v, true), T_FUNC);
    //putc
    v.clear();
    v.push_back(new TonyType(TYPE_char, nullptr));
    st.insert(std::string("putc"), new TonyType(TYPE_function, nullptr,new TonyType(TYPE_void, nullptr), v, true), T_FUNC);

    //putb
    v.clear();
    v.push_back(new TonyType(TYPE_bool, nullptr));
    st.insert(std::string("putb"), new TonyType(TYPE_function, nullptr,new TonyType(TYPE_void, nullptr), v, true), T_FUNC);

    //puts
    v.clear();
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    st.insert(std::string("puts"), new TonyType(TYPE_function, nullptr,new TonyType(TYPE_void, nullptr), v, true), T_FUNC);

    //geti
    v.clear();
    st.insert(std::string("geti"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_int, nullptr), v, true), T_FUNC);

    //getb
    v.clear();
    st.insert(std::string("getb"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_bool, nullptr), v, true), T_FUNC);

    //getc
    v.clear();
    st.insert(std::string("getc"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_char, nullptr), v, true), T_FUNC);

    //gets
    v.clear();
    v.push_back(new TonyType(TYPE_int, nullptr));
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    st.insert(std::string("gets"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_void, nullptr), v, true), T_FUNC);

    //abs
    v.clear();
    v.push_back(new TonyType(TYPE_int, nullptr));
    st.insert(std::string("abs"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_int, nullptr), v, true), T_FUNC);

    //ord
    v.clear();
    v.push_back(new TonyType(TYPE_char, nullptr));
    st.insert(std::string("ord"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_int, nullptr), v, true), T_FUNC);

    //chr
    v.clear();
    v.push_back(new TonyType(TYPE_int, nullptr));
    st.insert(std::string("chr"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_char, nullptr), v, true), T_FUNC);

    //strlen
    v.clear();
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    st.insert(std::string("strlen"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_int, nullptr), v, true), T_FUNC);

    //strcmp
    v.clear();
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    st.insert(std::string("strcmp"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_int, nullptr), v, true), T_FUNC);

    //strcpy
    v.clear();
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    st.insert(std::string("strcpy"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_void, nullptr), v, true), T_FUNC);

    //strcat
    v.clear();
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    v.push_back(new TonyType(TYPE_array, new TonyType(TYPE_char, nullptr)));
    st.insert(std::string("strcat"), new TonyType(TYPE_function, nullptr, new TonyType(TYPE_void, nullptr), v, true), T_FUNC);
}