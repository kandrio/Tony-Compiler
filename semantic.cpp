#include "ast.hpp"

void Id::sem() {
    SymbolEntry *e = st.lookup(var, T_BOTH);
    if(e == nullptr) {
        yyerror("Variable \"%s\" not found!", var.c_str());
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
        yyerror("Accessing array value of non-array object.");
    }
    expr->sem();
    if(expr->get_type()->get_current_type() != TYPE_int){
        yyerror("Index of an array must be an integer.");
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
        yyerror("Array size must be an integer.");
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
        if (!left->type_check(TYPE_int) || !right->type_check(TYPE_int)) {
        // TODO: We must be more specific in our errors. This is temporary.
        yyerror("TonyType mismatch. Both expressions must be of type 'int'.\n");
        }
        type = new TonyType(TYPE_int, nullptr);
    } else if (op == "=" || op == "<>" || op == "<" || op == ">" || op == "<=" || op == ">=") {
        left->sem();
        right->sem();
        if (!check_type_equality(left->get_type(), right->get_type())) {
        yyerror("TonyType mismatch. Expressions must have the same type.\n");
        }
        type = new TonyType(TYPE_bool, nullptr);
    } else if (op == "and" || op == "or") {
        if (!left->type_check(TYPE_bool) || !right->type_check(TYPE_bool)) {
        yyerror("TonyType mismatch. Both expressions must be of type 'bool'.\n");
        }
        type = new TonyType(TYPE_bool, nullptr);
    } else if (op == "#") {
        left->sem();
        right->sem();

        if (right->get_type()->get_current_type() != TYPE_list) {
        yyerror("TonyType mismatch. Expression on the right of '#' operator \
                must be a list.\n");
        }

        if (right->get_type()->get_nested_type() != nullptr && 
            !check_type_equality(left->get_type(), right->get_type()->get_nested_type())) {
        yyerror("TonyType mismatch. Expression on the left of '#' operator \
                must be have the same type as the elements of the list on the right \
                of the operator.\n");
        }
        type = new TonyType(TYPE_list, left->get_type());
    } else {
        yyerror("Wrong binary operator.\n");
    }
}

void UnOp::sem(){
    if (op == "+" || op == "-") {
        if (!right->type_check(TYPE_int)) {
        yyerror("TonyType mismatch. Expression must be of type 'int'.");
        }
        type = new TonyType(TYPE_int, nullptr);
    } else if (op == "not") { 
        if (!right->type_check(TYPE_bool)) {
        yyerror("TonyType mismatch. Expression must be of type 'bool'.");
        }
        type = new TonyType(TYPE_bool, nullptr);
    } else if (op == "head") {
        // Compute the type of the expression.
        right->sem();
        TonyType *operand_type = right->get_type();
        // Check that the expression is a list.
        if (operand_type->get_current_type() != TYPE_list) {
        yyerror("TonyType mismatch. Expression after 'head' must be a list.");
        }
        // Check that the expression is not the 'nil' constant (empty list).
        if (is_nil_constant(operand_type)) {
        yyerror("TonyType mismatch. Expression after 'head' cannot be a 'nil' list.");
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
        yyerror("TonyType mismatch. Expression after 'tail' must be a list.");
        }
        // Check that the expression is not the 'nil' constant (empty list).
        if (is_nil_constant(operand_type)) {
        yyerror("TonyType mismatch. Expression after 'tail' cannot be a 'nil' list.");
        }
        // The type of the expression is the type of the tail.
        type = operand_type; 
    } else if (op == "nil?") {
        // Compute the type of the expression.
        right->sem();
        TonyType *operand_type = right->get_type();
        if(operand_type->get_current_type() != TYPE_list) {
        yyerror("TonyType mismatch. Expression after 'nil?' must be a list.");
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
        yyerror("Expected type function.");
        }

        if(!t->isDeclared()){
        //this means function is redefined
        yyerror("Multiple definitions of function in the same scope!");
        }

        // This means function was previously declared
        //TODO: TonyType check if the vars in declaration match the definition
        t->toggleDeclDef();
        if(!check_type_equality(t, fun)){
        yyerror("Function definition different from declaration");
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
        yyerror("Return type different than the one declared.");
        }
    st.setScopeHasReturn();
}

void Exit::sem(){
    TonyType *t = st.getCurrentScopeReturnType();
    if(t->get_current_type() != TYPE_void){
        yyerror("Found 'exit' statement in a typed function.");
    }
}

void StmtBody::sem(){
    for (Stmt *s : stmts) {
        s->sem();
    }
}

void Assign::sem() {
    atom->sem();
    if (!expr->type_check(atom->get_type())) {
        yyerror("Atom on the left and expression on the right should have the same type during assignment.");
    }
    if(!atom->isLvalue()){
        yyerror("Atom is not a valid l-value.");
    }   
}

void Skip::sem(){
    // Intentionally left empty
}

void If::sem(){
    if(condition != nullptr && !condition->type_check(TYPE_bool)) {
        yyerror("TonyType mismatch. 'If-condition' is not boolean.");
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
        yyerror("For condition is not boolean.");
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
        yyerror("Function call, expected a function");
    std::vector<TonyType *> args = name->get_type()->get_function_args();
    std::vector<Expr*> expressions;
    if (hasParams){
        expressions = params->get_expr_list();
    }

    if(expressions.size() != args.size()){
        yyerror("Function call: Different number of arguments than expected");
    }

    for (int i=0; i<(int) args.size();++i){
        expressions[i]->sem();
        if(!check_type_equality(args[i],expressions[i]->get_type())){
        yyerror("Function call: Different argument type than expected");
        }      
    }
    type = name->get_type()->get_return_type();
}

void FunctionDeclaration::sem(){
    header->semHeaderDecl();
    SymbolEntry *e = st.lookupCurentScope(header->getName(), T_FUNC);
    if(e != nullptr){
        functionType = e->type;
    }else{
        yyerror("Fatal error: Couldn't find function declaration.");
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
        yyerror("No return value on typed function.");
    }

    std::map<std::string, TonyType*> previous = functionType->getPreviousScopeArgs();

    // Transfering previous scope variables
    st.closeScope();

    for(auto it:previous){
        if(st.lookupCurentScope(it.first, T_VAR) == nullptr)
        prevFunctionType->addPreviousScopeArg(it.first, it.second);
    }

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