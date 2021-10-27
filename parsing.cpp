#include "ast.hpp"

void Id::printOn(std::ostream &out) {
    out << "<Id name=\"" << var << "\"> ";
}

void ArrayElement::printOn(std::ostream &out) {
    out << "\n<ArrayElement>\n" << *atom << "\n" << *expr << "\n</ArrayElement>\n";
}

void StringLiteral::printOn(std::ostream &out) {
    out << "<String value=\"" << strlit << "\"> ";
  }

void CharConst::printOn(std::ostream &out) {
    out << "<CharConst value='"<< char_const << "\' ascii="<< (int) char_const << "> ";
}

void IntConst::printOn(std::ostream &out) {
    out << "<IntConst value=" << num << "> ";
}

void New::printOn(std::ostream &out)  {
    out << "<New> " << type_of_elems << *expr << "</New> ";
}

void Nil::printOn(std::ostream &out)  {
    out << "<Nil> ";
}

void Boolean::printOn(std::ostream &out)  {
    out << "<Boolean value=" << boolean_value << "> ";  
}

void BinOp::printOn(std::ostream &out)  {
    out << "\n<Binop op=\"" << op << "\">\n" << *left << *right << "\n</BinOp>\n";
}

void UnOp::printOn(std::ostream &out)  {
    out << "\n<UnOp op=\"" << op << "\">\n" << *right << "\n</UnOp>\n";
}

 void VarList::printOn(std::ostream &out)  {
    out << "\n<VarList>\n";
    out << type;
    for (Id * i : ids) {
      out << *i;
    }
    out << "\n</VarList>\n";
}

void Formal::printOn(std::ostream &out)  {
    out << "\n<Formal isRef=\"" << (is_ref ? "yes" : "no") << "\">\n" << *var_list << "</Formal>";
}

void FormalList::printOn(std::ostream &out)  {
    out << "\n<FormalList>\n";

    for (Formal * f : formals) {
        out << *f;
    }
    out << "\n</FormalList>\n";
}

void Header::printOn(std::ostream &out)  {
    out << "<Header>\n"; 
    if(!isTyped) {
      out << "<NoType>";
    } else {
      out << type;
    }
    out << *id; 
    if(!formals) {
      out << "<NoFormalList>";
    } else {
      out << *formals;
    }
    out << "\n</Header>\n";
}

void Return::printOn(std::ostream &out)  {
    out << "\n<Return>\n" << *ret_expr << "\n</Return>\n";
}

void Exit::printOn(std::ostream &out)  {
    out << "\n<Exit>\n";
}

void StmtBody::printOn(std::ostream &out)  {
    out << "\n<StmtBody>\n";
    for (Stmt *s : stmts) {
      out << *s;
    }
    out << "\n</StmtBody>\n";
}

void Assign::printOn(std::ostream &out)  {
    out << "\n<Assign>\n" << *atom << *expr << "\n</Assign>\n";
}

void Skip::printOn(std::ostream &out)  {
    out << "\n<Skip>\n";
}

void If::printOn(std::ostream &out)  {
    out << "\n<If>\n"; 
    if (condition != nullptr) out << *condition;
    out << *stmt_body;
    if (next_if != nullptr) out << *next_if;
    out << "\n</If>\n";
}

void SimpleList::printOn(std::ostream &out)  {
    out << "\n<SimplesList>\n";
    for (Simple *s : simples) {
        out << *s;
    }
    out << "\n</SimpleList>\n";
}

void For::printOn(std::ostream &out)  {
    out << "\n<For>\n" << *initializations << *condition << *steps  << *stmt_body << "\n</For>\n";
}

void ExprList::printOn(std::ostream &out)  {
    out << "\n<ExprList>\n";
    for (Expr *e : expressions) {
      out << *e;
    }
    out << "\n</ExprList>\n";
}

void FunctionCall::printOn(std::ostream &out)  {
    if(!hasParams)
        out << "\n<FunctionCall>\n" << *name << "\n</FunctionCall>\n";
    else
        out << "\n<FunctionCall>\n" << *name << *params << "\n</FunctionCall>\n";
}

void FunctionDeclaration::printOn(std::ostream &out)  {
    out << "\n<FunctionDeclaration>\n" << *header << "\n</FunctionDeclaration>\n" ;
}

void FunctionDefinition::printOn(std::ostream &out)  {
    out << "\n<FunctionDefinition>\n" << *header; 
    for (AST *a : local_definitions) out << *a;
    out << *body << "\n</FunctionDefinition>\n" ;
}


