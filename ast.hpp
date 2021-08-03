#ifndef __AST_HPP___
#define __AST_HPP___

#pragma once

#include <iostream>
#include <map>
#include <vector>
#include <string>
#include "symbol.hpp"
#include "type.hpp"

void yyerror(const char *msg);


class AST {
public:
  virtual ~AST() {}
  virtual void printOn(std::ostream &out) const = 0;
  virtual void sem() {}; // NOTE: After we implement `sem()` for all the subclasses
                         // it would make sense to make this: `virtual void sem() = 0`.
};

inline std::ostream& operator<< (std::ostream &out, const AST &t) {
  t.printOn(out);
  return out;
}


class Expr: public AST {
public:
  bool type_check(Type* t) {
    sem();
    return check_type_equality(t, type);
  }
  bool type_check(TypeBlock t) {
    /*
      This is just an overloading of the method above.
      It is used for simpler cases.
    */    
    sem();
    return (t == type->get_current_type() && type->get_nested_type() == nullptr);
  }
  Type* get_type() {
    /* 
      We want to be able to access `type` from a pointer to an `Expr`.
      But `type` is protected, so we need this `public` method.
    */
    return type;
  }
protected:
  Type* type;
};

class Stmt: public AST {
};


class Atom: public Expr { 
};


class Simple: public Stmt {  
};

class Id: public Atom {
public:
  Id(std::string v): var(v) {}
  void printOn(std::ostream &out) const override {
    out << "<Id name=\"" << var << "\"> ";
  }

  void setType(Type* t){
    type = t;
  }

  void insertIntoScope(){
    st.insert(var, type);
  }

  virtual void sem() override {
  }
private:
  std::string var;
};


class ArrayElement: public Atom {
public:
  ArrayElement(Atom *a, Expr *e): atom(a), expr(e) {}
  ~ArrayElement() {delete atom; delete expr;}
  void printOn(std::ostream &out) const override {
    out << "\n<ArrayElement>\n" << *atom << "\n" << *expr << "\n</ArrayElement>\n";
  }
  void sem() override {
    // FIXME: `type` here should be the type of the element of the array not the type
    // of the array itself.
    type = new Type(TYPE_array, nullptr);
  }
private:
  Atom *atom;
  Expr *expr;
};


class StringLiteral: public Atom {
public:
  StringLiteral(std::string str): strlit(str) {}
  void printOn(std::ostream &out) const override {
    out << "<String value=\"" << strlit << "\"> ";
  }
private:
  std::string strlit;
};


class CharConst: public Expr {
public:
  CharConst(unsigned char c): char_const(c) {}
  void printOn(std::ostream &out) const override {
    out << "<CharConst value='"<< char_const << "\' ascii="<< (int) char_const << "> ";
  }
  void sem() override {
    type = new Type(TYPE_char, nullptr);
  }
private:
  unsigned char char_const;
};


class IntConst: public Expr {
public:
  IntConst(int n): num(n) {}
  void printOn(std::ostream &out) const override {
    out << "<IntConst value=" << num << "> ";
  }
  void sem() override {
    type = new Type(TYPE_int, nullptr);
  }
private:
  int num;
};


class New: public Expr {
public:
  New(Type *t, Expr *right): type_of_elems(t), expr(right) {}
  ~New() {delete type_of_elems; delete expr;}
  void printOn(std::ostream &out) const override {
    out << "<New> " << type_of_elems << *expr << "</New> ";
  }
private:
  Type *type_of_elems;
  Expr *expr;
};


class Nil: public Expr {
public:
  Nil() {}
  void printOn(std::ostream &out) const override {
    out << "<Nil> ";
  }
};


class Boolean: public Expr {
public:
  Boolean(std::string b): boolean_value(b) {}
  void printOn(std::ostream &out) const override {
    out << "<Boolean value=" << boolean_value << "> ";
  }
  void sem() override {
    type = new Type(TYPE_bool, nullptr);
  }
private:
  std::string boolean_value;
};


class BinOp: public Expr {
public:
  BinOp(Expr *l, std::string o, Expr *r): left(l), op(o), right(r) {}
  ~BinOp() { delete left; delete right; }
  void printOn(std::ostream &out) const override {
    out << "\n<Binop op=\"" << op << "\">\n" << *left << *right << "\n</BinOp>\n";
  }
  void sem() {
    if (op == "+" || op == "-" || op == "*" || op == "/" || op == "mod") {
      if (!left->type_check(TYPE_int) || !right->type_check(TYPE_int)) {
        // TODO: We must be more specific in our errors. This is temporary.
        yyerror("Type mismatch. Both expressions must be of type 'int'.\n");
      }
      type = new Type(TYPE_int, nullptr);
    } else if (op == "=" || op == "<>" || op == "<" || op == ">" || op == "<=" || op == ">=") {
      left->sem();
      right->sem();
      if (left->get_type() != right->get_type()) {
        yyerror("Type mismatch. Expressions must have the same type.\n");
      }
      type = new Type(TYPE_bool, nullptr);
    } else if (op == "and" || op == "or") {
      if (!left->type_check(TYPE_bool) || !right->type_check(TYPE_bool)) {
        yyerror("Type mismatch. Both expressions must be of type 'bool'.\n");
      }
      type = new Type(TYPE_bool, nullptr);
    }
    // TODO: A case for the `#` binary operator.
  }
private:
  Expr *left;
  std::string op;
  Expr *right;
};


class UnOp: public Expr {
public:
  UnOp(std::string(o), Expr *r): op(o), right(r) {}
  ~UnOp() { delete right; }
  void printOn(std::ostream &out) const override {
    out << "\n<UnOp op=\"" << op << "\">\n" << *right << "\n</UnOp>\n";
  }
  void sem() override {
    if (op == "+" || op == "-") {
      if (!right->type_check(TYPE_int)) {
        yyerror("Type mismatch. Expression must be of type 'int'.");
      }
      type = new Type(TYPE_int, nullptr);
    } else if (op == "not") { 
      if (!right->type_check(TYPE_bool)) {
        yyerror("Type mismatch. Expression must be of type 'bool'.");
      }
      type = new Type(TYPE_bool, nullptr);
    }
  }
private:
  std::string op;
  Expr *right;
};


class VarList: public AST {
public:
  VarList() {}
  ~VarList(){
    for (Id * i : ids) delete i;
  }
  void append(Id * id) {
    ids.push_back(id);
  }
  void set_type(Type* t) {
    type = t;
  }

  void printOn(std::ostream &out) const override {
    out << "\n<VarList>\n";
    out << type;
    for (Id * i : ids) {
      out << *i;
    }
    out << "\n</VarList>\n";
  }
  
  virtual void sem() override {
    for (Id * i : ids) {i->setType(type); i->insertIntoScope();}
  }

protected:
  std::vector<Id *> ids;
  Type* type;
};



class Formal: public AST {
public:
  Formal(VarList* v, bool isref): varlist(v), isRef(isref) {}
  ~Formal() {delete varlist;}
  void printOn(std::ostream &out) const override {
    out << "\n<Formal isRef=\"" << (isRef ? "yes" : "no") << "\">\n" << *varlist << "</Formal>";
  }
  
  virtual void sem() override {
    varlist->sem();
  }
private:
  VarList* varlist;
  bool isRef;
};


class FormalList: public AST {
public:
  FormalList() {}
  ~FormalList(){
    for (Formal * f : formals) delete f;
  }

  void append(Formal * f) {
    formals.push_back(f);
  }

  void printOn(std::ostream &out) const override {
    out << "\n<FormalList>\n";

    for (Formal * f : formals) {
      out << *f;
    }
    out << "\n</FormalList>\n";
  }

  virtual void sem() override {
    for (Formal *f: formals) f->sem();
  }
private:
  std::vector<Formal *> formals;
};


// Sem should probably include name of header somewhere?
class Header: public AST {
public:
  Header(Type *t, Id *name, FormalList *f): type(t), formals(f), id(name), isTyped(true) {}
  Header(Id *name, FormalList *f): formals(f), id(name), isTyped(false) {}
  ~Header(){ delete formals; delete id;}
  void printOn(std::ostream &out) const override {
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


  virtual void sem() override {
    if (formals) formals->sem();
  }

private:
  Type *type;
  FormalList *formals;
  Id *id;
  bool isTyped;
};


class Return: public Stmt{
public:
  Return(Expr* e): ret_expr(e) {}
  ~Return() {delete ret_expr;}
  void printOn(std::ostream &out) const override {
    out << "\n<Return>\n" << *ret_expr << "\n</Return>\n";
  }
private:
  Expr* ret_expr;
};


class Exit: public Stmt{
public:
  Exit() {}
  void printOn(std::ostream &out) const override {
    out << "\n<Exit>\n";
  }
};


class StmtBody: public AST {
public:
  StmtBody(): stmts() {}
  ~StmtBody() { for (Stmt *s : stmts) delete s; }
  void append(Stmt* stmt) {
    stmts.push_back(stmt);
    return;
  }
  void printOn(std::ostream &out) const override {
    out << "\n<StmtBody>\n";
    for (Stmt *s : stmts) {
      out << *s;
    }
    out << "\n</StmtBody>\n";
  }
  void sem() override {
    for (Stmt *s : stmts) {
      s->sem();
    }
  }
  
private:
  std::vector<Stmt*> stmts;
};


class Assign: public Simple {
public:
  Assign(Atom *a, Expr *e): atom(a), expr(e) {}
  ~Assign() {delete atom; delete expr;}
  void printOn(std::ostream &out) const override {
    out << "\n<Assign>\n" << *atom << *expr << "\n</Assign>\n";
  }
  void sem() override {
    // NOTE: Here, we should `type_check` that `expr` has the same
    // type as the `atom` that's on the left.
    expr->sem();
  }
private:
  Atom *atom;
  Expr *expr;
};


class Skip: public Simple{
public:
  Skip() {}
  void printOn(std::ostream &out) const override {
    out << "\n<Skip>\n";
  }
};


class Elsif: public Stmt {
public:
  Elsif() {}
  ~Elsif() {
    for (Expr* e: elsif_conds) delete e;
    for (StmtBody* s: elsif_stmt_bodies) delete s;
  }
  std::vector<Expr *> get_conds() {
    return elsif_conds;
  }
  std::vector<StmtBody *> get_stmt_bodies() {
    return elsif_stmt_bodies;
  }

  void clear(){
    elsif_conds.clear();
    elsif_stmt_bodies.clear();
  }

  bool isEmpty(){
    return elsif_conds.empty();
  }
  void append(Expr* e, StmtBody* s) {
    elsif_conds.push_back(e);
    elsif_stmt_bodies.push_back(s);
  }

    void printOn(std::ostream &out) const override {
    // This is never used
    out << "<Elsif> </Elsif>";
    
  }

private:
  std::vector<Expr *> elsif_conds;
  std::vector<StmtBody *> elsif_stmt_bodies;
};


class Else: public Stmt {
public:
  Else() {}
  Else(StmtBody *s){else_stmt.push_back(s);}
  ~Else() {
    if (else_stmt.empty()){
      delete else_stmt[0];
    }
  }
  StmtBody * get_stmt() {
    return else_stmt[0]; 
  }

  void clear(){
    else_stmt.clear();
  }

  bool isEmpty(){
    return else_stmt.empty();
  }

  // This is never used, only for testing
  void printOn(std::ostream &out) const override {
    
    if(!else_stmt.empty()){
      out << "\n<Else>\n" << *else_stmt[0] << "\n</Else>\n";
    }
  }

private:
  std::vector<StmtBody *> else_stmt;
};


class If: public Stmt {
public:
  If(Expr *if_condition, StmtBody *if_stmt_body, Elsif * elsif_stmt, Else *else_stmt){
    conditions.push_back(if_condition);
    statements.push_back(if_stmt_body);
    
    std::vector<Expr *> elsif_conds = elsif_stmt->get_conds();
    std::vector<StmtBody *> elsif_stmt_bodies = elsif_stmt->get_stmt_bodies();
    for(int i=0; i < (int) elsif_conds.size(); i++){
      conditions.push_back(elsif_conds[i]);
      statements.push_back(elsif_stmt_bodies[i]);
    }

    if(!else_stmt->isEmpty()) {
      conditions.push_back(new Boolean("true"));
      statements.push_back(else_stmt->get_stmt());
      hasElse=true;
    }
  }
  ~If() {
    for (Expr* e: conditions) delete e;
    for (StmtBody* s: statements) delete s; 
  }

  void printOn(std::ostream &out) const override {
    out << "\n<If>\n";
    out << *conditions[0] << *statements[0];

    int size = (int) conditions.size();

    if(hasElse) {
      for (int i=1; i<(size-1); i++) {
        out << "\n<ElsIf>\n" << *conditions[i] << *statements[i] << "\n</ElsIf>\n";
      }
      out << "\n<Else>\n" << *conditions[size-1] << *statements[size-1] << "\n</Else>\n";
    } else {
      for (int i=1; i<size; i++) {
        out << "\n<ElsIf>\n" << *conditions[i] << *statements[i] << "\n</ElsIf>\n";
      }
    }
    out << "\n</If>\n";
  }

private:
  std::vector<Expr *>     conditions;
  std::vector<StmtBody *> statements;
  bool hasElse = false;
};


class SimpleList: public AST {
public:
  SimpleList(): simples() {}
  ~SimpleList() { for (Simple *s : simples) delete s; }

  void append(Simple *s) { simples.push_back(s); }
  void insert_front(Simple *s) { simples.insert(simples.begin(), s);}

  void printOn(std::ostream &out) const override {
    out << "\n<SimplesList>\n";
    for (Simple *s : simples) {
      out << *s;
    }
    out << "\n</SimpleList>\n";
  }
  std::vector<Simple *> get_simples_list(){
    return simples;
  }
private:
  std::vector<Simple *> simples;
};


class For: public Stmt {
public:
  For(SimpleList *sl1, Expr *e, SimpleList *sl2, StmtBody *sb):
    simple_list1(sl1), expr(e), simple_list2(sl2), stmt_body(sb)  {}
  ~For() {
    delete simple_list1;
    delete expr;
    delete simple_list2;
    delete stmt_body; }
  void printOn(std::ostream &out) const override {
    out << "\n<For>\n" << *simple_list1 << *expr << *simple_list2  << *stmt_body << "\n</For>\n";
  }

private:
  SimpleList *simple_list1;
  Expr *expr;
  SimpleList *simple_list2;
  StmtBody *stmt_body;
};


class ExprList: public AST {
public:
  ExprList(): expressions() {}
  ~ExprList() { for (Expr *e : expressions) delete e; }

  void append(Expr *e) { expressions.push_back(e); }
  void insert_front(Expr *e) { expressions.insert(expressions.begin(), e);}

  void printOn(std::ostream &out) const override {
    out << "\n<ExprList>\n";
    for (Expr *e : expressions) {
      out << *e;
    }
    out << "\n</ExprList>\n";
  }

  std::vector<Expr*> get_expr_list(){
    return expressions;
  }
private:
  std::vector<Expr*> expressions;
};


class FunctionCall: public Simple, public Atom {
public:
  FunctionCall(Id *n): name(n), hasParams(false) {}
  FunctionCall(Id *n, ExprList *el): name(n), params(el), hasParams(true) {}
  ~FunctionCall() {delete name; if (hasParams) delete params;}
  void printOn(std::ostream &out) const override {
    if(!hasParams)
      out << "\n<FunctionCall>\n" << *name << "\n</FunctionCall>\n";
    else
      out << "\n<FunctionCall>\n" << *name << *params << "\n</FunctionCall>\n";
  }
  
private:
  Id *name;
  ExprList *params;
  bool hasParams;
};


class FunctionDeclaration: public AST {
public:
  FunctionDeclaration(Header *hd): header(hd){}
  ~FunctionDeclaration() {delete header;}
  void printOn(std::ostream &out) const override {
    out << "\n<FunctionDeclaration>\n" << *header << "\n</FunctionDeclaration>\n" ;
  }

private:
  Header *header;
};

class FunctionDefinition: public AST {
public:
  FunctionDefinition(): header(), body(){}
  ~FunctionDefinition() {
    delete header; delete body;
    for (VarList *a : var_definitions) delete a;
    for (FunctionDeclaration *a : function_declarations) delete a;
    for (FunctionDefinition *a : function_definitions) delete a;
  }

  void append(VarList* a) {
    var_definitions.push_back(a);
  }

  void append(FunctionDeclaration* a) {
    function_declarations.push_back(a);
  }

  void append(FunctionDefinition* a) {
    function_definitions.push_back(a);
  }

  void merge(Header *hd, StmtBody *st) {

    header = hd;
    body = st;
  }

  void printOn(std::ostream &out) const override {
    out << "\n<FunctionDefinition>\n" << *header; 
    
    for (VarList *a : var_definitions) out << *a;
    for (FunctionDeclaration *a : function_declarations) out << *a;
    for (FunctionDefinition *a : function_definitions) out << *a;

    out << *body << "\n</FunctionDefinition>\n" ;
  }

  virtual void sem() override {
    st.openScope();
    header->sem();
    for (VarList *a : var_definitions) a->sem();
    for (FunctionDefinition *a : function_definitions) a->sem();
    body->sem();
    st.printSymbolTable();
    st.closeScope();
  }


private:
  Header *header;
  StmtBody *body;
  std::vector<VarList *> var_definitions;
  std::vector<FunctionDeclaration *> function_declarations;
  std::vector<FunctionDefinition *> function_definitions;
};


#endif