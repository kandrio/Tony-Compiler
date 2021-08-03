%{
#include <cstdio>
#include <string.h>
#include "ast.hpp"
#include "lexer.hpp"
#include "symbol.hpp"

SymbolTable st;
%}

%token T_and      "and"
%token T_bool     "bool"
%token T_char     "char"
%token T_decl     "decl"
%token T_def      "def"
%token T_else     "else"
%token T_elsif    "elsif"
%token T_end      "end"
%token T_exit     "exit"
%token T_false    "false"
%token T_for      "for"
%token T_head     "head"
%token T_if       "if"
%token T_int      "int"
%token T_list     "list"
%token T_mod      "mod"
%token T_new      "new"
%token T_not      "not"
%token T_nil      "nil"
%token T_nil2     "nil?"
%token T_or       "or"
%token T_ref      "ref"
%token T_return   "return"
%token T_skip     "skip"
%token T_tail     "tail"
%token T_true     "true"
%token T_le       "<="
%token T_ge       ">="
%token T_ne       "<>"
%token T_assign   ":="

%left "or" 
%left "and"
%nonassoc "not"
%nonassoc '=' "<>" "<=" ">=" '<' '>'
%right '#'
%left '+' '-'
%left '*' '/' "mod"
%left UMINUS UPLUS

%union {
    char *name;
    int num;
    unsigned char c;

    Expr *expr;
    Atom *atom;
    ExprList *exprlist;
    FunctionCall *call;
    SimpleList *simplelist;
    Simple *simple;
    Stmt *stmt;
    StmtBody *stmtbody;
    Type *type;
    VarList *varlist;
    Formal *formal;
    FormalList *formallist;
    Header *header;
    FunctionDeclaration *funcdecl;
    FunctionDefinition *funcdef;
    Elsif *elsif;
    Else *els;
    If *iff;
    For *fo;
 }

%token<name> T_id      
%token<num> T_const     
%token<name> T_string    
%token<c> T_singlechar  

%type<funcdef> Func_def
%type<funcdef> Func_def_dec
%type<funcdecl> Func_Decl
%type<header> Header
%type<formallist> Par
%type<formal> Formal
%type<varlist> Var_Def Var_Comma
%type<type> Type
%type<stmtbody> Stmt_Body Stmt_Full
%type<iff> If_Clause
%type<elsif> Elsif_Clause
%type<els> Else_Clause
%type<fo> For_Clause
%type<stmt> Stmt  
%type<simple> Simple
%type<simplelist> Simple_Comma Simple_List
%type<call> Call
%type<exprlist> Expr_List Expr_Comma
%type<atom> Atom
%type<expr> Expr; 
%%

/*=============================================
    Function Definitions
==============================================*/

Program:
    Func_def {$1->sem();}
;

Func_def:
    "def" Header ':' Func_def_dec  Stmt_Body "end" {$4->merge($2, $5); $$ = $4;}
;

Func_def_dec:
    Func_def Func_def_dec   {$2->append($1); $$ = $2;}
|   Func_Decl Func_def_dec  {$2->append($1); $$ = $2;}
|   Var_Def Func_def_dec    {$2->append($1); $$ = $2;}
|   /*ε*/                   {$$ = new FunctionDefinition();}
;

Header:
    Type T_id '(' ')'               {$$ = new Header($1, new Id(std::string($2)), NULL);}
|   Type T_id '(' Formal Par ')'    {$5->append($4); $$ = new Header($1, new Id(std::string($2)), $5);}
|   T_id '('')'                     {$$ = new Header(new Id(std::string($1)),  NULL);}
|   T_id '(' Formal Par ')'         {$4->append($3); $$ = new Header(new Id(std::string($1)), $4);}
;

Func_Decl:
    "decl" Header   {$$ = new FunctionDeclaration($2);}
;

Par:   
    ';' Formal Par  {$3->append($2); $$ = $3;}
|   /*e*/           {$$ = new FormalList();}
;

Formal:
    "ref" Var_Def   {$$ = new Formal($2, true);}
|   Var_Def         {$$ = new Formal($1, false);}
;

Var_Def: 
    Type T_id Var_Comma    {$3->append(new Id(std::string($2))); $3->set_type($1); $$ = $3;}
;

Var_Comma:
    /*e*/                  {$$ = new VarList();}
|   ',' T_id Var_Comma {}  {$3->append(new Id(std::string($2))); $$ = $3;}
;

Type:
    "int"   {$$ = new Type(TYPE_int, nullptr);}
|   "char"  {$$ = new Type(TYPE_char, nullptr);}
|   "bool"  {$$ = new Type(TYPE_bool, nullptr);}
|   Type '[' ']'        {$$ = new Type(TYPE_array, $1);}
|   "list" '[' Type ']' {$$ = new Type(TYPE_list, $3);}
;

Stmt:
    Simple          {$$ = $1;}  
|   "exit"          {$$ = new Exit();}
|   "return" Expr   {$$ = new Return($2);}
|   If_Clause       {$$=$1;}
|   For_Clause      {$$=$1;}
;

Stmt_Body: 
    Stmt Stmt_Full  {$2->append($1); $$ = $2;}
;

Stmt_Full:   
    Stmt Stmt_Full  {$2->append($1); $$ = $2;}
|   /*e*/           {$$ = new StmtBody();}
; 

If_Clause:
    "if" Expr ':' Stmt_Body Elsif_Clause Else_Clause "end" {$$ = new If($2, $4, $5, $6);}
;

Elsif_Clause: 
    "elsif" Expr ':' Stmt_Body Elsif_Clause {$5->append($2, $4); $$ = $5;}
|   /*e*/                                   {$$ = new Elsif();}
;

Else_Clause:  
    "else" ':' Stmt_Body {$$ = new Else($3);}
|   /*e*/                {$$ = new Else();}
;

For_Clause: 
    "for" Simple_List ';' Expr ';' Simple_List ':' Stmt_Body "end" {$$ = new For($2, $4, $6, $8);}
;

Simple:
    "skip"           {$$ = new Skip();}
|   Atom ":=" Expr   {$$ = new Assign($1, $3);}  
|   Call             {$$ = $1;}
;

Simple_List: 
    Simple  Simple_Comma    {$2->append($1); $$ = $2;}
;

/* Υλοποιεί το ("," Simple)* της γραμματικής.
Το SimpleList() κατασκευάζει μια κενή λίστα από Simples η οποία
θα γίνει append. */
Simple_Comma:
    /*ε*/                    {$$ = new SimpleList();}
|   ',' Simple Simple_Comma  {$3->append($2); $$ = $3;}
;

Call:
    T_id  '(' ')'           {$$ = new FunctionCall(new Id(std::string($1)));}
|   T_id  '(' Expr_List ')' {$$ = new FunctionCall(new Id(std::string($1)), $3);}
;

Expr_List: 
    Expr  Expr_Comma    {$2->append($1); $$ = $2;}
;

Expr_Comma:
    /*ε*/               {$$ = new ExprList();}
|   ',' Expr Expr_Comma {$3->append($2); $$ = $3;}
;

Atom:
    T_id                {$$ = new Id(std::string($1));}
|   T_string            {$$ = new StringLiteral(std::string($1));}
|   Atom '[' Expr ']'   {$$ = new ArrayElement($1, $3);}
|   Call                {$$ = $1;}
;

Expr:
    Atom               {$$ = $1;}       
    |   T_const        {$$ = new IntConst($1);}
    |   T_singlechar   {$$ = new CharConst($1);}
    |   '(' Expr ')'   {$$ = $2;}
    |   '+' Expr   %prec UPLUS  {$$ = new UnOp(std::string("+"), $2);}
    |   '-' Expr   %prec UMINUS {$$ = new UnOp(std::string("-"), $2);}
    |   "nil?" '(' Expr ')'     {$$ = new UnOp(std::string("nil?"), $3);}
    |   "head" '(' Expr ')'     {$$ = new UnOp(std::string("head"), $3);}
    |   "tail" '(' Expr ')'     {$$ = new UnOp(std::string("tail"), $3);}
    |   "not" Expr              {$$ = new UnOp(std::string("not"), $2);}
    |   Expr '+' Expr    {$$ = new BinOp($1, std::string("+"), $3);}
    |   Expr '-' Expr    {$$ = new BinOp($1, std::string("-"), $3);}
    |   Expr '*' Expr    {$$ = new BinOp($1, std::string("*"), $3);}
    |   Expr '/' Expr    {$$ = new BinOp($1, std::string("/"), $3);}
    |   Expr "mod" Expr  {$$ = new BinOp($1, std::string("mod"), $3);}
    |   Expr '=' Expr    {$$ = new BinOp($1, std::string("="), $3);}
    |   Expr "<>" Expr   {$$ = new BinOp($1, std::string("<>"), $3);}
    |   Expr '<' Expr    {$$ = new BinOp($1, std::string("<"), $3);}
    |   Expr '>' Expr    {$$ = new BinOp($1, std::string(">"), $3);}
    |   Expr "<=" Expr   {$$ = new BinOp($1, std::string("<="), $3);}
    |   Expr ">=" Expr   {$$ = new BinOp($1, std::string(">="), $3);}
    |   Expr "and" Expr  {$$ = new BinOp($1, std::string("and"), $3);}
    |   Expr "or" Expr   {$$ = new BinOp($1, std::string("or"), $3);}
    |   Expr '#' Expr    {$$ = new BinOp($1, std::string("#"), $3);}
    |   "true"           {$$ = new Boolean(std::string("true"));}
    |   "false"          {$$ = new Boolean(std::string("false"));}
    |   "new" Type '[' Expr ']' {$$ = new New($2, $4);}
    |   "nil"                   {$$ = new Nil();}
;

%%

int main(){
    int result = yyparse();
}
