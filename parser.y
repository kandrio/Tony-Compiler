%{
#include <cstdio>
#include <string.h>
#include "ast.hpp"
#include "lexer.hpp"
#include "symbol.hpp"
#include "error.hpp"

extern FILE *yyin;
SymbolTable st;
LLVMListTypes llvm_list_types;

AST *root;
char *filename;

%}

%locations
%code requires {extern int yylineno;}
%define parse.error verbose

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
    char c;

    Expr *expr;
    Atom *atom;
    ExprList *exprlist;
    FunctionCall *call;
    SimpleList *simplelist;
    Simple *simple;
    Stmt *stmt;
    StmtBody *stmtbody;
    TonyType *type;
    VarList *varlist;
    Formal *formal;
    FormalList *formallist;
    Header *header;
    FunctionDeclaration *funcdecl;
    FunctionDefinition *funcdef;
    If *iff;
    For *fo;
 }

%token<name> T_id      
%token<num> T_const     
%token<name> T_string    
%token<c> T_singlechar  

%type<funcdef> Program
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
%type<iff> Elsif_Clause
%type<iff> Else_Clause
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
    Func_def {
        $1->setIsMain();
        root = $$;
    }
;

Func_def:
    "def" Header ':' Func_def_dec  Stmt_Body "end" {$4->merge($2, $5); $4->reverse(); $$ = $4; $$->setLineno(yylineno);}
;

Func_def_dec:
    Func_def Func_def_dec   {$2->append($1); $$ = $2; }
|   Func_Decl Func_def_dec  {$2->append($1); $$ = $2; }
|   Var_Def Func_def_dec    {$2->append($1); $$ = $2; }
|   /*ε*/                   {$$ = new FunctionDefinition(); $$->setLineno(yylineno);}
;

Header:
    Type T_id '(' ')'               {$$ = new Header($1, new Id(std::string($2)), NULL); $$->setLineno(yylineno);}
|   Type T_id '(' Formal Par ')'    {$5->append($4); $5->reverse(); $$ = new Header($1, new Id(std::string($2)), $5); $$->setLineno(yylineno);}
|   T_id '('')'                     {$$ = new Header(new Id(std::string($1)),  NULL); $$->setLineno(yylineno);}
|   T_id '(' Formal Par ')'         {$4->append($3); $4->reverse(); $$ = new Header(new Id(std::string($1)), $4); $$->setLineno(yylineno);}
;

Func_Decl:
    "decl" Header   {$$ = new FunctionDeclaration($2); $$->setLineno(yylineno);}
;

Par:   
    ';' Formal Par  {$3->append($2); $$ = $3;}
|   /*e*/           {$$ = new FormalList(); $$->setLineno(yylineno);}
;

Formal:
    "ref" Var_Def   {$$ = new Formal($2, true); $$->setLineno(yylineno); }
|   Var_Def         {$$ = new Formal($1, false); $$->setLineno(yylineno);}
;

Var_Def: 
    Type T_id Var_Comma    {$3->append(new Id(std::string($2))); $3->set_type($1); $3->reverse(); $$ = $3; $$->setLineno(yylineno);}
;

Var_Comma:
    /*e*/                  {$$ = new VarList(); $$->setLineno(yylineno);}
|   ',' T_id Var_Comma {}  {$3->append(new Id(std::string($2))); $$ = $3; $$->setLineno(yylineno);}
;

Type:
    "int"   {$$ = new TonyType(TYPE_int, nullptr);}
|   "char"  {$$ = new TonyType(TYPE_char, nullptr);}
|   "bool"  {$$ = new TonyType(TYPE_bool, nullptr);}
|   Type '[' ']'        {$$ = new TonyType(TYPE_array, $1); $$->set_array_size(0);}
|   "list" '[' Type ']' {$$ = new TonyType(TYPE_list, $3);}
;

Stmt:
    Simple          {$$ = $1; $$->setLineno(yylineno);}  
|   "exit"          {$$ = new Exit(); $$->setLineno(yylineno);}
|   "return" Expr   {$$ = new Return($2); $$->setLineno(yylineno);}
|   If_Clause       {$$=$1; $$->setLineno(yylineno);}
|   For_Clause      {$$=$1; $$->setLineno(yylineno);}
;

Stmt_Body: 
    Stmt Stmt_Full  {$2->append($1); $2->reverse(); $$ = $2;}
;

Stmt_Full:   
    Stmt Stmt_Full  {$2->append($1); $$ = $2;}
|   /*e*/           {$$ = new StmtBody(); $$->setLineno(yylineno);}
; 

If_Clause:
    "if" Expr ':' Stmt_Body Elsif_Clause "end" {$$ = new If($2, $4, $5); $$->setLineno(yylineno);}
;

Elsif_Clause: 
    "elsif" Expr ':' Stmt_Body Elsif_Clause {$$ = new If($2, $4, $5);}
|   Else_Clause                             {$$ = $1;}
;

Else_Clause:  
    "else" ':' Stmt_Body {$$ = new If(nullptr, $3, nullptr);}
|   /*e*/                {$$ = nullptr;}
;

For_Clause: 
    "for" Simple_List ';' Expr ';' Simple_List ':' Stmt_Body "end" {$$ = new For($2, $4, $6, $8); $$->setLineno(yylineno);}
;

Simple:
    "skip"           {$$ = new Skip(); $$->setLineno(yylineno);}
|   Atom ":=" Expr   {$$ = new Assign($1, $3); $$->setLineno(yylineno);}   
|   Call             {$$ = $1; $$->setLineno(yylineno);}
;

Simple_List: 
    Simple  Simple_Comma    {$2->append($1); $2->reverse(); $$ = $2;}
;

/* Υλοποιεί το ("," Simple)* της γραμματικής.
Το SimpleList() κατασκευάζει μια κενή λίστα από Simples η οποία
θα γίνει append. */
Simple_Comma:
    /*ε*/                    {$$ = new SimpleList(); $$->setLineno(yylineno);}
|   ',' Simple Simple_Comma  {$3->append($2); $$ = $3;}
;

Call:
    T_id  '(' ')'           {$$ = new FunctionCall(new Id(std::string($1))); $$->setLineno(yylineno);}
|   T_id  '(' Expr_List ')' {$$ = new FunctionCall(new Id(std::string($1)), $3); $$->setLineno(yylineno);}
;

Expr_List: 
    Expr  Expr_Comma    {$2->append($1); $2->reverse(); $$ = $2;}
;

Expr_Comma:
    /*ε*/               {$$ = new ExprList(); $$->setLineno(yylineno);}
|   ',' Expr Expr_Comma {$3->append($2); $$ = $3;}
;

Atom:
    T_id                {$$ = new Id(std::string($1)); $$->setLineno(yylineno);}
|   T_string            {$$ = new StringLiteral(std::string($1)); $$->setLineno(yylineno);}
|   Atom '[' Expr ']'   {$$ = new ArrayElement($1, $3); $$->setLineno(yylineno);}
|   Call                {$$ = $1;}
;

Expr:
    Atom               {$$ = $1;}       
    |   T_const        {$$ = new IntConst($1); $$->setLineno(yylineno);}
    |   T_singlechar   {$$ = new CharConst($1); $$->setLineno(yylineno);}
    |   '(' Expr ')'   {$$ = $2;}
    |   '+' Expr   %prec UPLUS  {$$ = new UnOp(std::string("+"), $2); $$->setLineno(yylineno);}
    |   '-' Expr   %prec UMINUS {$$ = new UnOp(std::string("-"), $2); $$->setLineno(yylineno);}
    |   "nil?" '(' Expr ')'     {$$ = new UnOp(std::string("nil?"), $3); $$->setLineno(yylineno);}
    |   "head" '(' Expr ')'     {$$ = new UnOp(std::string("head"), $3); $$->setLineno(yylineno);}
    |   "tail" '(' Expr ')'     {$$ = new UnOp(std::string("tail"), $3); $$->setLineno(yylineno);}
    |   "not" Expr              {$$ = new UnOp(std::string("not"), $2); $$->setLineno(yylineno);}
    |   Expr '+' Expr    {$$ = new BinOp($1, std::string("+"), $3); $$->setLineno(yylineno);}
    |   Expr '-' Expr    {$$ = new BinOp($1, std::string("-"), $3);$$->setLineno(yylineno);}
    |   Expr '*' Expr    {$$ = new BinOp($1, std::string("*"), $3);$$->setLineno(yylineno);}
    |   Expr '/' Expr    {$$ = new BinOp($1, std::string("/"), $3); $$->setLineno(yylineno);}
    |   Expr "mod" Expr  {$$ = new BinOp($1, std::string("mod"), $3); $$->setLineno(yylineno);}
    |   Expr '=' Expr    {$$ = new BinOp($1, std::string("="), $3); $$->setLineno(yylineno);}
    |   Expr "<>" Expr   {$$ = new BinOp($1, std::string("<>"), $3); $$->setLineno(yylineno);}
    |   Expr '<' Expr    {$$ = new BinOp($1, std::string("<"), $3); $$->setLineno(yylineno);}
    |   Expr '>' Expr    {$$ = new BinOp($1, std::string(">"), $3); $$->setLineno(yylineno);}
    |   Expr "<=" Expr   {$$ = new BinOp($1, std::string("<="), $3); $$->setLineno(yylineno);}
    |   Expr ">=" Expr   {$$ = new BinOp($1, std::string(">="), $3); $$->setLineno(yylineno);}
    |   Expr "and" Expr  {$$ = new BinOp($1, std::string("and"), $3); $$->setLineno(yylineno);}
    |   Expr "or" Expr   {$$ = new BinOp($1, std::string("or"), $3); $$->setLineno(yylineno);}
    |   Expr '#' Expr    {$$ = new BinOp($1, std::string("#"), $3); $$->setLineno(yylineno);}
    |   "true"           {$$ = new Boolean(std::string("true")); $$->setLineno(yylineno);}
    |   "false"          {$$ = new Boolean(std::string("false")); $$->setLineno(yylineno);}
    |   "new" Type '[' Expr ']' {$$ = new New($2, $4); $$->setLineno(yylineno);}
    |   "nil"                   {$$ = new Nil(); $$->setLineno(yylineno);}
;

%%

int main(int argc, char **argv){
    bool optimization = false;
    for(int i=1; i<argc; i++){
        if(std::string(argv[i]) == std::string("-O")){
            optimization = true;
        }
        else{
            filename = argv[i];
            FILE * pt = fopen(filename, "r" );
            if(pt==nullptr){
                yyerror("Input file couldnn't be opened.");
            }
            yyin = pt;
        }
    }

    int result = yyparse();
    if(result!=0){
        yyerror("Parsing Failed!");
    }

    root->sem();
    root->llvm_compile_and_dump(optimization);
    delete root;
    fclose(yyin);
    return 0;
}
