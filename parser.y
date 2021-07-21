%{
#include <cstdio>
#include <string.h>
#include "lexer.hpp"
//#include "ast.hpp"

%}

/*Ορίζουμε τους σημασιολογικούς τύπους των τερματικών που φαίνονται στα tokens παρακάτω. 
πχ: στο ' %token<op> T_and "and" ' λέμε ότι το τερματικό "and" έχει τύπο op (που έχουμε ορίσει
στο union) ο οποίος πρακτικά είναι ένας πίνακας από characters.

Επίσης, ορίζουμε και τους σημασιολογικούς τύπους των μη τερματικών συμβόλων όπως το Expr,
το Stmt κλπ.*/
%union {
    /* AST *ast;
    Expr *expr;
    Stmt *stmt;
    SimpleList *simple_list;
    ExprList *expr_list;
    VarList *var_list;
    int num;
    char c;
    bool b;
    char op[4];
    char sep[2];
    char name[80]; //!!
    char str[80]; //!! */
    const char *name;
    int num;
    struct expr {
        int temp;
    } Expr;
    struct valu {
        int temp;
    } RValue;
    struct stmt {
        int temp;
    } Stmt;
    struct call {
        int temp;
    } Call;
    struct atom {
        int temp;
    } Atom;    
}

%token T_and            "and"
%token T_bool           "bool"
%token T_char           "char"
%token T_decl           "decl"
%token T_def            "def"
%token T_else           "else"
%token T_elsif          "elsif"
%token T_end            "end"
%token T_exit           "exit"
%token T_false          "false"
%token T_for            "for"
%token T_head           "head"
%token T_if             "if"
%token T_int            "int"
%token T_list           "list"
%token T_mod        "mod"
%token T_new            "new"
%token T_not          "not"
%token T_nil            "nil"
%token T_nil2           "nil?"
%token T_or         "or"
%token T_ref            "ref"
%token T_return         "return"
%token T_skip           "skip"
%token T_tail           "tail"
%token T_true           "true"
%token T_le             "<="
%token T_ge         ">="
%token T_ne         "<>"
%token T_assign     ":="

%token<name> T_id      
%token<num> T_const     
%token<name> T_string    
%token<num> T_singlechar  

%type<name> Header
%type<Stmt> Stmt
%type<Stmt> Stmt_Body
%type<Stmt> Stmt_Full
%type<name> Type
%type<Stmt> If_Clause
%type<Stmt> For_Clause
%type<Expr> Expr
%type<RValue> RValue
%type<Atom> Atom
%type<Call> Call

%left "or" 
%left "and"
%nonassoc "not"
%nonassoc '=' "<>" "<=" ">=" '<' '>'
%right '#'
%left '+' '-'
%left '*' '/' "mod"
%left UMINUS UPLUS


%%

/*=============================================
    Function Definitions
==============================================*/

Program:
    {cout << "Program { "; identcounter++;} Func_def {cout << " }";}
;

Func_def:
    {cout << "Function { ";}
    "def" Header ':' Func_def_dec  Stmt_Body "end" {cout << " }";}
;

Func_def_dec:
    Func_def Func_def_dec
|   Func_Decl Func_def_dec 
|   Var_Def Func_def_dec
|   /*ε*/
;

Header:
    {cout << "Header { ";} Type T_id '(' ')' {cout << " }";}
|   {cout << "Header { ";} Type T_id '(' Formal Par ')' {cout << " }";}
|   {cout << "Header { ";} T_id '('')' {cout << "}";}
|   {cout << "Header { ";} T_id '(' Formal Par ')' {cout << " }";}
;

Func_Decl:
    "decl" Header 
;

Par:
|   ';' Formal Par {cout << ", Param";}
|   /*e*/
;

Formal:
    "ref" Type T_id Var_Comma {cout << "Ref Param";}
|   Type T_id Var_Comma {cout << "Param";}
;


Var_Comma:
    /* e*/  {}
|   ',' T_id Var_Comma {} {cout << ", Variable"}
;

Type:
    "int" {$$ = new Type(std::string("int"));}
|   "char" {$$ = new Type(std::string("char"));}
|   "bool" {$$ = new Type(std::string("bool"));}
|   Type '[' ']' {$$ = new Type(std::string("array"), $1);}
|   "list" '[' Type ']' {$$ = new Type(std::string("list"), $3);}
;


Var_Def: Type T_id Var_Comma 

Stmt:
    Simple {$$ = $1}  
|   "exit"  {$$ = new Exit();}
|   "return" Expr  {$$ = new Return($2);}
|   If_Clause {$$=$1;}
|   For_Clause {$$=$1;}
;

Stmt_Body: Stmt Stmt_Full {$2->append($1); $$ = $2;}
;

Stmt_Full:   Stmt Stmt_Full   {$2->append($1); $$ = $2; }
| /*e*/ {$$ = new StmtBody();}
; 



If_Clause   :
    "if" Expr ':' Stmt_Body Elsif_Clause Else_Clause "end" {$$ = new If($2, $4, $5, $6);}
;


Elsif_Clause : "elsif" Expr ':' Stmt_Body Elsif_Clause {$1->append($2, $4)}
| /*e*/ {$$ = new Elsif();}
;

Else_Clause:  "else" ':' Stmt_Body {$$ = new Else($3);}
| /*e*/ {$$ = new Else();}
;

For_Clause: "for" Simple_List ';' Expr ';' Simple_List ':' Stmt_Body "end" {$$ = new For($2, $4, $6, $8)}
;

Simple:
    "skip"                                  {$$ = new Skip(); }
|   Atom ":=" Expr                          {$$ = new Assign($1, $3) }  
|   Call                                    {$$ = $1 }
;


Simple_List: Simple  Simple_Comma    {$2->append($1); $$ = $2; }
;

/* Υλοποιεί το ("," Simple)* της γραμματικής.
Το SimpleList() κατασκευάζει μια κενή λίστα από Simples η οποία
θα γίνει append. */
Simple_Comma:
    /*ε*/                   {$$ = new SimpleList();}
|  ',' Simple Simple_Comma  {$3->append($2); $$ = $3;}
;

Call:
    {cout << "FunctionCall{ ";} T_id  '(' ')'                  {$$ = new FunctionCall($1);}
|   {cout << "FunctionCall{ ";} T_id  '(' Expr_List ')' { }    {$$ = new FunctionCall($1, $3);}
;

Expr_List: Expr  Expr_Comma    { $2->append($1); $$ = $2;}
;

Expr_Comma:
    /*ε*/               { $$ = new ExprList();}
|   ',' Expr Expr_Comma { $3->append($2); $$ = $3;}
;

Atom:
    T_id         {$$ = new Id(std::string($1));  }
|   T_string     {$$ = new StringLiteral(std::string($1)); }
|   Atom '[' Expr ']'   {$$ = new Array($1, $3);}
|   Call                {$$ = $1;}
;

Expr:
    Atom      {$$ = $1;}       
|   RValue     {$$ = $1;} 
;

RValue:
        T_const        { $$ = new IntConst($1); }
    |   T_singlechar   { $$ = new CharConst($1); }
    |   '(' Expr ')'     {$$ = $2;}
    |   '+' Expr   %prec UPLUS  { $$ = new UnOp(std::string("+"), $2);  }
    |   '-' Expr   %prec UMINUS { $$ = new UnOp(std::string("-"), $2); }
    |   "nil?" '(' Expr ')'     {$$ = new UnOp(std::string("nil?"), $3); }
    |   "head" '(' Expr ')'     {$$ = new UnOp(std::string("head"), $3); }
    |   "tail" '(' Expr ')'     {$$ = new UnOp(std::string("tail"), $3); }
    |   "not" Expr              {$$ = new UnOp($1, std::string("not"), $2); }
    |   Expr '+' Expr    {$$ = new BinOp($1, std::string("+"), $3); }
    |   Expr '-' Expr    {$$ = new BinOp($1, std::string("-"), $3);  }
    |   Expr '*' Expr    {$$ = new BinOp($1, std::string("*"), $3); }
    |   Expr '/' Expr    {$$ = new BinOp($1, std::string("/"), $3);  }
    |   Expr "mod" Expr  {$$ = new BinOp($1, std::string("mod"), $3);  }
    |   Expr '=' Expr    {$$ = new BinOp($1, std::string("="), $3);  }
    |   Expr "<>" Expr   {$$ = new BinOp($1, std::string("<>"), $3);  }
    |   Expr '<' Expr    {$$ = new BinOp($1, std::string("<"), $3);  }
    |   Expr '>' Expr    {$$ = new BinOp($1, std::string(">"), $3); }
    |   Expr "<=" Expr   {$$ = new BinOp($1, std::string("<="), $3);  }
    |   Expr ">=" Expr   {$$ = new BinOp($1, std::string(">="), $3);  }
    |   Expr "and" Expr  {$$ = new BinOp($1, std::string("and"), $3);  }
    |   Expr "or" Expr   {$$ = new BinOp($1, std::string("or"), $3); }
    |   Expr '#' Expr    {$$ = new BinOp($1, std::string("#"), $3);  }
    |   "true"           {$$ = new Boolean(std::string("true"));}
    |   "false"          {$$ = new Boolean(std::string("false"));}
    |   "new" Type '[' Expr ']' {$$ = new New($2, $4);}
    |   "nil"            {$$ = new Nil();}
    ;

%%

int main(){
  int result = yyparse();
}
