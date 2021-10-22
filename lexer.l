%{
#include <cstdio>
#include <cstdlib>
#include <stdarg.h>
#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "error.hpp"

int comment_counter = 0;
int linecount=1;
char fixSingleChar(char *str);

/*
    L -> Letters
    D -> Digits
    W -> White characaters
    E -> Escape characaters
    O -> Operators
    S -> Seperators
    P -> Printable characters
*/
%}

L [a-zA-Z]
D [0-9]
W [ \r\t]
E [ntr0'"\\]|[x][0-9a-f][0-9a-f]
O [\+\-\*\/\#\=\<\>]
S [\(\)\[\]\,\;\:]
P [\x20-\x7e]

%option noyywrap
%option yylineno
%x COMMENT

%%
"and"     {return T_and;}
"bool"    {return T_bool;}
"char"    {return T_char;}
"decl"    {return T_decl;}
"def"     {return T_def;}
"else"    {return T_else;}
"elsif"   {return T_elsif;}
"end"     {return T_end;}
"exit"    {return T_exit;}
"false"   {return T_false;}
"for"     {return T_for;}
"head"    {return T_head;}
"if"      {return T_if;}
"int"     {return T_int;}
"list"    {return T_list;}
"mod"     {return T_mod;}
"new"     {return T_new;}
"nil"     {return T_nil;}
"nil?"    {return T_nil2;}
"not"     {return T_not;}
"or"      {return T_or;}
"ref"     {return T_ref;}
"return"  {return T_return;}
"skip"    {return T_skip;}
"tail"    {return T_tail;}
"true"    {return T_true;}


{D}+ {yylval.num=std::stoi(yytext); return T_const;}

{O}  {return yytext[0];}
"<="				{return T_le;}
">="				{return T_ge;}
"<>"				{return T_ne;}

{S}                 {return yytext[0];}
":="				{return T_assign;}

{L}({L}|{D}|"_"|"?")*              {yylval.name= strdup(yytext); return T_id;} 

"'"([\x20-\x26\x28-\x7e]|\\{E})"'" {
  
  

  yylval.c=fixSingleChar(yytext); return T_singlechar;}

\"([\x20-\x21\x23-\x7e]|\\\")*\"   {
  char *str = yytext; 
  str++; 
  str[strlen(str)-1] = '\0';
  yylval.name= strdup(str); return T_string;
                                }

%[^\n]*\n			    {linecount++;}

\<\*                    { BEGIN(COMMENT); }
<COMMENT>\<\*           {++comment_counter;}
<COMMENT>\*\>           {if (comment_counter > 0) --comment_counter;
                            else BEGIN(INITIAL);}

<COMMENT>[^\n]          {/* nothing */}
<COMMENT>\n             {linecount++;}
{W}+                    {/* nothing */ }
\n			                {linecount++;}

.                       {error(linecount, "Lexer error: Unexpected token %s", yytext);}

%%

//TODO: Fix to add lines
void yyerror(const char *msg, ...) {
  error(yylineno, msg);
}

char fixSingleChar(char *str){ 
  
  str++;
  str[strlen(str)-1] = '\0';
  if(str[0] != '\\') return str[0];
  str++;
  char x;
  switch(str[0]){
    case 'n': return '\n';
    case 't': return '\t';
    case 'r': return '\r';
    case '0': return '\0';
    case '\\': return '\\';
    case '\'': return '\'';
    case '\"': return '\"';
    case 'x': str++; x =  (char) strtol(str, NULL, 16); return x; break;
    default : 
      error(linecount, "Lexer error: Unexpected token"); 
      return '\0';
      break;
  }
}