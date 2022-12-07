/* Nomes Integrantes Grupo */
%{

#include "tokens.h"

int line_number = 1;

%}
/* \/\*(?s:.)\*\/ {} */


%option noyywrap

%option yylineno

space [ \t\n\r\s]
alpha [a-zA-Z]+
digit [[:digit:]]
integer [\+\-]?{digit}+

commentstart	"/*"
commentend	\*+\/
notbullet [^*]
bulletwobar \*+[^\*\/]

/*\/\*[^(\*\/)]*\*\/ {}*/
%x mlcomment

%%

{commentstart}	  	{BEGIN(mlcomment);}
<mlcomment>{notbullet}	 {}
<mlcomment>{bulletwobar} {}
<mlcomment>{commentend}  {BEGIN(INITIAL);}

(\/\/).* {}
{space} {}
\'(?-s:.)?\' { return TK_LIT_CHAR; }

"int" { return TK_PR_INT; }
"float" { return TK_PR_FLOAT; }
"bool" { return TK_PR_BOOL; }
"char" { return TK_PR_CHAR; }
"if" { return TK_PR_IF; }
"then" { return TK_PR_THEN; }
"else" { return TK_PR_ELSE; }
"while" { return TK_PR_WHILE; }
"input" { return TK_PR_INPUT; }
"output" { return TK_PR_OUTPUT; }
"return" { return TK_PR_RETURN; }
"for" { return TK_PR_FOR; }

"<=" { return TK_OC_LE; }
">=" { return TK_OC_GE; }
"==" { return TK_OC_EQ; }
"!=" { return TK_OC_NE; }
"&&" { return TK_OC_AND; }
"||" { return TK_OC_OR; }

"false" { return TK_LIT_FALSE; }
"true" { return TK_LIT_TRUE; }
{integer} { return TK_LIT_INT; }
{integer}.{digit}+ { return TK_LIT_FLOAT; }
{integer}.{digit}+(e|E){integer} { return TK_LIT_FLOAT; }

{alpha} { return TK_IDENTIFICADOR; }

[\,\;\:\(\)\{\}\+\-\*\/\%\^\<\>\=\!] { return yytext[0]; }

. { return TK_ERRO; }

%%

