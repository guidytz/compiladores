/* Nomes Integrantes Grupo */
%{

#include "tokens.h"

int line_number = 1;

%}
/* \/\*(?s:.)\*\/ {} */


%option noyywrap

%option yylineno

espaco [ \t\n\r\s]
alfabetico [a-zA-Z]+
digito [[:digit:]]
inteiro [\+\-]?{digito}+

START	"/*"
END	"*/"
SIMPLE	[^*]
COMPLEX	\*/[^/]

/*\/\*[^(\*\/)]*\*\/ {}*/
%x C

%%

{START}	  	{yymore(); BEGIN C;}
<C>{SIMPLE}	{yymore();}
<C>{COMPLEX} {yymore();}
<C>{END}  	{ECHO; BEGIN 0;}

(\/\/).* {}
{espaco} {}
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
{inteiro} { return TK_LIT_INT; }
{inteiro}.{digito}+ { return TK_LIT_FLOAT; }
{inteiro}.{digito}+(e|E){inteiro} { return TK_LIT_FLOAT; }

{alfabetico} { return TK_IDENTIFICADOR; }

[\,\;\:\(\)\{\}\+\-\*\/\%\^\<\>\=\!] { return yytext[0]; }

. { return TK_ERRO; }

%%

