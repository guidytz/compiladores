
%%

(?s)/\*.*?\*/ ;

(?-s)//.* ;
[ \t\n\r\s] ;

\'(?-s:.)?\' "TK_LIT_CHAR"

int "TK_PR_INT"
float "TK_PR_FLOAT"
bool "TK_PR_BOOL"
char "TK_PR_CHAR"
if "TK_PR_IF"
then "TK_PR_THEN"
else "TK_PR_ELSE"
while "TK_PR_WHILE"
input "TK_PR_INPUT"
output "TK_PR_OUTPUT"
return "TK_PR_RETURN"
for "TK_PR_FOR"

\<\= "TK_OC_LE"
>= "TK_OC_GE"
== "TK_OC_EQ"
!= "TK_OC_NE"
&& "TK_OC_AND"
\|\| "TK_OC_OR"


false "TK_LIT_FALSE"
true "TK_LIT_TRUE"
[0-9]+ "TK_LIT_INT"
[0-9]+\.[0-9]+([eE][\+\-]?[0-9]+)? "TK_LIT_FLOAT"

[a-zA-Z]+ "TK_IDENTIFICADOR"

[\,] ","
[\;] ";"
[\:] ":"
[\(] "("
[\)] ")"
[\{] "{"
[\}] "}"
[\+] "+"
[\-] "-"
[\*] "*"
[\/] "/"
[\%] "%"
[\^] "^"
[\<] "<"
[\>] ">"
[\=] "="
[\!] "!"
[\[] "["
[\]] "]"

. "TK_ERRO"