/* Funcoes auxiliares */

#include <stdio.h>

extern int yylineno;

int get_line_number()
{
	return yylineno;
}

void yyerror(const char *msg)
{
	printf("%s at line %d\n", msg, get_line_number());
}
