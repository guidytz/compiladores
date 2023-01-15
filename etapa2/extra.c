/* Funcoes auxiliares */

#include <stdio.h>
#include <string.h>

extern int yylineno;

int get_line_number()
{
	return yylineno;
}

char *match(char *word)
{
	if (strcmp(word, "TK_OC_LE") == 0)
		return "'<='";
	if (strcmp(word, "TK_OC_GE") == 0)
		return "'>='";
	if (strcmp(word, "TK_OC_EQ") == 0)
		return "'=='";
	if (strcmp(word, "TK_OC_NE") == 0)
		return "'!='";
	if (strcmp(word, "TK_OC_AND") == 0)
		return "'&&'";
	if (strcmp(word, "TK_OC_OR") == 0)
		return "'||'";

	return word;
}

void yyerror(const char *msg)
{
	char *word;
	word = strtok(msg, " ");
	while (word != NULL)
	{
		printf("%s ", match(word));
		word = strtok(NULL, " ");
	}
	printf("at line %d\n", get_line_number());
}
