main: main.c
	gcc main.c -c
	gcc extra.c -c

scanner: scanner.l
	flex scanner.l
	gcc lex.yy.c -c # gera lex.yy.o

all: main scanner
	gcc lex.yy.o main.o extra.o -o etapa1

clean:
	rm *.o
	rm etapa1