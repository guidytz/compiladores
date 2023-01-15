all: parser scanner main
	gcc parser.tab.o lex.yy.o main.o extra.o -o etapa2

main: main.c
	gcc main.c -c
	gcc extra.c -c -Wno-discarded-qualifiers

scanner: parser scanner.l
	flex scanner.l
	gcc lex.yy.c -c

parser: parser.y
	bison -d parser.y
	gcc parser.tab.c -c

clean:
	rm *.o
	rm etapa2
	rm lex.yy.c
	rm parser.tab.*