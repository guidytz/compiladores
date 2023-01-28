#!/bin/bash
create=$1

if [[ $create == "specials" ]]; then
    specials=", ; : ( ) { } + - / % ^ < > = ! [ ]" # falta o '*'
    count=0
    for ch in $specials; do
        ((++count))
        echo "Creating: $ch"
        echo -n $ch > "inputs/sp${count}.txt"
        echo "1 TK_ESPECIAL [$ch]" > "outputs/sp${count}.txt"
    done
    ((++count))
    echo "Creating: *"
    echo -n "*" > "inputs/sp${count}.txt"
    echo "1 TK_ESPECIAL [*]" > "outputs/sp${count}.txt"
fi

if [[ $create == "int" ]]; then
    lit_int="1 2 10 -5 0123 045 0098 -15"
    count=0
    for ch in $lit_int; do
        echo "Creating: $ch"
        ((++count))
        echo -n $ch > "inputs/lit_int${count}.txt"
        echo -n "1 TK_LIT_INT [$ch]" > "outputs/lit_int${count}.txt"
    done
fi

if [[ $create == "float" ]]; then
    lit_float="1.09 2.123 10.0 -5.76 0123.098 045.123 0098.1230 -15.76 12.0e-2 3.0e1 12.2E4 -09.2E9"
    count=0
    for ch in $lit_float; do
        echo "Creating: $ch"
        ((++count))
        echo -n $ch > "inputs/lit_float${count}.txt"
        echo -n "1 TK_LIT_FLOAT [$ch]" > "outputs/lit_float${count}.txt"
    done
fi

if [[ $create == "char" ]]; then
    lit_char="'1' 'a' 'b' ',' ';' ':' '(' ')' '{' '}' '+' '-' '/' '%' '^' '<' '>' '=' '!' '*' ''" # falta o ' '
    count=0
    for ch in $lit_char; do
        echo "Creating: $ch"
        ((++count))
        echo -n $ch > "inputs/lit_char${count}.txt"
        echo -n "1 TK_LIT_CHAR [$ch]" > "outputs/lit_char${count}.txt"
    done
fi

