#!/bin/bash

flex lexer.l
bison -d parser.y
gcc -o lab *.c
./lab need_format.txt