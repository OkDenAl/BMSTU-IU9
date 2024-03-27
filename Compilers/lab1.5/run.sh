flex -o lex.yy.cpp lab6.l
g++ -std=c++17 -o lexer *.cpp
./lexer < test1.txt
rm *.cpp