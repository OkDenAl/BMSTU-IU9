%option reentrant noyywrap bison-bridge bison-locations
%option extra-type="struct Extra *"

/* Подавление предупреждений для -Wall */
%option noinput nounput

%{

#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "parser.tab.h"  /* файл генерируется Bison’ом */

#define YY_USER_ACTION \
  { \
    int i; \
    struct Extra *extra = yyextra; \
    if (! extra->continued ) { \
      yylloc->first_line = extra->cur_line; \
      yylloc->first_column = extra->cur_column; \
    } \
    extra->continued = false; \
    for (i = 0; i < yyleng; ++i) { \
      if (yytext[i] == '\n') { \
        extra->cur_line += 1; \
        extra->cur_column = 1; \
      } else { \
        extra->cur_column += 1; \
      } \
    } \
    yylloc->last_line = extra->cur_line; \
    yylloc->last_column = extra->cur_column; \
  }

void yyerror(YYLTYPE *loc, yyscan_t scanner, long env[26], int tab, bool need_tab, const char *message) {
    printf("\nError (%d,%d): %s\n", loc->first_line, loc->first_column, message);
}
%}

%%

[\t ]+

[\n]+ {
printf("\n");
return NEW_LINE;
}

int return INT;
char return CHAR;
bool return BOOL;

_and_ return AND;
_eq_ return EQ;
_ge_ return GE;
_gt_ return GT;
_lt_ return LT;
_le_ return LE;
_mod_ return MOD;
_ne_ return NE;
_or_ return OR;
_xor_ return XOR;
_pow_ return POW;
new_ return NEW;
not_ return NOT;

(%%) return STATEMENTS_END;
(%) return STATEMENT_EXPR_END;
\? return IF;
\\ return WARNING;
\+\+\+ return ELSE;
\& return WHILE;
:=  return ASSIGN;
\: return COLON;
\+  return PLUS;
\-  return MINUS;
\*  return MUL;
\/  return DIV;
\^  return RETURN;
\(  return LEFT_PAREN_1;
\)  return RIGHT_PAREN_1;
\[  return LEFT_PAREN_2;
\]  return RIGHT_PAREN_2;
\<  return LEFT_PAREN_3;
\>  return RIGHT_PAREN_3;
, return COMMA;
true return TRUE;
false return FALSE;

nothing return REF_CONST;

(([A-Za-z0-9]+\{[0-9]+\})|[0-9]+) {
    yylval->number = yytext;
    return NUMBER;
}

([_|!|@|.|#][A-Za-z]+)  {
    yylval->varname = yytext;
    return VARNAME;
}

(\'[A-Za-z ]*\')   {
    yylval->string = yytext;
    return STRING_CONST;
}

[A-Za-z]*  {
    yylval->funcname = yytext;
    return FUNCNAME;
}

\"[A-Za-z]?\"   {
    yylval->char_const = yytext;
    return CHAR_CONST;
}

\{.*\} {
    yylval->comment = yytext;
    return COMMENT;
}

%%

void init_scanner(FILE *input, yyscan_t *scanner, struct Extra *extra) {
    extra->continued = false;
    extra->cur_line = 1;
    extra->cur_column = 1;

    yylex_init(scanner);
    yylex_init_extra(extra, scanner);
    yyset_in(input, *scanner);
}

void destroy_scanner(yyscan_t scanner) {
    yylex_destroy(scanner);
}