% "Лабораторная работа 3.2 «Форматтер исходных текстов»"
% 15 мая 2023 г.
% Окутин Денис, ИУ9-61Б

# Цель работы
Целью данной работы является приобретение навыков использования генератора синтаксических анализаторов bison.

# Индивидуальный вариант
Слабый форматтер, для языка [L4](https://hw.iu9.bmstu.ru/static/assets/L4.pdf).

# Реализация
Грамматика и Лексическая структура

```
Program → Program Func | Func
Func →  FuncHeader FuncBody
FuncHeader → (Type [FUNCNAME FuncParams]) | [FUNCNAME FuncParams]
FuncParams → FuncParams BasicVar | BasicVar
BasicVar → (Type VARNAME)
Type → BasicType | <Type>
BasicType → INTEGER | CHAR | BOOL
FuncBody → Statements %%
Statements → Statement | Statements , Statement
Statement → Var := Expr
          | [FUNCNAME Args]
          | BasicVar
          | ( StatementTail
          | ^ Expr
          | \ Expr  
StatementTail → Type VARNAME ) InnerVar | & Expr) Statements % | Cycle Statements % 
          | (? Expr) Statements ElseStatement
ElseStatement → +++ Statements % | %
InnerVar → EPS | := Expr
Cycle → CycleVar : Expr, Expr CycleTail
CycleTail → , INT_CONST ) | )
CycleVar → Type VARNAME                 
Args → VARNAME | Args VARNAME
Expr → LogicalExpr
     | Expr _or_ LogicalExpr
     | Expr _xor_ LogicalExpr
LogicalExpr → CompareExpr | LogicalExpr _and_ ComareExpr
CompareExpr → ArithmExpr | ArithmExpr CmpOp ArithmExpr      
CmpOp → _eq_ | _ne_ | _lt_ | _gt_ | _le_ | _ge_
ArithmExpr → PowExpr | ArithmExpr AddOp PowExpr
AddOp → + | - 
PowExpr → Term | Term _pow_ PowExpr
Term → Factor | Term MulOp Factor
MulOp → * | / | _mod_
Factor → not_ Spec | - Spec | Spec
Spec → [VARNAME Args] | new_ Type NewTail | Const | Var
NewTail → VARNAME | INT_CONST
Var → <Spec Expr> | VARNAME
Const → INT_CONST | CHAR_CONST | STRING_CONST | REF_CONST | TRUE | FALSE 
```
Лексическая струтктура:
```
VARNAME = [_|!|@|.|#][\p{L}]*
FUNCNAME = [\p{L}]*
REF_CONST = nothing
INT_CONST  = (([A-Za-z0-9]+{\d+})|\d+)
CHAR_CONST  = \"\p{L}?\"
STRING_CONST  = \'\.*\'
```

Лексер
```lex
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
```

Парсер
```c
%{
#include <stdio.h>
#include "lexer.h"
%}

%define parse.error verbose
%define api.pure
%locations
%lex-param {yyscan_t scanner}  /* параметр для yylex() */
/* параметры для yyparse() */
%parse-param {yyscan_t scanner}
%parse-param {long env[26]}
%parse-param {int tab}
%parse-param {bool need_tab}

%union {
    char* number;
    char* char_const;
    char* string;
    char* varname;
    char* funcname;
    char* comment;
}

%left OR XOR
%left AND
%left EQ NE LT GT LE GE
%left PLUS MINUS
%right POW
%left MUL DIV MOD
%left NOT UNARY_MINUS
%left NEW

%token LEFT_PAREN_1 RIGHT_PAREN_1 LEFT_PAREN_2 RIGHT_PAREN_2 LEFT_PAREN_3 RIGHT_PAREN_3
%token INT CHAR BOOL STATEMENTS_END COMMA ASSIGN IF ELSE STATEMENT_EXPR_END
%token WHILE WARNING RETURN COLON TRUE FALSE NEW_LINE

%token <funcname> FUNCNAME
%token <varname> VARNAME
%token <number> NUMBER
%token <char_const> CHAR_CONST
%token <string> STRING_CONST
%token <ref_const> REF_CONST
%token <comment> COMMENT

%{
int yylex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, yyscan_t scanner);
void yyerror(YYLTYPE *loc, yyscan_t scanner, long env[26], int tab, bool need_tab, const char *message);
%}

%{
void print_tabs(int tab) {
    for(int i = 0; i < tab; i++) {
        printf("    ");
    }
}


%}


%%
Program:
        Program NewLineCheck {printf("\n"); tab=0;} CommentCheck Func
        | Func
        ;
Func:
        FuncHeader NewLineCheck {if (!need_tab) printf(" "); tab++;} CommentCheck FuncBody
        ;
CommentCheck:
        | COMMENT {if (need_tab) { print_tabs(tab); need_tab = false;} printf("%s", $COMMENT);} NewLineCheck CommentCheck
        ;
NewLineCheck:
        NEW_LINE {need_tab=true;}
        | {need_tab = false;}
        ;
FuncHeader:
        LEFT_PAREN_1 {printf("(");} Type NewLineCheck CommentCheck LEFT_PAREN_2 {if (!need_tab) printf(" "); else {tab++; print_tabs(tab);} printf("[");}
         FUNCNAME[FN] {printf("%s ", $FN);} NewLineCheck CommentCheck FuncParams {if (need_tab) {tab=1; print_tabs(tab);}}
         RIGHT_PAREN_2 {printf("]");} NewLineCheck  {if (need_tab) tab=0;} CommentCheck RIGHT_PAREN_1 {printf(")");}
        | LEFT_PAREN_2 {printf("[");} FUNCNAME[FN] {printf("%s ", $FN);} NewLineCheck CommentCheck FuncParams {if (need_tab) {tab--; print_tabs(tab);}}
          RIGHT_PAREN_2 {printf("]");}
        ;
FuncParams:
        FuncParams {if (need_tab) print_tabs(tab); else printf(" ");} BasicVar
        | {if (need_tab) {tab++; print_tabs(tab);}} BasicVar
        ;
BasicVar:
        LEFT_PAREN_1 {printf("(");} Type VARNAME[L] {printf(" %s", $L);} RIGHT_PAREN_1 {printf(")");} NewLineCheck  CommentCheck
        ;
Type:
        BasicType
        | LEFT_PAREN_3 {printf("<");} Type RIGHT_PAREN_3 {printf(">");}
        ;
BasicType:
        INT {printf("int");}
        | CHAR {printf("char");}
        | BOOL {printf("bool");}
        ;
FuncBody:
        Statements NewLineCheck CommentCheck STATEMENTS_END {{if (!need_tab) printf(" "); else {tab--; print_tabs(tab);}}printf("%%%%");}
        ;
Statements:
        {if (need_tab) {print_tabs(tab);}} Statement
        | Statements COMMA {printf(",");} NewLineCheck {if (!need_tab) printf(" ");} CommentCheck {if (!need_tab) printf(" "); else {print_tabs(tab);}} Statement
        ;
Statement:
        LEFT_PAREN_2 {printf("[");} FUNCNAME[L] {printf("%s ", $L);} Args RIGHT_PAREN_2 {printf("]");}
        | LEFT_PAREN_1 {printf("(");} StatementTail
        | WARNING {printf("\\ ");} Expr
        | RETURN {printf("^ ");} Expr
        | Var ASSIGN {printf(" := ");} Expr
        ;
StatementTail:
        Cycle NewLineCheck {if (!need_tab) printf(" "); tab++;} CommentCheck Statements NewLineCheck {if (!need_tab) printf(" ");}
         CommentCheck {if (!need_tab) printf(" "); tab--;} STATEMENT_EXPR_END {if (need_tab) print_tabs(tab); printf("%%");}
        | WHILE {printf("& ");} Expr RIGHT_PAREN_1 {printf(")");} NewLineCheck {if (!need_tab) printf(" "); tab++;} CommentCheck Statements
         NewLineCheck {if (!need_tab) printf(" "); tab--;} CommentCheck STATEMENT_EXPR_END {if (need_tab) print_tabs(tab); printf("%%");}
        | IF {printf("? ");} Expr RIGHT_PAREN_1 {printf(")");} NewLineCheck {if (!need_tab) printf(" "); tab++;} CommentCheck Statements
         NewLineCheck {if (!need_tab) printf(" "); tab--;} CommentCheck ElseStatement
        | Type VARNAME {printf(" %s", $VARNAME);} RIGHT_PAREN_1 VarTail
        ;
VarTail:
        ASSIGN {printf(" := ");} Expr
        |
        ;
ElseStatement:
        ELSE {if (need_tab) print_tabs(tab); printf("+++");} NewLineCheck {if (!need_tab) printf(" "); tab++;} CommentCheck Statements
         NewLineCheck {if (!need_tab) printf(" "); tab--;} CommentCheck STATEMENT_EXPR_END {if (need_tab) print_tabs(tab); printf("%%");}
        | STATEMENT_EXPR_END {if (need_tab) print_tabs(tab); printf("%%");}
        ;
Cycle:
        CycleVar COLON {printf(" ");} Expr COMMA {printf(", ");} Expr CycleTail
        ;
CycleTail:
        COMMA {printf(", ");} NUMBER {printf("%s",$NUMBER);} RIGHT_PAREN_1 {printf(")");}
        | RIGHT_PAREN_1 {printf(")");}
        ;
CycleVar:
        Type VARNAME[VN] {printf(" %s", $VN);}
        ;
Args:
        VARNAME {printf("%s ", $VARNAME);}
        | Args VARNAME {printf("%s", $VARNAME);}
        ;
Expr:
        LogicalExpr
        | Expr OR {printf(" _or_ ");} LogicalExpr
        | Expr XOR {printf(" _xor_ ");} LogicalExpr
        ;
LogicalExpr:
        CompareExpr
        | LogicalExpr AND {printf(" _and_ ");} CompareExpr
        ;
CompareExpr:
        ArithmExpr
        | ArithmExpr CmpOp ArithmExpr
        ;
CmpOp:
        EQ {printf(" _eq_ ");}
        | NE {printf(" _ne_ ");}
        | LT {printf(" _lt_ ");}
        | GT {printf(" _gt_ ");}
        | LE {printf(" _le_ ");}
        | GE {printf(" _ge_ ");}
        ;
ArithmExpr:
        PowExpr
        | ArithmExpr AddOp PowExpr
        ;
AddOp:
        PLUS {printf(" + ");}
        | MINUS {printf(" - ");}
        ;
PowExpr:
        Term
        | Term POW {printf(" _pow_ ");} PowExpr
        ;
Term:
        Factor
        | Term MulOp Factor
        ;
MulOp:
        MUL {printf(" * ");}
        | DIV {printf(" / ");}
        | MOD {printf(" _mod_ ");}
        ;
Factor:
        NOT {printf(" not_ ");} Spec
        | MINUS {printf("-");} Spec %prec UNARY_MINUS
        | Spec
        ;
Spec:
        LEFT_PAREN_2 {printf("[");} FUNCNAME {printf("%s ", $FUNCNAME);} Args RIGHT_PAREN_2 {printf("]");}
        | NEW {printf("new_ ");} Type NewTail
        | Const
        | Var
        | LEFT_PAREN_1 {printf("(");} Expr RIGHT_PAREN_1 {printf(")");}
        ;
NewTail:
        VARNAME {printf(" %s", $VARNAME);}
        | NUMBER {printf(" %s", $NUMBER);}
        ;
Var:
        LEFT_PAREN_3 {printf("<");} Spec {printf(" ");} Expr RIGHT_PAREN_3 {printf(">");}
        | VARNAME {printf("%s", $VARNAME);}
        ;
Const:
        NUMBER {printf("%s", $NUMBER);}
        | CHAR_CONST {printf("%s", $CHAR_CONST);}
        | STRING_CONST {printf("%s", $STRING_CONST);}
        | REF_CONST {printf("nothing");}
        | TRUE {printf("true");}
        | FALSE {printf("false");}
        ;
%%



int main(int argc, char *argv[]) {
    FILE *input = 0;
    long env[26] = { 0 };
    int tab = 0;
    bool need_tab = false;
    yyscan_t scanner;
    struct Extra extra;

    if (argc > 1) {
        printf("Read file %s\n", argv[1]);
        input = fopen(argv[1], "r");
    } else {
        printf("No file in command line, use stdin\n");
        input = stdin;
    }

    init_scanner(input, &scanner, &extra);
    yyparse(scanner, env, tab, need_tab);
    destroy_scanner(scanner);

    if (input != stdin) {
        fclose(input);
    }

    return 0;
}
```
# Тестирование

Входные данные

```
(<int>
[SumVectors
(int !A)(char !B)(int !B)
]
){ another comma}
(int #C) :=new_ int #size,
#x := #a * 2 + 10,
#x := #a * (2 + 10), { good job } #x := #a _pow_ 2 _pow_ 10,
#x := #a _pow_ 2 _pow_ 10,
<!A 5> := <#B 6> * 10,
<[Func #x #y] !i + 1> := 0,
(<int>#P) :=   new_ <int> 10,
                            (&#a _lt_ 0)#a := -1%,
(<int> #i : 0, #size - 1)
<#C #i> := <!A#i>+ <!B #i>
{ <#C 5> := <!A #i> + <!B #i> }
%,
(? #a _lt_ 0)
#sign := -1
+++
(? #a _eq_ 0)
#sign := 0
+++
#sign := 1
%
%,
^ #C
%%
(<int> [Main (<<int>> !A) (<int> !B)])
(int @a) := 'i like books',
@a := <<!A 1> 2>,
^ #C
%%
```

Вывод на `stdout`

```vb
(<int>
    [SumVectors 
        (int !A) (char !B) (int !B)
    ]
) { another comma}
    (int #C) := new_ int #size,
    #x := #a * 2 + 10,
    #x := #a * (2 + 10), { good job } #x := #a _pow_ 2 _pow_ 10,
    #x := #a _pow_ 2 _pow_ 10,
    <!A 5> := <#B 6> * 10,
    <[Func #x #y] !i + 1> := 0,
    (<int> #P) := new_ <int> 10,
    (& #a _lt_ 0) #a := -1 %,
    (<int> #i : 0, #size - 1)
        <#C #i> := <!A #i> + <!B #i>
        { <#C 5> := <!A #i> + <!B #i> }
    %,
    (? #a _lt_ 0)
        #sign := -1
    +++
        (? #a _eq_ 0)
            #sign := 0
        +++
            #sign := 1
        %
    %,
    ^ #C
%%

(<int> [Main (<<int>> !A) (<int> !B)])
    (int @a) := 'i like books',
    @a := <<!A 1> 2>,
    ^ #C
%%
```

# Вывод
В ходе данной лабораторной работы был получен навык использования генератора
синтаксических анализаторов bison и закреплены навыки работы с flex.
Был разработан слабый форматтер, который преобразует исходный код без отступов в более
читабельный.
Лабораторная работа оказалась очень интересной, так как получилось разработать то, что 
используется повсеместно в языках программирования, в частности в любимом мною Golang.