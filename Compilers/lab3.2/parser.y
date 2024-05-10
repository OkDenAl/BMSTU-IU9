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