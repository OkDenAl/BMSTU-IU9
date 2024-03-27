% Лабораторная работа № 1.5 «Порождение лексического анализатора с помощью flex»
% 20 марта 2024г.
% Денис Окутин, ИУ9-61Б

# Цель работы
Целью данной работы является изучение генератора лексических анализаторов flex.

# Индивидуальный вариант
Числовые литералы: знак «0» либо последовательности знаков «1».
Строковые литералы: регулярные строки — ограничены двойными кавычками, могут содержать
escape-последовательности `«\"»`, `«\t»`, `«\n»`, не пересекают границы строк текста;
буквальные строки — начинаются на «@"», заканчиваются на двойную кавычку,
пересекают границы строк текста, для включения двойной кавычки она удваивается.

# Реализация

```lex
%{
   #include <iostream>
   #include <string>

   #define TAG_NUM 1
   #define TAG_STRING 2
   #define TAG_ERROR 3

   using namespace std;

   string tag_names[] = {
           "END_OF_PROGRAM", "NUM", "STRING", "ERROR"
   };

   typedef struct Position Position;
   struct Position {
       int line, pos, index;
   };

   void print_pos(Position *p) {
       cout<<"("<<p->line<<","<< p->pos <<")";
   }

   struct Fragment {
       Position starting, following;
   };

   typedef struct Fragment YYLTYPE;

   void print_frag(struct Fragment* f) {
       print_pos(&(f->starting));
       cout<<"-";
       print_pos(&(f->following));
       cout<<": ";
   }

   struct Token {
       long num;
       string str;

       Token() {
           num=0;
           str="";
       }
   };

   typedef struct Token YYSTYPE;

   int continued;
   struct Position cur;
   struct Position start;

   #define YY_USER_ACTION {             \
           int i;                           \
           if (!continued)                  \
               yylloc->starting = cur;      \
               start=cur;                   \
           continued = 0;                   \
                                            \
           for (i = 0; i < yyleng; i++) {   \
               if (yytext[i] == '\n') {     \
                   cur.line++;              \
                   cur.pos = 1;             \
               }                            \
               else                         \
                   cur.pos++;               \
               cur.index++;                 \
           }                                \
           yylloc->following = cur;         \
       }

   void init_scanner (const char * program){
       continued = 0;
       cur.line = 1;
       cur.pos = 1;
       cur.index = 0;
       yy_scan_string(program);
   }

   void err (const string& msg, const string& ch){
       cout<< "Error";
       print_pos(&start);
       cout<<": "<< msg<<" \""<<ch<<"\""<<endl;
   }
%}

%option noyywrap bison-bridge bison-locations

NUM             (0|(1)+)

%x LITERAL REGULAR

%%

[\n\t ]+

\" {
                    yylval->str="";
    				BEGIN(REGULAR);
    				continued = 1;
}

<REGULAR>\" {
           BEGIN(0);
           return TAG_STRING;
}

<REGULAR>\n {
          err ("ERROR unknown symbol","\\n");
          BEGIN(0);
}

<REGULAR><<EOF>>  {
          err ("end of program found, \" expected;", "EOF");
          BEGIN(0);
}

<REGULAR>\\n {
          yylval->str.append("\n");
          continued = 1;
}

<REGULAR>\\t {
          yylval->str.append("\t");
          continued = 1;
}

<REGULAR>\\\" {
          yylval->str.append("\"");
          continued = 1;
}

<REGULAR>. {
          yylval->str.append(string(1, yytext[0]));
          continued = 1;
}

@\" {
        yylval->str="";
    	BEGIN(LITERAL);
    	continued = 1;
}

<LITERAL>\"\" {
          yylval->str.append("\"");
          continued = 1;
}

<LITERAL>\" {
                     BEGIN(0);
                     return TAG_STRING;
}

<LITERAL>\n {
          yylval->str.append("\n");
          continued = 1;
}

<LITERAL>. {
        yylval->str.append(string(1, yytext[0]));
        continued = 1;
}

<LITERAL><<EOF>>  {
          err ("end of program found, \" expected;", "EOF");
          BEGIN(0);
}


{NUM} {
       yylval->num = atoi(yytext);
       return TAG_NUM;
}

. {
    err ("ERROR unknown symbol",string(1, yytext[0]));
}

<<EOF>>               return 0;

%%

int main(){
    YYSTYPE value;
    YYLTYPE coords;
    FILE *input;
    char *buf;
    struct Token token;
    input = fopen("test1.txt","r");
    fseek(input, 0, SEEK_END);
    long size = ftell(input);
    rewind(input);
    buf = (char*)malloc(sizeof(char) * (size + 1));
    fread(buf, sizeof(char), size, input);
    buf[size] = '\0';

    init_scanner(buf);
    int tag = yylex(&value,&coords);

    while (tag != 0){
        cout<< tag_names[tag];
        print_frag(&coords);

        if (tag == TAG_NUM) {
            cout<< value.num;
        }

        if (tag == TAG_STRING) {
            cout<<value.str;
        }

       cout<<endl;
       tag = yylex(&value,&coords);
       if (tag == 0) break;
    }

    free(buf);
    return 0;
}
```

# Тестирование

Входные данные

```
111
00
"regular \n1\""
&
@"test1
test2
test3"
ident "
```

Вывод на `stdout`

```
NUM(1,1)-(1,4): 111
NUM(2,1)-(2,2): 0
NUM(2,2)-(2,3): 0
STRING(3,1)-(3,16): regular 
1"
Error(4,1): ERROR unknown symbol "&"
STRING(5,1)-(7,7): test1
test2
test3
Error(8,1): ERROR unknown symbol "i"
Error(8,2): ERROR unknown symbol "d"
Error(8,3): ERROR unknown symbol "e"
Error(8,4): ERROR unknown symbol "n"
Error(8,5): ERROR unknown symbol "t"
Error(8,7): end of program found, " expected; "EOF"
```

# Вывод
Входе выполнения данной работы был приобретён практический навык работы
с генератором лексических анализаторов flex. Т.к flex поддерживается на C или C++,
то пришлось вспомнить как работать с языком C, однако для удобной работы
со строками я всё же подключил возможности C++.

Кодогенерация это, безусловно, мощный инструментарий, но как по мне
это немного повышает порог входа в понимание того, что происходит в программе.
Тем неменее это был достаточно интересный опыт.