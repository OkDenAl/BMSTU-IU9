% Лабораторная работа № 2.2 «Абстрактные синтаксические деревья»
% 27 марта 2024 г.
% Окутин Денис, ИУ9-61Б

# Цель работы
Целью данной работы является получение навыков составления грамматик и 
проектирования синтаксических деревьев.

# Индивидуальный вариант
Язык [L4](https://hw.iu9.bmstu.ru/static/assets/L4.pdf).

Вложенные комментарии реализовывать не нужно.

# Реализация

## Абстрактный синтаксис  
Сформулируем абстрактный синтаксис.

Программа представляет собой набор определений функций:
```
Program → Funcs*
```
Функция состоит из заголовка и тела:
```
Funcs → FuncHeader FuncBody
```
Заголовок функции бывает 2х типов, для функции, которая
возвращает значение, и для функции, которая не вовзращает значение:
```
FuncHeader → (Type [FUNCNAME FuncParams]) | [FUNCNAME FuncParams]
```
Параметры функции — ноль и более объявлений параметров.
```
FuncParams → Var*
```
Тело функции состоит из последовательности операторов и завершается %%:
```
FuncBody → Statements %%
```
Тип — целый, символьный, логический, массив, двумерный массив:
```
Type → INT | CHAR | BOOL | ARRAY | DOUBLE_ARRAY
```
Последовательность операторов — ноль или более операторов, разделённых запятой:
```
Statements → Statement , … , Statement | ε
```
Оператор — присваивание новой переменной, вызов функции, присваивание существующей
переменной, объяавление перменной, условие с 2 ветками, условие с 1 веткой,
цикл с условием, цикл for, выход из функции, оператор-предупреждение

```
Statement → Var := Expr
          | [VARNAME VARNAME*]
          | VARNAME := Expr
          | Var
          | (? Expr) Statements +++ Statements %
          | (? Expr) Statements %
          | (& Expr) Statements %
          | (Var : Expr, Expr, Expr) Statements %
          | ^ Expr
          | \ Expr
          | ε
```
Переменные представляются как (тип имя):
```
Var → (Type VARNAME)
```
Выражение — переменная, константа, двуместная операция, одноместная операция:
```
Expr → VARNAME
     | SpecOp
     | Const
     | Expr BinOp Expr
     | UnOp Expr
```
```
SpecOp → ARRAY_ACCESS | FUNC_CALL | NEW
Const → INT_CONST | CHAR_CONST | STRING_CONST | REF_CONST | TRUE | FALSE 
BinOp → + | - | * | / | POW | AND | OR | XOR | EQ | NE | LT | GT | LE | GE | MOD
UnOp →  - | NOT
```

## Лексическая структура и конкретный синтаксис

Перейдём к конкретной грамматике:
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

Statement → ExtendedVar := Expr
          | [FUNCNAME Args]
          | BasicVar
          | (? Expr) Statements +++ Statements %
          | (? Expr) Statements %
          | (& Expr) Statements %
          | Cycle Statements %
          | ^ Expr
          | \ Expr  
          
Cycle → (CycleNewVar : Expr, Expr, INT_CONST) | (CycleNewVar : Expr, Expr)
CycleNewVar → Type VARNAME                 
Args → VARNAME | Args VARNAME

Expr → LogicalExpr
     | Expr OR LogicalExpr
     | Expr XOR LogicalExpr
     
LogicalExpr → CompareExpr | LogicalExpr AND ComareExpr
      
CompareExpr → ArithmExpr | ArithmExpr CmpOp ArithmExpr      
CmpOp → _eq_ | _ne_ | _lt_ | _gt_ | _le_ | _ge_

ArithmExpr → PowExpr | ArithmExpr AddOp PowExpr
AddOp → + | - 

PowExpr → Term | Term _pow_ PowExpr

Term → Factor | Term MulOp Factor
MulOp → * | / | MOD

Factor → NOT Spec | - Spec | Spec

Spec → [VARNAME Args] | new_ Type VARNAME | new_ Type INT_CONST | Const | Var

ExtendedVar → BasicVar | Var
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
## Программная реализация

```python
import abc
import enum
import parser_edsl.parser_edsl as pe
import sys
from dataclasses import dataclass
from pprint import pprint
from typing import Any


class Type(abc.ABC):
    pass


class BasicType(enum.Enum):
    Integer = 'int'
    Char = 'char'
    Boolean = 'bool'

@dataclass
class CharSequenceType(Type):
    value: str

@dataclass
class ArrayType:
    type: Type


@dataclass
class BasicVar:
    type: Type or BasicType
    var_name: Any


@dataclass
class CycleNewVar:
    type: Type or BasicType
    var_name: Any


class FuncHeader(abc.ABC):
    pass


@dataclass
class FuncHeaderFull(FuncHeader):
    type: Type or BasicType
    func_name: Any
    func_params: list[BasicVar]


@dataclass
class FuncHeaderShort(FuncHeader):
    func_name: Any
    func_params: list[BasicVar]


class FuncBody(abc.ABC):
    pass


class Statement(abc.ABC):
    pass


@dataclass
class Func:
    header: FuncHeader
    body: FuncBody


class Expr(abc.ABC):
    pass


@dataclass
class AssignStatement(Statement):
    variable: Any
    expr: Expr


@dataclass
class FuncCallStatement(Expr):
    func_name: Any
    args: Expr


@dataclass
class NewVarStatement(Statement):
    variable: Any


@dataclass
class NewAllocStatement(Statement):
    type: Type
    alloc_size: Any


@dataclass
class IfStatementFull(Statement):
    condition: Expr
    then_branch: Statement
    else_branch: Statement


@dataclass
class IfStatementOneBranch(Expr):
    condition: Expr
    then_branch: Statement


@dataclass
class ArrayElemByVar(Expr):
    array: Expr
    index: Expr


@dataclass
class WhileStatement(Statement):
    condition: Expr
    body: Statement


class ForHeader(abc.ABC):
    pass


@dataclass
class ForHeaderFull(ForHeader):
    for_val: CycleNewVar
    start: Expr
    end: Expr
    step: Any


@dataclass
class ForHeaderShort(ForHeader):
    for_val: CycleNewVar
    start: Expr
    end: Expr


@dataclass
class ForStatement(Statement):
    header: ForHeader
    body: Statement


@dataclass
class ReturnStatement(Statement):
    expr: Expr


@dataclass
class WarningStatement(Statement):
    expr: Expr


@dataclass
class Var(Expr):
    var_name: Any


@dataclass
class ConstExpr(Expr):
    value: Any
    type: Type or BasicType


@dataclass
class BinOpExpr(Expr):
    left: Expr
    op: str
    right: Expr


@dataclass
class UnOpExpr(Expr):
    op: str
    expr: Expr


# лексическая структура
INTEGER = pe.Terminal('INTEGER', '(([A-Za-z0-9]+{\d+})|\d+)', str, priority=7)
CHAR = pe.Terminal('CHAR', '\"\p{L}?\"', str)
STRING = pe.Terminal('STRING', '\'.*\'', str)
VARNAME = pe.Terminal('VARNAME', '[_|!|@|.|#][\p{L}]*', str)
FUNCNAME = pe.Terminal('FUNCNAME', '[\p{L}]*', str)
REF_CONST = ('REF_CONST', 'nothing', str)


def make_keyword(image):
    return pe.Terminal(image, image, lambda name: None, priority=10)


KW_INT, KW_CHAR, KW_BOOL = \
    map(make_keyword, 'int char bool'.split())

KW_XOR, KW_OR, KW_MOD, KW_AND, KW_NOT, KW_TRUE, KW_FALSE = \
    map(make_keyword, 'XOR OR MOD AND NOT TRUE FALSE'.split())

(NProgram, NFunc, NFuncHeader, NFuncParams, NBasicVar,
 NType, NBasicType, NStatements, NFuncCall) = \
    map(pe.NonTerminal, 'Program Func FuncHeader FuncParams'
                        ' BasicVar Type BasicType Statements FuncCall'.split())

NStatement, NExpr, NCycle, NCycleVar, NArgs, NLogicalExpr, NCompareExpr, NFuncBody = \
    map(pe.NonTerminal, 'Statement Expr Cycle CycleNewVar Args LogicalExpr CompareExpr FuncBody'.split())

(NCmpOp, NArithmExpr,NExtendedVar, NAddOp, NPowExpr,
 NTerm, NMulOp, NFactor, NSpec, NConst, NVar) = \
    map(pe.NonTerminal, 'CmpOp ArithmExpr NExtendedVar '
                        'AddOp PowExpr Term MulOp Factor Spec Const Var'.split())

# грамматика
NProgram |= NFunc, lambda st: [st]
NProgram |= NProgram, NFunc, lambda fncs, fn: fncs + [fn]

NFunc |= NFuncHeader, NFuncBody, Func

NFuncHeader |= '(', NType, '[', FUNCNAME, NFuncParams, ']', ')', FuncHeaderFull
NFuncHeader |= '[', FUNCNAME, NFuncParams, ']', FuncHeaderShort

NFuncBody |= NStatements, '%%'

NFuncParams |= NBasicVar, lambda st: [st]
NFuncParams |= NFuncParams, NBasicVar, lambda vars, var: vars + [var]

NBasicVar |= '(', NType, VARNAME, ')', BasicVar

NType |= NBasicType
NType |= '<', NType, '>', ArrayType

NBasicType |= KW_INT, lambda: BasicType.Integer
NBasicType |= KW_CHAR, lambda: BasicType.Char
NBasicType |= KW_BOOL, lambda: BasicType.Boolean

NStatements |= NStatement, lambda st: [st]
NStatements |= NStatements, ',', NStatement, lambda sts, st: sts + [st]

NStatement |= '^', NExpr, ReturnStatement
NStatement |= '\\', NExpr, WarningStatement
NStatement |= NExtendedVar, ':=', NExpr, AssignStatement
NStatement |= '[', FUNCNAME, NArgs, ']', FuncCallStatement
NStatement |= NBasicVar, NewVarStatement
NStatement |= '(', '?', NExpr, ')', NStatements, '+++', NStatements, '%', IfStatementFull
NStatement |= '(', '?', NExpr, ')', NStatements, '%', IfStatementOneBranch
NStatement |= '(', '&', NExpr, ')', NStatements, '%', WhileStatement
NStatement |= NCycle, NStatements, '%', ForStatement

NCycle |= '(', NCycleVar, ':', NExpr, ',', NExpr, ',', INTEGER, ')', ForHeaderFull
NCycle |= '(', NCycleVar, ':', NExpr, ',', NExpr, ')', ForHeaderShort
NCycleVar |= NType, VARNAME, CycleNewVar

NArgs |= VARNAME, lambda vn: [vn]
NArgs |= NArgs, VARNAME, lambda args, arg: args + [arg]


def make_op_lambda(op):
    return lambda: op


for op in ('_eq_', '_ne_', '_lt_', '_gt_', '_le_', '_ge_'):
    NCmpOp |= op, make_op_lambda(op)

NExpr |= NLogicalExpr
NExpr |= NExpr, KW_OR, NLogicalExpr, BinOpExpr
NExpr |= NExpr, KW_XOR, NLogicalExpr, BinOpExpr

NLogicalExpr |= NCompareExpr
NLogicalExpr |= NLogicalExpr, KW_AND, NCompareExpr, BinOpExpr

NCompareExpr |= NArithmExpr
NCompareExpr |= NArithmExpr, NCmpOp, NArithmExpr, BinOpExpr

NArithmExpr |= NPowExpr
NArithmExpr |= NPowExpr, NAddOp, NPowExpr, BinOpExpr

NAddOp |= '+', lambda: '+'
NAddOp |= '-', lambda: '-'

NPowExpr |= NTerm
NPowExpr |= NTerm, '_pow_', NPowExpr, lambda p, f: BinOpExpr(p, '_pow_', f)

NTerm |= NFactor
NTerm |= NFactor, NMulOp, NTerm, BinOpExpr

NMulOp |= '*', lambda: '*'
NMulOp |= '/', lambda: '/'
NMulOp |= KW_MOD, lambda: 'mod'

NFactor |= KW_NOT, NSpec, lambda p: UnOpExpr('not', p)
NFactor |= '-', NSpec, lambda t: UnOpExpr('-', t)
NFactor |= NSpec

NFuncCall |= '[', FUNCNAME, NArgs, ']', FuncCallStatement

NSpec |= NFuncCall
NSpec |= 'new_', NType, VARNAME, NewAllocStatement
NSpec |= 'new_', NType, INTEGER, NewAllocStatement
NSpec |= NConst
NSpec |= NVar
NSpec |= '(', NExpr, ')'

NVar |= VARNAME, Var
NVar |= '<', NSpec, NExpr, '>', ArrayElemByVar

NExtendedVar |= NBasicVar
NExtendedVar |= NVar

NConst |= INTEGER, lambda v: ConstExpr(v, BasicType.Integer)
NConst |= CHAR, lambda v: ConstExpr(v, BasicType.Char)
NConst |= STRING, CharSequenceType
NConst |= KW_TRUE, lambda: ConstExpr(True, BasicType.Boolean)
NConst |= KW_FALSE, lambda: ConstExpr(False, BasicType.Boolean)

parser = pe.Parser(NProgram)
assert parser.is_lalr_one()
# parser.print_table()

# пробельные символы
parser.add_skipped_domain('\s')
# комментарии вида { … }
parser.add_skipped_domain('\\{.*?\\}')

for filename in sys.argv[1:]:
    try:
        with open(filename) as f:
            tree = parser.parse(f.read())
            pprint(tree)
    except pe.Error as e:
        print(f'Ошибка {e.pos}: {e.message}')
    except Exception as e:
        print(e)
```

# Тестирование

## Входные данные

```
(<int> [SumVectors (<<int>> !A) (<int> !B)])
    (int #size) := [length !A],
    \ #size _eq_ [length !B],
    (<int> #C) := new_ <int> 10f{10},
    #x := #a*2 + 10,
    #x := #a*(2 + 10),
    #x := #a _pow_ 2 _pow_ 10,
    <!A 5> := <#B 6> * 10,
    <[Func #x #y] !i+1> := 0,
    (<int> #P) := new_ <int> 10,
    (<int> #i : 0, #size - 1)
        <#C #i> := <!A #i> + <!B #i>
        { <#C 5> := <!A #i> + <!B #i> }
    %,
    (?#a _lt_ 0)
        #sign := -1
    +++
        (?#a _eq_ 0)
            #sign := 0
        +++
            #sign :=1
        %
    %,
    ^ #C
%%
```

## Вывод на `stdout`
<!-- ENABLE LONG LINES -->
```
[Func(header=FuncHeaderFull(type=ArrayType(type=<BasicType.Integer: 'int'>),
                            func_name='SumVectors',
                            func_params=[BasicVar(type=ArrayType(type=ArrayType(type=<BasicType.Integer: 'int'>)),
                                                  var_name='!A'),
                                         BasicVar(type=ArrayType(type=<BasicType.Integer: 'int'>),
                                                  var_name='!B')]),
      body=[AssignStatement(variable=BasicVar(type=<BasicType.Integer: 'int'>,
                                              var_name='#size'),
                            expr=FuncCallStatement(func_name='length',
                                                   args=['!A'])),
            WarningStatement(expr=BinOpExpr(left=Var(var_name='#size'),
                                            op='_eq_',
                                            right=FuncCallStatement(func_name='length',
                                                                    args=['!B']))),
            AssignStatement(variable=BasicVar(type=ArrayType(type=<BasicType.Integer: 'int'>),
                                              var_name='#C'),
                            expr=NewAllocStatement(type=ArrayType(type=<BasicType.Integer: 'int'>),
                                                   alloc_size='10f{10}')),
            AssignStatement(variable='#x',
                            expr=BinOpExpr(left=BinOpExpr(left=Var(var_name='#a'),
                                                          op='*',
                                                          right=ConstExpr(value='2',
                                                                          type=<BasicType.Integer: 'int'>)),
                                           op='+',
                                           right=ConstExpr(value='10',
                                                           type=<BasicType.Integer: 'int'>))),
            AssignStatement(variable='#x',
                            expr=BinOpExpr(left=Var(var_name='#a'),
                                           op='_pow_',
                                           right=BinOpExpr(left=ConstExpr(value='2',
                                                                          type=<BasicType.Integer: 'int'>),
                                                           op='_pow_',
                                                           right=ConstExpr(value='10',
                                                                           type=<BasicType.Integer: 'int'>)))),
            AssignStatement(variable='#x',
                            expr=BinOpExpr(left=Var(var_name='#a'),
                                           op='*',
                                           right=BinOpExpr(left=ConstExpr(value='2',
                                                                          type=<BasicType.Integer: 'int'>),
                                                           op='+',
                                                           right=ConstExpr(value='10',
                                                                           type=<BasicType.Integer: 'int'>)))),
            AssignStatement(variable=ArrayElemByVar(array='!A',
                                                    index=ConstExpr(value='5',
                                                                    type=<BasicType.Integer: 'int'>)),
                            expr=BinOpExpr(left=ArrayElemByVar(array='#B',
                                                               index=ConstExpr(value='6',
                                                                               type=<BasicType.Integer: 'int'>)),
                                           op='*',
                                           right=ConstExpr(value='10',
                                                           type=<BasicType.Integer: 'int'>))),
            AssignStatement(variable=ArrayElemByVar(array=FuncCallStatement(func_name='Func',
                                                                            args=['#x',
                                                                                  '#y']),
                                                    index=BinOpExpr(left=Var(var_name='!i'),
                                                                    op='+',
                                                                    right=ConstExpr(value='1',
                                                                                    type=<BasicType.Integer: 'int'>))),
                            expr=ConstExpr(value='0',
                                           type=<BasicType.Integer: 'int'>)),
            AssignStatement(variable=BasicVar(type=ArrayType(type=<BasicType.Integer: 'int'>),
                                              var_name='#P'),
                            expr=NewAllocStatement(type=ArrayType(type=<BasicType.Integer: 'int'>),
                                                   alloc_size='10')),
            ForStatement(header=ForHeaderShort(for_val=CycleNewVar(type=ArrayType(type=<BasicType.Integer: 'int'>),
                                                                var_name='#i'),
                                               start=ConstExpr(value='0',
                                                               type=<BasicType.Integer: 'int'>),
                                               end=BinOpExpr(left=Var(var_name='#size'),
                                                             op='-',
                                                             right=ConstExpr(value='1',
                                                                             type=<BasicType.Integer: 'int'>))),
                         body=[AssignStatement(variable=ArrayElemByVar(array='#C',
                                                                       index=Var(var_name='#i')),
                                               expr=BinOpExpr(left=ArrayElemByVar(array='!A',
                                                                                  index=Var(var_name='#i')),
                                                              op='+',
                                                              right=ArrayElemByVar(array='!B',
                                                                                   index=Var(var_name='#i'))))]),
            IfStatementFull(condition=BinOpExpr(left=Var(var_name='#a'),
                                                op='_lt_',
                                                right=ConstExpr(value='0',
                                                                type=<BasicType.Integer: 'int'>)),
                            then_branch=[AssignStatement(variable='#sign',
                                                         expr=UnOpExpr(op='-',
                                                                       expr=ConstExpr(value='1',
                                                                                      type=<BasicType.Integer: 'int'>)))],
                            else_branch=[IfStatementFull(condition=BinOpExpr(left=Var(var_name='#a'),
                                                                             op='_eq_',
                                                                             right=ConstExpr(value='0',
                                                                                             type=<BasicType.Integer: 'int'>)),
                                                         then_branch=[AssignStatement(variable='#sign',
                                                                                      expr=ConstExpr(value='0',
                                                                                                     type=<BasicType.Integer: 'int'>))],
                                                         else_branch=[AssignStatement(variable='#sign',
                                                                                      expr=ConstExpr(value='1',
                                                                                                     type=<BasicType.Integer: 'int'>))])]),
            ReturnStatement(expr=Var(var_name='#C'))])]
```

# Вывод
В ходе данной лабораторной работы был получен навык составления грамматик и 
проектирования синтаксических деревьев.

Самым трудным и вместе с тем интересным в данной лабораторной работе мне показалось составление
описания грамматики языка, после этого разобраться с интерфейсом библиотеки уже не составило труда.

Кстати, интересный факт, стандартная библиотека regex, которая была использована в коде библиотеки,
не умела работать с выражениями вида `p{L}` - регулярное описание любого символа Unicode (с
такой записью я познакомился в рамках предыдущих лабораторных работ по курсу 
Конструирование Компиляторов). Так что я её заменил на библиотеку, поддерживающую такое написание.
Вот так :)