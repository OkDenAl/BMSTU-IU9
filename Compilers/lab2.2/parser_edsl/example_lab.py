import abc
import enum
import parser_edsl as pe
import sys
import re
import typing
from dataclasses import dataclass
from pprint import pprint


class Type(enum.Enum):
    Integer = 'INTEGER'
    Real = 'REAL'
    Boolean = 'BOOLEAN'


@dataclass
class VarDef:
    name : str
    type : Type


class Statement(abc.ABC):
    pass


@dataclass
class Program:
    var_defs : list[VarDef]
    statements : list[Statement]


class Expr(abc.ABC):
    pass


@dataclass
class AssignStatement(Statement):
    variable : str
    expr : Expr


@dataclass
class IfStatement(Statement):
    condition : Expr
    then_branch : Statement
    else_branch : Statement


@dataclass
class WhileStatement(Statement):
    condition : Expr
    body : Statement


@dataclass
class ForStatement(Statement):
    variable : str
    start : Expr
    end : Expr
    body : Statement


@dataclass
class BlockStatement(Statement):
    body : list[Statement]


@dataclass
class EmptyStatement(Statement):
    pass


@dataclass
class VariableExpr(Expr):
    varname : str


@dataclass
class ConstExpr(Expr):
    value : typing.Any
    type : Type


@dataclass
class BinOpExpr(Expr):
    left : Expr
    op : str
    right : Expr


@dataclass
class UnOpExpr(Expr):
    op : str
    expr : Expr


INTEGER = pe.Terminal('INTEGER', '[0-9]+', int, priority=7)
REAL = pe.Terminal('REAL', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
VARNAME = pe.Terminal('VARNAME', '[A-Za-z][A-Za-z0-9]*', str.upper)

def make_keyword(image):
    return pe.Terminal(image, image, lambda name: None,
                       re_flags=re.IGNORECASE, priority=10)

KW_VAR, KW_BEGIN, KW_END, KW_INTEGER, KW_REAL, KW_BOOLEAN = \
    map(make_keyword, 'var begin end integer real boolean'.split())

KW_IF, KW_THEN, KW_ELSE, KW_WHILE, KW_DO, KW_FOR, KW_TO = \
    map(make_keyword, 'if then else while do for to'.split())

KW_OR, KW_DIV, KW_MOD, KW_AND, KW_NOT, KW_TRUE, KW_FALSE = \
    map(make_keyword, 'or div mod and not true false'.split())


NProgram, NVarDefs, NVarDef, NType, NStatements = \
    map(pe.NonTerminal, 'Program VarDefs VarDef Type Statements'.split())

NStatement, NExpr, NCmpOp, NArithmExpr, NAddOp = \
    map(pe.NonTerminal, 'Statement Expr CmpOp ArithmOp AddOp'.split())

NTerm, NMulOp, NFactor, NPower, NConst = \
    map(pe.NonTerminal, 'Term MulOp Factor Power Const'.split())


NProgram |= KW_VAR, NVarDefs, KW_BEGIN, NStatements, KW_END, Program

NVarDefs |= lambda: []
NVarDefs |= NVarDefs, NVarDef, lambda vds, vd: vds + [vd]

NVarDef |= VARNAME, ':', NType, ';', VarDef

NType |= KW_INTEGER, lambda: Type.Integer
NType |= KW_REAL, lambda: Type.Real
NType |= KW_BOOLEAN, lambda: Type.Boolean

NStatements |= NStatement, lambda st: [st]
NStatements |= NStatements, ';', NStatement, lambda sts, st: sts + [st]

NStatement |= VARNAME, ':=', NExpr, AssignStatement
NStatement |= (
    KW_IF, NExpr, KW_THEN, NStatement, KW_ELSE, NStatement, IfStatement
)
NStatement |= KW_WHILE, NExpr, KW_DO, NStatement, WhileStatement
NStatement |= (
    KW_FOR, VARNAME, ':=', NExpr, KW_TO, NExpr, KW_DO, NStatement, ForStatement
)
NStatement |= KW_BEGIN, NStatements, KW_END, BlockStatement
NStatement |= EmptyStatement

NExpr |= NArithmExpr
NExpr |= NArithmExpr, NCmpOp, NArithmExpr, BinOpExpr

def make_op_lambda(op):
    return lambda: op

for op in ('>', '<', '>=', '<=', '=', '<>'):
    NCmpOp |= op, make_op_lambda(op)

NArithmExpr |= NTerm
NArithmExpr |= '+', NTerm, lambda t: UnOpExpr('+', t)
NArithmExpr |= '-', NTerm, lambda t: UnOpExpr('-', t)
NArithmExpr |= NArithmExpr, NAddOp, NTerm, BinOpExpr

NAddOp |= '+', lambda: '+'
NAddOp |= '-', lambda: '-'
NAddOp |= KW_OR, lambda: 'or'

NTerm |= NFactor
NTerm |= NTerm, NMulOp, NFactor, BinOpExpr

NMulOp |= '*', lambda: '*'
NMulOp |= '/', lambda: '/'
NMulOp |= KW_DIV, lambda: 'div'
NMulOp |= KW_MOD, lambda: 'mod'
NMulOp |= KW_AND, lambda: 'and'

NFactor |= NPower
NFactor |= NPower, '**', NFactor, lambda p, f: BinOpExpr(p, '**', f)

NPower |= KW_NOT, NPower, lambda p: UnOpExpr('not', p)
NPower |= VARNAME, VariableExpr
NPower |= NConst
NPower |= '(', NExpr, ')'

NConst |= INTEGER, lambda v: ConstExpr(v, Type.Integer)
NConst |= REAL, lambda v: ConstExpr(v, Type.Real)
NConst |= KW_TRUE, lambda: ConstExpr(True, Type.Boolean)
NConst |= KW_FALSE, lambda: ConstExpr(False, Type.Boolean)


p = pe.Parser(NProgram)
assert p.is_lalr_one()

p.add_skipped_domain('\\s')
p.add_skipped_domain('(\\(\\*|\\{).*?(\\*\\)|\\})')


for filename in sys.argv[1:]:
    try:
        with open(filename) as f:
            tree = p.parse(f.read())
            pprint(tree)
    except pe.Error as e:
        print(f'Ошибка {e.pos}: {e.message}')
    except Exception as e:
        print(e)
