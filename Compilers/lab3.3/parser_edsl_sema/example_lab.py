import abc
import enum
import parser_edsl as pe
import sys
import re
import typing
from dataclasses import dataclass


class SemanticError(pe.Error):
    pass


class RepeatedVariable(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'Повторная переменная {self.varname}'


class UnknownVar(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'Необъявленная переменная {self.varname}'


class BinBadType(SemanticError):
    def __init__(self, pos, left, op, right):
        self.pos = pos
        self.left = left
        self.op = op
        self.right = right

    @property
    def message(self):
        return f'Несовместимые типы: {self.left} {self.op} {self.right}'


class UnBadType(SemanticError):
    def __init__(self, pos, op, type_):
        self.pos = pos
        self.op = op
        self.type = type_

    @property
    def message(self):
        return f'Несовместимый тип: {self.op} {self.type}'


class NotBoolCond(SemanticError):
    def __init__(self, pos, type_):
        self.pos = pos
        self.type = type_

    @property
    def message(self):
        return f'Условие имеет тип {self.type} вместо логического'


class NotIntFor(SemanticError):
    def __init__(self, pos, type_):
        self.pos = pos
        self.type = type_

    @property
    def message(self):
        return f'Ожидался целый тип, получен {self.type}'

    @staticmethod
    def check(type_, pos):
        if type_ != Type.Integer:
            raise NotIntFor(pos, type_)


class Type(enum.Enum):
    Integer = 'INTEGER'
    Real = 'REAL'
    Boolean = 'BOOLEAN'


@dataclass
class VarDef:
    name : str
    name_coord : pe.Position
    type : Type

    @pe.ExAction
    def create(attrs, coords, res_coord):
        name, type_ = attrs
        cname, ccolon, ctype, csemicolon = coords
        return VarDef(name, cname.start, type_)


class Statement(abc.ABC):
    @abc.abstractmethod
    def check(self, vars):
        pass


@dataclass
class Program:
    var_defs : list[VarDef]
    statements : list[Statement]

    def check(self):
        vars = {}
        for var in self.var_defs:
            if var.name in vars:
                raise RepeatedVariable(var.name_coord, var.name)
            else:
                vars[var.name] = var.type

        for statement in self.statements:
            statement.check(vars)


@dataclass
class Expr(abc.ABC):
    @abc.abstractmethod
    def check(self, vars):
        pass


@dataclass
class AssignStatement(Statement):
    variable : str
    var_coord : pe.Position
    expr : Expr

    @pe.ExAction
    def create(attrs, coords, res_coord):
        var, expr = attrs
        cvar, cass, cexpr = coords
        return AssignStatement(var, cass.start, expr)

    def check(self, vars):
        if self.variable not in vars:
            raise UnknownVar(self.variable, self.var_coord)

        self.expr.check(vars)
        if vars[self.variable] == self.expr.type:
            return
        if vars[self.variable] == Type.Real and self.expr.type == Type.Integer:
            return

        raise BinBadType(self.var_coord, vars[self.variable], ':=', self.expr.type)


@dataclass
class IfStatement(Statement):
    condition : Expr
    cond_coord : pe.Fragment
    then_branch : Statement
    else_branch : Statement

    @pe.ExAction
    def create(attrs, coords, res_coord):
        cond, then_branch, else_branch = attrs
        cif, ccond, cthen_kw, cthen_br, celse_kw, celse_br = coords
        return IfStatement(cond, ccond, then_branch, else_branch)

    def check(self, vars):
        self.condition.check(vars)
        if self.condition.type != Type.Boolean:
            raise NotBoolCond(self.cond_coord, self.condition.type)
        self.then_branch.check(vars)
        self.else_branch.check(vars)


@dataclass
class WhileStatement(Statement):
    condition : Expr
    cond_coord : pe.Fragment
    body : Statement

    @pe.ExAction
    def create(attrs, coords, res_coord):
        cond, body = attrs
        cwhile_kw, ccond, cdo_kw, cbody = coords
        return WhileStatement(cond, ccond, body)

    def check(self, vars):
        self.condition.check(vars)
        if self.condition.type != Type.Boolean:
            raise NotBoolCond(self.cond_coord, self.condition.type)
        self.body.check(vars)


@dataclass
class ForStatement(Statement):
    variable : str
    var_coord : pe.Position
    start : Expr
    start_coord : pe.Fragment
    end : Expr
    end_coord : pe.Fragment
    body : Statement

    @pe.ExAction
    def create(attrs, coords, res_coord):
        varname, start, end, body = attrs
        cfor_kw, cvar, cass, cstart, cto_kw, cend, cdo_kw, cbody = coords
        return ForStatement(varname, cvar.start, start, cstart, end, cend, body)

    def check(self, vars):
        if self.variable not in vars:
            raise UnknownVar(self.variable, self.var_coord)
        NotIntFor.check(vars[self.variable], self.var_coord)
        self.start.check(vars)
        NotIntFor.check(self.start.type, self.start_coord)
        self.end.check(vars)
        NotIntFor.check(self.end.type, self.end_coord)
        self.body.check(vars)


@dataclass
class BlockStatement(Statement):
    body : list[Statement]

    def check(self, vars):
        for statement in self.body:
            statement.check(vars)


@dataclass
class EmptyStatement(Statement):
    def check(self, vars):
        pass


@dataclass
class VariableExpr(Expr):
    varname : str
    var_coord : pe.Position

    @pe.ExAction
    def create(attrs, coords, res_coord):
        varname, = attrs
        cvarname, = coords
        return VariableExpr(varname, cvarname)

    def check(self, vars):
        try:
            self.type = vars[self.varname]
        except KeyError:
            raise UnknownVar(self.var_coord, self.varname)


@dataclass
class ConstExpr(Expr):
    value : typing.Any
    type : Type

    def check(self, vars):
        pass


@dataclass
class BinOpExpr(Expr):
    left : Expr
    op : str
    op_coord : pe.Position
    right : Expr

    @pe.ExAction
    def create(attrs, coords, res_coord):
        left, op, right = attrs
        cleft, cop, cright = coords
        return BinOpExpr(left, op, cop.start, right)

    def check(self, vars):
        self.left.check(vars)
        self.right.check(vars)

        common_type = None
        is_numeric = lambda t: t in (Type.Integer, Type.Real)

        if self.left.type == self.right.type:
            common_type = self.left.type
        elif is_numeric(self.left.type) and is_numeric(self.right.type):
            common_type = Type.Real

        self.type = None
        if self.op in ('<', '>', '<=', '>=', '=', '<>'):
            if common_type != None:
                self.type = Type.Boolean
        elif self.op in ('and', 'or'):
            if common_type == Type.Boolean:
                self.type = Type.Boolean
        elif self.op in ('div', 'mod'):
            if common_type == Type.Integer:
                self.type = Type.Integer
        elif self.op in ('+', '-', '*', '**'):
            if is_numeric(common_type):
                self.type = common_type
        else:
            assert self.op == '/'
            if is_numeric(common_type):
                self.type = Type.Real

        if self.type == None:
            raise BinBadType(self.op_coord, self.left.type,
                    self.op, self.right.type)


@dataclass
class UnOpExpr(Expr):
    op : str
    op_coord : pe.Position
    expr : Expr

    @staticmethod
    def create(op):
        @pe.ExAction
        def action(attrs, coords, res_coords):
            expr, = attrs
            cop, cexpr = coords
            return UnOpExpr(op, cop.start, expr)

        return action

    def check(self, vars):
        self.expr.check(vars)
        if self.op in ('+', '-') and \
                self.expr.type not in (Type.Integer, Type.Real):
            raise UnBadType(self.op_coord, self.op, self.expr.type)
        if self.op == 'not' and self.expr.type != Type.Boolean:
            raise UnBadType(self.op_coord, self.op, self.expr.type)
        self.type = self.expr.type


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

NTerm, NMulOp, NFactor, NPowerOp, NPower, NConst = \
    map(pe.NonTerminal, 'Term MulOp Factor PowerOp Power Const'.split())


NProgram |= KW_VAR, NVarDefs, KW_BEGIN, NStatements, KW_END, Program

NVarDefs |= lambda: []
NVarDefs |= NVarDefs, NVarDef, lambda vds, vd: vds + [vd]

NVarDef |= VARNAME, ':', NType, ';', VarDef.create

NType |= KW_INTEGER, lambda: Type.Integer
NType |= KW_REAL, lambda: Type.Real
NType |= KW_BOOLEAN, lambda: Type.Boolean

NStatements |= NStatement, lambda st: [st]
NStatements |= NStatements, ';', NStatement, lambda sts, st: sts + [st]

NStatement |= VARNAME, ':=', NExpr, AssignStatement.create
NStatement |= (
    KW_IF, NExpr, KW_THEN, NStatement, KW_ELSE, NStatement, IfStatement.create
)
NStatement |= KW_WHILE, NExpr, KW_DO, NStatement, WhileStatement.create
NStatement |= (
    KW_FOR, VARNAME, ':=', NExpr, KW_TO, NExpr, KW_DO, NStatement,
    ForStatement.create
)
NStatement |= KW_BEGIN, NStatements, KW_END, BlockStatement
NStatement |= EmptyStatement

NExpr |= NArithmExpr
NExpr |= NArithmExpr, NCmpOp, NArithmExpr, BinOpExpr.create

def make_op_lambda(op):
    return lambda: op

for op in ('>', '<', '>=', '<=', '=', '<>'):
    NCmpOp |= op, make_op_lambda(op)

NArithmExpr |= NTerm
NArithmExpr |= '+', NTerm, UnOpExpr.create('+')
NArithmExpr |= '-', NTerm, UnOpExpr.create('-')
NArithmExpr |= NArithmExpr, NAddOp, NTerm, BinOpExpr.create

NAddOp |= '+', lambda: '+'
NAddOp |= '-', lambda: '-'
NAddOp |= KW_OR, lambda: 'or'

NTerm |= NFactor
NTerm |= NTerm, NMulOp, NFactor, BinOpExpr.create

NMulOp |= '*', lambda: '*'
NMulOp |= '/', lambda: '/'
NMulOp |= KW_DIV, lambda: 'div'
NMulOp |= KW_MOD, lambda: 'mod'
NMulOp |= KW_AND, lambda: 'and'

NFactor |= NPower
NFactor |= NPower, NPowerOp, NFactor, BinOpExpr.create
NPowerOp |= '**', lambda: '**'

NPower |= KW_NOT, NPower, UnOpExpr.create('not')
NPower |= VARNAME, VariableExpr.create
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
            tree.check()
            print('Семантических ошибок не найдено')
    except pe.Error as e:
        print(f'Ошибка {e.pos}: {e.message}')
