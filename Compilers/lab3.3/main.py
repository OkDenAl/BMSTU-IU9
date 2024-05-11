import abc
import enum
from abc import ABC

import parser_edsl_sema.parser_edsl as pe
import sys
from dataclasses import dataclass
from pprint import pprint
from typing import Any

vars = {}
need_return = False


def open_array_type(tp, c):
    if type(tp) == ArrayType:
        c += 1
        return open_array_type(tp.type, c)
    else:
        return tp, c


def pretty_type_view(type):
    t, c = open_array_type(type, 0)

    res_t = "<" * c
    if t == BasicType.Integer:
        res_t += "int"
    elif t == BasicType.Char:
        res_t += "char"
    elif t == BasicType.Boolean:
        res_t += "bool"
    elif t == CharSequenceType:
        res_t += "string_const"
    elif t == RefConstType:
        res_t += "ref_const"
    elif t == "void":
        res_t += "void"
    res_t += ">" * c

    return res_t


class SemanticError(pe.Error):
    pass


class RepeatedVariable(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'Повторное объявление переменной {self.varname}'


class AssignTypeMismatched(SemanticError):
    def __init__(self, pos, lt, rt):
        self.pos = pos
        self.lt = lt
        self.rt = rt

    @property
    def message(self):
        return f'Невозможно типу {self.lt} присвоить тип {self.rt}'


class UnknownVar(SemanticError):
    def __init__(self, pos, varname):
        self.pos = pos
        self.varname = varname

    @property
    def message(self):
        return f'Необъявленная переменная: {self.varname}'


class UnknownFunc(SemanticError):
    def __init__(self, pos, funcname):
        self.pos = pos
        self.funcname = funcname

    @property
    def message(self):
        return f'Необъявленная функция {self.funcname}'


class RepeatedFunc(SemanticError):
    def __init__(self, pos, funcname):
        self.pos = pos
        self.funcname = funcname

    @property
    def message(self):
        return f'Повторное объявление функции {self.funcname}'


class MismatchedFuncArgsCount(SemanticError):
    def __init__(self, pos, fn, need, actual):
        self.pos = pos
        self.fn = fn
        self.need = need
        self.actual = actual

    @property
    def message(self):
        return (f'Для вызова функции {self.fn} необходимо {self.need} аргументa(ов),'
                f' но получено {self.actual}')


class MismatchedFuncArgsType(SemanticError):
    def __init__(self, pos, fn, need, actual):
        self.pos = pos
        self.fn = fn
        self.need = need
        self.actual = actual

    @property
    def message(self):
        return (f'Для вызова функции {self.fn} необходим аргумент типа {self.need},'
                f' но получен аргумент типа {self.actual}')


class CycleVarBadType(SemanticError):
    def __init__(self, pos, type):
        self.pos = pos
        self.type = type

    @property
    def message(self):
        return f'Ожидался тип char или int, получен тип: {self.type}'


class AllocSizeBadType(SemanticError):
    def __init__(self, pos, type):
        self.pos = pos
        self.type = type

    @property
    def message(self):
        return f'Ожидался тип int, получен тип: {self.type}'


class ArrayIndexBadType(SemanticError):
    def __init__(self, pos, type):
        self.pos = pos
        self.type = type

    @property
    def message(self):
        return f'Ожидался тип char или int, получен тип: {self.type}'


class ArrayBadType(SemanticError):
    def __init__(self, pos, type):
        self.pos = pos
        self.type = type

    @property
    def message(self):
        return f'Ожидался тип массива, получен тип: {self.type}'


class BinBadType(SemanticError):
    def __init__(self, pos, left, op, right):
        self.pos = pos
        self.left = left
        self.op = op
        self.right = right

    @property
    def message(self):
        return f'Несовместимые типы для операции \'{self.op}\': {self.left} {self.op} {self.right}'


class ReturnBadType(SemanticError):
    def __init__(self, pos, f_t, r_t):
        self.pos = pos
        self.f_t = f_t
        self.r_t = r_t

    @property
    def message(self):
        return f'Ожидалось вовзращаемое значение типа {self.f_t}, но получено значение типа {self.r_t}'


class UnBadType(SemanticError):
    def __init__(self, pos, op, type_):
        self.pos = pos
        self.op = op
        self.type = type_

    @property
    def message(self):
        return f'Несовместимый тип: {self.op} {self.type}'


class Type(abc.ABC):
    pass


class Statement(abc.ABC):
    @abc.abstractmethod
    def check(self, funcs, ret_type):
        pass


class FuncHeader(abc.ABC):
    pass

    @abc.abstractmethod
    def check(self):
        pass


class Expr(abc.ABC):
    @abc.abstractmethod
    def check(self, funcs, ret_type):
        pass


class BasicType(enum.Enum):
    Integer = 'int'
    Char = 'char'
    Boolean = 'bool'


@dataclass
class CharSequenceType(Type):
    value: str
    type: Type

    def check(self, funcs, ret_type):
        pass


@dataclass
class RefConstType(Type):
    value: str
    type: Type

    def check(self, funcs, ret_type):
        pass


@dataclass
class ArrayType:
    type: Type


@dataclass
class BasicVar:
    type: Type or BasicType
    var_name: str
    var_name_pos: pe.Position

    @pe.ExAction
    def create(attrs, coords, res_coord):
        t, var_name = attrs
        cob, ct, cvar, ccb = coords
        return BasicVar(t, var_name, cvar.start)

    def check(self, funcs, ret_type):
        if self.var_name in vars:
            raise RepeatedVariable(self.var_name_pos, self.var_name)
        vars[self.var_name] = self.type


@dataclass
class CycleNewVar:
    type: BasicType
    type_pos: pe.Position
    var_name: str
    var_name_pos: pe.Position

    @pe.ExAction
    def create(attrs, coords, res_coord):
        t, var_name = attrs
        ct, cvar = coords
        return CycleNewVar(t, ct.start, var_name, cvar.start)

    def check(self):
        if self.type not in (BasicType.Integer, BasicType.Char):
            res_t = pretty_type_view(self.type)
            raise CycleVarBadType(self.type_pos, res_t)

        if self.var_name in vars:
            raise RepeatedVariable(self.var_name_pos, self.var_name)

        vars[self.var_name] = self.type


@dataclass
class FuncHeaderFull(FuncHeader):
    type: Type or BasicType
    func_name: str
    func_name_pos: pe.Position
    func_params: list[BasicVar]

    @pe.ExAction
    def create(attrs, coords, res_coord):
        t, func_name, func_params = attrs
        return FuncHeaderFull(t, func_name, coords[3].start, func_params)

    def check(self):
        for param in self.func_params:
            param.check(None, None)


@dataclass
class FuncHeaderShort(FuncHeader):
    func_name: str
    func_name_pos: pe.Position
    func_params: list[BasicVar]

    @pe.ExAction
    def create(attrs, coords, res_coord):
        func_name, func_params = attrs
        return FuncHeaderShort(func_name, coords[1].start, func_params)

    def check(self):
        for param in self.func_params:
            param.check(None, None)


@dataclass
class FuncBody(abc.ABC):
    statements: list[Statement]

    def check(self, funcs, ret_type):
        for stmt in self.statements:
            stmt.check(funcs, ret_type)


@dataclass
class Func:
    header: FuncHeader
    body: FuncBody

    def check(self, funcs):
        global need_return
        self.header.check()
        if isinstance(self.header, FuncHeaderFull):
            need_return = True
            ret_type = self.header.type
        else:
            need_return = False
            ret_type = "void"
        self.body.check(funcs, ret_type)
        if need_return:
            raise Exception("Need return statement")
        need_return = False


@dataclass
class Program:
    funcs: list[Func]

    def check(self):
        global vars
        funcs = {}
        for func in self.funcs:
            if func.header.func_name in funcs:
                raise RepeatedFunc(func.header.func_name_pos, func.header.func_name)
            funcs[func.header.func_name] = func.header
        for func in self.funcs:
            vars = {}
            func.check(funcs)


@dataclass
class Var(Expr):
    var_name: str
    var_name_pos: pe.Position
    type: Type or BasicType

    @pe.ExAction
    def create(attrs, coords, res_coord):
        return Var(attrs[0], coords[0].start, None)

    def check(self, funcs, ret_type):
        if self.var_name not in vars:
            raise UnknownVar(self.var_name_pos, self.var_name)
        self.type = vars[self.var_name]


@dataclass
class ArrayElemByVar(Expr):
    array: Expr
    index: Expr
    index_pos: pe.Position
    type: Type or BasicType

    @pe.ExAction
    def create(attrs, coords, res_coord):
        arr, ind = attrs
        return ArrayElemByVar(arr, ind, coords[3].start, None)

    def check(self, funcs, ret_type):
        self.array.check(funcs, ret_type)
        self.index.check(funcs, ret_type)

        is_numeric = lambda t: t in (BasicType.Integer, BasicType.Char)

        if isinstance(self.array.type, ArrayType):
            self.type = self.array.type.type
        else:
            res_t = pretty_type_view(self.type)
            raise ArrayBadType(self.array.var_name_pos, res_t)

        if not is_numeric(self.index.type):
            res_t = pretty_type_view(self.type)
            raise ArrayIndexBadType(self.index_pos, res_t)

        if not isinstance(self.array.type, ArrayType):
            if self.array.var_name not in vars:
                raise UnknownVar(self.array.var_name_pos, self.array.var_name)


@dataclass
class NewVarStatement(Statement):
    variable: BasicVar
    var_coord: pe.Position

    @pe.ExAction
    def create(attrs, coords, res_coord):
        return NewVarStatement(attrs[0], coords[0].start)

    def check(self, funcs, ret_type):
        self.variable.check(funcs, ret_type)


@dataclass
class AssignStatement(Statement):
    variable: Var or BasicVar or ArrayElemByVar
    var_coord: pe.Position
    expr: Expr

    @pe.ExAction
    def create(attrs, coords, res_coord):
        var, expr = attrs
        cvar, cassign, cexpr = coords
        return AssignStatement(var, cassign.start, expr)

    def check(self, funcs, ret_type):
        if isinstance(self.expr, ArrayElemByVar):
            self.expr.check(funcs, ret_type)
            self.variable.check(funcs, ret_type)
        else:
            self.variable.check(funcs, ret_type)
            self.expr.check(funcs, ret_type)

        if (type(self.variable.type) == ArrayType or isinstance(self.variable, RefConstType)) \
                and (type(self.expr.type) == ArrayType or isinstance(self.expr, RefConstType)):
            return

        if self.variable.type == BasicType.Integer and self.expr.type == BasicType.Char:
            return

        if self.variable.type != self.expr.type:
            res_t_l = pretty_type_view(self.variable.type)
            res_t_r = pretty_type_view(self.expr.type)

            if res_t_l == "<char>" and res_t_r =="string_const":
                return

            raise AssignTypeMismatched(self.var_coord, res_t_l, res_t_r)


@dataclass
class FuncCallStatement(Statement):
    func_name: str
    func_name_coord: pe.Position
    args: Expr
    args_coord: pe.Position
    type: Type or BasicType

    @pe.ExAction
    def create(attrs, coords, res_coord):
        fn, expr = attrs
        cob, cfn, cargs, ccb = coords
        return FuncCallStatement(fn, cfn.start, expr, cargs.start, None)

    def check(self, funcs, ret_type):
        if self.func_name not in funcs:
            raise UnknownFunc(self.func_name_coord, self.func_name)

        if len(self.args) != len(funcs[self.func_name].func_params):
            raise MismatchedFuncArgsCount(self.args_coord, self.func_name,
                                          len(funcs[self.func_name].func_params), len(self.args))

        for i in range(len(self.args)):
            arg = self.args[i]
            if arg not in vars:
                raise UnknownVar(self.args_coord, arg)
            if vars[arg] != funcs[self.func_name].func_params[i].type:
                need = pretty_type_view(funcs[self.func_name].func_params[i].type)
                actual = pretty_type_view(vars[arg])
                raise MismatchedFuncArgsType(self.args_coord, self.func_name, need, actual)
        if isinstance(funcs[self.func_name], FuncHeaderShort):
            self.type = "void"
        else:
            self.type = funcs[self.func_name].type


@dataclass
class NewAllocStatement(Statement):
    type: Type
    alloc_size: Any
    alloc_size_pos: pe.Position
    alloc_type: Type or BasicType

    @staticmethod
    def create(alloc_t):
        @pe.ExAction
        def action(attrs, coords, res_coords):
            t, var = attrs
            cnew, ct, cas = coords
            return NewAllocStatement(t, var, cas.start, alloc_t)

        return action

    def check(self, funcs, ret_type):
        if self.alloc_type is None:
            if vars[self.alloc_size] != BasicType.Integer:
                res_t = pretty_type_view(self.type)
                raise AllocSizeBadType(self.alloc_size_pos, res_t)


@dataclass
class IfStatementFull(Statement):
    condition: Expr
    then_branch: Statement
    else_branch: Statement

    def check(self, funcs, ret_type):
        self.condition.check(funcs, ret_type)
        self.then_branch.check(funcs, ret_type)
        self.else_branch.check(funcs, ret_type)


@dataclass
class IfStatementOneBranch(Statement):
    condition: Expr
    then_branch: Statement

    def check(self, funcs, ret_type):
        self.condition.check(funcs, ret_type)
        self.then_branch.check(funcs, ret_type)


@dataclass
class WhileStatement(Statement):
    condition: Expr
    body: Statement

    def check(self, funcs, ret_type):
        self.condition.check(funcs, ret_type)
        self.body.check(funcs, ret_type)


class ForHeader(abc.ABC):
    @abc.abstractmethod
    def check(self, funcs, ret_type):
        pass


@dataclass
class ForHeaderFull(ForHeader):
    for_val: CycleNewVar
    start: Expr
    end: Expr
    step: int

    def check(self, funcs, ret_type):
        self.for_val.check()
        self.start.check(funcs, ret_type)
        self.end.check(funcs, ret_type)


@dataclass
class ForHeaderShort(ForHeader):
    for_val: CycleNewVar
    start: Expr
    end: Expr

    def check(self, funcs, ret_type):
        self.for_val.check()
        self.start.check(funcs, ret_type)
        self.end.check(funcs, ret_type)


@dataclass
class ForStatement(Statement):
    header: ForHeader
    statements: list[Statement]

    def check(self, funcs, ret_type):
        global vars
        v = vars.copy()
        self.header.check(funcs, ret_type)
        for stmt in self.statements:
            stmt.check(funcs, ret_type)
        vars = v


@dataclass
class ReturnStatement(Statement):
    expr: Expr
    expr_pos: pe.Position

    @pe.ExAction
    def create(attrs, coords, res_coord):
        return ReturnStatement(attrs[0], coords[0].start)

    def check(self, funcs, ret_type):
        global need_return
        self.expr.check(funcs, ret_type)
        if ret_type != self.expr.type:
            ret_t = pretty_type_view(self.expr.type)
            raise ReturnBadType(self.expr_pos, ret_type, ret_t)
        need_return = False


@dataclass
class WarningStatement(Statement):
    expr: Expr

    def check(self, funcs, ret_type):
        self.expr.check(funcs, ret_type)


@dataclass
class ConstExpr(Expr):
    value: Any
    type: Type or BasicType

    def check(self, funcs, ret_type):
        pass


@dataclass
class BinOpExpr(Expr):
    left: Expr
    op: str
    op_coord: pe.Position
    right: Expr
    type: Type or BasicType

    @pe.ExAction
    def create(attrs, coords, res_coord):
        left, op, right = attrs
        cleft, cop, cright = coords
        return BinOpExpr(left, op, cop.start, right, None)

    def check(self, funcs, ret_type):
        self.left.check(funcs, ret_type)
        self.right.check(funcs, ret_type)

        is_numeric = lambda t: t in (BasicType.Integer, BasicType.Char)

        if self.op in ('_lt_', '_gt_', '_le_', '_ge_'):
            if is_numeric(self.left.type) and is_numeric(self.right.type):
                self.type = BasicType.Boolean
        elif self.op in ('_add_', '_or_', '_xor_'):
            if self.left.type == BasicType.Boolean and self.right.type == BasicType.Boolean:
                self.type = BasicType.Boolean
        elif self.op in ('_eq_', '_ne_'):
            if self.left.type != CharSequenceType and self.right.type != CharSequenceType:
                if is_numeric(self.left.type) and is_numeric(self.right.type):
                    self.type = BasicType.Boolean
                elif self.left.type == self.right.type:
                    self.type = BasicType.Boolean
                elif (type(self.left.type) == ArrayType or isinstance(self.left, RefConstType)) \
                        and (type(self.right.type) == ArrayType or isinstance(self.right, RefConstType)):
                    self.type = BasicType.Boolean

        elif self.op in ('*', '/', '_mod_', '_pow_'):
            if self.left.type == BasicType.Integer and self.right.type == BasicType.Integer:
                self.type = BasicType.Integer
        elif self.op in ('-'):
            if is_numeric(self.left.type) and is_numeric(self.right.type):
                if self.left.type != BasicType.Integer or self.right.type != BasicType.Char:
                    if self.left.type == BasicType.Char and self.right.type == BasicType.Integer:
                        self.type = BasicType.Char
                    else:
                        self.type = BasicType.Integer
        elif self.op in ('+'):
            if is_numeric(self.left.type) and is_numeric(self.right.type):
                if self.left.type != BasicType.Char or self.right.type != BasicType.Char:
                    if self.left.type == BasicType.Integer and self.right.type == BasicType.Integer:
                        self.type = BasicType.Integer
                    else:
                        self.type = BasicType.Char

        if self.type == None:
            res_t_l = pretty_type_view(self.left.type)
            res_t_r = pretty_type_view(self.right.type)
            raise BinBadType(self.op_coord, res_t_l, self.op, res_t_r)


@dataclass
class UnOpExpr(Expr):
    op: str
    op_coord: pe.Position
    expr: Expr
    type: Type or BasicType

    @staticmethod
    def create(op):
        @pe.ExAction
        def action(attrs, coords, res_coords):
            expr, = attrs
            cop, cexpr = coords
            return UnOpExpr(op, cop.start, expr, None)

        return action

    def check(self, funcs, ret_type):
        self.expr.check(funcs, ret_type)
        if self.op == '-':
            if self.expr.type not in (BasicType.Integer, BasicType.Char):
                raise UnBadType(self.op_coord, self.op, self.expr.type)
            else:
                self.type = BasicType.Integer
        elif self.op == 'not_':
            if self.expr.type not in (BasicType.Integer, BasicType.Char):
                raise UnBadType(self.op_coord, self.op, self.expr.type)
            else:
                self.type = BasicType.Boolean


# лексическая структура
INTEGER = pe.Terminal('INTEGER', '(([A-Za-z0-9]+{\d+})|\d+)', str, priority=7)
CHAR = pe.Terminal('CHAR', '\"\p{L}?\"', str)
STRING = pe.Terminal('STRING', '\'.*\'', str)
VARNAME = pe.Terminal('VARNAME', '[_|!|@|.|#][\p{L}]*', str)
FUNCNAME = pe.Terminal('FUNCNAME', '[\p{L}]*', str)
REF_CONST = pe.Terminal('REF_CONST', 'nothing', str)


def make_keyword(image):
    return pe.Terminal(image, image, lambda name: None, priority=10)


KW_INT, KW_CHAR, KW_BOOL = \
    map(make_keyword, 'int char bool'.split())

KW_XOR, KW_OR, KW_MOD, KW_AND, KW_NOT, KW_TRUE, KW_FALSE = \
    map(make_keyword, '_xor_ _or_ _mod_ _and_ not_ true false'.split())

(NProgram, NFuncs, NFunc, NFuncHeader, NFuncParams, NBasicVar,
 NType, NBasicType, NStatements, NFuncCall) = \
    map(pe.NonTerminal, 'Program Funcs Func FuncHeader FuncParams'
                        ' BasicVar Type BasicType Statements FuncCall'.split())

NStatement, NExpr, NCycle, NCycleVar, NArgs, NLogicalExpr, NCompareExpr, NFuncBody = \
    map(pe.NonTerminal, 'Statement Expr Cycle CycleNewVar Args LogicalExpr CompareExpr FuncBody'.split())

(NCmpOp, NPowOp, NArithmExpr, NExtendedVar, NAddOp, NPowExpr,
 NTerm, NMulOp, NFactor, NSpec, NConst, NVar) = \
    map(pe.NonTerminal, 'CmpOp PowOp ArithmExpr NExtendedVar '
                        'AddOp PowExpr Term MulOp Factor Spec Const Var'.split())

# грамматика
NProgram |= NFuncs, Program

NFuncs |= NFunc, lambda st: [st]
NFuncs |= NFuncs, NFunc, lambda fncs, fn: fncs + [fn]

NFunc |= NFuncHeader, NFuncBody, Func

NFuncHeader |= '(', NType, '[', FUNCNAME, NFuncParams, ']', ')', FuncHeaderFull.create
NFuncHeader |= '[', FUNCNAME, NFuncParams, ']', FuncHeaderShort.create

NFuncBody |= NStatements, '%%', FuncBody

NFuncParams |= NBasicVar, lambda st: [st]
NFuncParams |= NFuncParams, NBasicVar, lambda vars, var: vars + [var]

NBasicVar |= '(', NType, VARNAME, ')', BasicVar.create

NType |= NBasicType
NType |= '<', NType, '>', ArrayType

NBasicType |= KW_INT, lambda: BasicType.Integer
NBasicType |= KW_CHAR, lambda: BasicType.Char
NBasicType |= KW_BOOL, lambda: BasicType.Boolean

NStatements |= NStatement, lambda st: [st]
NStatements |= NStatements, ',', NStatement, lambda sts, st: sts + [st]

NStatement |= '^', NExpr, ReturnStatement.create
NStatement |= '\\', NExpr, WarningStatement
NStatement |= NExtendedVar, ':=', NExpr, AssignStatement.create
NStatement |= '[', FUNCNAME, NArgs, ']', FuncCallStatement.create
NStatement |= NBasicVar
NStatement |= '(', '?', NExpr, ')', NStatements, '+++', NStatements, '%', IfStatementFull
NStatement |= '(', '?', NExpr, ')', NStatements, '%', IfStatementOneBranch
NStatement |= '(', '&', NExpr, ')', NStatements, '%', WhileStatement
NStatement |= NCycle, NStatements, '%', ForStatement

NCycle |= '(', NCycleVar, ':', NExpr, ',', NExpr, ',', INTEGER, ')', ForHeaderFull
NCycle |= '(', NCycleVar, ':', NExpr, ',', NExpr, ')', ForHeaderShort

NCycleVar |= NType, VARNAME, CycleNewVar.create
NCycleVar |= VARNAME, Var.create

NArgs |= VARNAME, lambda vn: [vn]
NArgs |= NArgs, VARNAME, lambda args, arg: args + [arg]


def make_op_lambda(op):
    return lambda: op


for op in ('_eq_', '_ne_', '_lt_', '_gt_', '_le_', '_ge_'):
    NCmpOp |= op, make_op_lambda(op)

NExpr |= NLogicalExpr
NExpr |= NExpr, KW_OR, NLogicalExpr, BinOpExpr.create
NExpr |= NExpr, KW_XOR, NLogicalExpr, BinOpExpr.create

NLogicalExpr |= NCompareExpr
NLogicalExpr |= NLogicalExpr, KW_AND, NCompareExpr, BinOpExpr.create

NCompareExpr |= NArithmExpr
NCompareExpr |= NArithmExpr, NCmpOp, NArithmExpr, BinOpExpr.create

NArithmExpr |= NPowExpr
NArithmExpr |= NPowExpr, NAddOp, NPowExpr, BinOpExpr.create

NAddOp |= '+', lambda: '+'
NAddOp |= '-', lambda: '-'

NPowExpr |= NTerm
NPowExpr |= NTerm, NPowOp, NPowExpr, BinOpExpr.create

NPowOp |= '_pow_', lambda: '_pow_'

NTerm |= NFactor
NTerm |= NFactor, NMulOp, NTerm, BinOpExpr.create

NMulOp |= '*', lambda: '*'
NMulOp |= '/', lambda: '/'
NMulOp |= KW_MOD, lambda: 'mod'

NFactor |= KW_NOT, NSpec, UnOpExpr.create('not_')
NFactor |= '-', NSpec, UnOpExpr.create('-')
NFactor |= NSpec

NFuncCall |= '[', FUNCNAME, NArgs, ']', FuncCallStatement.create

NSpec |= NFuncCall
NSpec |= 'new_', NType, VARNAME, NewAllocStatement.create(None)
NSpec |= 'new_', NType, INTEGER, NewAllocStatement.create(BasicType.Integer)
NSpec |= NConst
NSpec |= NVar
NSpec |= '(', NExpr, ')'

NVar |= '<', NSpec, NExpr, '>', ArrayElemByVar.create
NVar |= VARNAME, Var.create

NExtendedVar |= NBasicVar
NExtendedVar |= NVar

NConst |= INTEGER, lambda v: ConstExpr(v, BasicType.Integer)
NConst |= CHAR, lambda v: ConstExpr(v, BasicType.Char)
NConst |= STRING, lambda v: CharSequenceType(v, CharSequenceType)
NConst |= REF_CONST, lambda v: RefConstType(v, RefConstType)
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
            # pprint(tree)
            tree.check()
            print('Семантических ошибок не найдено')
    except pe.Error as e:
        print(f'Error {e.pos}: {e.message}')
    except Exception as e:
        print(e)
