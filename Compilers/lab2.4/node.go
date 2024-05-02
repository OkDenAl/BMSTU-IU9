package main

import "fmt"

const (
	mainIdent  = "\t"
	blockIdent = "  "
)

type NodePrinter interface {
	Print(indent string)
}

type Program struct {
	funcs []Func
}

func NewProgram(funcs []Func) Program {
	return Program{funcs: funcs}
}

func (p Program) Print(indent string) {
	fmt.Println(indent + "Program:")
	for _, f := range p.funcs {
		f.Print(indent + mainIdent)
	}
}

type Func struct {
	header FuncHeader
	body   FuncBody
}

func NewFunc(header FuncHeader, body FuncBody) Func {
	return Func{header: header, body: body}
}

func (f Func) Print(indent string) {
	fmt.Println(indent + "Func:")
	f.header.Print(indent + mainIdent)
	f.body.Print(indent + mainIdent)
}

type FuncHeader struct {
	t        Type
	funcName Token
	params   FuncParams
}

func NewFuncHeader(t Type, funcName Token, params FuncParams) FuncHeader {
	return FuncHeader{t: t, funcName: funcName, params: params}
}

func (fh FuncHeader) Print(indent string) {
	fmt.Println(indent + "FuncHeader:")
	if fh.t != nil {
		fmt.Println(indent + mainIdent + "FuncType:")
		fh.t.Print(indent + mainIdent + blockIdent)
	}
	fmt.Println(indent + mainIdent + "FuncName: " + fh.funcName.val)
	fh.params.Print(indent + mainIdent)
}

type FuncBody struct {
	statements Statements
}

func NewFuncBody(statements Statements) FuncBody {
	return FuncBody{statements: statements}
}

func (fb FuncBody) Print(indent string) {
	fmt.Println(indent + "FuncBody:")
	fb.statements.Print(indent + mainIdent)
}

// FuncParams -> (BasicVar)*
type FuncParams struct {
	basicVars []BasicVar
}

func NewFuncParams(basicVars []BasicVar) FuncParams {
	return FuncParams{basicVars: basicVars}
}

func (fp FuncParams) Print(indent string) {
	fmt.Println(indent + "FuncParams:")
	for _, bv := range fp.basicVars {
		bv.Print(indent + mainIdent)
	}
}

type BasicVar struct {
	t       Type
	varname Token
}

func NewBasicVar(t Type, varname Token) BasicVar {
	return BasicVar{t: t, varname: varname}
}

func (bv BasicVar) Print(indent string) {
	fmt.Println(indent + "BasicVar:")
	fmt.Println(indent + mainIdent + "VarType:")
	bv.t.Print(indent + mainIdent + blockIdent)
	fmt.Println(indent + mainIdent + "VarName: " + bv.varname.val)
}

type Type interface {
	Print(indent string)
	TypeFunc()
}

type BasicType struct {
	varType Token
}

func NewVarType(varType Token) BasicType {
	return BasicType{varType: varType}
}

func (vt BasicType) Print(indent string) {
	fmt.Println(indent + "BasicType: " + vt.varType.val)
}

func (vt BasicType) TypeFunc() {}

type RefType struct {
	arrType Type
}

func NewArrType(varType Type) RefType {
	return RefType{arrType: varType}
}

func (at RefType) Print(indent string) {
	fmt.Println(indent + "RefType: ")
	at.arrType.Print(indent + blockIdent)
}

func (at RefType) TypeFunc() {}

type Statements struct {
	statements []Statement
}

func NewStatements(statements []Statement) Statements {
	return Statements{statements: statements}
}

func (s Statements) Print(indent string) {
	fmt.Println(indent + "Statements: ")
	for _, statement := range s.statements {
		statement.Print(indent + mainIdent)
	}
}

type Statement interface {
	Print(indent string)
	StatementFunc()
}

type ReturnStatement struct {
	expr Expr
}

func NewReturnStatement(expr Expr) ReturnStatement {
	return ReturnStatement{expr: expr}
}

func (rs ReturnStatement) Print(indent string) {
	fmt.Println(indent + "ReturnStatement: ")
	rs.expr.Print(indent + mainIdent)
}

func (rs ReturnStatement) StatementFunc() {}

type WarningStatement struct {
	expr Expr
}

func NewWarningStatement(expr Expr) WarningStatement {
	return WarningStatement{expr: expr}
}

func (ws WarningStatement) Print(indent string) {
	fmt.Println(indent + "WarningStatement: ")
	ws.expr.Print(indent + mainIdent)
}

func (ws WarningStatement) StatementFunc() {}

type AssignmentStatement struct {
	v    NodePrinter
	expr Expr
}

func NewAssignmentStatement(v NodePrinter, expr Expr) AssignmentStatement {
	return AssignmentStatement{expr: expr, v: v}
}

func (as AssignmentStatement) Print(indent string) {
	fmt.Println(indent + "AssignmentStatement: ")
	as.v.Print(indent + mainIdent)
	as.expr.Print(indent + mainIdent)
}

func (as AssignmentStatement) StatementFunc() {}

type WhileStatement struct {
	expr       Expr
	statements Statements
}

func NewWhileStatement(expr Expr, statements Statements) WhileStatement {
	return WhileStatement{expr: expr, statements: statements}
}

func (ws WhileStatement) Print(indent string) {
	fmt.Println(indent + "WhileStatement: ")
	indent += blockIdent
	fmt.Println(indent + "WhileCondition: ")
	ws.expr.Print(indent + mainIdent)
	fmt.Println(indent + "WhileBody: ")
	ws.statements.Print(indent + mainIdent)
}

func (ws WhileStatement) StatementFunc() {}

type IfStatement struct {
	expr       Expr
	thenBranch Statements
	elseBranch *Statements
}

func NewIfStatement(expr Expr, thenBranch Statements, elseBranch *Statements) IfStatement {
	return IfStatement{expr: expr, thenBranch: thenBranch, elseBranch: elseBranch}
}

func (is IfStatement) Print(indent string) {
	fmt.Println(indent + "IfStatement: ")
	indent += blockIdent
	fmt.Println(indent + "IfCondition: ")
	is.expr.Print(indent + mainIdent)
	fmt.Println(indent + "ThenBranchBody: ")
	is.expr.Print(indent + mainIdent)
	if is.elseBranch != nil {
		fmt.Println(indent + "ElseBranchBody: ")
		is.elseBranch.Print(indent + mainIdent)
	}
}

func (is IfStatement) StatementFunc() {}

type VarDeclarationStatement struct {
	t       Type
	varname Token
}

func NewVarDeclarationStatement(t Type, varname Token) VarDeclarationStatement {
	return VarDeclarationStatement{t: t, varname: varname}
}

func (vs VarDeclarationStatement) Print(indent string) {
	fmt.Println(indent + "VarDeclarationStatement: ")
	fmt.Println(indent + mainIdent + "VarType: ")
	vs.t.Print(indent + mainIdent + blockIdent)
	fmt.Println(indent + mainIdent + "VarName: " + vs.varname.val)
}

func (vs VarDeclarationStatement) StatementFunc() {}

type ForStatement struct {
	t          Type
	varname    Token
	start      Expr
	end        Expr
	step       *Token
	statements Statements
}

func NewForStatement(t Type, varname Token, start Expr, end Expr, step *Token, statements Statements) ForStatement {
	return ForStatement{start: start, t: t, varname: varname, end: end, step: step, statements: statements}
}

func (fs ForStatement) Print(indent string) {
	fmt.Println(indent + "ForStatement: ")
	fmt.Println(indent + mainIdent + "ForHeader: ")
	fmt.Println(indent + mainIdent + blockIdent + "VarType: ")
	fs.t.Print(indent + mainIdent + blockIdent + blockIdent)
	fmt.Println(indent + mainIdent + blockIdent + "VarName: " + fs.varname.val)

	fmt.Println(indent + mainIdent + blockIdent + "ForStart: ")
	fs.start.Print(indent + mainIdent + blockIdent + blockIdent)
	fmt.Println(indent + mainIdent + blockIdent + "ForEnd: ")
	fs.end.Print(indent + mainIdent + blockIdent + blockIdent)
	if fs.step != nil {
		fmt.Println(indent + mainIdent + blockIdent + "ForStep: " + fs.step.val)
	}
}

func (fs ForStatement) StatementFunc() {}

type Cycle struct {
	start Expr
	end   Expr
	step  *Token
}

func NewCycle(start Expr, end Expr, step *Token) Cycle {
	return Cycle{start: start, end: end, step: step}
}

type Expr interface {
	Print(indent string)
	ExprFunc()
}

type ConstExpr struct {
	t   Type
	val Token
}

func NewConstExpr(t Type, val Token) ConstExpr {
	return ConstExpr{t: t, val: val}
}

func (ce ConstExpr) Print(indent string) {
	fmt.Println(indent + "ConstExpr: ")
	if ce.t != nil {
		ce.t.Print(indent + mainIdent)
	}
	if ce.val.tag == INT_CONST || ce.val.tag == CHAR_CONST || ce.val.tag == STRING_CONST {
		fmt.Println(indent + mainIdent + "ConstType: " + tagToString[ce.val.tag])
	}
	fmt.Println(indent + mainIdent + "Val: " + ce.val.val)
}

func (ce ConstExpr) ExprFunc() {}

type BinOpExpr struct {
	left  Expr
	op    Token
	right Expr
}

func NewBinOpExpr(left Expr, op Token, right Expr) BinOpExpr {
	return BinOpExpr{left: left, op: op, right: right}
}

func (be BinOpExpr) Print(indent string) {
	fmt.Println(indent + "BinOpExpr: ")
	be.left.Print(indent + mainIdent)
	fmt.Println(indent + mainIdent + "Op: " + be.op.val)
	be.right.Print(indent + mainIdent)
}

func (be BinOpExpr) ExprFunc() {}

type UnOpExpr struct {
	op   Token
	expr Expr
}

func NewUnOpExpr(op Token, expr Expr) UnOpExpr {
	return UnOpExpr{op: op, expr: expr}
}

func (ue UnOpExpr) Print(indent string) {
	fmt.Println(indent + "BinOpExpr: ")
	fmt.Println(indent + mainIdent + "Op: " + ue.op.val)
	ue.expr.Print(indent + mainIdent)
}

func (ue UnOpExpr) ExprFunc() {}

type VarExpr struct {
	varName Token
	array   Expr
	index   Expr
}

func NewVar(varName Token, array Expr, index Expr) VarExpr {
	return VarExpr{varName: varName, array: array, index: index}
}

func (ve VarExpr) Print(indent string) {
	fmt.Println(indent + "VarExpr: ")
	if ve.array != nil {
		fmt.Println(indent + mainIdent + "ArrayElemByIndex: " + ve.varName.val)
		ve.array.Print(indent + mainIdent + blockIdent)
		ve.index.Print(indent + mainIdent + blockIdent)
	} else {
		fmt.Println(indent + mainIdent + "VarExprName: " + ve.varName.val)
	}
}

func (ve VarExpr) ExprFunc() {}

type AllocExpr struct {
	t    Type
	size Token
}

func NewAllocExpr(t Type, size Token) AllocExpr {
	return AllocExpr{t: t, size: size}
}

func (ae AllocExpr) Print(indent string) {
	fmt.Println(indent + "AllocExpr: ")
	ae.t.Print(indent + mainIdent)
	fmt.Println(indent + mainIdent + "Size: " + ae.size.val)
}

func (ae AllocExpr) ExprFunc() {}

type FuncCall struct {
	funcName Token
	args     []Expr
}

func NewFuncCall(funcName Token, args []Expr) FuncCall {
	return FuncCall{args: args, funcName: funcName}
}

func (fs FuncCall) Print(indent string) {
	fmt.Println(indent + "FuncCall: ")
	fmt.Println(indent + mainIdent + "FuncName: " + fs.funcName.val)
	fmt.Println(indent + mainIdent + "Args: ")
	for _, arg := range fs.args {
		arg.Print(indent + mainIdent + blockIdent)
	}
}

func (fs FuncCall) ExprFunc() {}

func (fs FuncCall) StatementFunc() {}
