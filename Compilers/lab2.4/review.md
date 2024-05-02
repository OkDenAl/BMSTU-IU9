% Лабораторная работа № 2.4 «Рекурсивный спуск»
% 2 мая 2024 г.
% Денис Окутин, ИУ9-61Б

# Цель работы
Целью данной работы является изучение алгоритмов построения парсеров 
методом рекурсивного спуска.

# Индивидуальный вариант
Язык [L4](https://hw.iu9.bmstu.ru/static/assets/L4.pdf).

# Реализация

## Лексическая структура

```
VARNAME = [_|!|@|.|#][\p{L}]+
FUNCNAME = [\p{L}]*
REF_CONST = nothing
INT_CONST  = (([A-Za-z0-9]+{\d+})|\d+)
CHAR_CONST  = \"\p{L}?\"
STRING_CONST  = \'.*\'
BOOL_CONST = true | false
VAR_TYPE = int | char | bool
Сomments = {.*}
```

## Грамматика языка

```
Program -> (Func)+
Func ->  FuncHeader FuncBody 
FuncHeader -> "(" Type "[" FUNCNAME FuncParams "]" ")" 
            | "[" FUNCNAME FuncParams "]" 
FuncBody -> Statements "%%"
FuncParams -> (BasicVar)*
BasicVar -> "(" Type VARNAME ")"
Type -> VAR_TYPE | "<" Type ">"
Statements -> Statement ("," Statement)*
Statement -> "^" Expr 
           | "\\" Expr
           | Var ":=" Expr
           | "[" FUNCNAME Args "]"
           | "(" StatementTail
StatementTail -> Type VARNAME ((")" (":=" Expr)?) | (Cycle Statements "%"))
               | "&" Expr ")" Statements "%"
               | "?" Expr ")" Statements ("+++" Statements)? "%"
Cycle -> ":" Expr "," Expr ("," INT_CONST)? ")"
Args -> (Spec)+ 
Expr -> LogicalExpr ((_or_ | _xor_) LogicalExpr)*
LogicalExpr -> CompareExpr (_and_ CompareExpr)*
CompareExpr -> ArithmExpr (CmpOp ArithmExpr)?
CmpOp → _eq_ | _ne_ | _lt_ | _gt_ | _le_ | _ge_
ArithmExpr -> PowExpr (("+" | "-")  PowExpr)*
PowExpr -> Term (_pow_ PowExpr)?
Term -> Factor (("*" | "/" | _mod_) Factor)*
Factor -> (not_ | "-")? Spec
FuncCall -> "[" FUNCNAME Args "]"
Spec -> FuncCall 
      | new_ Type (VARNAME | INT_CONST) 
      | Const
      | Var 
      | "(" Expr ")"
Var -> VARNAME | "<" Spec Expr ">" .
Const → INT_CONST | CHAR_CONST | STRING_CONST | REF_CONST | BOOL_CONST 
```

## Программная реализация

main.go
```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println(r)
			os.Exit(1)
		}
	}()

	if len(os.Args) < 2 {
		log.Fatal("usage must be: go run main.go <fileTag.txt>\n")
	}
	filePath := os.Args[1]

	file, err := os.Open(filePath)
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	reader := bufio.NewReader(file)

	compiler := NewCompiler()
	scn := NewScanner(reader, &compiler)

	parser := NewParser(&scn)

	p := parser.Program()
	p.Print("")


	fmt.Println("COMMENTS:")
	scn.printComments()
}
```

compiler.go
```go
package main

import (
	"fmt"
	"reflect"
	"sort"
	"strings"
)

func SortedMapKeys(m map[Position]Message) (keyList []Position) {
	keys := reflect.ValueOf(m).MapKeys()

	for _, key := range keys {
		keyList = append(keyList, key.Interface().(Position))
	}
	sort.Slice(keyList, func(i, j int) bool {
		return keyList[i].line < keyList[j].line ||
			(keyList[i].line == keyList[j].line && keyList[i].pos < keyList[j].pos)
	})
	return
}

type Compiler struct {
	messages  map[Position]Message
	nameCodes map[string]int
	names     []string
}

func NewCompiler() Compiler {
	return Compiler{nameCodes: make(map[string]int), messages: make(map[Position]Message)}
}

func (c *Compiler) GetIdentsNames() {
	fmt.Println("IDENTs:")
	for i, name := range c.names {
		fmt.Printf("%d: %s\n", i, name)
	}
	fmt.Println()
}

func (c *Compiler) AddName(name string) int {
	name1 := strings.ToLower(name)
	if code, ok := c.nameCodes[name1]; ok {
		return code
	}
	code := len(c.names)
	c.names = append(c.names, name)
	c.nameCodes[name1] = code
	return code
}

func (c *Compiler) Name(code int) string {
	return c.names[code]
}

func (c *Compiler) AddMessage(isErr bool, p Position, text string) {
	c.messages[p] = NewMessage(isErr, text)
}

func (c *Compiler) OutputMessages() {
	list := SortedMapKeys(c.messages)
	for _, key := range list {
		val := c.messages[key]
		if val.isError {
			fmt.Print("Error")
		} else {
			fmt.Print("Warning")
		}
		fmt.Print(" ", key.String(), ": ")
		fmt.Println(val.text)
	}
}
```

fragment.go
```go
package main

import (
	"fmt"
)

type Fragment struct {
	starting  Position
	following Position
}

func NewFragment(starting, following Position) Fragment {
	return Fragment{starting: starting, following: following}
}

func (f Fragment) String() string {
	return fmt.Sprintf("%s-%s", f.starting.String(), f.following.String())
}
```

msg.go
```go
package main

type Message struct {
	isError bool
	text    string
}

func NewMessage(isError bool, text string) Message {
	return Message{isError: isError, text: text}
}
```

node.go
```go
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
```

parser.go
```go
package main

import "fmt"

type Parser struct {
	sym     Token
	scanner *Scanner
}

func NewParser(sc *Scanner) Parser {
	return Parser{sym: sc.NextToken(), scanner: sc}
}

func (p *Parser) ExpectStr(vals ...string) Token {
	for _, val := range vals {
		if p.sym.val == val {
			sym := p.sym
			p.sym = p.scanner.NextToken()
			if p.sym.tag == ERR {
				panic(fmt.Sprintf("parse error: unexpected token"))
			}
			return sym
		}
	}

	panic(fmt.Sprintf("parse error: expected %s, but got %s", vals, p.sym))
}

func (p *Parser) ExpectTags(tags ...DomainTag) Token {
	for _, tag := range tags {
		if p.sym.tag == tag {
			sym := p.sym
			p.sym = p.scanner.NextToken()
			if p.sym.tag == ERR {
				panic(fmt.Sprintf("parse error: unexpected token"))
			}
			return sym
		}
	}

	panic(fmt.Sprintf("parse error: expected %s, but got %s", tags, p.sym))
}

// Program -> (Func)+
func (p *Parser) Program() Program {
	funcs := make([]Func, 0)
	funcs = append(funcs, p.Func())
	for p.sym.val == "[" || p.sym.val == "(" {
		funcs = append(funcs, p.Func())
	}

	return NewProgram(funcs)
}

// Func ->  FuncHeader FuncBody
func (p *Parser) Func() Func {
	header := p.FuncHeader()
	body := p.FuncBody()

	return NewFunc(header, body)
}

// FuncHeader -> "(" Type "[" FUNCNAME FuncParams "]" ")"
// FuncHeader -> "[" FUNCNAME FuncParams "]"
func (p *Parser) FuncHeader() FuncHeader {
	if p.sym.val == "(" {
		p.ExpectStr("(")
		t := p.Type()
		p.ExpectStr("[")
		funcName := p.ExpectTags(FUNCNAME)
		params := p.FuncParams()
		p.ExpectStr("]")
		p.ExpectStr(")")
		return NewFuncHeader(t, funcName, params)
	}

	p.ExpectStr("[")
	funcName := p.ExpectTags(FUNCNAME)
	params := p.FuncParams()
	p.ExpectStr("]")
	return NewFuncHeader(nil, funcName, params)
}

// FuncBody -> Statements "%%"
func (p *Parser) FuncBody() FuncBody {
	statements := p.Statements()
	p.ExpectStr("%%")
	return NewFuncBody(statements)
}

// FuncParams -> (BasicVar)*
func (p *Parser) FuncParams() FuncParams {
	basicVars := make([]BasicVar, 0)
	for p.sym.val == "(" {
		basicVars = append(basicVars, p.BasicVar())
	}

	return NewFuncParams(basicVars)
}

// BasicVar -> "(" Type VARNAME ")"
func (p *Parser) BasicVar() BasicVar {
	p.ExpectStr("(")
	t := p.Type()
	varname := p.ExpectTags(VARNAME)
	p.ExpectStr(")")

	return NewBasicVar(t, varname)
}

// Type -> VAR_TYPE | "<" Type ">"
func (p *Parser) Type() Type {
	if p.sym.val == "<" {
		p.ExpectStr("<")
		t := p.Type()
		p.ExpectStr(">")
		return NewArrType(t)
	}
	varT := p.ExpectStr("int", "bool", "char")
	return NewVarType(varT)
}

// Statements -> Statement ("," Statement)*
func (p *Parser) Statements() Statements {
	statement := make([]Statement, 0)
	statement = append(statement, p.Statement())
	for p.sym.val == "," {
		p.ExpectStr(",")
		statement = append(statement, p.Statement())
	}

	return NewStatements(statement)
}

// Statement -> "^" Expr
// Statement -> "\\" Expr
// Statement -> Var ":=" Expr
// Statement -> "[" FUNCNAME Args "]"
// Statement -> "(" StatementTail
func (p *Parser) Statement() Statement {
	switch {
	case p.sym.val == "^":
		p.ExpectStr("^")
		expr := p.Expr()
		return NewReturnStatement(expr)
	case p.sym.val == "\\":
		p.ExpectStr("\\")
		expr := p.Expr()
		return NewWarningStatement(expr)
	case p.sym.val == "[":
		p.ExpectStr("[")
		funcName := p.ExpectTags(FUNCNAME)
		args := p.Args()
		p.ExpectStr("]")
		return NewFuncCall(funcName, args)
	case p.sym.tag == VARNAME || p.sym.val == "<":
		v := p.Var()
		p.ExpectStr(":=")
		expr := p.Expr()
		return NewAssignmentStatement(v, expr)
	default:
		p.ExpectStr("(")
		return p.StatementTail()
	}
}

// StatementTail -> Type VARNAME ((")" (":=" Expr)?) | (Cycle Statements "%"))
// StatementTail -> "&" Expr ")" Statements "%"
// StatementTail -> "?" Expr ")" Statements ("+++" Statements)? "%"
func (p *Parser) StatementTail() Statement {
	switch {
	case p.sym.val == "<" || p.sym.val == "int" || p.sym.val == "bool" || p.sym.val == "char":
		t := p.Type()
		varname := p.ExpectTags(VARNAME)
		if p.sym.val == ")" {
			p.ExpectStr(")")
			if p.sym.val == ":=" {
				p.ExpectStr(":=")
				expr := p.Expr()
				return NewAssignmentStatement(NewVarDeclarationStatement(t, varname), expr)
			}
			return NewVarDeclarationStatement(t, varname)
		} else if p.sym.val == ":" {
			cycle := p.Cycle()
			statements := p.Statements()
			p.ExpectStr("%")
			return NewForStatement(t, varname, cycle.start, cycle.end, cycle.step, statements)
		} else {
			panic("parse error")
		}
	case p.sym.val == "?":
		p.ExpectStr("?")
		expr := p.Expr()
		p.ExpectStr(")")
		thenBranch := p.Statements()
		var elseBranch *Statements
		if p.sym.val == "+++" {
			p.ExpectStr("+++")
			elseBranch_ := p.Statements()
			elseBranch = &elseBranch_
		}
		p.ExpectStr("%")
		return NewIfStatement(expr, thenBranch, elseBranch)
	default:
		p.ExpectStr("&")
		expr := p.Expr()
		p.ExpectStr(")")
		statements := p.Statements()
		p.ExpectStr("%")
		return NewWhileStatement(expr, statements)
	}
}

// Cycle -> VARNAME ":" Expr "," Expr ("," INT_CONST)? ")"
func (p *Parser) Cycle() Cycle {
	p.ExpectStr(":")
	startExpr := p.Expr()
	p.ExpectStr(",")
	endExpr := p.Expr()
	var step *Token
	if p.sym.val == "," {
		p.ExpectStr(",")
		token_ := p.ExpectTags(INT_CONST)
		step = &token_
	}
	p.ExpectStr(")")

	return NewCycle(startExpr, endExpr, step)
}

// Args -> (Spec)+
func (p *Parser) Args() []Expr {
	specs := make([]Expr, 0)
	specs = append(specs, p.Spec())
	for p.sym.val == "(" || p.sym.val == "[" || p.sym.val == "<" || p.sym.val == "new_" || p.sym.val == "true" ||
		p.sym.val == "false" || p.sym.tag == INT_CONST || p.sym.tag == CHAR_CONST || p.sym.tag == STRING_CONST ||
		p.sym.tag == VARNAME {
		specs = append(specs, p.Spec())
	}

	return specs
}

// Expr -> LogicalExpr ((_or_ | _xor_) LogicalExpr)*
func (p *Parser) Expr() Expr {
	expr := p.LogicalExpr()
	for p.sym.val == "_or_" || p.sym.val == "_xor_" {
		op := p.ExpectStr("_or_", "_xor_")
		rightExpr := p.LogicalExpr()
		expr = NewBinOpExpr(expr, op, rightExpr)
	}

	return expr
}

// LogicalExpr -> CompareExpr (_and_ CompareExpr)*
func (p *Parser) LogicalExpr() Expr {
	expr := p.CompareExpr()
	for p.sym.val == "_and_" {
		op := p.ExpectStr("_and_")
		rightExpr := p.CompareExpr()
		expr = NewBinOpExpr(expr, op, rightExpr)
	}

	return expr
}

// CompareExpr -> ArithmExpr (CmpOp ArithmExpr)*
func (p *Parser) CompareExpr() Expr {
	expr := p.ArithmExpr()
	if p.sym.val == "_eq_" || p.sym.val == "_ne_" || p.sym.val == "_lt_" || p.sym.val == "_gt_" ||
		p.sym.val == "_le_" || p.sym.val == "_ge_" {
		op := p.CmpOp()
		rightExpr := p.ArithmExpr()

		expr = NewBinOpExpr(expr, op, rightExpr)
	}

	return expr
}

// CmpOp → _eq_ | _ne_ | _lt_ | _gt_ | _le_ | _ge_
func (p *Parser) CmpOp() Token {
	return p.ExpectStr("_eq_", "_ne_", "_lt_", "_gt_", "_le_", "_ge_")
}

// ArithmExpr -> PowExpr (("+" | "-")  PowExpr)*
func (p *Parser) ArithmExpr() Expr {
	expr := p.PowExpr()
	for p.sym.val == "+" || p.sym.val == "-" {
		op := p.ExpectStr("+", "-")
		rightExpr := p.PowExpr()

		expr = NewBinOpExpr(expr, op, rightExpr)
	}

	return expr
}

// PowExpr -> Term (_pow_ PowExpr)?
func (p *Parser) PowExpr() Expr {
	expr := p.Term()
	if p.sym.val == "_pow_" {
		op := p.ExpectStr("_pow_")
		rightExpr := p.PowExpr()
		expr = NewBinOpExpr(expr, op, rightExpr)
	}

	return expr
}

// Term -> Factor (("*" | "/" | _mod_) Factor)*
func (p *Parser) Term() Expr {
	expr := p.Factor()
	for p.sym.val == "*" || p.sym.val == "/" || p.sym.val == "_mod_" {
		op := p.ExpectStr("*", "/", "_mod_")
		rightExpr := p.Factor()
		return NewBinOpExpr(expr, op, rightExpr)
	}

	return expr
}

// Factor -> (not_ | "-")? Spec
func (p *Parser) Factor() Expr {
	if p.sym.val == "not_" {
		p.ExpectStr("not_")

	} else if p.sym.val == "-" {
		p.ExpectStr("-")
	}

	return p.Spec()
}

// FuncCall -> "[" FUNCNAME Args "]"
func (p *Parser) FuncCall() FuncCall {
	p.ExpectStr("[")
	funcname := p.ExpectTags(FUNCNAME)
	args := p.Args()
	p.ExpectStr("]")

	return NewFuncCall(funcname, args)
}

// Spec -> FuncCall
// Spec -> new_ Type (VARNAME | INT_CONST)
// Spec -> Const
// Spec -> Var
// Spec -> "(" Expr ")"
func (p *Parser) Spec() Expr {
	switch {
	case p.sym.val == "(":
		p.ExpectStr("(")
		expr := p.Expr()
		p.ExpectStr(")")
		return expr
	case p.sym.val == "<" || p.sym.tag == VARNAME:
		varExpr := p.Var()
		return varExpr
	case p.sym.val == "true" || p.sym.val == "false" || p.sym.val == "nothing" || p.sym.tag == INT_CONST ||
		p.sym.tag == CHAR_CONST || p.sym.tag == STRING_CONST:
		return p.Const()
	case p.sym.val == "[":
		return p.FuncCall()
	default:
		p.ExpectStr("new_")
		t := p.Type()
		size := p.ExpectTags(VARNAME, INT_CONST)
		return NewAllocExpr(t, size)
	}
}

// Var -> VARNAME | "<" Spec Expr ">"
func (p *Parser) Var() Expr {
	if p.sym.val == "<" {
		p.ExpectStr("<")
		arrayName := p.Spec()
		expr := p.Expr()
		p.ExpectStr(">")
		return NewVar(Token{}, arrayName, expr)
	}

	varname := p.ExpectTags(VARNAME)
	return NewVar(varname, nil, nil)
}

// Const → INT_CONST | CHAR_CONST | STRING_CONST | REF_CONST | BOOL_CONST
func (p *Parser) Const() ConstExpr {
	if p.sym.tag == INT_CONST || p.sym.tag == CHAR_CONST || p.sym.tag == STRING_CONST {
		val := p.ExpectTags(INT_CONST, CHAR_CONST, STRING_CONST)
		return NewConstExpr(nil, val)
	}
	str := p.ExpectStr("true", "false", "nothing")
	return NewConstExpr(nil, str)
}
```

position.go
```go
package main

import (
	"bufio"
	"fmt"
	"unicode"
)

type position struct {
	symb rune
	line int
	pos  int
}

func newPosition(symb rune) position {
	return position{symb: symb, line: 1, pos: 1}
}

type Position struct {
	position
	reader *bufio.Reader
}

func NewPosition(reader *bufio.Reader) Position {
	r, _, err := reader.ReadRune()
	if err != nil {
		r = -1
	}
	return Position{position: newPosition(r), reader: reader}
}

func (p *Position) String() string {
	return fmt.Sprintf("(%d,%d)", p.line, p.pos)
}

func (p *Position) Cp() int {
	return int(p.symb)
}

func (p *Position) IsWhiteSpace() bool {
	return p.symb == ' '
}

func (p *Position) IsLetter() bool {
	return unicode.IsLetter(p.symb)
}

func (p *Position) IsUnderlining() bool {
	return p.symb == '_'
}

func (p *Position) IsDigit() bool {
	return unicode.IsDigit(p.symb)
}

func (p *Position) IsLetterOrDigit() bool {
	return unicode.IsDigit(p.symb) || unicode.IsLetter(p.symb)
}

func (p *Position) IsNewLine() bool {
	return p.symb == '\n'
}

func (p *Position) IsCloseBracket() bool {
	return p.symb == ')' || p.symb == ']' || p.symb == '>' || p.symb == ','
}

func (p *Position) Next() Position {
	r, _, err := p.reader.ReadRune()
	if err == nil {
		if p.IsNewLine() {
			p.line++
			p.pos = 1
		} else {
			p.pos++
		}
		p.symb = r
	} else {
		p.symb = -1
	}
	return *p
}

func (p *Position) SkipErrors() {
	for !p.IsWhiteSpace() {
		pos := *p
		if p.Next() == pos {
			break
		}
	}
}

func (p *Position) GetSymbol() rune {
	return p.symb
}
```

scanner.go
```go
package main

import (
	"bufio"
	"fmt"
)

type comment struct {
	Fragment
	value string
}

func newComment(starting, following Position, value string) comment {
	return comment{NewFragment(starting, following), value}
}

func (c comment) String() string {
	return fmt.Sprintf("COMMENT %s-%s: %s", c.starting.String(), c.following.String(), c.value)
}

type Scanner struct {
	programReader *bufio.Reader
	compiler      *Compiler
	curPos        Position
	comments      []comment
}

func NewScanner(programFile *bufio.Reader, compiler *Compiler) Scanner {
	return Scanner{programReader: programFile, compiler: compiler, curPos: NewPosition(programFile)}
}

func (s *Scanner) printComments() {
	for _, comm := range s.comments {
		fmt.Println(comm)
	}
}

func (s *Scanner) NextToken() Token {
	curWord := ""
	for s.curPos.Cp() != -1 {
		for s.curPos.IsWhiteSpace() {
			s.curPos.Next()
		}
		start := s.curPos

		switch s.curPos.Cp() {
		case '\n':
			s.curPos.Next()
		case ':':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			if s.curPos.GetSymbol() == '=' {
				curWord += string(rune(s.curPos.Cp()))
				pos := s.curPos
				s.curPos.Next()
				return NewToken(SPEC_SYMB, start, pos, curWord)
			}

			return NewToken(SPEC_SYMB, start, start, curWord)
		case '%':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			if s.curPos.GetSymbol() == '%' {
				curWord += string(rune(s.curPos.Cp()))
				pos := s.curPos
				s.curPos.Next()
				return NewToken(SPEC_SYMB, start, pos, curWord)
			}

			return NewToken(SPEC_SYMB, start, start, curWord)
		case '"':
			s.curPos.Next()
			var pos Position
			if !s.curPos.IsNewLine() && s.curPos.Cp() != -1 && s.curPos.GetSymbol() != '"' {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()

				if s.curPos.GetSymbol() == '"' {
					pos = s.curPos
					s.curPos.Next()
					return NewToken(CHAR_CONST, start, pos, curWord)
				}
			}
			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")

		case '\'':
			s.curPos.Next()
			var pos Position
			for s.curPos.Cp() != -1 && s.curPos.GetSymbol() != '\'' && !s.curPos.IsNewLine() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}
			if s.curPos.GetSymbol() == '\'' {
				pos = s.curPos
				s.curPos.Next()
				return NewToken(STRING_CONST, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")

		case '+':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			pos := s.curPos
			for s.curPos.GetSymbol() == '+' {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}

			if !s.curPos.IsNewLine() && !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ERR, s.curPos, s.curPos, "")
			}

			if curWord == "+++" || curWord == "+" {
				return NewToken(SPEC_SYMB, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")
		case '{':
			s.curPos.Next()
			for !s.curPos.IsNewLine() && s.curPos.Cp() != -1 && s.curPos.GetSymbol() != '}' {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
			}

			s.comments = append(s.comments, newComment(start, s.curPos, curWord))
			s.curPos.Next()
			curWord = ""
		case '_', '!', '@', '.', '#':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			var pos Position
			for s.curPos.IsLetter() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}

			if s.curPos.IsUnderlining() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
				if _, ok := keywordsWithUnderliningStart[curWord]; ok {
					return NewToken(KEYWORD, start, pos, curWord)
				}

				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ERR, s.curPos, s.curPos, "")
			}

			if !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() && !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ERR, s.curPos, s.curPos, "")
			}

			return NewToken(VARNAME, start, pos, curWord)
		default:
			if _, ok := specSymbsInOneRune[string(s.curPos.GetSymbol())]; ok {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				return NewToken(SPEC_SYMB, start, start, curWord)
			}

			if s.curPos.IsDigit() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				var pos Position
				for s.curPos.IsLetterOrDigit() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}

				if s.curPos.GetSymbol() == '{' {
					curWord += string(rune(s.curPos.Cp()))
					s.curPos.Next()
					for s.curPos.IsDigit() {
						curWord += string(rune(s.curPos.Cp()))
						pos = s.curPos
						s.curPos.Next()
					}

					if s.curPos.GetSymbol() != '}' && !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() &&
						!s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {

						s.compiler.AddMessage(true, start, "invalid syntax")
						s.curPos.SkipErrors()

						return NewToken(ERR, s.curPos, s.curPos, "")
					}

					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
					return NewToken(INT_CONST, start, pos, curWord)

				}

				if !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() &&
					!s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {

					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewToken(ERR, s.curPos, s.curPos, "")
				}

				return NewToken(INT_CONST, start, pos, curWord)
			}

			if s.curPos.IsLetter() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				var pos Position
				for s.curPos.IsLetter() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}

				if s.curPos.IsDigit() {
					continue
				}

				if s.curPos.IsUnderlining() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
					if _, ok := keywords[curWord]; ok {
						return NewToken(KEYWORD, start, pos, curWord)
					}

					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewToken(ERR, s.curPos, s.curPos, "")
				}

				if !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() &&
					!s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {

					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewToken(ERR, s.curPos, s.curPos, "")
				}

				if _, ok := keywords[curWord]; ok {
					return NewToken(KEYWORD, start, pos, curWord)
				}

				return NewToken(FUNCNAME, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")
		}
	}

	return NewToken(EOP, s.curPos, s.curPos, "")
}
```

tag.go
```go
package main

type DomainTag int

const (
	VARNAME DomainTag = iota
	FUNCNAME
	INT_CONST
	CHAR_CONST
	STRING_CONST
	KEYWORD
	SPEC_SYMB
	ERR
	EOP
)

var tagToString = map[DomainTag]string{
	VARNAME:      "VARNAME",
	FUNCNAME:     "FUNCNAME",
	INT_CONST:    "INT_CONST",
	CHAR_CONST:   "CHAR_CONST",
	STRING_CONST: "STRING_CONST",
	KEYWORD:      "KEYWORD",
	SPEC_SYMB:    "SPEC_SYMB",
	EOP:          "EOP",
	ERR:          "ERR",
}

var keywords = map[string]struct{}{
	"bool":    {},
	"char":    {},
	"int":     {},
	"false":   {},
	"true":    {},
	"nothing": {},
	"new_":    {},
	"not_":    {},
}

var keywordsWithUnderliningStart = map[string]struct{}{
	"_and_": {},
	"_eq_":  {},
	"_ge_":  {},
	"_gt_":  {},
	"_le_":  {},
	"_lt_":  {},
	"_mod_": {},
	"_ne_":  {},
	"_or_":  {},
	"_pow_": {},
	"_xor_": {},
}

var specSymbsInOneRune = map[string]struct{}{
	"<":  {},
	">":  {},
	"(":  {},
	")":  {},
	"[":  {},
	"]":  {},
	",":  {},
	"?":  {},
	"&":  {},
	"\\": {},
	"^":  {},
	"-":  {},
	"*":  {},
	"/":  {},
}
```

tokens.go
```go
package main

import (
	"fmt"
)

type Token struct {
	tag    DomainTag
	coords Fragment
	val    string
}

func NewToken(tag DomainTag, starting, following Position, val string) Token {
	return Token{tag: tag, coords: NewFragment(starting, following), val: val}
}

func (t Token) String() string {
	return fmt.Sprintf("%s %s: %s", tagToString[t.tag], t.coords, t.val)
}

func (t Token) Tag() DomainTag {
	return t.tag
}
```

# Тестирование

Входные данные

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


(<int> [SumVectors (<<int>> !A) (<int> !B)])
   (int @a) := 'я русский',
   @a := <<!A 1> 2>,
    ^ #C
%%
```

Вывод на `stdout`
<!-- ENABLE LONG LINES -->
```
Program:
        Func:
                FuncHeader:
                        FuncType:
                          RefType: 
                            BasicType: int
                        FuncName: SumVectors
                        FuncParams:
                                BasicVar:
                                        VarType:
                                          RefType: 
                                            RefType: 
                                              BasicType: int
                                        VarName: !A
                                BasicVar:
                                        VarType:
                                          BasicType: int
                                        VarName: !B
                FuncBody:
                        Statements: 
                                AssignmentStatement: 
                                        VarDeclarationStatement: 
                                                VarType: 
                                                  BasicType: int
                                                VarName: #size
                                        FuncCall: 
                                                FuncName: length
                                                Args: 
                                                  VarExpr: 
                                                        VarExprName: !A
                                WarningStatement: 
                                        BinOpExpr: 
                                                VarExpr: 
                                                        VarExprName: #size
                                                Op: _eq_
                                                FuncCall: 
                                                        FuncName: length
                                                        Args: 
                                                          VarExpr: 
                                                                VarExprName: !B
                                AssignmentStatement: 
                                        VarDeclarationStatement: 
                                                VarType: 
                                                  RefType: 
                                                    BasicType: int
                                                VarName: #C
                                        AllocExpr: 
                                                RefType: 
                                                  BasicType: int
                                                Size: 10f{10}
                                AssignmentStatement: 
                                        VarDeclarationStatement: 
                                                VarType: 
                                                  RefType: 
                                                    BasicType: int
                                                VarName: #C
                                        AllocExpr: 
                                                RefType: 
                                                  BasicType: int
                                                Size: fg1{10}
                                AssignmentStatement: 
                                        VarExpr: 
                                                VarExprName: #x
                                        BinOpExpr: 
                                                BinOpExpr: 
                                                        VarExpr: 
                                                                VarExprName: #a
                                                        Op: _and_
                                                        ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 2
                                                Op: _and_
                                                ConstExpr: 
                                                        ConstType: INT_CONST
                                                        Val: 10
                                AssignmentStatement: 
                                        VarExpr: 
                                                VarExprName: #x
                                        BinOpExpr: 
                                                VarExpr: 
                                                        VarExprName: #a
                                                Op: *
                                                BinOpExpr: 
                                                        ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 2
                                                        Op: +
                                                        ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 10
                                AssignmentStatement: 
                                        VarExpr: 
                                                VarExprName: #x
                                        BinOpExpr: 
                                                VarExpr: 
                                                        VarExprName: #a
                                                Op: _pow_
                                                BinOpExpr: 
                                                        ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 2
                                                        Op: _pow_
                                                        ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 10
                                AssignmentStatement: 
                                        VarExpr: 
                                                ArrayElemByIndex: 
                                                  VarExpr: 
                                                        VarExprName: !A
                                                  ConstExpr: 
                                                        ConstType: INT_CONST
                                                        Val: 5
                                        BinOpExpr: 
                                                VarExpr: 
                                                        ArrayElemByIndex: 
                                                          VarExpr: 
                                                                VarExprName: #B
                                                          ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 6
                                                Op: *
                                                ConstExpr: 
                                                        ConstType: INT_CONST
                                                        Val: 10
                                AssignmentStatement: 
                                        VarExpr: 
                                                ArrayElemByIndex: 
                                                  FuncCall: 
                                                        FuncName: Func
                                                        Args: 
                                                          VarExpr: 
                                                                VarExprName: #x
                                                          VarExpr: 
                                                                VarExprName: #y
                                                  BinOpExpr: 
                                                        VarExpr: 
                                                                VarExprName: !i
                                                        Op: +
                                                        ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 1
                                        ConstExpr: 
                                                ConstType: INT_CONST
                                                Val: 0
                                AssignmentStatement: 
                                        VarDeclarationStatement: 
                                                VarType: 
                                                  RefType: 
                                                    BasicType: int
                                                VarName: #P
                                        AllocExpr: 
                                                RefType: 
                                                  BasicType: int
                                                Size: 10
                                ForStatement: 
                                        ForHeader: 
                                          VarType: 
                                            RefType: 
                                              BasicType: int
                                          VarName: #i
                                          ForStart: 
                                            ConstExpr: 
                                                ConstType: INT_CONST
                                                Val: 0
                                          ForEnd: 
                                            BinOpExpr: 
                                                VarExpr: 
                                                        VarExprName: #size
                                                Op: -
                                                ConstExpr: 
                                                        ConstType: INT_CONST
                                                        Val: 1
                                IfStatement: 
                                  IfCondition: 
                                        BinOpExpr: 
                                                VarExpr: 
                                                        VarExprName: #a
                                                Op: _lt_
                                                ConstExpr: 
                                                        ConstType: INT_CONST
                                                        Val: 0
                                  ThenBranchBody: 
                                        BinOpExpr: 
                                                VarExpr: 
                                                        VarExprName: #a
                                                Op: _lt_
                                                ConstExpr: 
                                                        ConstType: INT_CONST
                                                        Val: 0
                                  ElseBranchBody: 
                                        Statements: 
                                                IfStatement: 
                                                  IfCondition: 
                                                        BinOpExpr: 
                                                                VarExpr: 
                                                                        VarExprName: #a
                                                                Op: _eq_
                                                                ConstExpr: 
                                                                        ConstType: INT_CONST
                                                                        Val: 0
                                                  ThenBranchBody: 
                                                        BinOpExpr: 
                                                                VarExpr: 
                                                                        VarExprName: #a
                                                                Op: _eq_
                                                                ConstExpr: 
                                                                        ConstType: INT_CONST
                                                                        Val: 0
                                                  ElseBranchBody: 
                                                        Statements: 
                                                                AssignmentStatement: 
                                                                        VarExpr: 
                                                                                VarExprName: #sign
                                                                        ConstExpr: 
                                                                                ConstType: INT_CONST
                                                                                Val: 1
                                ReturnStatement: 
                                        VarExpr: 
                                                VarExprName: #C
        Func:
                FuncHeader:
                        FuncType:
                          RefType: 
                            BasicType: int
                        FuncName: SumVectors
                        FuncParams:
                                BasicVar:
                                        VarType:
                                          RefType: 
                                            RefType: 
                                              BasicType: int
                                        VarName: !A
                                BasicVar:
                                        VarType:
                                          RefType: 
                                            BasicType: int
                                        VarName: !B
                FuncBody:
                        Statements: 
                                AssignmentStatement: 
                                        VarDeclarationStatement: 
                                                VarType: 
                                                  BasicType: int
                                                VarName: @a
                                        ConstExpr: 
                                                ConstType: STRING_CONST
                                                Val: я русский
                                AssignmentStatement: 
                                        VarExpr: 
                                                VarExprName: @a
                                        VarExpr: 
                                                ArrayElemByIndex: 
                                                  VarExpr: 
                                                        ArrayElemByIndex: 
                                                          VarExpr: 
                                                                VarExprName: !A
                                                          ConstExpr: 
                                                                ConstType: INT_CONST
                                                                Val: 1
                                                  ConstExpr: 
                                                        ConstType: INT_CONST
                                                        Val: 2
                                ReturnStatement: 
                                        VarExpr: 
                                                VarExprName: #C
COMMENTS:
COMMENT (14,9)-(14,39):  <#C 5> := <!A #i> + <!B #i> 
COMMENT (31,4)-(31,13):  сложно 
```

# Вывод
В ходе данной лабораторной работы был повторён (повторён, т.к вспомнилось одно из ДЗ
по Основам Информатики с 1 курса на Scheme) алгоритм нисходящего рекурсивного
спуска для парсера. Также был получен опыт перевода грамматики из LALR(1) в LL(1).
Помимо этого был адаптирован однопроходный лексический анализатор под лексическую структуру 
языка L4.

Могу сказать, что нисходящий рекурсивный парсер является довольно несложным механическим
способом написание парсеров, где самое главное правильно составить грамматику, но, на мой взгляд
он довольно плохо подходит для больших грамматик, т.к мы получаем большое количество кода
и много часов работы.