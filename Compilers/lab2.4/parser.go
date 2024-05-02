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
