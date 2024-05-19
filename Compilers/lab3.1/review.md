% Лабораторная работа № 3.1 «Самоприменимый генератор компиляторов
  на основе предсказывающего анализа»
% 22 мая 2024 г.
% Денис Окутин, ИУ9-61Б

# Цель работы
Целью данной работы является изучение алгоритма построения таблиц предсказывающего анализатора.

# Индивидуальный вариант
```
$AXIOM E
$NTERM E' T T' F
$TERM  "+"  "*"  "("  ")"  "n"

* правила грамматики
$RULE  E  = T E'
$RULE  E' = "+" T E'
            $EPS
$RULE  T  = F T'
$RULE  T' = "*" F T'
            $EPS
$RULE  F  = "n"
            "(" E ")"
```

# Реализация

## Лексическая структура 
`Term ::= \"[a-z\+\*\(\)]+\"`   
`NonTerm ::= [A-Z]+'?`  
`KW_AXIOM ::= $AXIOM`  
`KW_NTERM ::= $NTERM`  
`KW_TERM ::= $TERM`  
`KW_RULE ::= $RULE`  
`KW_EPS ::= $EPS`
`KW_EQ ::= =`
`Comment ::= \*.*`
`NL ::= \n`

## Грамматика языка
```
PROG ::= DECLS RULES
DECLS ::= DECL DECLS | $$epsilon$$
DECL ::= AXIOM NL | NTERM NL | TERM NL | NL
AXIOM := KW_AXIOM NonTerm
NTERM := KW_NTERM NonTerm NON_TERMS
TERM ::= KW_TERM Term TERMS
NON_TERMS := NonTerm NON_TERMS | $$epsilon$$
TERMS := Term TERMS | $$epsilon$$
RULES ::= RULE INNER_RULES
INNER_RULES ::= NL INNER_RULES | RULE INNER_RULES | $$epsilon$$
RULE ::= KW_RULE NonTerm KW_EQ BODY
BODY ::= ALT ALTS
ALTS ::= ALT ALTS | $$epsilon$$
ALT  ::= KW_EPS NL | NonTerm SYMBOLS NL | Term SYMBOLS NL
SYMBOLS ::= NonTerm SYMBOLS | Term SYMBOLS | $$epsilon$$
```

## Программная реализация

### Программа, порождающая таблицы разбора
main.go
```go
package main

import (
  "bufio"
  "log"
  "os"
)

func main() {
  defer func() {
    if r := recover(); r != nil {
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

  parser := NewParser()

  tree, err := parser.TopDownParse(&scn)
  if err != nil {
    log.Panic(err)
  }

  gr := GetGrammar(tree)
  //gr.Print()

  table := NewParsingTable(gr)

  //table.ConvertToGoFile("./calculator/parsing_table_gen.go", gr)

  table.ConvertToGoFile("parsing_table_gen.go", gr)
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

type NodePrinter interface {
	Print(indent string)
}

type InnerNode struct {
	nterm    string
	children []NodePrinter
}

func NewInnerNode(nterm string) *InnerNode {
	return &InnerNode{nterm: nterm, children: make([]NodePrinter, 0)}
}

func (in *InnerNode) Print(indent string) {
	fmt.Println(indent+"Внутренний узел: ", in.nterm)
	for _, child := range in.children {
		child.Print(indent + "\t")
	}
}

type Leaf struct {
	tok Token
}

func NewLeaf(t Token) *Leaf {
	return &Leaf{tok: t}
}

func (l *Leaf) Print(indent string) {
	if l.tok.Tag() == TermTag || l.tok.Tag() == NonTermTag {
		fmt.Println(indent + fmt.Sprintf("Лист: %s - %s", tagToString[l.tok.Tag()], l.tok.val))
	} else {
		fmt.Println(indent + fmt.Sprintf("Лист: %s", tagToString[l.tok.Tag()]))
	}
}
```

parser.go
```go
package main

import (
  "errors"
  "fmt"
)

type Parser struct {
  table map[string][]string
  axiom string
}

func NewParser() Parser {
  return Parser{table: parsingTable, axiom: grammarAxiom}
}

var ErrParsingFailed = errors.New("parsing failed")

func (p Parser) TopDownParse(scn *Scanner) (NodePrinter, error) {
  type stackVal struct {
    in  *InnerNode
    val string
  }

  dumpNode := NewInnerNode("")
  stack := make([]stackVal, 0)
  stack = append(stack, stackVal{val: p.axiom, in: dumpNode})

  t := scn.NextToken()
  for t.Tag() != EOPTag {
    if t.Tag() != ErrTag {
      parent := stack[len(stack)-1].in
      val := stack[len(stack)-1].val
      stack = stack[:len(stack)-1]

      if isTerminal(val) {
        if val == tagToString[t.Tag()] {
          parent.children = append(parent.children, NewLeaf(t))
          t = scn.NextToken()
        } else {
          printErr(val, t)
          return nil, ErrParsingFailed
        }
      } else if _, ok := p.table[val+" "+tagToString[t.Tag()]]; ok {
        inner := NewInnerNode(val)
        parent.children = append(parent.children, inner)

        mayToGo := p.table[val+" "+tagToString[t.Tag()]]
        for i := len(mayToGo) - 1; i >= 0; i-- {
          stack = append(stack, stackVal{val: mayToGo[i], in: inner})
        }
      } else {
        printErr(val, t)
        return nil, ErrParsingFailed
      }
    } else {
      return nil, ErrParsingFailed
    }
  }
  return dumpNode.children[0], nil
}

func printErr(val string, t Token) {
  str := fmt.Sprintf("Невалидный токен для раскрытия правила: %s, получен токен: %s", val, tagToString[t.Tag()])
  fmt.Println(t.String())
  fmt.Println(str)
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
	return p.symb == ' ' || p.symb == '\t'
}

func (p *Position) IsLetter() bool {
	return unicode.IsLetter(p.symb)
}

func (p *Position) IsNonTerminal() bool {
	return unicode.IsLetter(p.symb) && unicode.IsUpper(p.symb)
}

func (p *Position) IsTerminal() bool {
	return unicode.IsLetter(p.symb) && unicode.IsLower(p.symb) ||
		p.symb == '*' || p.symb == '(' || p.symb == ')' || p.symb == '+'
}

func (p *Position) IsNewLine() bool {
	return p.symb == '\n'
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
	for s.curPos.Cp() != -1 {
		for s.curPos.IsWhiteSpace() {
			s.curPos.Next()
		}
		start := s.curPos
		curWord := ""

		switch s.curPos.Cp() {
		case '\n':
			s.curPos.Next()
			return NewToken(NLTag, start, start, "NEW_LINE")
		case '"':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			if s.curPos.IsTerminal() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
			}

			if s.curPos.Cp() == -1 || s.curPos.GetSymbol() != '"' {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ErrTag, s.curPos, s.curPos, "")
			}

			curWord += string(rune(s.curPos.Cp()))
			pos := s.curPos
			s.curPos.Next()

			return NewToken(TermTag, start, pos, curWord)
		case '*':
			for !s.curPos.IsNewLine() && s.curPos.Cp() != -1 {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
			}
			s.comments = append(s.comments, newComment(start, s.curPos, curWord))
		case '=':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			return NewToken(KW_EQTag, start, s.curPos, curWord)
		case '$':
			var pos Position
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			for s.curPos.IsLetter() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}

			if !s.curPos.IsNewLine() && !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ErrTag, s.curPos, s.curPos, "")
			}

			switch curWord {
			case "$AXIOM":
				return NewToken(KW_AXIOMTag, start, pos, curWord)
			case "$EPS":
				return NewToken(KW_EPSTag, start, pos, curWord)
			case "$NTERM":
				return NewToken(KW_NTERMTag, start, pos, curWord)
			case "$TERM":
				return NewToken(KW_TERMTag, start, pos, curWord)
			case "$RULE":
				return NewToken(KW_RULETag, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ErrTag, s.curPos, s.curPos, "")
		default:
			if s.curPos.IsNonTerminal() {
				curWord += string(rune(s.curPos.Cp()))
				pos := s.curPos
				s.curPos.Next()
				if s.curPos.GetSymbol() == '\'' {
					curWord += string(rune(s.curPos.Cp()))
					s.curPos.Next()
				}
				return NewToken(NonTermTag, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ErrTag, s.curPos, s.curPos, "")
		}
	}

	return NewToken(EOPTag, s.curPos, s.curPos, "")
}
```

tag.go
```go
package main

type DomainTag int

const (
	KW_EPSTag DomainTag = iota
	KW_AXIOMTag
	KW_RULETag
	KW_NTERMTag
	KW_TERMTag
	KW_EQTag
	ErrTag
	NLTag
	EOPTag
	NonTermTag
	TermTag
)

var tagToString = map[DomainTag]string{
	NonTermTag:  "NonTerm",
	TermTag:     "Term",
	KW_EPSTag:   "KW_EPS",
	KW_AXIOMTag: "KW_AXIOM",
	KW_RULETag:  "KW_RULE",
	KW_NTERMTag: "KW_NTERM",
	KW_TERMTag:  "KW_TERM",
	KW_EQTag:    "KW_EQ",
	NLTag:       "NL",
	EOPTag:      "EOP",
	ErrTag:      "ERR",
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

table_generator.go
```go
package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

type ParsingTable map[string][]string

func (pt ParsingTable) PrintGenTable(genTable map[string][]string) {
	fmt.Println("Таблица парсинга:")
	for k, v := range genTable {
		fmt.Println(k, v)
	}
}

func (pt ParsingTable) ConvertToGoFile(filename string, gr Grammar) {
	file, err := os.OpenFile(filename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 777)
	if err != nil {
		log.Fatal(err)
	}
	file.WriteString("package main\n\n")

	file.WriteString("var parsingTable = map[string][]string{\n")
	for key, val := range pt {
		if len(val) == 1 && val[0] == "" {
			file.WriteString(fmt.Sprintf("\t\"%s\":{},\n", key))
		} else {
			file.WriteString(fmt.Sprintf("\t\"%s\":{%s},\n", key, customJoin(val)))
		}
	}

	file.WriteString("}\n")
	file.WriteString("\nvar grammarAxiom = " + "\"" + gr.Axiom + "\"\n")
	genIsTerminalFunc(file, gr)
	file.Close()
}

func genIsTerminalFunc(file *os.File, gr Grammar) {
	file.WriteString("\nvar nonTerms = []string{\n")
	for nt := range gr.NonTerms {
		file.WriteString("\t\"" + nt + "\",\n")
	}
	file.WriteString("}\n")
	file.WriteString("\nfunc isTerminal(s string) bool {\n\treturn !slices.Contains(nonTerms, s)\n}")
}

func customJoin(arr []string) string {
	res := ""
	for _, elem := range arr {
		res += "\"" + elem + "\","
	}
	return strings.TrimSuffix(res, ",")
}

func NewParsingTable(gr Grammar) ParsingTable {
	firstByNonTerm := make(map[string]map[string]struct{})
	for n := range gr.NonTerms {
		findFirst(firstByNonTerm, n, gr)
	}

	followByNonTerm := make(map[string]map[string]struct{})
	for n := range gr.NonTerms {
		findFollow(firstByNonTerm, followByNonTerm, n, gr)
	}

	table := make(map[string][]string)

	for left, right := range gr.Rules {
		isEps := false
		for _, r := range right {
			r := strings.Split(r, " ")
			firstSet := make(map[string]struct{})
			for _, el := range r {
				isEps = false
				first := findFirst(firstByNonTerm, el, gr)
				for f := range first {
					if f == "" {
						isEps = true
					} else {
						firstSet[f] = struct{}{}
					}
				}

				if _, ok := first[""]; !ok {
					break
				}

			}

			for term := range firstSet {
				key := fmt.Sprintf("%s %s", left, term)
				if _, ok := table[key]; ok {
					log.Fatalf("rules is not LL1: conflict %s", key)
				}
				table[key] = r
			}

			if isEps {
				follow := followByNonTerm[left]
				for term := range follow {
					key := fmt.Sprintf("%s %s", left, term)
					if _, ok := table[key]; ok {
						log.Fatalf("rules is not LL1: conflict %s", key)
					}
					table[key] = r
				}
			}
		}
	}

	return table
}

func findFirst(
	firstByNonTerm map[string]map[string]struct{},
	symbol string,
	gr Grammar,
) map[string]struct{} {
	first := make(map[string]struct{})

	if _, ok := firstByNonTerm[symbol]; ok {
		return firstByNonTerm[symbol]
	}

	if symbol == "" {
		first[""] = struct{}{}
		return first
	}

	if _, ok := gr.Terms[symbol]; ok {
		first[symbol] = struct{}{}
		return first
	}

	for _, alt := range gr.Rules[symbol] {
		ruleSymbs := strings.Split(alt, " ")
		for _, rightSymbol := range ruleSymbs {
			firstOfRight := findFirst(firstByNonTerm, rightSymbol, gr)

			for k := range firstOfRight {
				if len(firstOfRight) == 1 && k == "" {
					first[k] = struct{}{}
				} else if k != "" {
					first[k] = struct{}{}
				}
			}

			if _, ok := firstOfRight[""]; !ok {
				break
			}
		}

	}

	firstByNonTerm[symbol] = first
	return first
}

func findFollow(
	firstByNonTerm map[string]map[string]struct{},
	followByNonTerm map[string]map[string]struct{},
	symbol string,
	gr Grammar,
) map[string]struct{} {
	followSet := make(map[string]struct{})

	if _, ok := followByNonTerm[symbol]; ok {
		return followByNonTerm[symbol]
	}

	if symbol == gr.Axiom {
		followSet["$"] = struct{}{}
	}

	visited := make(map[string]bool)
	visited[symbol] = true

	for key, rule := range gr.Rules {
		for _, alt := range rule {
			ruleSymbs := strings.Split(alt, " ")
			for i, rightSymbol := range ruleSymbs {
				if rightSymbol == symbol && i < len(ruleSymbs)-1 {
					next := ruleSymbs[i+1]

					for firstSymbol := range findFirst(firstByNonTerm, next, gr) {
						if firstSymbol != "" {
							followSet[firstSymbol] = struct{}{}
						}
					}

					if _, ok := firstByNonTerm[next][""]; ok {
						if !visited[key] {
							for followSymbol := range findFollow(firstByNonTerm, followByNonTerm, key, gr) {
								followSet[followSymbol] = struct{}{}
							}
						}
					}
				}

				if rightSymbol == symbol && i == len(ruleSymbs)-1 {
					if !visited[key] {
						for followSymbol := range findFollow(firstByNonTerm, followByNonTerm, key, gr) {
							followSet[followSymbol] = struct{}{}
						}
					}
				}
			}
		}
	}

	followByNonTerm[symbol] = followSet

	return followSet
}
```

parsing_tble_gen.go
```go
package main

import "slices"

var parsingTable = map[string][]string{
	"ALTS KW_EPS":         {"ALT", "ALTS"},
	"ALTS NL":             {},
	"BODY NonTerm":        {"ALT", "ALTS"},
	"TERM KW_TERM":        {"KW_TERM", "Term", "TERMS"},
	"NTERM KW_NTERM":      {"KW_NTERM", "NonTerm", "NON_TERMS"},
	"DECLS KW_NTERM":      {"DECL", "DECLS"},
	"DECLS NL":            {"DECL", "DECLS"},
	"PROG KW_NTERM":       {"DECLS", "RULES"},
	"AXIOM KW_AXIOM":      {"KW_AXIOM", "NonTerm"},
	"ALTS NonTerm":        {"ALT", "ALTS"},
	"ALTS $":              {},
	"ALT KW_EPS":          {"KW_EPS", "NL"},
	"PROG KW_TERM":        {"DECLS", "RULES"},
	"INNER_RULES KW_RULE": {"RULE", "INNER_RULES"},
	"INNER_RULES $":       {},
	"ALT Term":            {"Term", "SYMBOLS", "NL"},
	"TERMS Term":          {"Term", "TERMS"},
	"PROG NL":             {"DECLS", "RULES"},
	"DECL KW_AXIOM":       {"AXIOM", "NL"},
	"DECL KW_NTERM":       {"NTERM", "NL"},
	"DECL KW_TERM":        {"TERM", "NL"},
	"DECL NL":             {"NL"},
	"RULES KW_RULE":       {"RULE", "INNER_RULES"},
	"RULE KW_RULE":        {"KW_RULE", "NonTerm", "KW_EQ", "BODY"},
	"SYMBOLS Term":        {"Term", "SYMBOLS"},
	"SYMBOLS NonTerm":     {"NonTerm", "SYMBOLS"},
	"NON_TERMS NonTerm":   {"NonTerm", "NON_TERMS"},
	"INNER_RULES NL":      {"NL", "INNER_RULES"},
	"NON_TERMS NL":        {},
	"ALTS Term":           {"ALT", "ALTS"},
	"ALTS KW_RULE":        {},
	"ALT NonTerm":         {"NonTerm", "SYMBOLS", "NL"},
	"DECLS KW_RULE":       {},
	"TERMS NL":            {},
	"BODY KW_EPS":         {"ALT", "ALTS"},
	"SYMBOLS NL":          {},
	"DECLS KW_AXIOM":      {"DECL", "DECLS"},
	"DECLS KW_TERM":       {"DECL", "DECLS"},
	"BODY Term":           {"ALT", "ALTS"},
	"PROG KW_RULE":        {"DECLS", "RULES"},
	"PROG KW_AXIOM":       {"DECLS", "RULES"},
}

var grammarAxiom = "PROG"

var nonTerms = []string{
	"NON_TERMS",
	"INNER_RULES",
	"AXIOM",
	"NTERM",
	"TERM",
	"RULE",
	"ALT",
	"DECLS",
	"RULES",
	"ALTS",
	"SYMBOLS",
	"PROG",
	"DECL",
	"TERMS",
	"BODY",
}

func isTerminal(s string) bool {
	return !slices.Contains(nonTerms, s)
}
```

grammar.go
```go
package main

import (
	"fmt"
	"log"
)

type Grammar struct {
	Rules    map[string][]string
	Axiom    string
	NonTerms map[string]struct{}
	Terms    map[string]struct{}
}

func GetGrammar(tree NodePrinter) Grammar {
	gr := Grammar{
		Rules:    make(map[string][]string),
		NonTerms: make(map[string]struct{}),
		Terms:    make(map[string]struct{}),
	}

	var traverse func(treeNode NodePrinter)
	traverse = func(treeNode NodePrinter) {
		if node, ok := treeNode.(*InnerNode); ok {
			switch node.nterm {
			case "PROG":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else {
					log.Fatal("Невалидная длина PROG")
				}
			case "DECLS":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина DECLS")
				}
			case "DECL":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина DECLS")
				}
			case "AXIOM":
				if len(node.children) == 2 {
					if val, ok := node.children[1].(*Leaf); ok {
						if gr.Axiom == "" {
							gr.Axiom = val.tok.val
							gr.NonTerms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Больше чем 1 аксиома")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде Аксиомы")
					}
				} else {
					log.Fatal("Невалидная длина AXIOM")
				}
			case "NTERM":
				if len(node.children) == 3 {
					if val, ok := node.children[1].(*Leaf); ok {
						if _, ok = gr.NonTerms[val.tok.val]; !ok {
							gr.NonTerms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный нетерминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде NTERM")
					}
					traverse(node.children[2])
				} else {
					log.Fatal("Невалидная длина NTERM")
				}
			case "TERM":
				if len(node.children) == 3 {
					if val, ok := node.children[1].(*Leaf); ok {
						if _, ok = gr.Terms[val.tok.val]; !ok {
							gr.Terms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный терминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде TERM")
					}
					traverse(node.children[2])
				} else {
					log.Fatal("Невалидная длина TERM")
				}
			case "NON_TERMS":
				if len(node.children) == 2 {
					if val, ok := node.children[0].(*Leaf); ok {
						if _, ok = gr.NonTerms[val.tok.val]; !ok {
							gr.NonTerms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный нетерминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде NON_TERMS")
					}
					traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина NON_TERMS")
				}
			case "TERMS":
				if len(node.children) == 2 {
					if val, ok := node.children[0].(*Leaf); ok {
						if _, ok = gr.Terms[val.tok.val]; !ok {
							gr.Terms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный терминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде TERMS")
					}
					traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина TERMS")
				}
			case "RULES":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else {
					log.Fatal("Невалидная длина RULES")
				}
			case "INNER_RULES":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					traverse(node.children[0])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина INNER_RULES")
				}
			case "RULE":
				if len(node.children) == 4 {
					var left string
					if val, ok := node.children[1].(*Leaf); ok {
						if _, ok := gr.NonTerms[val.tok.val]; !ok {
							log.Fatal("Необъявленный нетерминал в правой части правила: " + val.tok.String())
						}
						left = val.tok.val
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде RULE")
					}

					body := getBody(gr, node.children[3])
					gr.Rules[left] = append(gr.Rules[left], body...)
				} else {
					log.Fatal("Невалидная длина RULE")
				}
			}

		}
	}

	traverse(tree)
	if gr.Axiom == "" {
		log.Fatal("Не указано ни одной аксиомы")
	}

	for nterm := range gr.NonTerms {
		if _, ok := gr.Rules[nterm]; !ok {
			log.Fatal("Нетерминал " + nterm + " не присутствует ни в одном правиле в левой части")
		}
	}

	return gr
}

func getBody(gr Grammar, tree NodePrinter) []string {
	rules := make([]string, 0)
	curRule := ""
	var traverse func(treeNode NodePrinter)
	traverse = func(treeNode NodePrinter) {
		if node, ok := treeNode.(*InnerNode); ok {
			switch node.nterm {
			case "BODY":
				if len(node.children) == 2 {
					traverse(node.children[0])
					rules = append(rules, curRule)
					curRule = ""
					traverse(node.children[1])
				} else {
					log.Fatal("Невалидная длина BODY")
				}
			case "ALTS":
				if len(node.children) == 2 {
					traverse(node.children[0])
					rules = append(rules, curRule)
					curRule = ""
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					traverse(node.children[0])
					rules = append(rules, curRule)
					curRule = ""
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина ALTS")
				}
			case "ALT":
				if len(node.children) == 3 {
					if n, ok := node.children[0].(*Leaf); ok {
						_, termsOK := gr.Terms[n.tok.val]
						if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
							log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
						}
						if curRule != "" {
							curRule += " " + n.tok.val
						} else {
							curRule += n.tok.val
						}
					}
					traverse(node.children[1])
				} else if len(node.children) == 2 {
					if n, ok := node.children[0].(*Leaf); ok {
						if n.tok.Tag() != KW_EPSTag {
							_, termsOK := gr.Terms[n.tok.val]
							if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
								log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
							}
							if curRule != "" {
								curRule += " " + n.tok.val
							} else {
								curRule += n.tok.val
							}
							traverse(node.children[1])
						}
					}
				} else if len(node.children) != 1 {
					log.Fatal("Невалидная длина ALT")
				}
			case "SYMBOLS":
				if len(node.children) == 2 {
					if n, ok := node.children[0].(*Leaf); ok {
						_, termsOK := gr.Terms[n.tok.val]
						if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
							log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
						}
						if curRule != "" {
							curRule += " " + n.tok.val
						} else {
							curRule += n.tok.val
						}
					}
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					if n, ok := node.children[0].(*Leaf); ok {
						_, termsOK := gr.Terms[n.tok.val]
						if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
							log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
						}
						if curRule != "" {
							curRule += " " + n.tok.val
						} else {
							curRule += n.tok.val
						}
					}
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина SYMBOLS")
				}
			}
		}
	}

	traverse(tree)
	return rules
}

func (g Grammar) Print() {
	fmt.Println("Аксиома:", g.Axiom)
	fmt.Print("Нетерминалы: ")
	var str string
	for val := range g.NonTerms {
		str += val + " "
	}
	fmt.Println(str)

	fmt.Print("Терминалы: ")
	str = ""
	for val := range g.Terms {
		str += val + " "
	}
	fmt.Println(str)

	fmt.Println("Правила:")
	for key, val := range g.Rules {
		str = ""
		for _, r := range val {
			if str == "" {
				str += r
			} else {
				if r == "" {
					r = "ε"
				}
				str += " | " + r
			}
		}
		fmt.Println(key + " -> " + str)
	}
}
```

### Программа калькулятора

calculator/parsing_tble_gen.go
```go
package main

import "slices"

var parsingTable = map[string][]string{
  "T' $": {},
  "F n":  {"n"},
  "F (":  {"(", "E", ")"},
  "E n":  {"T", "E'"},
  "E (":  {"T", "E'"},
  "E' +": {"+", "T", "E'"},
  "E' $": {},
  "T (":  {"F", "T'"},
  "E' )": {},
  "T n":  {"F", "T'"},
  "T' *": {"*", "F", "T'"},
  "T' +": {},
  "T' )": {},
}

var grammarAxiom = "E"

var nonTerms = []string{
  "E",
  "E'",
  "T",
  "T'",
  "F",
}

func isTerminal(s string) bool {
  return !slices.Contains(nonTerms, s)
}
```


calculator/scanner.go
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
	for s.curPos.Cp() != -1 {
		for s.curPos.IsWhiteSpace() {
			s.curPos.Next()
		}
		start := s.curPos
		curWord := ""

		switch s.curPos.Cp() {
		case '+':
			s.curPos.Next()
			return NewToken(TagPlus, start, start, "+")
		case '*':
			s.curPos.Next()
			return NewToken(TagMult, start, start, "*")
		case '(':
			s.curPos.Next()
			return NewToken(TagOpenBracket, start, start, "(")
		case ')':
			s.curPos.Next()
			return NewToken(TagCloseBracket, start, start, ")")
		default:
			if s.curPos.IsDigit() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				var pos Position
				for s.curPos.IsDigit() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}

				return NewToken(TagNumber, start, pos, curWord)
			} else {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()
				return NewToken(ErrTag, start, start, curWord)
			}
		}

	}

	return NewToken(EOFTag, s.curPos, s.curPos, "")
}
```


calculator/tag.go
```go
package main

type DomainTag int

const (
	TagPlus DomainTag = iota
	TagMult
	TagNumber
	TagOpenBracket
	TagCloseBracket
	EOFTag
	ErrTag
)

var tagToString = map[DomainTag]string{
	TagPlus:         "+",
	TagMult:         "*",
	TagNumber:       "n",
	TagOpenBracket:  "(",
	TagCloseBracket: ")",
	EOFTag:          "EOP",
	ErrTag:          "ERR",
}
```

calculator/node.go
```go
package main

import "fmt"

type NodePrinter interface {
	Print(indent string)
}

type InnerNode struct {
	nterm    string
	children []NodePrinter
}

func NewInnerNode(nterm string) *InnerNode {
	return &InnerNode{nterm: nterm, children: make([]NodePrinter, 0)}
}

func (in *InnerNode) Print(indent string) {
	fmt.Println(indent+"Внутренний узел: ", in.nterm)
	for _, child := range in.children {
		child.Print(indent + "\t")
	}
}

type Leaf struct {
	tok Token
}

func NewLeaf(t Token) *Leaf {
	return &Leaf{tok: t}
}

func (l *Leaf) Print(indent string) {
	fmt.Println(indent + fmt.Sprintf("Лист: %s", tagToString[l.tok.Tag()]))
}
```


calculator/interpret.go
```go
package main

import (
	"log"
	"strconv"
)

func Interpret(tree NodePrinter) int {
	var traverse func(treeNode NodePrinter) int
	traverse = func(treeNode NodePrinter) int {
		if node, ok := treeNode.(*InnerNode); ok {
			switch node.nterm {
			case "E":
				if len(node.children) == 2 {
					left := traverse(node.children[0])
					right := traverse(node.children[1])
					return left + right
				} else if len(node.children) == 1 {
					return traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина E")
				}
			case "E'":
				if len(node.children) == 3 {
					left := traverse(node.children[1])
					right := traverse(node.children[2])
					return left + right
				} else if len(node.children) == 2 {
					return traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина E'")
				}
			case "T":
				if len(node.children) == 2 {
					left := traverse(node.children[0])
					right := traverse(node.children[1])
					return left * right
				} else if len(node.children) == 1 {
					return traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина T")
				}
			case "T'":
				if len(node.children) == 3 {
					left := traverse(node.children[1])
					right := traverse(node.children[2])
					return left * right
				} else if len(node.children) == 2 {
					return traverse(node.children[1])
				} else if len(node.children) == 0 {
					return 1
				} else {
					log.Fatal("Невалидная длина T'")
				}
			case "F":
				if len(node.children) == 3 {
					return traverse(node.children[1])
				} else if len(node.children) == 1 {
					return traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина F")
				}
			default:
				log.Fatal("Неизвестный нетерминал", node, len(node.children))
			}
		} else if node, ok := treeNode.(*Leaf); ok {
			value, _ := strconv.Atoi(node.tok.val)
			return value
		}

		return 0
	}

	return traverse(tree)
}
```

calculator/main.go
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

	//t := scn.NextToken()
	//for t.Tag() != EOFTag {
	//	if t.Tag() != ErrTag {
	//		fmt.Println(t.String())
	//	}
	//	t = scn.NextToken()
	//}

	parser := NewParser()

	tree, err := parser.TopDownParse(&scn)
	if err != nil {
		log.Panic(err)
	}

	//tree.Print("")

	fmt.Println(Interpret(tree))
	//
	////compiler.GetIdentsNames()
	compiler.OutputMessages()
}
```

calculator/pasition.go
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

func (p *Position) IsNewLine() bool {
	return p.symb == '\n'
}

func (p *Position) IsDigit() bool {
	return unicode.IsDigit(p.symb)
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


# Тестирование

## Генератор таблиц

### Грамматика на самой себе

Входные данные

```
$AXIOM PROG
$NTERM DECLS DECL AXIOM NTERM TERM NON_TERMS TERMS RULES INNER_RULES RULE BODY ALTS ALT SYMBOLS
$TERM "NL" "KW_AXIOM" "KW_NTERM" "KW_TERM" "KW_RULE" "NonTerm" "Term" "KW_EQ" "KW_EPS"

$RULE PROG = DECLS RULES
$RULE DECLS = DECL DECLS
              $EPS
$RULE DECL = AXIOM "NL"
             NTERM "NL"
             TERM "NL"
             "NL"
$RULE AXIOM = "KW_AXIOM" "NonTerm"
$RULE NTERM = "KW_NTERM" "NonTerm" NON_TERMS
$RULE TERM = "KW_TERM" "Term" TERMS
$RULE NON_TERMS = "NonTerm" NON_TERMS
                  $EPS
$RULE TERMS = "Term" TERMS
              $EPS
$RULE RULES = RULE INNER_RULES
$RULE INNER_RULES = "NL" INNER_RULES
                    RULE INNER_RULES
                    $EPS
$RULE RULE = "KW_RULE" "NonTerm" "KW_EQ" BODY
$RULE BODY = ALT ALTS
$RULE ALTS = ALT ALTS
             $EPS
$RULE ALT  = "KW_EPS" "NL"
             "NonTerm" SYMBOLS "NL"
             "Term" SYMBOLS "NL"
$RULE SYMBOLS = "NonTerm" SYMBOLS
                "Term" SYMBOLS
                $EPS
```

Вывод в файл parsing_table_gen.go

```go
package main

import "slices"

var parsingTable = map[string][]string{
	"ALTS KW_EPS":         {"ALT", "ALTS"},
	"ALTS NL":             {},
	"BODY NonTerm":        {"ALT", "ALTS"},
	"TERM KW_TERM":        {"KW_TERM", "Term", "TERMS"},
	"NTERM KW_NTERM":      {"KW_NTERM", "NonTerm", "NON_TERMS"},
	"DECLS KW_NTERM":      {"DECL", "DECLS"},
	"DECLS NL":            {"DECL", "DECLS"},
	"PROG KW_NTERM":       {"DECLS", "RULES"},
	"AXIOM KW_AXIOM":      {"KW_AXIOM", "NonTerm"},
	"ALTS NonTerm":        {"ALT", "ALTS"},
	"ALTS $":              {},
	"ALT KW_EPS":          {"KW_EPS", "NL"},
	"PROG KW_TERM":        {"DECLS", "RULES"},
	"INNER_RULES KW_RULE": {"RULE", "INNER_RULES"},
	"INNER_RULES $":       {},
	"ALT Term":            {"Term", "SYMBOLS", "NL"},
	"TERMS Term":          {"Term", "TERMS"},
	"PROG NL":             {"DECLS", "RULES"},
	"DECL KW_AXIOM":       {"AXIOM", "NL"},
	"DECL KW_NTERM":       {"NTERM", "NL"},
	"DECL KW_TERM":        {"TERM", "NL"},
	"DECL NL":             {"NL"},
	"RULES KW_RULE":       {"RULE", "INNER_RULES"},
	"RULE KW_RULE":        {"KW_RULE", "NonTerm", "KW_EQ", "BODY"},
	"SYMBOLS Term":        {"Term", "SYMBOLS"},
	"SYMBOLS NonTerm":     {"NonTerm", "SYMBOLS"},
	"NON_TERMS NonTerm":   {"NonTerm", "NON_TERMS"},
	"INNER_RULES NL":      {"NL", "INNER_RULES"},
	"NON_TERMS NL":        {},
	"ALTS Term":           {"ALT", "ALTS"},
	"ALTS KW_RULE":        {},
	"ALT NonTerm":         {"NonTerm", "SYMBOLS", "NL"},
	"DECLS KW_RULE":       {},
	"TERMS NL":            {},
	"BODY KW_EPS":         {"ALT", "ALTS"},
	"SYMBOLS NL":          {},
	"DECLS KW_AXIOM":      {"DECL", "DECLS"},
	"DECLS KW_TERM":       {"DECL", "DECLS"},
	"BODY Term":           {"ALT", "ALTS"},
	"PROG KW_RULE":        {"DECLS", "RULES"},
	"PROG KW_AXIOM":       {"DECLS", "RULES"},
}

var grammarAxiom = "PROG"

var nonTerms = []string{
	"NON_TERMS",
	"INNER_RULES",
	"AXIOM",
	"NTERM",
	"TERM",
	"RULE",
	"ALT",
	"DECLS",
	"RULES",
	"ALTS",
	"SYMBOLS",
	"PROG",
	"DECL",
	"TERMS",
	"BODY",
}

func isTerminal(s string) bool {
	return !slices.Contains(nonTerms, s)
}
```

### Грамматика калькулятора

Входные данные

```
$AXIOM E
$NTERM E' T T' F
$TERM  "+"  "*"  "("  ")"  "n"

$RULE  E  = T E'
$RULE  E' = "+" T E'
            $EPS
$RULE  T  = F T'
$RULE  T' = "*" F T'
            $EPS
$RULE  F  = "n"
            "(" E ")"
```

Вывод в файл calculator/parsing_table_gen.go

```go
package main

import "slices"

var parsingTable = map[string][]string{
  "T' $": {},
  "F n":  {"n"},
  "F (":  {"(", "E", ")"},
  "E n":  {"T", "E'"},
  "E (":  {"T", "E'"},
  "E' +": {"+", "T", "E'"},
  "E' $": {},
  "T (":  {"F", "T'"},
  "E' )": {},
  "T n":  {"F", "T'"},
  "T' *": {"*", "F", "T'"},
  "T' +": {},
  "T' )": {},
}

var grammarAxiom = "E"

var nonTerms = []string{
  "E",
  "E'",
  "T",
  "T'",
  "F",
}

func isTerminal(s string) bool {
  return !slices.Contains(nonTerms, s)
}
```

## Калькулятор

Входные данные

```
2 * (2*(3 + 7) + 5) + (1+1)
```

Вывод на `stdout`
```
52
```


# Вывод
В данной работе были реализованы алгоритмы построения таблиц предсказывающего разбора,
согласно описанию из лекций. Получилось программа, способная генерировать таблицы
предсказывающего разбора. Достаточно интересно, ведь можно описать абсолютно
любую грамматику в нотации варианта и получить для неё таблицу разбора.

Также была сделана тестовая программа-калькулятор, используя таблицу предсказывающего разбора,
сгенерированную в первой части лабы. В ней был применён интерпретатор, чего раньше
ещё не было в лабах.

Это была последняя лаба по очерёдности, которую я делал по курсу Констурирование
Компиляторов и могу с уверенностью сказать, что лабы в курсе были одними из самых
интересных за всё время, ведь здесь мы подробно рассмотрели и научились реализовывать то,
с чем сталкиваемся каждый день при написании кода.