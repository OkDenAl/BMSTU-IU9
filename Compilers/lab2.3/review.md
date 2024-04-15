% Лабораторная работа № 2.3 «Синтаксический анализатор на основе
  предсказывающего анализа»
% 10 апреля 2023 г.
% Окутин Денис, ИУ9-61Б

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

## Неформальное описание синтаксиса входного языка
В качестве входного языка выступает язык представления правил грамматики,
лексика и синтаксис которого восстанавливаются из примера в индивидуальном варианте.

Отметим, что каждое определение программы (PROG) начинается с 
объявления используемых символов и продолжается объявлением правил грамматики.  
`PROG ::= DECLS RULES`

DECLS представляет собой одно или несколько объявлений символов,
используемых в описании грамматики.  
`DECLS ::= DECL DECLS | $$epsilon$$`

DECL представляет собой либо аксиому, либо объявление списка
нетерминалов, либо объявление списка терминалов, заканчивающееся знаком переноса строки (NL).  
`DECL ::= AXIOM NL | NTERM NL | TERM NL`

AXIOM начинается с ключевого слова $AXIOM, за которым следует
какой-то нетерминал.  
`AXIOM := KW_AXIOM NonTerm`

NTERM начинается с ключевого слова $NTERM, за которым следует
множество нетерминалов.  
`NTERM := KW_NTERM NON_TERMS`

TERM начинается с ключевого слова $TERM, за которым следует
множество терминалов.  
`TERM ::= KW_TERM TERMS`

RULES представляет собой одно или несколько объявлений 
правил грамматики или перенос строки.
`RULES ::= RULE RULES | NL RULES`

RULE представляет собой объявление правила грамматики,
которое начинается с ключевого слова $RULE, за ним следует
нетерминал, знак равенства и тело правила.  
`RULE ::= KW_RULE NonTerm = BODY`

BODY представляет собой тело правила, которое может включать
в себя одну или несколько альтернатив.  
`BODY ::= ALT ALTS `

ALT представляет собой тело альтернативы, которое может начинаться либо с
эпсиолна, либо с терминала, либо с нетерминала (символы) и заканчиваться на перенос строки.  
`ALT ::= KW_EPS NL | SYMBOLS NL `


Таким образом, имеем следующие токены:
- Терминал (имя с маленькой буквы или знак пунктуации, кроме скобок, заключенный в двойные кавычки);
- Нетерминал (имя с заглавной буквы, с возможной одинарной кавычкой после);
- Ключевые слова: `$AXIOM`,`$NTERM`, `$TERM`,`$RULE`, `$EPS`;
- Перенос строки.

## Лексическая структура 
`Term ::= \"[a-z+*()]\"`   
`NonTerm ::= [A-Z]'?`  
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
	tree.Print("")

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
}

func NewParser() Parser {
	return Parser{map[string][]string{
		"PROG NL":       {"DECLS", "RULES"},
		"PROG KW_AXIOM": {"DECLS", "RULES"},
		"PROG KW_RULE":  {"DECLS", "RULES"},
		"PROG KW_NTERM": {"DECLS", "RULES"},
		"PROG KW_TERM":  {"DECLS", "RULES"},

		"DECLS NL":       {"DECL", "DECLS"},
		"DECLS KW_AXIOM": {"DECL", "DECLS"},
		"DECLS KW_NTERM": {"DECL", "DECLS"},
		"DECLS KW_TERM":  {"DECL", "DECLS"},
		"DECLS KW_RULE":  {},

		"DECL NL":       {"NL"},
		"DECL KW_AXIOM": {"AXIOM", "NL"},
		"DECL KW_NTERM": {"NTERM", "NL"},
		"DECL KW_TERM":  {"TERM", "NL"},

		"AXIOM KW_AXIOM": {"KW_AXIOM", "NonTerm"},

		"NTERM KW_NTERM": {"KW_NTERM", "NON_TERMS"},

		"TERM KW_TERM": {"KW_TERM", "TERMS"},

		"NON_TERMS NL":      {},
		"NON_TERMS NonTerm": {"NonTerm", "NON_TERMS"},

		"TERMS Term": {"Term", "TERMS"},
		"TERMS NL":   {},

		"RULES KW_RULE": {"RULE", "INNER_RULES"},

		"INNER_RULES NL":      {"NL", "INNER_RULES"},
		"INNER_RULES KW_RULE": {"RULE", "INNER_RULES"},
		"INNER_RULES EOF":     {},

		"RULE KW_RULE": {"KW_RULE", "NonTerm", "KW_EQ", "BODY"},

		"BODY NonTerm": {"ALT", "ALTS"},
		"BODY Term":    {"ALT", "ALTS"},
		"BODY KW_EPS":  {"ALT", "ALTS"},

		"ALTS NL":      {},
		"ALTS KW_RULE": {},
		"ALTS EOF":     {},
		"ALTS ALT":     {"ALT", "ALTS"},
		"ALTS Term":    {"ALT", "ALTS"},
		"ALTS KW_EPS":  {"ALT", "ALTS"},

		"ALT NonTerm": {"NonTerm", "SYMBOLS", "NL"},
		"ALT Term":    {"Term", "SYMBOLS", "NL"},
		"ALT KW_EPS":  {"KW_EPS", "NL"},

		"SYMBOLS NonTerm": {"NonTerm", "SYMBOLS"},
		"SYMBOLS Term":    {"Term", "SYMBOLS"},
		"SYMBOLS NL":      {},
	}}
}

func isTerminal(s string) bool {
	return !(s == "PROG" || s == "DECLS" || s == "AXIOM" || s == "RULE" || s == "NTERM" || s == "BODY" ||
		s == "DECL" || s == "TERM" || s == "NON_TERMS" || s == "INNER_RULES" ||
		s == "TERMS" || s == "RULES" || s == "ALT" || s == "SYMBOLS" || s == "ALTS")
}

var ErrParsingFailed = errors.New("parsing failed")

func (p Parser) TopDownParse(scn *Scanner) (NodePrinter, error) {
	type stackVal struct {
		in  *InnerNode
		val string
	}

	dumpNode := NewInnerNode("")
	stack := make([]stackVal, 0)
	stack = append(stack, stackVal{val: "PROG", in: dumpNode})

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
	str := fmt.Sprintf("Невалидный токен для раскрытия правила: %s, получен токен: %s",
		val, tagToString[t.Tag()])
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

# Тестирование

Входные данные

```
$AXIOM E
$NTERM E' T T' F
$TERM  "+"  "*"  "("  ")"  "n"

* правила грамматики
$RULE  E  = T E'
$RULE  E' = "+" T E'
            $EPS
            * прикольная лаба
$RULE  T  = F T'
$RULE  T' = "*" F T'
            $EPS
$RULE  F  = "n"
            "(" E ")"
```

Вывод на `stdout`
<!-- ENABLE LONG LINES -->
```
Внутренний узел:  PROG
        Внутренний узел:  DECLS
                Внутренний узел:  DECL
                        Внутренний узел:  AXIOM
                                Лист: KW_AXIOM
                                Лист: NonTerm - E
                        Лист: NL
                Внутренний узел:  DECLS
                        Внутренний узел:  DECL
                                Внутренний узел:  NTERM
                                        Лист: KW_NTERM
                                        Внутренний узел:  NON_TERMS
                                                Лист: NonTerm - E'
                                                Внутренний узел:  NON_TERMS
                                                        Лист: NonTerm - T
                                                        Внутренний узел:  NON_TERMS
                                                                Лист: NonTerm - T'
                                                                Внутренний узел:  NON_TERMS
                                                                        Лист: NonTerm - F
                                                                        Внутренний узел:  NON_TERMS
                                Лист: NL
                        Внутренний узел:  DECLS
                                Внутренний узел:  DECL
                                        Внутренний узел:  TERM
                                                Лист: KW_TERM
                                                Внутренний узел:  TERMS
                                                        Лист: Term - "+"
                                                        Внутренний узел:  TERMS
                                                                Лист: Term - "*"
                                                                Внутренний узел:  TERMS
                                                                        Лист: Term - "("
                                                                        Внутренний узел:  TERMS
                                                                                Лист: Term - ")"
                                                                                Внутренний узел:  TERMS
                                                                                        Лист: Term - "n"
                                                                                        Внутренний узел:  TERMS
                                        Лист: NL
                                Внутренний узел:  DECLS
                                        Внутренний узел:  DECL
                                                Лист: NL
                                        Внутренний узел:  DECLS
                                                Внутренний узел:  DECL
                                                        Лист: NL
                                                Внутренний узел:  DECLS
        Внутренний узел:  RULES
                Внутренний узел:  RULE
                        Лист: KW_RULE
                        Лист: NonTerm - E
                        Лист: KW_EQ
                        Внутренний узел:  BODY
                                Внутренний узел:  ALT
                                        Лист: NonTerm - T
                                        Внутренний узел:  SYMBOLS
                                                Лист: NonTerm - E'
                                                Внутренний узел:  SYMBOLS
                                        Лист: NL
                                Внутренний узел:  ALTS
                Внутренний узел:  INNER_RULES
                        Внутренний узел:  RULE
                                Лист: KW_RULE
                                Лист: NonTerm - E'
                                Лист: KW_EQ
                                Внутренний узел:  BODY
                                        Внутренний узел:  ALT
                                                Лист: Term - "+"
                                                Внутренний узел:  SYMBOLS
                                                        Лист: NonTerm - T
                                                        Внутренний узел:  SYMBOLS
                                                                Лист: NonTerm - E'
                                                                Внутренний узел:  SYMBOLS
                                                Лист: NL
                                        Внутренний узел:  ALTS
                                                Внутренний узел:  ALT
                                                        Лист: KW_EPS
                                                        Лист: NL
                                                Внутренний узел:  ALTS
                        Внутренний узел:  INNER_RULES
                                Лист: NL
                                Внутренний узел:  INNER_RULES
                                        Внутренний узел:  RULE
                                                Лист: KW_RULE
                                                Лист: NonTerm - T
                                                Лист: KW_EQ
                                                Внутренний узел:  BODY
                                                        Внутренний узел:  ALT
                                                                Лист: NonTerm - F
                                                                Внутренний узел:  SYMBOLS
                                                                        Лист: NonTerm - T'
                                                                        Внутренний узел:  SYMBOLS
                                                                Лист: NL
                                                        Внутренний узел:  ALTS
                                        Внутренний узел:  INNER_RULES
                                                Внутренний узел:  RULE
                                                        Лист: KW_RULE
                                                        Лист: NonTerm - T'
                                                        Лист: KW_EQ
                                                        Внутренний узел:  BODY
                                                                Внутренний узел:  ALT
                                                                        Лист: Term - "*"
                                                                        Внутренний узел:  SYMBOLS
                                                                                Лист: NonTerm - F
                                                                                Внутренний узел:  SYMBOLS
                                                                                        Лист: NonTerm - T'
                                                                                        Внутренний узел:  SYMBOLS
                                                                        Лист: NL
                                                                Внутренний узел:  ALTS
                                                                        Внутренний узел:  ALT
                                                                                Лист: KW_EPS
                                                                                Лист: NL
                                                                        Внутренний узел:  ALTS
                                                Внутренний узел:  INNER_RULES
                                                        Внутренний узел:  RULE
                                                                Лист: KW_RULE
                                                                Лист: NonTerm - F
                                                                Лист: KW_EQ
                                                                Внутренний узел:  BODY
                                                                        Внутренний узел:  ALT
                                                                                Лист: Term - "n"
                                                                                Внутренний узел:  SYMBOLS
                                                                                Лист: NL
                                                                        Внутренний узел:  ALTS
                                                                                Внутренний узел:  ALT
                                                                                        Лист: Term - "("
                                                                                        Внутренний узел:  SYMBOLS
                                                                                                Лист: NonTerm - E
                                                                                                Внутренний узел:  SYMBOLS
                                                                                                        Лист: Term - ")"
COMMENTS:
COMMENT (5,1)-(5,21): * правила грамматики
COMMENT (9,13)-(9,30): * прикольная лаба
```

# Вывод
В ходе данной лабораторной работы был изучен алгоритм построения таблиц
предсказывающего анализатора. Синтаксический анализатор, разработанный на
основе предсказывающего анализа, принимает на входе текст на входном языке в
соответствии с индивидуальным вариантом, а на выходе порождает дерево вывода
для входного текста.

Был получен интересный опыт составления грамматики к грамматике, позволило взглянуть
на задачу составления грамматики на один абстрактный уровень выше. В очередной раз
вспомнилось ТФЯ при построении таблицы разбора, которую оказалось достаточно удобным
реализовать в виде хеш-таблицы. Также воспользовался наработками с предыдущих лаб
по лексическому анализу, так что он и тут однопроходный.