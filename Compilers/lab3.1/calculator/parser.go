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
	for t.Tag() != EOFTag {
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
