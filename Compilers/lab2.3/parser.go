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
		s == "DECL" || s == "TERM" || s == "NON_TERMS" || s == "INNER_RULES" || s == "TERMS" || s == "RULES" || s == "ALT" || s == "SYMBOLS" || s == "ALTS")
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
	str := fmt.Sprintf("Невалидный токен для раскрытия правила: %s, получен токен: %s", val, tagToString[t.Tag()])
	fmt.Println(t.String())
	fmt.Println(str)
}
