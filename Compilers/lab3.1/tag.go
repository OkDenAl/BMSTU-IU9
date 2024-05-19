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
