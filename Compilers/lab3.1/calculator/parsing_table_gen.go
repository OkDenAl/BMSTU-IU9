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
