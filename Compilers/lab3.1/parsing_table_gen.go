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
