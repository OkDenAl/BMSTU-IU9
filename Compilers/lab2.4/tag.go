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
