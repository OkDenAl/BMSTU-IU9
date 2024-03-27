package main

type DomainTag int

const (
	IdentTag DomainTag = iota
	NumTag
	GOTOTag
	PRINTTag
	GOSUBTag
	ErrTag
	EOPTag
)

var tagToString = map[DomainTag]string{
	IdentTag: "IDENT",
	NumTag:   "NUM",
	GOTOTag:  "GOTO",
	PRINTTag: "PRINT",
	GOSUBTag: "GOSUB",
	EOPTag:   "EOP",
	ErrTag:   "ERR",
}
