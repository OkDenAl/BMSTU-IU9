package main

type DomainTag int

const (
	ErrTag DomainTag = iota
	IdentTag
	NumTag
	CommaTag DomainTag = iota + 1
	Ident1Tag
	Ident2Tag
	Ident3Tag
	GOTOTag
	Ident4Tag
	Ident5Tag
	GOSUBTag
	LBTag
	RBTag
	EOPTag
)

var tagToString = map[DomainTag]string{
	IdentTag:  "IDENT",
	NumTag:    "NUM",
	GOTOTag:   "GOTO",
	Ident1Tag: "IDENT",
	Ident2Tag: "IDENT",
	Ident3Tag: "IDENT",
	Ident4Tag: "IDENT",
	Ident5Tag: "IDENT",
	LBTag:     "L_B",
	RBTag:     "R_B",
	CommaTag:  "COMMA",
	GOSUBTag:  "GOSUB",
	EOPTag:    "EOP",
	ErrTag:    "ERR",
}
