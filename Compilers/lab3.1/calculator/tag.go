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
