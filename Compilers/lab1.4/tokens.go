package main

import (
	"errors"
	"fmt"
	"strconv"
)

var ErrInvalidSpecToken = errors.New("invalid spec token")

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

type IdentToken struct {
	Token
}

func NewIdentToken(code int, starting, following Position) IdentToken {
	return IdentToken{Token: NewToken(IdentTag, starting, following, strconv.Itoa(code))}
}

type ErrToken struct {
	Token
}

func NewErrToken(starting, following Position) ErrToken {
	return ErrToken{Token: NewToken(ErrTag, starting, following, "")}
}

type NumberToken struct {
	Token
	code int
}

func NewNumberToken(code int, val string, starting, following Position) NumberToken {
	return NumberToken{Token: NewToken(NumTag, starting, following, val), code: code}
}

type EOPToken struct {
	Token
}

func NewEOPToken(starting, following Position) EOPToken {
	return EOPToken{Token: NewToken(EOPTag, starting, following, "")}
}

type CommaToken struct {
	Token
}

func NewCommaToken(starting, following Position, val string) CommaToken {
	return CommaToken{Token: NewToken(CommaTag, starting, following, val[2:])}
}

type SpecToken struct {
	Token
}

func NewSpecToken(tag DomainTag, val string, starting, following Position) SpecToken {
	if tag != GOTOTag && tag != GOSUBTag {
		panic(ErrInvalidSpecToken)
	}
	return SpecToken{Token: NewToken(tag, starting, following, val)}
}
