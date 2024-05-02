package main

import (
	"bufio"
	"fmt"
	"unicode"
)

type position struct {
	symb rune
	line int
	pos  int
}

func newPosition(symb rune) position {
	return position{symb: symb, line: 1, pos: 1}
}

type Position struct {
	position
	reader *bufio.Reader
}

func NewPosition(reader *bufio.Reader) Position {
	r, _, err := reader.ReadRune()
	if err != nil {
		r = -1
	}
	return Position{position: newPosition(r), reader: reader}
}

func (p *Position) String() string {
	return fmt.Sprintf("(%d,%d)", p.line, p.pos)
}

func (p *Position) Cp() int {
	return int(p.symb)
}

func (p *Position) IsWhiteSpace() bool {
	return p.symb == ' '
}

func (p *Position) IsLetter() bool {
	return unicode.IsLetter(p.symb)
}

func (p *Position) IsUnderlining() bool {
	return p.symb == '_'
}

func (p *Position) IsDigit() bool {
	return unicode.IsDigit(p.symb)
}

func (p *Position) IsLetterOrDigit() bool {
	return unicode.IsDigit(p.symb) || unicode.IsLetter(p.symb)
}

func (p *Position) IsNewLine() bool {
	return p.symb == '\n'
}

func (p *Position) IsCloseBracket() bool {
	return p.symb == ')' || p.symb == ']' || p.symb == '>' || p.symb == ','
}

func (p *Position) Next() Position {
	r, _, err := p.reader.ReadRune()
	if err == nil {
		if p.IsNewLine() {
			p.line++
			p.pos = 1
		} else {
			p.pos++
		}
		p.symb = r
	} else {
		p.symb = -1
	}
	return *p
}

func (p *Position) SkipErrors() {
	for !p.IsWhiteSpace() {
		pos := *p
		if p.Next() == pos {
			break
		}
	}
}

func (p *Position) GetSymbol() rune {
	return p.symb
}
