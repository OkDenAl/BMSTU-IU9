package main

import (
	"bufio"
	"fmt"
)

type comment struct {
	Fragment
	value string
}

func newComment(starting, following Position, value string) comment {
	return comment{NewFragment(starting, following), value}
}

func (c comment) String() string {
	return fmt.Sprintf("COMMENT %s-%s: %s", c.starting.String(), c.following.String(), c.value)
}

type Scanner struct {
	programReader *bufio.Reader
	compiler      *Compiler
	curPos        Position
	comments      []comment
}

func NewScanner(programFile *bufio.Reader, compiler *Compiler) Scanner {
	return Scanner{programReader: programFile, compiler: compiler, curPos: NewPosition(programFile)}
}

func (s *Scanner) printComments() {
	for _, comm := range s.comments {
		fmt.Println(comm)
	}
}

func (s *Scanner) NextToken() Token {
	for s.curPos.Cp() != -1 {
		for s.curPos.IsWhiteSpace() {
			s.curPos.Next()
		}
		start := s.curPos
		curWord := ""

		switch s.curPos.Cp() {
		case '\n':
			s.curPos.Next()
			return NewToken(NLTag, start, start, "NEW_LINE")
		case '"':
			s.curPos.Next()
			for s.curPos.IsTerminal() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
			}

			if s.curPos.Cp() == -1 || s.curPos.GetSymbol() != '"' {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ErrTag, s.curPos, s.curPos, "")
			}

			pos := s.curPos
			s.curPos.Next()

			return NewToken(TermTag, start, pos, curWord)
		case '*':
			for !s.curPos.IsNewLine() && s.curPos.Cp() != -1 {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
			}
			s.comments = append(s.comments, newComment(start, s.curPos, curWord))
		case '=':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			return NewToken(KW_EQTag, start, s.curPos, curWord)
		case '$':
			var pos Position
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			for s.curPos.IsLetter() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}

			if !s.curPos.IsNewLine() && !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ErrTag, s.curPos, s.curPos, "")
			}

			switch curWord {
			case "$AXIOM":
				return NewToken(KW_AXIOMTag, start, pos, curWord)
			case "$EPS":
				return NewToken(KW_EPSTag, start, pos, curWord)
			case "$NTERM":
				return NewToken(KW_NTERMTag, start, pos, curWord)
			case "$TERM":
				return NewToken(KW_TERMTag, start, pos, curWord)
			case "$RULE":
				return NewToken(KW_RULETag, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ErrTag, s.curPos, s.curPos, "")
		default:
			if s.curPos.IsNonTerminal() {
				var pos Position
				for s.curPos.IsNonTerminal() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}

				if s.curPos.GetSymbol() == '\'' {
					curWord += string(rune(s.curPos.Cp()))
					s.curPos.Next()
				}

				if !s.curPos.IsNewLine() && !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewToken(ErrTag, s.curPos, s.curPos, "")
				}
				return NewToken(NonTermTag, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ErrTag, s.curPos, s.curPos, "")
		}
	}

	return NewToken(EOPTag, s.curPos, s.curPos, "")
}
