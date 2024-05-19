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
		case '+':
			s.curPos.Next()
			return NewToken(TagPlus, start, start, "+")
		case '*':
			s.curPos.Next()
			return NewToken(TagMult, start, start, "*")
		case '(':
			s.curPos.Next()
			return NewToken(TagOpenBracket, start, start, "(")
		case ')':
			s.curPos.Next()
			return NewToken(TagCloseBracket, start, start, ")")
		default:
			if s.curPos.IsDigit() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				var pos Position
				for s.curPos.IsDigit() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}

				return NewToken(TagNumber, start, pos, curWord)
			} else {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()
				return NewToken(ErrTag, start, start, curWord)
			}
		}

	}

	return NewToken(EOFTag, s.curPos, s.curPos, "")
}
