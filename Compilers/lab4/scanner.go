package main

import (
	"bufio"
	"strconv"
)

type Scanner struct {
	programReader *bufio.Reader
	compiler      Compiler
	curPos        Position
	comments      []Fragment
}

func NewScanner(programFile *bufio.Reader, compiler Compiler) Scanner {
	return Scanner{programReader: programFile, compiler: compiler, curPos: NewPosition(programFile)}
}

func (s *Scanner) NextToken() Token {
	for s.curPos.Cp() != -1 {
		for s.curPos.IsWhiteSpace() {
			s.curPos.Next()
		}
		start := s.curPos
		curWord := ""

		switch s.curPos.Cp() {
		case '&':
			globalStart := s.curPos
			s.curPos.Next()
			if s.curPos.GetSymbol() == 'h' {
				s.curPos.Next()
				start = s.curPos
				var pos Position
				for s.curPos.IsHexagonalNumber() {
					curWord += string(s.curPos.Cp())
					pos = s.curPos
					s.curPos.Next()
				}
				if !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
					s.compiler.AddMessage(true, globalStart, "invalid syntax")
					s.curPos.SkipErrors()

					return NewErrToken(s.curPos, s.curPos).Token
				}
				num, _ := strconv.ParseInt(curWord, 16, 64)

				return NewNumberToken(int(num), curWord, start, pos).Token
			} else {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewErrToken(s.curPos, s.curPos).Token
			}
		default:
			if s.curPos.IsLetter() {
				var pos Position
				for s.curPos.IsLetterOrDigit() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}
				if !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewErrToken(s.curPos, s.curPos).Token
				}
				if curWord == "print" {
					return NewSpecToken(PRINTTag, curWord, start, pos).Token
				}
				if curWord == "goto" {
					return NewSpecToken(GOTOTag, curWord, start, pos).Token
				}
				if curWord == "gosub" {
					return NewSpecToken(GOSUBTag, curWord, start, pos).Token
				}

				return NewIdentToken(s.compiler.AddName(curWord), curWord, start, pos).Token
			} else if s.curPos.IsDigit() {
				var pos Position
				for s.curPos.IsDigit() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}
				if !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewErrToken(s.curPos, s.curPos).Token
				}
				num, _ := strconv.ParseInt(curWord, 10, 64)

				return NewNumberToken(int(num), curWord, start, pos).Token
			} else {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewErrToken(s.curPos, s.curPos).Token
			}
		}
	}

	return NewEOPToken(s.curPos, s.curPos).Token
}
