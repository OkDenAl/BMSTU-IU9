package main

import (
	"bufio"
)

type Scanner struct {
	programReader *bufio.Reader
	compiler      *Compiler
	curPos        Position
	comments      []Fragment
	automata      LexerAutomata
}

func NewScanner(programFile *bufio.Reader, compiler *Compiler) Scanner {
	return Scanner{
		programReader: programFile,
		compiler:      compiler,
		curPos:        NewPosition(programFile),
		automata:      NewLexerAutomata(),
	}
}

func (s *Scanner) NextToken() Token {
	for s.curPos.Cp() != -1 {
		for s.curPos.IsWhiteSpace() {
			s.curPos.Next()
		}
		state := 0
		prevState := 0
		start := s.curPos
		end := Position{}
		curWord := ""
		for state != -1 && s.curPos.Cp() != -1 {
			prevState = state
			state = s.automata.NextState(state, s.curPos)
			if state != -1 {
				end = s.curPos
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
			}
		}
		if state != -1 {
			return s.chooseToken(state, curWord, start, end)
		}
		if prevState != 0 && prevState != 3 && prevState != 14 {
			return s.chooseToken(prevState, curWord, start, end)
		} else {
			for s.curPos.Cp() != -1 && !s.curPos.IsWhiteSpace() {
				s.curPos.Next()
			}
			s.compiler.AddMessage(true, start, "syntax error")

			return NewErrToken(start, s.curPos).Token
		}
	}

	return NewEOPToken(s.curPos, s.curPos).Token
}

func (s *Scanner) chooseToken(prevState int, curWord string, start, end Position) Token {
	tag := DomainTag(prevState)
	if tag == Ident1Tag || tag == Ident2Tag || tag == Ident3Tag ||
		tag == Ident4Tag || tag == Ident5Tag || tag == IdentTag {
		return NewIdentToken(s.compiler.AddName(curWord), start, end).Token
	}
	if tag == GOSUBTag || tag == GOTOTag {
		return NewSpecToken(tag, curWord, start, end).Token
	}
	if tag == CommaTag {
		s.comments = append(s.comments, NewFragment(start, end))

		return NewCommaToken(start, end, curWord).Token
	}

	return NewToken(DomainTag(prevState), start, end, curWord)
}
