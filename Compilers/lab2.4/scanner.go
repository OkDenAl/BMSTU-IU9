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
	curWord := ""
	for s.curPos.Cp() != -1 {
		for s.curPos.IsWhiteSpace() {
			s.curPos.Next()
		}
		start := s.curPos

		switch s.curPos.Cp() {
		case '\n':
			s.curPos.Next()
		case ':':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			if s.curPos.GetSymbol() == '=' {
				curWord += string(rune(s.curPos.Cp()))
				pos := s.curPos
				s.curPos.Next()
				return NewToken(SPEC_SYMB, start, pos, curWord)
			}

			return NewToken(SPEC_SYMB, start, start, curWord)
		case '%':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			if s.curPos.GetSymbol() == '%' {
				curWord += string(rune(s.curPos.Cp()))
				pos := s.curPos
				s.curPos.Next()
				return NewToken(SPEC_SYMB, start, pos, curWord)
			}

			return NewToken(SPEC_SYMB, start, start, curWord)
		case '"':
			s.curPos.Next()
			var pos Position
			if !s.curPos.IsNewLine() && s.curPos.Cp() != -1 && s.curPos.GetSymbol() != '"' {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()

				if s.curPos.GetSymbol() == '"' {
					pos = s.curPos
					s.curPos.Next()
					return NewToken(CHAR_CONST, start, pos, curWord)
				}
			}
			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")

		case '\'':
			s.curPos.Next()
			var pos Position
			for s.curPos.Cp() != -1 && s.curPos.GetSymbol() != '\'' && !s.curPos.IsNewLine() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}
			if s.curPos.GetSymbol() == '\'' {
				pos = s.curPos
				s.curPos.Next()
				return NewToken(STRING_CONST, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")

		case '+':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			pos := s.curPos
			for s.curPos.GetSymbol() == '+' {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}

			if !s.curPos.IsNewLine() && !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ERR, s.curPos, s.curPos, "")
			}

			if curWord == "+++" || curWord == "+" {
				return NewToken(SPEC_SYMB, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")
		case '{':
			s.curPos.Next()
			for !s.curPos.IsNewLine() && s.curPos.Cp() != -1 && s.curPos.GetSymbol() != '}' {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
			}

			s.comments = append(s.comments, newComment(start, s.curPos, curWord))
			s.curPos.Next()
			curWord = ""
		case '_', '!', '@', '.', '#':
			curWord += string(rune(s.curPos.Cp()))
			s.curPos.Next()
			var pos Position
			for s.curPos.IsLetter() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
			}

			if s.curPos.IsUnderlining() {
				curWord += string(rune(s.curPos.Cp()))
				pos = s.curPos
				s.curPos.Next()
				if _, ok := keywordsWithUnderliningStart[curWord]; ok {
					return NewToken(KEYWORD, start, pos, curWord)
				}

				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ERR, s.curPos, s.curPos, "")
			}

			if !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() && !s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {
				s.compiler.AddMessage(true, start, "invalid syntax")
				s.curPos.SkipErrors()

				return NewToken(ERR, s.curPos, s.curPos, "")
			}

			return NewToken(VARNAME, start, pos, curWord)
		default:
			if _, ok := specSymbsInOneRune[string(s.curPos.GetSymbol())]; ok {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				return NewToken(SPEC_SYMB, start, start, curWord)
			}

			if s.curPos.IsDigit() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				var pos Position
				for s.curPos.IsLetterOrDigit() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}

				if s.curPos.GetSymbol() == '{' {
					curWord += string(rune(s.curPos.Cp()))
					s.curPos.Next()
					for s.curPos.IsDigit() {
						curWord += string(rune(s.curPos.Cp()))
						pos = s.curPos
						s.curPos.Next()
					}

					if s.curPos.GetSymbol() != '}' && !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() &&
						!s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {

						s.compiler.AddMessage(true, start, "invalid syntax")
						s.curPos.SkipErrors()

						return NewToken(ERR, s.curPos, s.curPos, "")
					}

					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
					return NewToken(INT_CONST, start, pos, curWord)

				}

				if !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() &&
					!s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {

					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewToken(ERR, s.curPos, s.curPos, "")
				}

				return NewToken(INT_CONST, start, pos, curWord)
			}

			if s.curPos.IsLetter() {
				curWord += string(rune(s.curPos.Cp()))
				s.curPos.Next()
				var pos Position
				for s.curPos.IsLetter() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
				}

				if s.curPos.IsDigit() {
					continue
				}

				if s.curPos.IsUnderlining() {
					curWord += string(rune(s.curPos.Cp()))
					pos = s.curPos
					s.curPos.Next()
					if _, ok := keywords[curWord]; ok {
						return NewToken(KEYWORD, start, pos, curWord)
					}

					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewToken(ERR, s.curPos, s.curPos, "")
				}

				if !s.curPos.IsCloseBracket() && !s.curPos.IsNewLine() &&
					!s.curPos.IsWhiteSpace() && s.curPos.Cp() != -1 {

					s.compiler.AddMessage(true, start, "invalid syntax")
					s.curPos.SkipErrors()

					return NewToken(ERR, s.curPos, s.curPos, "")
				}

				if _, ok := keywords[curWord]; ok {
					return NewToken(KEYWORD, start, pos, curWord)
				}

				return NewToken(FUNCNAME, start, pos, curWord)
			}

			s.compiler.AddMessage(true, start, "invalid syntax")
			s.curPos.SkipErrors()

			return NewToken(ERR, s.curPos, s.curPos, "")
		}
	}

	return NewToken(EOP, s.curPos, s.curPos, "")
}
