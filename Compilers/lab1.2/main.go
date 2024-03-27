package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"regexp"
)

var (
	ErrLexerStop        = errors.New("stop lexer")
	ErrValidationFailed = errors.New("validation failed")
)

var (
	identRegexpStart = regexp.MustCompile(`^\p{L}(\d|\p{L})* `)
	identRegexpFull  = regexp.MustCompile(`^\p{L}(\d|\p{L})*$`)

	numRegexpStart = regexp.MustCompile(`^\b(\d+|\d[a-fA-F0-9]*h)\b `)
	numRegexpFull  = regexp.MustCompile(`^\b(\d+|\d[a-fA-F0-9]*h)\b$`)

	assemblyCommandRegexpStart = regexp.MustCompile(`^(mov|eax) `)
	assemblyCommandRegexpFull  = regexp.MustCompile(`^(mov|eax)$`)
)

type Tag int

const (
	ErrTag Tag = iota
	IdentTag
	NumTag
	AssemblyCommandTag
)

var tagToName = map[Tag]string{
	IdentTag:           "IDENT",
	NumTag:             "NUM",
	AssemblyCommandTag: "ASSEMBLY",
	ErrTag:             "ERR",
}

type Token struct {
	tag  Tag
	line int
	pos  int
	val  string
}

func (t Token) String() string {
	return fmt.Sprintf("%s (%d, %d): %s", tagToName[t.tag], t.line, t.pos, t.val)
}

type ILexer interface {
	NextToken() (Token, error)
}

type lexer struct {
	data    []string
	curLine int
	curPos  int
}

func NewLexer(data []string) (ILexer, error) {
	if len(data) == 0 {
		return nil, ErrValidationFailed
	}
	return &lexer{data: data}, nil
}

func (l *lexer) validateLine() error {
	lineIsEmpty := true
	for lineIsEmpty {
		if len(l.data[l.curLine]) == 0 {
			l.curLine += 1
			l.curPos = 0
		}
		if l.curLine >= len(l.data) {
			return ErrLexerStop
		}

		line := l.data[l.curLine]
		for line[0] == ' ' || line[0] == '\t' {
			line = line[1:]
			l.curPos += 1
			if len(line) == 0 {
				break
			}
		}

		if len(line) != 0 {
			lineIsEmpty = false
		}
		l.data[l.curLine] = line
	}
	return nil
}

func (l *lexer) correctLex(tag Tag, line string, start, end int) Token {
	defer func() {
		l.curPos += end - start
		l.data[l.curLine] = line[end:]
	}()
	return Token{
		tag:  tag,
		line: l.curLine + 1,
		pos:  l.curPos + 1,
		val:  line[start:end],
	}
}

func (l *lexer) errLex(line string) Token {
	defer func() {
		for len(line) > 0 && line[0] != ' ' {
			line = line[1:]
			l.curPos += 1
			l.data[l.curLine] = line
		}
	}()
	return Token{
		tag:  ErrTag,
		line: l.curLine + 1,
		pos:  l.curPos + 1,
	}
}

func (l *lexer) NextToken() (Token, error) {
	if err := l.validateLine(); err != nil {
		return Token{}, err
	}
	line := l.data[l.curLine]

	assemblyCommandPos := assemblyCommandRegexpStart.FindStringIndex(line)
	if assemblyCommandPos == nil {
		assemblyCommandPos = assemblyCommandRegexpFull.FindStringIndex(line)
	}

	identPos := identRegexpStart.FindStringIndex(line)
	if identPos == nil {
		identPos = identRegexpFull.FindStringIndex(line)
	}

	numPos := numRegexpStart.FindStringIndex(line)
	if numPos == nil {
		numPos = numRegexpFull.FindStringIndex(line)
	}

	switch {
	case assemblyCommandPos != nil:
		return l.correctLex(AssemblyCommandTag, line, assemblyCommandPos[0], assemblyCommandPos[1]), nil
	case identPos != nil:
		return l.correctLex(IdentTag, line, identPos[0], identPos[1]), nil
	case numPos != nil:
		return l.correctLex(NumTag, line, numPos[0], numPos[1]), nil
	default:
		return l.errLex(line), nil
	}
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("usage must be: go run main.go <fileTag.txt>\n")
	}
	filePath := os.Args[1]

	file, err := os.Open(filePath)
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	var data []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		data = append(data, scanner.Text())
	}

	lex, err := NewLexer(data)
	if err != nil {
		log.Fatal(err.Error())
	}

	hasNext := true
	for hasNext {
		token, err := lex.NextToken()
		if err != nil {
			switch {
			case errors.Is(err, ErrLexerStop):
				hasNext = false
				continue
			default:
				log.Fatal(err.Error())
			}
		}

		if token.tag != ErrTag {
			fmt.Println(token)
		} else {
			fmt.Printf("syntax error (%d, %d)\n", token.line, token.pos)
		}
	}
}
