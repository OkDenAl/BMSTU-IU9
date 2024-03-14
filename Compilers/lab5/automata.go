package main

type LexerAutomata struct {
	automata    [15][14]int
	finalStates []int
}

func NewLexerAutomata() LexerAutomata {
	return LexerAutomata{
		automata: [15][14]int{
			/*     Lt,Nu, g, o, t, s, u, b, \, (,  ),  Sp, \n, X */
			/*0 */ {1, 2, 5, 1, 1, 1, 1, 1, 3, -1, -1, 14, 14, -1},
			/*     Lt,Nu, g, o, t, s, u, b, \, (,   ),  Sp, \n  X */
			/*1 */ {1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1},
			/*     Lt, Nu, g,  o,  t,  s,  u,  b,  \,  (,   ), Sp, \n, X */
			/*2 */ {-1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
			/*3 */ {-1, -1, -1, -1, -1, -1, -1, -1, 4, 12, 13, -1, -1, -1},
			/*      Lt,Nu,g, o, t, s, u, b, \, (, ),Sp, \n, X */
			/*4 */ {4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, -1, 4},
			/*     Lt,Nu, g, o, t, s, u, b, \,  (,  ),  Sp, \n,  X */
			/*5 */ {1, 1, 1, 6, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1},
			/*6 */ {1, 1, 1, 1, 7, 9, 1, 1, -1, -1, -1, -1, -1, -1},
			/*7 */ {1, 1, 1, 8, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1},
			/*8 */ {1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1},
			/*9 */ {1, 1, 1, 1, 1, 1, 10, 1, -1, -1, -1, -1, -1, -1},
			/*10*/ {1, 1, 1, 1, 1, 1, 1, 11, -1, -1, -1, -1, -1, -1},
			/*11*/ {1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1},
			/*12*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
			/*13*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
			/*14*/ {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, 14, -1},
		},
		finalStates: []int{1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14},
	}
}

func (a LexerAutomata) NextState(curState int, pos Position) int {
	switch {
	case pos.Cp() == 'g':
		return a.automata[curState][2]
	case pos.Cp() == 'o':
		return a.automata[curState][3]
	case pos.Cp() == 't':
		return a.automata[curState][4]
	case pos.Cp() == 's':
		return a.automata[curState][5]
	case pos.Cp() == 'u':
		return a.automata[curState][6]
	case pos.Cp() == 'b':
		return a.automata[curState][7]
	case pos.Cp() == '\\':
		return a.automata[curState][8]
	case pos.Cp() == '(':
		return a.automata[curState][9]
	case pos.Cp() == ')':
		return a.automata[curState][10]
	case pos.IsLetter():
		return a.automata[curState][0]
	case pos.IsDigit():
		return a.automata[curState][1]
	case pos.IsNewLine():
		return a.automata[curState][12]
	case pos.IsWhiteSpace():
		return a.automata[curState][11]
	default:
		return a.automata[curState][13]
	}
}
