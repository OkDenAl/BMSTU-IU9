package entity

type Matrix struct {
	FirstLine  [3]int `json:"firstLine"`
	SecondLine [3]int `json:"secondLine"`
	ThirdLine  [3]int `json:"thirdLine"`
}

// 0 1 2
////////
// 1 2 3
// 4 5 6
// 7 8 9
func (m *Matrix) Det() int {
	pos := m.FirstLine[0]*m.SecondLine[1]*m.ThirdLine[2] +
		+m.FirstLine[2]*m.SecondLine[0]*m.ThirdLine[1] +
		+m.FirstLine[1]*m.SecondLine[2]*m.ThirdLine[0]

	neg := m.FirstLine[2]*m.SecondLine[1]*m.ThirdLine[0] +
		+m.FirstLine[0]*m.SecondLine[2]*m.ThirdLine[1] +
		+m.FirstLine[1]*m.SecondLine[0]*m.ThirdLine[2]

	return pos - neg
}
