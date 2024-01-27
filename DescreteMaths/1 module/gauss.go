package main

import "fmt"

func abs(a int64) int64 {
	if a > 0 {
		return a
	} else {
		return -a
	}
}

func gcd(a, b int64) int64 {
	a, b = abs(a), abs(b)
	for b != 0 {
		t := b
		b = a % b
		a = t
	}

	return a
}

func swapLines(firstLine, secondLine []Fract) {
	for i := 0; i < len(firstLine); i++ {
		firstLine[i], secondLine[i] = secondLine[i], firstLine[i]
	}
}

type Fract struct {
	num, den int64
}

func (f Fract) simplify() Fract {
	num, den := f.num, f.den
	if num < 0 && den < 0 {
		num, den = abs(num), abs(den)
	} else if num < 0 || den < 0 {
		num, den = -abs(num), abs(den)
	}
	k := gcd(num, den)
	num /= k
	den /= k
	return Fract{num, den}
}
func (a Fract) devide(b Fract) Fract {
	return Fract{a.num * b.den, a.den * b.num}.simplify()
}
func (a Fract) subtraction(b Fract) Fract {
	return Fract{a.num*b.den - a.den*b.num, a.den * b.den}.simplify()
}
func (a Fract) multiply(b Fract) Fract {
	return Fract{a.num * b.num, a.den * b.den}.simplify()
}

func fractorize(matrix [][]int64, n int) [][]Fract {
	fracted := make([][]Fract, n)
	for i := 0; i < n; i++ {
		fracted[i] = make([]Fract, n+1)
		for j := 0; j < n+1; j++ {
			fracted[i][j] = Fract{matrix[i][j], 1}
		}
	}
	return fracted
}

func gauss(matrix [][]int64, n int) []Fract {
	fracted := fractorize(matrix, n)
	for i := 0; i < n; i++ {
		if fracted[i][i].num == 0 {
			f := 0
			for k := i + 1; k < n; k++ {
				if abs(fracted[k][i].num) > abs(fracted[i][i].num) {
					swapLines(fracted[i], fracted[k])
					f = 1
					break
				}
			}
			if f == 0 {
				return nil
			}
		}
		if (fracted[i][i].num != 1 && fracted[i][i].num != 1) || fracted[i][i].num != 0 {
			devider := fracted[i][i]
			for k := 0; k < n+1; k++ {
				fracted[i][k] = fracted[i][k].devide(devider).simplify()
			}
		}
		for j := i + 1; j < n; j++ {
			mul := fracted[j][i]
			for k := 0; k < n+1; k++ {
				fracted[j][k] = fracted[j][k].subtraction(fracted[i][k].multiply(mul).simplify()).simplify()
			}
		}
	}
	ans := make([]Fract, n)
	ans[n-1] = fracted[n-1][n].simplify()
	curInd := 2
	for n-curInd != -1 {
		ans[n-curInd] = fracted[n-curInd][n]
		for k := n - 1; k > n-curInd; k-- {
			ans[n-curInd] = ans[n-curInd].subtraction(fracted[n-curInd][k].multiply(ans[k]).simplify()).simplify()
		}
		curInd++
	}

	return ans
}

func main() {
	var n int
	fmt.Scan(&n)
	matrix := make([][]int64, n)
	for i := 0; i < n; i++ {
		matrix[i] = make([]int64, n+1)
		for j := 0; j <= n; j++ {
			fmt.Scan(&matrix[i][j])
		}
	}
	res := gauss(matrix, n)
	if res == nil {
		fmt.Println("No solution")
	} else {
		for _, f := range res {
			fmt.Printf("%d/%d\n", f.num, f.den)
		}
	}
}
