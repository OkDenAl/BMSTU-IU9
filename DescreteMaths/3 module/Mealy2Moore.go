package main

import (
	"fmt"
	"sort"
	"strconv"
)

type MealyMachine struct {
	q0 int
	Q  []int
	X  []string
	Y  []string
	s  [][]int
	r  [][]string
}

func (MM MealyMachine) ArrayOfFrequency() [][]string {
	var i, j int
	type help struct {
		name int
		out  string
	}
	MyMap := make(map[help]bool)
	newQ := make([][]string, len(MM.Q))
	for i = 0; i < len(MM.Q); i++ {
		newQ[i] = make([]string, 0)
	}
	for i = 0; i < len(MM.Q); i++ {
		for j = 0; j < len(MM.X); j++ {
			if !MyMap[help{MM.s[i][j], MM.r[i][j]}] {
				newQ[MM.s[i][j]] = append(newQ[MM.s[i][j]], MM.r[i][j])
				MyMap[help{MM.s[i][j], MM.r[i][j]}] = true
			}
		}
	}
	for i = 0; i < len(MM.Q); i++ {
		sort.Strings(newQ[i])
	}
	return newQ
}

func (MM MealyMachine) PrintMur() {
	AOF := MM.ArrayOfFrequency()
	renamer := make(map[string]int, 0)

	createMap := func() map[string]int {
		counter := 0
		for i, arr := range AOF {
			for _, str := range arr {
				renamer[fmt.Sprintf("\"(%d,%s)\"", i, str)] = counter
				counter++
			}
		}
		return renamer
	}

	renamer = createMap()

	fmt.Printf("digraph {\n")
	fmt.Printf("\trankdir = LR\n")
	for i, arr := range AOF {
		for _, str := range arr {
			num, _ := strconv.Atoi(str)
			nameOfNode := renamer[fmt.Sprintf("\"(%d,%s)\"", i, str)]
			fmt.Printf("\t%d [label = \"(%d,%s)\"]\n", nameOfNode, i, MM.Y[num])
		}
	}
	for i, arr := range AOF {
		for _, str := range arr {
			for j, num := range MM.s[i] {
				nameOfNode1 := renamer[fmt.Sprintf("\"(%d,%s)\"", i, str)]
				nameOfNode2 := renamer[fmt.Sprintf("\"(%d,%s)\"", num, MM.r[i][j])]
				fmt.Printf("\t%d -> %d [label = \"%s\"]\n", nameOfNode1, nameOfNode2, MM.X[j])
			}
		}
	}
	fmt.Printf("}\n")
}

func main() {
	var kX, kY, n, i, j int
	fmt.Scan(&kX)
	X := make([]string, kX)
	for i = 0; i < kX; i++ {
		fmt.Scan(&X[i])
	}

	fmt.Scan(&kY)
	Y := make([]string, kY)
	for i = 0; i < kY; i++ {
		fmt.Scan(&Y[i])
	}

	fmt.Scan(&n)
	Q := make([]int, n)
	for i = 0; i < n; i++ {
		Q[i] = i
	}

	s := make([][]int, n)
	for i = 0; i < n; i++ {
		s[i] = make([]int, kX)
		for j = 0; j < kX; j++ {
			fmt.Scan(&s[i][j])
		}
	}

	r := make([][]string, n)
	for i = 0; i < n; i++ {
		r[i] = make([]string, kX)
		for j = 0; j < kX; j++ {
			fmt.Scan(&r[i][j])
		}
	}
	mealy := &MealyMachine{
		q0: 0,
		Q:  Q,
		X:  X,
		Y:  Y,
		s:  s,
		r:  r,
	}
	mealy.PrintMur()
}
