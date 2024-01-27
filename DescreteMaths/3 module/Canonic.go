package main

import (
	"bufio"
	"fmt"
	"os"
)

type Mealy struct {
	states, alhabetSize, startState int
	transMatrix                     [][]int
	inputMatrix                     [][]string
}

func makeMealy(states, alhabet, start int) Mealy {
	mealy := Mealy{states: states, alhabetSize: alhabet, startState: start,
		transMatrix: make([][]int, states), inputMatrix: make([][]string, states)}
	for i := 0; i < states; i++ {
		mealy.transMatrix[i] = make([]int, alhabet)
		mealy.inputMatrix[i] = make([]string, alhabet)
	}
	return mealy
}

func (mealy *Mealy) canonizedNumbering() ([]int, []bool) {
	visited := make([]bool, mealy.states)
	canonizeNumbering := make([]int, mealy.states)
	for i := 0; i < mealy.states; i++ {
		canonizeNumbering[i] = -1
	}
	count := 0
	var DFS func(int)
	DFS = func(ind int) {
		canonizeNumbering[ind] = count
		visited[ind] = true
		count++
		for i := 0; i < mealy.alhabetSize; i++ {
			if !visited[mealy.transMatrix[ind][i]] {
				DFS(mealy.transMatrix[ind][i])
			}
		}
	}
	DFS(mealy.startState)
	return canonizeNumbering, visited
}

func countInt(target int, source []int) int {
	c := 0
	for _, x := range source {
		if x == target {
			c++
		}
	}
	return c
}

func (mealy *Mealy) getCanonized() Mealy {
	canonNumbers, visited := mealy.canonizedNumbering()
	canonized := makeMealy(mealy.states-countInt(-1, canonNumbers), mealy.alhabetSize, 0)
	for i := 0; i < mealy.states; i++ {
		if canonNumbers[i] == -1 && !visited[i] {
			continue
		}
		for j := 0; j < mealy.alhabetSize; j++ {
			canonized.transMatrix[canonNumbers[i]][j] = canonNumbers[mealy.transMatrix[i][j]]
			canonized.inputMatrix[canonNumbers[i]][j] = mealy.inputMatrix[i][j]
		}
	}
	return canonized
}

func printMatrix(mealy Mealy) {
	fmt.Println(mealy.states)
	fmt.Println(mealy.alhabetSize)
	fmt.Println(mealy.startState)
	for i := 0; i < mealy.states; i++ {
		for j := 0; j < mealy.alhabetSize; j++ {
			fmt.Printf("%d ", mealy.transMatrix[i][j])
		}
		fmt.Println()
	}
	for i := 0; i < mealy.states; i++ {
		for j := 0; j < mealy.alhabetSize; j++ {
			fmt.Printf("%s ", mealy.inputMatrix[i][j])
		}
		fmt.Println()
	}
}

func main() {
	stdin := bufio.NewReader(os.Stdin)
	var statesCount, alhabetSize, startState int
	fmt.Fscan(stdin, &statesCount, &alhabetSize, &startState)
	mealy := makeMealy(statesCount, alhabetSize, startState)
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Fscan(stdin, &mealy.transMatrix[i][j])
		}
	}
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Fscan(stdin, &mealy.inputMatrix[i][j])
		}
	}
	printMatrix(mealy.getCanonized())
}
