package main

import "fmt"

type Mealy struct {
	states, alhabetSize, startState int
	transMatrix                     [][]int
	inputMatrix                     [][]string
}

func (mealy *Mealy) printMealy() {
	fmt.Println("digraph {")
	fmt.Println("\trankdir = LR")
	for i := 0; i < mealy.states; i++ {
		for j := 0; j < mealy.alhabetSize; j++ {
			fmt.Printf("\t%d -> %d [label = \"%c(%s)\"]\n", i, mealy.transMatrix[i][j],
				'a'+j, mealy.inputMatrix[i][j])
		}
	}
	fmt.Println("}")
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

func main() {
	var statesCount, alhabetSize, startState int
	fmt.Scan(&statesCount, &alhabetSize, &startState)
	mealy := makeMealy(statesCount, alhabetSize, startState)
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Scan(&mealy.transMatrix[i][j])
		}
	}
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Scan(&mealy.inputMatrix[i][j])
		}
	}
	mealy.printMealy()
}
