package main

import "fmt"

type DSU struct {
	parent, depth []int
}

func makeDSU(n int) *DSU {
	dsu := &DSU{make([]int, n), make([]int, n)}
	for i := 0; i < n; i++ {
		dsu.parent[i] = i
	}
	return dsu
}
func (dsu *DSU) find(x int) int {
	if dsu.parent[x] == x {
		return x
	} else {
		dsu.parent[x] = dsu.find(dsu.parent[x])
		return dsu.parent[x]
	}
}

func (dsu *DSU) union(x, y int) {
	rootX := dsu.find(x)
	rootY := dsu.find(y)
	if dsu.depth[rootX] < dsu.depth[rootY] {
		dsu.parent[rootX] = rootY
	} else {
		dsu.parent[rootY] = rootX
		if dsu.depth[rootX] == dsu.depth[rootY] && rootX != rootY {
			dsu.depth[rootX]++
		}
	}
}

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

func (mealy *Mealy) split1() ([]int, int) {
	roots := make([]int, mealy.states)
	count := mealy.states
	dsu := makeDSU(count)
	for i := 0; i < mealy.states; i++ {
		for j := 0; j < mealy.states; j++ {
			if dsu.find(i) != dsu.find(j) {
				eq := true
				for k := 0; k < mealy.alhabetSize; k++ {
					if mealy.inputMatrix[i][k] != mealy.inputMatrix[j][k] {
						eq = false
						break
					}
				}
				if eq {
					dsu.union(i, j)
					count--
				}
			}
		}
	}
	for i := 0; i < mealy.states; i++ {
		roots[i] = dsu.find(i)
	}
	return roots, count
}

func (mealy *Mealy) split(roots []int) ([]int, int) {
	m := mealy.states
	dsu := makeDSU(m)
	for i := 0; i < mealy.states; i++ {
		for j := 0; j < mealy.states; j++ {
			if roots[i] == roots[j] && dsu.find(i) != dsu.find(j) {
				eq := true
				for k := 0; k < mealy.alhabetSize; k++ {
					w1 := mealy.transMatrix[i][k]
					w2 := mealy.transMatrix[j][k]
					if roots[w1] != roots[w2] {
						eq = false
						break
					}
				}
				if eq {
					dsu.union(i, j)
					m--
				}
			}
		}
	}
	for i := 0; i < mealy.states; i++ {
		roots[i] = dsu.find(i)
	}
	return roots, m
}

func (mealy *Mealy) aufenkampHohn() *Mealy {
	roots, m1 := mealy.split1()
	var m2 int
	for {
		roots, m2 = mealy.split(roots)
		if m1 == m2 {
			break
		}
		m1 = m2
	}
	a, b := make([]int, mealy.states), make([]int, mealy.states)
	counter := 0
	for i := 0; i < mealy.states; i++ {
		if roots[i] == i {
			a[counter] = i
			b[i] = counter
			counter++
		}
	}
	minimized := makeMealy(m1, mealy.alhabetSize, b[roots[mealy.startState]])
	for i := 0; i < minimized.states; i++ {
		for j := 0; j < mealy.alhabetSize; j++ {
			minimized.transMatrix[i][j] = b[roots[mealy.transMatrix[a[i]][j]]]
			minimized.inputMatrix[i][j] = mealy.inputMatrix[a[i]][j]
		}
	}
	return minimized
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

func (mealy *Mealy) getCanonized() *Mealy {
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

func makeMealy(states, alhabet, start int) *Mealy {
	mealy := &Mealy{states: states, alhabetSize: alhabet, startState: start,
		transMatrix: make([][]int, states), inputMatrix: make([][]string, states)}
	for i := 0; i < states; i++ {
		mealy.transMatrix[i] = make([]int, alhabet)
		mealy.inputMatrix[i] = make([]string, alhabet)
	}
	return mealy
}

func isEq(mealy1, mealy2 *Mealy) bool {
	if mealy1.states != mealy2.states || mealy1.alhabetSize != mealy2.alhabetSize ||
		mealy1.startState != mealy2.startState {
		return false
	}
	for i := 0; i < mealy1.states; i++ {
		for j := 0; j < mealy1.alhabetSize; j++ {
			if mealy1.transMatrix[i][j] != mealy2.transMatrix[i][j] ||
				mealy1.inputMatrix[i][j] != mealy2.inputMatrix[i][j] {
				return false
			}
		}
	}
	return true
}

func main() {
	var statesCount, alhabetSize, startState int
	fmt.Scan(&statesCount, &alhabetSize, &startState)
	mealy1 := makeMealy(statesCount, alhabetSize, startState)
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Scan(&mealy1.transMatrix[i][j])
		}
	}
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Scan(&mealy1.inputMatrix[i][j])
		}
	}
	fmt.Scan(&statesCount, &alhabetSize, &startState)
	mealy2 := makeMealy(statesCount, alhabetSize, startState)
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Scan(&mealy2.transMatrix[i][j])
		}
	}
	for i := 0; i < statesCount; i++ {
		for j := 0; j < alhabetSize; j++ {
			fmt.Scan(&mealy2.inputMatrix[i][j])
		}
	}
	minmizedMealy1 := mealy1.aufenkampHohn().getCanonized()
	minmizedMealy2 := mealy2.aufenkampHohn().getCanonized()
	if isEq(minmizedMealy1, minmizedMealy2) {
		fmt.Println("EQUAL")
	} else {
		fmt.Println("NOT EQUAL")
	}
}
