package main

import (
	"fmt"
	"sort"
)

type elem struct {
	vertex int
	label  string
}

type Stack struct {
	data   [](*sort.IntSlice)
	length int
}

func initStack() *Stack {
	return &Stack{make([](*sort.IntSlice), 1), 0}
}

func (stack *Stack) isEmpty() bool {
	return stack.length == 0
}

func (stack *Stack) Push(elem *sort.IntSlice) {
	stack.data[stack.length] = elem
	stack.length++
}

func (stack *Stack) Pop() *sort.IntSlice {
	stack.length--
	return stack.data[stack.length]
}

func Closure(list [][]elem, z []int) *sort.IntSlice {
	c, table := make(sort.IntSlice, 0), make(map[int]int)
	var DFS func([][]elem, int, *sort.IntSlice, *(map[int]int))
	DFS = func(list [][]elem, q int, c *sort.IntSlice, table *(map[int]int)) {
		_, flag := (*table)[q]
		if !flag {
			(*table)[q] = q
			*c = append(*c, q)
			for i := 0; i < len(list[q]); i++ {
				if list[q][i].label == "lambda" {
					DFS(list, list[q][i].vertex, c, table)
				}
			}
		}
	}
	for q := 0; q < len(z); q++ {
		DFS(list, z[q], &c, &table)
	}
	sort.Sort(c)
	return &c
}

func printer(delta [][]*sort.IntSlice, delta2 [][]int, X []string) {
	fmt.Printf("\tdummy -> 0\n")
	for i := 0; i < len(delta); i++ {
		table := make(map[int]int)
		count := 0
		for j := 0; j < len(X); j++ {
			_, flag := table[delta2[i][j]]
			if !flag {
				table[delta2[i][j]] = count
				fmt.Printf("\t%d -> %d [label = \"%s", i, delta2[i][j], X[j])
				for z := j + 1; z < len(X); z++ {
					if delta2[i][z] == delta2[i][j] {
						fmt.Printf(", %v", X[z])
					}
				}
				fmt.Printf("\"]\n")
				count++
			}
		}
	}
	fmt.Printf("}\n")
}

func Det(X []string, list [][]elem, circls []int, q int) {
	q_mass := make([]int, 1)
	q_mass[0] = q
	q0 := Closure(list, q_mass)
	arr_state, delta, F := make([]*sort.IntSlice, 1),
		make([][]*sort.IntSlice, 1), make([]*sort.IntSlice, 0)
	mas := make([]*sort.IntSlice, 0)
	arr_state[0] = q0
	count2 := 0
	stack := initStack()
	stack.Push(q0)

	fmt.Printf("digraph {\n\trankdir = LR\n\tdummy [label = \"\", shape = none]\n")

	for !stack.isEmpty() {
		z := stack.Pop()
		for u := 0; u < len(*z); u++ {
			if circls[(*z)[u]] == 1 {
				F = append(F, z)
				break
			}
		}
		for i := 0; i < len(X); i++ {
			arr_u := make([]int, 0)
			for j := 0; j < len(*z); j++ {
				for k := 0; k < len(list[(*z)[j]]); k++ {
					if list[(*z)[j]][k].label == X[i] {
						arr_u = append(arr_u, list[(*z)[j]][k].vertex)
					}
				}
			}
			z1 := Closure(list, arr_u)
			flag := 1
			for j := 0; j < len(arr_state); j++ {
				if len(*(arr_state[j])) == len(*z1) {
					flag2 := 1
					for k := 0; k < len(*z1); k++ {
						if (*z1)[k] != (*arr_state[j])[k] {
							flag2 = 0
							break
						}
					}
					if flag2 == 1 {
						z1 = (arr_state[j])
						flag = 0
						break
					}
				}
			}

			if flag == 1 {
				arr_state = append(arr_state, z1)

				var u []*sort.IntSlice
				delta = append(delta, u)

				if len(stack.data) <= stack.length {
					var u *sort.IntSlice
					stack.data = append(stack.data, u)
				}
				stack.Push(z1)
			}
			if i == 0 {
				flag = 1
				for j := 0; j < len(F); j++ {
					if F[j] == z {
						flag = 0
						break
					}
				}
				mas = append(mas, z)
				if flag == 1 {
					fmt.Printf("\t%v [label = \"%v\", shape = circle]\n", count2, *z)
				} else {
					fmt.Printf("\t%v [label = \"%v\", shape = doublecircle]\n", count2, *z)
				}

				count2++
			}

			delta[count2-1] = append(delta[count2-1], z1)
		}
	}

	delta2 := make([][]int, len(delta))
	for i := 0; i < len(delta); i++ {
		delta2[i] = make([]int, len(X))
	}

	for i := 0; i < len(delta); i++ {
		for j := 0; j < len(X); j++ {
			k := 0
			for k = 0; k < len(mas); k++ {
				if mas[k] == delta[i][j] {
					break
				}
			}
			delta2[i][j] = k
		}
	}
	printer(delta, delta2, X)
}

func main() {
	var n, m int
	fmt.Scan(&n, &m)
	table := make(map[string]string)
	X := make([]string, 0)
	circls := make([]int, n)

	list := make([][]elem, n)
	for i := 0; i < n; i++ {
		list[i] = make([]elem, 0)
	}
	for i := 0; i < m; i++ {
		var v elem
		var flag bool
		var u int
		fmt.Scan(&u, &v.vertex, &v.label)
		list[u] = append(list[u], v)
		_, flag = table[v.label]
		if !flag && v.label != "lambda" {
			table[v.label] = v.label
			X = append(X, v.label)
		}
	}
	for i := 0; i < n; i++ {
		fmt.Scan(&circls[i])
	}
	var start int
	fmt.Scan(&start)
	Det(X, list, circls, start)
}
