package main

import (
	"bufio"
	"fmt"
	"os"
)

type Vertex struct {
	id      int
	edges   []int
	visited bool
	isRed   bool
}
type Component struct {
	vertexes   []int
	edgesCount int
}

type Edges struct {
	indA, indB int
}

func findComps(graph *[]Vertex, n int) []Component {
	comps := make([]Component, 0)
	countOfComps := 0
	var dfs func(*Vertex)
	dfs = func(vertex *Vertex) {
		vertex.visited = true
		comps[countOfComps-1].vertexes = append(comps[countOfComps-1].vertexes, vertex.id)
		comps[countOfComps-1].edgesCount += len(vertex.edges)
		for i := 0; i < len(vertex.edges); i++ {
			to := vertex.edges[i]
			if !(*graph)[to].visited {
				dfs(&(*graph)[to])
			}
		}
	}
	for i := 0; i < n; i++ {
		if !(*graph)[i].visited {
			countOfComps++
			comps = append(comps, Component{})
			dfs(&(*graph)[i])
		}
	}
	return comps
}

func compComponents(a, b Component) bool {
	if len(a.vertexes) != len(b.vertexes) {
		return len(a.vertexes) < len(b.vertexes)
	}
	if a.edgesCount != b.edgesCount {
		return a.edgesCount < b.edgesCount
	}
	return a.vertexes[0] > b.vertexes[0]
}

func main() {
	//start1 := time.Now()
	stdin := bufio.NewReader(os.Stdin)
	var N, M int
	fmt.Fscan(stdin, &N, &M)
	graph := make([]Vertex, N)
	edges := make([]Edges, M)
	for i := 0; i < N; i++ {
		graph[i].id = i
	}
	for i := 0; i < M; i++ {
		var a, b int
		fmt.Fscan(stdin, &a, &b)
		edges[i] = Edges{a, b}
		graph[a].edges = append(graph[a].edges, b)
		graph[b].edges = append(graph[b].edges, a)
	}
	//fmt.Println(time.Since(start1))
	//start := time.Now()
	comps := findComps(&graph, N)
	maxcomp := comps[0]

	for i := 1; i < len(comps); i++ {
		if compComponents(maxcomp, comps[i]) {
			maxcomp = comps[i]
		}
	}
	for _, vertexInd := range maxcomp.vertexes {
		graph[vertexInd].isRed = true
	}
	//fmt.Println(time.Since(start))
	//return
	fmt.Println("graph {")
	for _, vertex := range graph {
		fmt.Print("\t")
		if vertex.isRed {
			fmt.Println(vertex.id, "[color = red]")
		} else {
			fmt.Println(vertex.id)
		}
	}

	for i := 0; i < len(edges); i++ {
		fmt.Print("\t")
		if graph[edges[i].indA].isRed {
			fmt.Println(edges[i].indA, "--", edges[i].indB, "[color = red]")
		} else {
			fmt.Println(edges[i].indA, "--", edges[i].indB)
		}
	}
	fmt.Println("}")
}
