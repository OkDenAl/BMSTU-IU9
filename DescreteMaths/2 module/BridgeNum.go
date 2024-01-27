package main

import "fmt"

type Vertex struct {
	edges        []int
	timeIn, time int
	visited      bool
}

func min(a, b int) int {
	if a > b {
		return b
	} else {
		return a
	}
}

func findBridges(graph []Vertex) int {
	bridgeCounter := 0
	timer := 0
	var dfs func(int, int)
	dfs = func(v, p int) {
		graph[v].visited = true
		graph[v].time = timer
		graph[v].timeIn = timer
		timer++
		for i := 0; i < len(graph[v].edges); i++ {
			to := graph[v].edges[i]
			if to == p {
				continue
			}
			if graph[to].visited {
				graph[v].time = min(graph[v].time, graph[to].timeIn)
			} else {
				dfs(to, v)
				graph[v].time = min(graph[v].time, graph[to].time)
				if graph[to].time > graph[v].timeIn {
					bridgeCounter++
				}
			}
		}
	}
	for i := 0; i < len(graph); i++ {
		if graph[i].visited == false {
			dfs(i, -1)
		}
	}
	return bridgeCounter
}

func main() {
	var N, M int
	fmt.Scan(&N, &M)
	graph := make([]Vertex, N)
	for i := 0; i < M; i++ {
		var u, v int
		fmt.Scan(&u, &v)
		graph[u].edges = append(graph[u].edges, v)
		graph[v].edges = append(graph[v].edges, u)
	}
	fmt.Println(findBridges(graph))
}
