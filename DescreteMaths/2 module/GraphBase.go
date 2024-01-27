package main

import "fmt"

var time = 1
var count = 1

type Vertex struct {
	id              int
	time, comp, low int
	edges           []int
}

type Stack struct {
	data []*Vertex
	top  int
}

func init_stack(n int) Stack {
	return Stack{make([]*Vertex, n), 0}
}

func push(stack *Stack, vertex *Vertex) {
	stack.data[stack.top] = vertex
	stack.top++
}

func pop(stack *Stack) *Vertex {
	stack.top--
	return stack.data[stack.top]
}

func tarjan(graph []*Vertex) {
	for i := 0; i < len(graph); i++ {
		graph[i].time = 0
		graph[i].comp = 0
	}
	stack := init_stack(len(graph))
	for i := 0; i < len(graph); i++ {
		if graph[i].time == 0 {
			visit_vertex(graph, graph[i], &stack)
		}
	}
}
func visit_vertex(graph []*Vertex, vertex *Vertex, stack *Stack) {
	vertex.time = time
	vertex.low = time
	time++
	push(stack, vertex)
	for i := 0; i < len(vertex.edges); i++ {
		u := graph[vertex.edges[i]]
		if u.time == 0 {
			visit_vertex(graph, u, stack)
		}
		if u.comp == 0 && vertex.low > u.low {
			vertex.low = u.low
		}
	}
	if vertex.time == vertex.low {
		for {
			v := pop(stack)
			v.comp = count
			if v == vertex {
				break
			}
		}
		count++
	}
}
func main() {
	var N, M int
	fmt.Scan(&N, &M)
	graph := make([]*Vertex, N)
	for i := 0; i < N; i++ {
		graph[i] = &Vertex{id: i}
	}
	for i := 0; i < M; i++ {
		var u, v int
		fmt.Scan(&u, &v)
		graph[u].edges = append(graph[u].edges, v)
	}
	tarjan(graph)

	condensMinimal := make([]int, count)
	сondensInBase := make([]bool, count)
	for i := 0; i < count; i++ {
		сondensInBase[i] = true
		condensMinimal[i] = -1
	}

	for i := 0; i < len(graph); i++ {
		if condensMinimal[graph[i].comp] == -1 {
			condensMinimal[graph[i].comp] = i
		}
		for j := 0; j < len(graph[i].edges); j++ {
			u := graph[graph[i].edges[j]]
			if graph[i].comp != u.comp {
				сondensInBase[u.comp] = false
			}
		}
	}

	for i := 1; i < count; i++ {
		if сondensInBase[i] == true {
			fmt.Print(condensMinimal[i], " ")
		}
	}
}
