package main

import (
	"bufio"
	"fmt"
	"os"
)

type Vertex struct {
	id             int
	edges          []int
	distanceToRoot []int
}

type Queue struct {
	data             []Vertex
	size, head, tail int
}

func initQueue(n int) Queue {
	return Queue{make([]Vertex, n), 0, 0, 0}
}

func (queue *Queue) queueIsEmpty() bool {
	return queue.size == 0
}

func (queue *Queue) enqueue(x Vertex) {
	queue.data[queue.tail] = x
	queue.tail++
	queue.size++
}

func (queue *Queue) dequeue() Vertex {
	vert := queue.data[queue.head]
	queue.head++
	queue.size--
	return vert
}

func bfs(graph []Vertex, rootInd, rootRelate int) {
	queue := initQueue(len(graph))
	visited := make([]bool, len(graph))
	queue.enqueue(graph[rootInd])
	for !queue.queueIsEmpty() {
		v := queue.dequeue()
		visited[v.id] = true
		for _, edge := range v.edges {
			if !visited[edge] {
				visited[edge] = true
				graph[edge].distanceToRoot[rootRelate] = graph[v.id].distanceToRoot[rootRelate] + 1
				queue.enqueue(graph[edge])
			}
		}
	}
}

func eqDistFilter(graph []Vertex, k int) bool {
	if k == 1 {
		for i, _ := range graph {
			fmt.Print(i, " ")
		}
		return true
	}
	f := 0
	for _, vert := range graph {
		c := 0
		x := vert.distanceToRoot[0]
		for i := 1; i < len(vert.distanceToRoot); i++ {
			if x != vert.distanceToRoot[i] || vert.distanceToRoot[i] == 0 {
				break
			}
			c++
		}
		if c == len(vert.distanceToRoot)-1 {
			f = 1
			fmt.Print(vert.id, " ")
		}
	}
	if f == 0 {
		return false
	}
	return true
}

func main() {
	stdin := bufio.NewReader(os.Stdin)
	var N, M int
	fmt.Fscan(stdin, &N, &M)
	graph := make([]Vertex, N)
	for i := 0; i < N; i++ {
		graph[i].id = i
	}
	for i := 0; i < M; i++ {
		var u, v int
		fmt.Fscan(stdin, &u, &v)
		graph[u].edges = append(graph[u].edges, v)
		graph[v].edges = append(graph[v].edges, u)
	}
	var K int
	fmt.Fscan(stdin, &K)
	for i := 0; i < N; i++ {
		graph[i].distanceToRoot = make([]int, K)
	}
	for i := 0; i < K; i++ {
		var rootVert int
		fmt.Fscan(stdin, &rootVert)
		bfs(graph, rootVert, i)
	}
	if !eqDistFilter(graph, K) {
		fmt.Println("-")
	}
}
