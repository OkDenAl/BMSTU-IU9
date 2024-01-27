package main

import "fmt"

type Vertex struct {
	index, key int
	edges      []Edge
}

type Edge struct {
	u, v, weight int
}

type PriorityQueue struct {
	size int
	heap []*Vertex
}

func initPriorityQueue(n int) PriorityQueue {
	return PriorityQueue{0, make([]*Vertex, n)}
}

func insert(queue *PriorityQueue, vertex *Vertex) {
	i := queue.size
	queue.size = i + 1
	queue.heap[i] = vertex
	for i > 0 && queue.heap[(i-1)/2].key > queue.heap[i].key {
		queue.heap[(i-1)/2], queue.heap[i] = queue.heap[i], queue.heap[(i-1)/2]
		queue.heap[i].index = i
		i = (i - 1) / 2
	}
	queue.heap[i].index = i
}

func extractMin(queue *PriorityQueue) *Vertex {
	vertex := queue.heap[0]
	queue.size--
	if queue.size > 0 {
		queue.heap[0] = queue.heap[queue.size]
		queue.heap[0].index = 0
		heapify(queue)
	}
	return vertex
}

func heapify(queue *PriorityQueue) {
	l, r, j, i := 0, 0, 0, 0
	for {
		l = 2*i + 1
		r = l + 1
		j = i
		if l < queue.size && queue.heap[i].key > queue.heap[l].key {
			i = l
		}
		if r < queue.size && queue.heap[i].key > queue.heap[r].key {
			i = r
		}
		if i == j {
			break
		}
		queue.heap[i], queue.heap[j] = queue.heap[j], queue.heap[i]
		queue.heap[i].index = i
		queue.heap[j].index = j
	}
}

func decreaseKey(queue *PriorityQueue, vertex *Vertex, key int) {
	i := vertex.index
	vertex.key = key
	for i > 0 && queue.heap[(i-1)/2].key > key {
		queue.heap[(i-1)/2], queue.heap[i] = queue.heap[i], queue.heap[(i-1)/2]
		queue.heap[i].index = i
		i = (i - 1) / 2
	}
	vertex.index = i
}

func mstPrim(graph []Vertex) int {
	res := 0
	queue := initPriorityQueue(len(graph))
	vertex := &graph[0]
	for {
		vertex.index = -2
		for i := 0; i < len(vertex.edges); i++ {
			_u := &graph[vertex.edges[i].v]
			if _u.index == -1 {
				_u.key = vertex.edges[i].weight
				insert(&queue, _u)
			} else if _u.index != -2 && vertex.edges[i].weight < _u.key {
				decreaseKey(&queue, _u, vertex.edges[i].weight)
			}
		}
		if queue.size == 0 {
			break
		}
		vertex = extractMin(&queue)
		res += vertex.key
	}
	return res
}

func main() {
	var n, m int
	fmt.Scan(&n, &m)
	graph := make([]Vertex, n)
	for i := 0; i < n; i++ {
		graph[i].index = -1
		graph[i].key = -1
	}

	for i := 0; i < m; i++ {
		var u, v, weight int
		fmt.Scan(&u, &v, &weight)
		graph[u].edges = append(graph[u].edges, Edge{u, v, weight})
		graph[v].edges = append(graph[v].edges, Edge{v, u, weight})
	}

	fmt.Println(mstPrim(graph))
}
