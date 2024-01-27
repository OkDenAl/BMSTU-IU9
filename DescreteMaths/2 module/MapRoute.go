package main

import "fmt"

const (
	MAXINT = 1<<63 - 1
)

type Vertex struct {
	index, dist, this_v_dist int
	edges                    []*Vertex
}

type PriorityQueue struct {
	heap []*Vertex
	size int
}

func init_priority_queue(n int) PriorityQueue {
	return PriorityQueue{make([]*Vertex, n), 0}
}

func (queue *PriorityQueue) is_empty() bool {
	return queue.size == 0
}

func (queue *PriorityQueue) insert(v *Vertex) {
	i := queue.size
	queue.size++
	queue.heap[i] = v
	for i > 0 && queue.heap[(i-1)/2].dist > queue.heap[i].dist {
		queue.heap[(i-1)/2], queue.heap[i] = queue.heap[i], queue.heap[(i-1)/2]
		queue.heap[i].index = i
		i = (i - 1) / 2
	}
	queue.heap[i].index = i
}

func (queue *PriorityQueue) extract_min() *Vertex {
	v := queue.heap[0]
	queue.size--
	if queue.size > 0 {
		queue.heap[0] = queue.heap[queue.size]
		queue.heap[0].index = 0
		queue.heapify()
	}
	return v
}

func (queue *PriorityQueue) heapify() {
	l, r, i, j := 0, 0, 0, 0
	for {
		l = i*2 + 1
		r = l + 1
		j = i
		if l < queue.size && queue.heap[i].dist > queue.heap[l].dist {
			i = l
		}
		if r < queue.size && queue.heap[i].dist > queue.heap[r].dist {
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

func (queue *PriorityQueue) decrease_key(v *Vertex, key int) {
	i := v.index
	v.dist = key
	for i > 0 && queue.heap[(i-1)/2].dist > key {
		queue.heap[(i-1)/2], queue.heap[i] = queue.heap[i], queue.heap[(i-1)/2]
		queue.heap[i].index = i
		i = (i - 1) / 2
	}
	v.index = i
}

func relax(u, v *Vertex, w int) bool {
	changed := u.dist+w < v.dist
	if changed {
		v.dist = u.dist + w
	}
	return changed
}

func dijkstra(graph [][]*Vertex, n int) {
	queue := init_priority_queue(n * n)
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			if i == 0 && j == 0 {
				graph[i][j].dist = graph[i][j].this_v_dist
			} else {
				graph[i][j].dist = MAXINT
			}
			queue.insert(graph[i][j])
		}
	}
	for !queue.is_empty() {
		v := queue.extract_min()
		v.index = -1
		for i := 0; i < len(v.edges); i++ {
			if v.edges[i].index != -1 && relax(v, v.edges[i], v.edges[i].this_v_dist) {
				queue.decrease_key(v.edges[i], v.edges[i].dist)
			}
		}
	}
}

func main() {
	var N int
	fmt.Scan(&N)
	matrix := make([][]*Vertex, N)
	for i := 0; i < N; i++ {
		matrix[i] = make([]*Vertex, N)
	}
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			var weight int
			fmt.Scan(&weight)
			matrix[i][j] = &Vertex{
				this_v_dist: weight,
			}
			if i > 0 {
				matrix[i][j].edges = append(matrix[i][j].edges, matrix[i-1][j])
				matrix[i-1][j].edges = append(matrix[i-1][j].edges, matrix[i][j])
			}
			if j > 0 {
				matrix[i][j].edges = append(matrix[i][j].edges, matrix[i][j-1])
				matrix[i][j-1].edges = append(matrix[i][j-1].edges, matrix[i][j])
			}
		}
	}
	dijkstra(matrix, N)
	fmt.Println(matrix[N-1][N-1].dist)
}
