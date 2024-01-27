package main

import "fmt"

const (
	MAXINT = 1<<63 - 1
)

type Vertex struct {
	index, index2, dist int
	thisDist            map[int][]int
	edges               []*Vertex
	visited             bool
	parent              *Vertex
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
		v.parent = u
	}
	return changed
}

func min(arr []int) int {
	res := MAXINT
	for _, x := range arr {
		if x < res {
			res = x
		}
	}
	return res
}

func dijkstra(graph []*Vertex, n int) {
	queue := init_priority_queue(n)
	for i := 0; i < n; i++ {
		if i == 0 {
			graph[i].dist = 0
		} else {
			graph[i].dist = MAXINT
		}
		queue.insert(graph[i])
	}
	var ind int
	for !queue.is_empty() {
		v := queue.extract_min()
		ind = v.index2
		v.index = -1
		for i := 0; i < len(v.edges); i++ {
			if v.edges[i].index != -1 && relax(v, v.edges[i], min(v.edges[i].thisDist[ind])) {
				queue.decrease_key(v.edges[i], v.edges[i].dist)
			}
		}
	}
}

/*type Queue struct {
	data             []*Vertex
	size, head, tail int
}

func initQueue(n int) Queue {
	return Queue{make([]*Vertex, n), 0, 0, 0}
}

func (queue *Queue) queueIsEmpty() bool {
	return queue.size == 0
}

func (queue *Queue) enqueue(x *Vertex) {
	queue.data[queue.tail] = x
	queue.tail++
	queue.size++
}

func (queue *Queue) dequeue() *Vertex {
	vert := queue.data[queue.head]
	queue.head++
	queue.size--
	return vert
}

func bfs(graph []*Vertex) {
	res := make()
	queue := initQueue(len(graph))
	for i := 0; i < len(graph); i++ {
		if !graph[i].visited {
			graph[i].visited = true
			queue.enqueue(graph[i])
			for !queue.queueIsEmpty() {
				v := queue.dequeue()

			}
		}
	}
}*/

func isInArr(arr []int, x int) bool {
	for _, a := range arr {
		if a == x {
			return true
		}
	}
	return false
}

func main() {
	var n, m int
	fmt.Scan(&n, &m)
	graph := make([]*Vertex, n)
	for i := 0; i < n; i++ {
		graph[i] = &Vertex{index: i, index2: i, thisDist: make(map[int][]int)}
	}
	for i := 0; i < m; i++ {
		var u, v, c int
		fmt.Scan(&u, &v, &c)
		u--
		v--
		graph[u].thisDist[v] = append(graph[u].thisDist[v], c)
		graph[v].thisDist[u] = append(graph[v].thisDist[u], c)
		graph[u].edges = append(graph[u].edges, graph[v])
		graph[v].edges = append(graph[v].edges, graph[u])
	}
	dijkstra(graph, n)
	c := 0
	ans := make([]int, 0)
	for _, vertex := range graph {
		if vertex.parent != nil && !isInArr(ans, vertex.parent.index2+1) {
			ans = append(ans, vertex.parent.index2+1)
			c++
		}
	}
	fmt.Println(c)
	for _, x := range ans {
		fmt.Println(x)
	}
}
