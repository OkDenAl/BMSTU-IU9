package main

import (
	"fmt"
	"sort"
)

var time, r int

type Vertex struct {
	name, mark, T1                     int
	parent, sdom, label, ancestor, dom *Vertex
	bucket                             []*Vertex
	edgesIn, edgesOut                  []*Edge
}

func makeVertex(name int) *Vertex {
	var res *Vertex
	res = &Vertex{name, -1, 0, nil, res, res, nil, res,
		make([]*Vertex, 0), make([]*Edge, 0), make([]*Edge, 0)}
	return res
}

type Edge struct {
	parent, target *Vertex
}

func makeEdge(parent, target *Vertex) *Edge {
	return &Edge{parent, target}
}

type VerticesAreSorted []*Vertex

func (graph VerticesAreSorted) Len() int {
	return len(graph)
}

func (graph VerticesAreSorted) Less(i, j int) bool {
	a, b := graph[i].T1, graph[j].T1
	return a > b
}

func (graph VerticesAreSorted) Swap(i, j int) {
	graph[i], graph[j] = graph[j], graph[i]
}

type Stack struct {
	data []*Vertex
	top  int
}

func init_stack(n int) Stack {
	return Stack{make([]*Vertex, n), 0}
}

func (stack *Stack) isEmpty() bool {
	return stack.top == 0
}

func (stack *Stack) push(vertex *Vertex) {
	stack.data[stack.top] = vertex
	stack.top++
}

func (stack *Stack) pop() *Vertex {
	stack.top--
	return stack.data[stack.top]
}

func dfs(vertexes map[int]*Vertex, r int) {
	visitVertex(vertexes[r], vertexes)
}

func visitVertex(v *Vertex, vertexes map[int]*Vertex) {
	v.mark = 1
	v.T1 = time
	time++
	n := len(v.edgesOut)
	for i := 0; i < n; i++ {
		x := v.edgesOut[i]
		if x.target.mark == -1 {
			x.target.parent = v
			visitVertex(x.target, vertexes)
		}
	}
}
func inIt(arr []*Vertex, target *Vertex) bool {
	for _, v := range arr {
		if v == target {
			return true
		}
	}
	return false
}

func Dominators(res []*Vertex) {
	for i := 0; i < len(res)-1; i++ {
		w := res[i]
		for j := 0; j < len(w.edgesIn); j++ {
			v := w.edgesIn[j].parent
			if inIt(res, v) {
				u := findMin(v)
				if u.sdom.T1 < w.sdom.T1 {
					w.sdom = u.sdom
				}
			}
		}
		w.ancestor = w.parent
		w.sdom.bucket = append(w.sdom.bucket, w)
		for j := 0; j < len(w.parent.bucket); j++ {
			v := w.parent.bucket[j]
			u := findMin(v)
			if u.sdom == v.sdom {
				v.dom = v.sdom
			} else {
				v.dom = u
			}
		}
	}
	for i := len(res) - 2; i >= 0; i-- {
		w := res[i]
		if w.dom != nil && w.dom == w.sdom {
			w.dom = w.dom.dom
		}
	}
	res[len(res)-1].dom = nil
}
func findMin(v *Vertex) *Vertex {
	var min *Vertex
	if v.ancestor == nil {
		return v
	} else {
		stack := init_stack(100)
		u := v
		for u.ancestor.ancestor != nil {
			stack.push(u)
			u = u.ancestor
		}
		for !stack.isEmpty() {
			v = stack.pop()
			if v.ancestor.label.sdom.T1 < v.label.sdom.T1 {
				v.label = v.ancestor.label
			}
			v.ancestor = u.ancestor

		}
		min = v.label
	}
	return min
}

func cycleSeek(res []*Vertex) int {
	count := 0
	for i := 0; i < len(res); i++ {
		x := res[i]
		for j := 0; j < len(x.edgesIn); j++ {
			z := x.edgesIn[j]
			y := z.parent
			if inIt(res, y) && x != y && domSeek(x, y) {
				count++
				break
			}
		}
	}
	return count
}

func domSeek(x, y *Vertex) bool {
	if y == nil {
		return false
	}
	if y == x {
		return false
	} else {
		return domSeek(x, y.dom)
	}

}

func main() {
	time = 0
	var n int
	fmt.Scan(&n)
	vertexes := make(map[int]*Vertex, 0)
	res := make([]*Vertex, 0)
	next := false
	from := 0
	for i := 0; i < n; i++ {
		var m int
		fmt.Scan(&m)
		var command string
		fmt.Scan(&command)
		vertexes[m] = makeVertex(m)
		if i == 0 {
			r = m
		}
		if next {
			next = false
			z := makeEdge(vertexes[from], vertexes[m])
			vertexes[from].edgesOut = append(vertexes[from].edgesOut, z)
			vertexes[m].edgesIn = append(vertexes[m].edgesIn, z)
		}
		if command == "ACTION" {
			next = true
			from = m
		} else if command == "JUMP" {
			var l int
			fmt.Scan(&l)
			vertexes[l] = makeVertex(l)
			z := makeEdge(vertexes[m], vertexes[l])
			vertexes[m].edgesOut = append(vertexes[m].edgesOut, z)
			vertexes[l].edgesIn = append(vertexes[l].edgesIn, z)
		} else if command == "BRANCH" {
			var l int
			fmt.Scan(&l)
			vertexes[l] = makeVertex(l)
			z := makeEdge(vertexes[m], vertexes[l])
			vertexes[m].edgesOut = append(vertexes[m].edgesOut, z)
			vertexes[l].edgesIn = append(vertexes[l].edgesIn, z)
			next = true
			from = m
		}
	}
	dfs(vertexes, r)
	for _, elem := range vertexes {
		if elem.mark != 0 {
			res = append(res, elem)
		}
	}
	sort.Sort(VerticesAreSorted(res))
	Dominators(res)
	fmt.Println(cycleSeek(res))
}
