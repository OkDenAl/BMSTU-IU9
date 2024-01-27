package main

import (
	"fmt"
	"math"
	"sort"
)

type Coords struct {
	x, y float64
}

type Edge struct {
	indexCurVertex    int
	indexLinkedVertex int
	weight            float64
}

func countWeight(pointA, pointB Coords) float64 {
	xDistance2 := (pointA.x - pointB.x) * (pointA.x - pointB.x)
	yDistance2 := (pointA.y - pointB.y) * (pointA.y - pointB.y)
	return math.Sqrt(xDistance2 + yDistance2)
}

func kruskal(edges []Edge, treeId []int, n int) float64 {
	minWeight := 0.0
	//used := make([]bool, n)
	//fmt.Println(edges)
	for i := 0; i < (n-1)*n/2; i++ {
		indexCurVertex, indexLinkedVertex := edges[i].indexCurVertex, edges[i].indexLinkedVertex
		if treeId[indexCurVertex] != treeId[indexLinkedVertex] {
			minWeight += edges[i].weight
			oldId := treeId[indexLinkedVertex]
			newId := treeId[indexCurVertex]
			for j := 0; j < n; j++ {
				if treeId[j] == oldId {
					treeId[j] = newId
				}
			}
		}
		//Не работает :(
		/*if (used[edges[i].indexCurVertex] && !used[edges[i].indexLinkedVertex]) ||
			(!used[edges[i].indexCurVertex] && !used[edges[i].indexLinkedVertex]) {
			minWeight += edges[i].weight
			//used[edges[i].indexCurVertex] = true
			fmt.Println(edges[i])
			if i == 0 {
				used[edges[i].indexCurVertex] = true
			}
			used[edges[i].indexLinkedVertex] = true
		}
		fmt.Println(used)*/
	}
	return minWeight
}

func main() {
	var n int
	fmt.Scan(&n)
	points := make([]Coords, n)
	for i := 0; i < n; i++ {
		var x, y float64
		fmt.Scan(&x, &y)
		points[i] = Coords{x, y}
	}
	edges := make([]Edge, (n-1)*n/2)
	counter := 0
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			edges[counter] = Edge{i, j, countWeight(points[i], points[j])}
			counter++
		}
	}
	sort.SliceStable(edges, func(i, j int) bool {
		return edges[i].weight < edges[j].weight
	})
	treeId := make([]int, n)
	for i := 0; i < n; i++ {
		treeId[i] = i
	}
	fmt.Printf("%.2f", kruskal(edges, treeId, n))
}
