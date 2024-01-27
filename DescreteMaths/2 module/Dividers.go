package main

import (
	"fmt"
	"sort"
)

func getDeviders(n int) []int {
	res := make([]int, 0)
	for i := 1; i*i <= n; i++ {
		if i*i == n {
			res = append(res, i)
		} else if n%i == 0 {
			res = append(res, i)
			res = append(res, n/i)
		}
	}
	sort.Slice(res, func(i, j int) bool {
		return res[j] < res[i]
	})
	return res
}

func isWExist(u, v int) bool {
	for i := 2; i < u/v/2+1; i++ {
		if (u/v)%i == 0 {
			return true
		}
	}
	return false
}

func isEdge(u, v int) bool {
	if u%v == 0 && !isWExist(u, v) {
		return true
	}
	return false
}

func main() {
	var n int
	fmt.Scanf("%d", &n)
	if n == 1 {
		fmt.Printf("graph {\n\t%d\n}", 1)
		return
	}
	deviders := getDeviders(n)
	fmt.Println("graph {")
	for _, devider := range deviders {
		fmt.Printf("\t%d\n", devider)
	}
	if len(deviders) == 2 {
		fmt.Printf("\t%d--%d\n", deviders[0], deviders[1])
		fmt.Println("}")
		return
	}
	for i, devider1 := range deviders {
		for _, devider2 := range deviders[i+1:] {
			if isEdge(devider1, devider2) {
				fmt.Printf("\t%d--%d\n", devider1, devider2)
			}
		}
	}
	fmt.Println("}")
}
