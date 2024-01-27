package main

import "fmt"

func qsort(n int,
	less func(i, j int) bool,
	swap func(i, j int)) {
	partition := func(left, right int) int {
		pivot := left - 1
		for i := left; i < right; i++ {
			if !less(right, i) {
				pivot++
				swap(i, pivot)
			}
		}
		pivot++
		swap(pivot, right)
		return pivot
	}
	var qsort_rec func(left, right int)
	qsort_rec = func(left, right int) {
		if left >= right {
			return
		}
		pi := partition(left, right)
		qsort_rec(left, pi-1)
		qsort_rec(pi+1, right)
	}
	qsort_rec(0, n-1)
}

func main() {
	var n int
	fmt.Scanf("%d", &n)
	a := make([]int, n)
	for i := 0; i < n; i++ {
		fmt.Scan(&a[i])
	}
	qsort(n, func(i, j int) bool { return a[i] > a[j] }, func(i, j int) { a[i], a[j] = a[j], a[i] })
	fmt.Println("Вывод")
	for _, i := range a {
		fmt.Println(i)
	}
}
