package main

import (
	"fmt"
	"math"
)

func minLen(a, b []int32) []int32 {
	if len(a) > len(b) {
		return b
	} else {
		return a
	}
}
func maxLen(a, b []int32) []int32 {
	if len(a) > len(b) {
		return a
	} else {
		return b
	}
}

func helper2(a, b []int32) []int32 {
	if len(a) > len(b) {
		return a
	} else {
		return b
	}
}

func add(a, b []int32, p int) []int32 {
	arr := make([]int32, int(math.Max(float64(len(a)), float64(len(b)))))
	var index int
	var ostat int32
	ostat = 0
	p_ := int32(p)
	for i, aNum := range minLen(a, b) {
		arr[i] = aNum + helper2(a, b)[i] + ostat
		if arr[i] >= p_ {
			ostat = arr[i] / p_
			arr[i] = arr[i] % p_
		} else {
			ostat = 0
		}
		index = i
	}
	for i := index + 1; i < len(maxLen(a, b)); i++ {
		arr[i] = (maxLen(a, b))[i] + ostat
		if arr[i] >= p_ {
			ostat = arr[i] / p_
			arr[i] = arr[i] % p_
		} else {
			ostat = 0
		}
	}
	if ostat != 0 {
		arr = append(arr, ostat)
	}
	return arr
}

func main() {
	a := []int32{674}
	b := []int32{422, 242}
	var ans []int32
	ans = add(a, b, 800)
	fmt.Println(ans)

}
