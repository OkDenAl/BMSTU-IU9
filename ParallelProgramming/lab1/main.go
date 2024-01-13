package main

import (
	"errors"
	"flag"
	"fmt"
	"math/rand"
	"sync"
	"time"
)

var (
	ErrIncorrectOptions = errors.New("incorrect options")
)

func createMatrix(n int) [][]int {
	rand.NewSource(time.Now().UnixNano())
	matrix := make([][]int, n)
	for i := 0; i < n; i++ {
		matrix[i] = make([]int, n)
		for j := 0; j < n; j++ {
			matrix[i][j] = rand.Intn(10)
		}
	}
	return matrix
}

func mulMatrixByRows(matrix1, matrix2, resMatrix [][]int, n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			for k := 0; k < n; k++ {
				resMatrix[i][j] += matrix1[i][k] * matrix2[k][j]
			}
		}
	}
}

func mulMatrixByRowsBatch(wg *sync.WaitGroup, matrix1, matrix2, resMatrix [][]int, n, startRow, endRow int) {
	defer wg.Done()
	for i := startRow; i < endRow; i++ {
		for j := 0; j < n; j++ {
			for k := 0; k < n; k++ {
				resMatrix[i][j] += matrix1[i][k] * matrix2[k][j]
			}
		}
	}
}

func mulMatrixByCols(matrix1, matrix2, resMatrix [][]int, n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			for k := 0; k < n; k++ {
				resMatrix[j][i] += matrix1[j][k] * matrix2[k][i]
			}
		}
	}
}

func isMatrixEqual(m1, m2 [][]int) bool {
	for i := 0; i < len(m1); i++ {
		for j := 0; j < len(m2); j++ {
			if m1[i][j] != m2[i][j] {
				return false
			}
		}
	}
	return true
}

type Options struct {
	matrixSize    int
	parallelParts int
}

func main() {
	opts := Options{}
	flag.IntVar(&opts.matrixSize, "size", 1000, "")
	flag.IntVar(&opts.parallelParts, "parts", 32, "")
	flag.Parse()
	if opts.parallelParts > opts.matrixSize {
		panic(ErrIncorrectOptions)
	}
	fmt.Printf("Size of matrix is %d\n\n", opts.matrixSize)
	//fmt.Println("Number of cores:", runtime.NumCPU())

	matrix1 := createMatrix(opts.matrixSize)
	matrix2 := createMatrix(opts.matrixSize)

	resMatrixDefault := make([][]int, opts.matrixSize)
	for i := 0; i < opts.matrixSize; i++ {
		resMatrixDefault[i] = make([]int, opts.matrixSize)
	}

	fmt.Println("Starting default counting...")
	t := time.Now()
	mulMatrixByRows(matrix1, matrix2, resMatrixDefault, opts.matrixSize)
	fmt.Println("mulMatrixByRows counting time:", time.Since(t))
	fmt.Println()

	resMatrixParallel := make([][]int, opts.matrixSize)
	for i := 0; i < opts.matrixSize; i++ {
		resMatrixParallel[i] = make([]int, opts.matrixSize)
	}

	var (
		wg   = &sync.WaitGroup{}
		k    = 0
		step = opts.matrixSize / opts.parallelParts
	)

	fmt.Println("Starting parallel counting...")
	fmt.Println("Count of parts:", opts.parallelParts)
	t = time.Now()
	for i := 0; i <= opts.matrixSize-step; i += step {
		wg.Add(1)
		if k == opts.parallelParts-1 {
			go mulMatrixByRowsBatch(wg, matrix1, matrix2, resMatrixParallel, opts.matrixSize, i, opts.matrixSize)
		} else {
			go mulMatrixByRowsBatch(wg, matrix1, matrix2, resMatrixParallel, opts.matrixSize, i, i+step)
		}
		k++
	}
	wg.Wait()
	fmt.Println("mulMatrixByRowsParallel counting time:", time.Since(t))
	fmt.Println()

	if isMatrixEqual(resMatrixParallel, resMatrixDefault) {
		fmt.Println("CORRECT")
		fmt.Println("Default and Parallel matrix are equal")
	} else {
		fmt.Println("INCORRECT")
		fmt.Println("Default and Parallel matrix are not equal")
	}
}
