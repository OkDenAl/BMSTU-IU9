package main

import "fmt"

type S struct {
}

func (s *S) test() {
	fmt.Printf("test\n")
}

func (s S) test1() {
	fmt.Printf("test\n")
}

func test() {
	fmt.Printf("test\n")
}

func Printf(string2 string) {

}

func test1() func() {
	return func() {
		Printf("test1")    // no change
		fmt.Print("test1") // no change
		fmt.Printf("test1")
	}
}

func main() {
	test()
	world := func() string {
		fmt.Printf("anonym func\n")
		func() {
			fmt.Printf("anonym1 func\n")
			fmt.Printf("anonym1 func\n")
		}()
		go func() {
			fmt.Printf("anonym1 func\n")
			var f func()
			f = func() {
				fmt.Printf("anonym1 func\n")
			}
			f()
			fmt.Printf("anonym1 func\n")
		}()

		fmt.Printf("anonym func\n")
		return "world"
	}
	fmt.Printf("Hello, %s", world())
	fmt.Printf("Hello, %s", world())
	fmt.Printf("Hello, %s", world())
}
