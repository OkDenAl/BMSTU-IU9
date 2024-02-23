package main

import "fmt"

type S struct {
}

func (s *S) test() {
	fmt.Printf("9((*S).test): test\n")
}

func (s S) test1() {
	fmt.Printf("13(S.test1): test\n")
}

func test() {
	fmt.Printf("17(test): test\n")
}

func Printf(string2 string) {

}

func test1() func() {
	return func() {
		Printf("test1")    // no change
		fmt.Print("test1") // no change
		fmt.Printf("28(func1): test1")
	}
}

func main() {
	test()
	world := func() string {
		fmt.Printf("35(func1): anonym func\n")
		func() {
			fmt.Printf("37(func2): anonym1 func\n")
			fmt.Printf("38(func2): anonym1 func\n")
		}()
		go func() {
			fmt.Printf("41(func3): anonym1 func\n")
			var f func()
			f = func() {
				fmt.Printf("44(func4): anonym1 func\n")
			}
			f()
			fmt.Printf("47(func3): anonym1 func\n")
		}()

		fmt.Printf("50(func1): anonym func\n")
		return "world"
	}
	fmt.Printf("53(main): Hello, %s", world())
	fmt.Printf("54(main): Hello, %s", world())
	fmt.Printf("55(main): Hello, %s", world())
}
