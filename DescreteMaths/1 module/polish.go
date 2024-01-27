package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func parenthesesChecker(expr string) bool {
	c := 0
	for _, symbol := range expr {
		if c < 0 {
			return false
		}
		if string(symbol) == "(" {
			c++
		} else if string(symbol) == ")" {
			c--
		}
	}
	if c == 0 {
		return true
	} else {
		return false
	}
}

func polishDec(expr string) int {
	opers := make([]string, 0)
	numbers := make([]int, 0)
	amountOfOpers := 0
	amountOfBrackets := 0
	curAmountOfNumsArr := make([]int, len(expr))
	AmountOfNums := 0
	var x int
	var curAmountOfNums int
	for _, sym := range expr {
		if string(sym) == ")" {
			if opers[amountOfOpers-1] == "+" {
				curAmountOfNums = curAmountOfNumsArr[amountOfBrackets]
				for i := AmountOfNums - 1; i >= AmountOfNums-curAmountOfNums; i-- {
					x += numbers[i]
				}
				numbers = append(numbers[:len(numbers)-curAmountOfNums], x)
				AmountOfNums = AmountOfNums - curAmountOfNums + 1
				curAmountOfNumsArr[amountOfBrackets] = 0
				x = 0
				amountOfBrackets--
				curAmountOfNumsArr[amountOfBrackets]++
				amountOfOpers--
				opers = opers[:amountOfOpers]
			} else if opers[amountOfOpers-1] == "-" {
				curAmountOfNums = curAmountOfNumsArr[amountOfBrackets]
				x = numbers[AmountOfNums-curAmountOfNums]

				for i := AmountOfNums - 1; i >= AmountOfNums-curAmountOfNums+1; i-- {
					x -= numbers[i]
				}
				numbers = append(numbers[:len(numbers)-curAmountOfNums], x)
				AmountOfNums = AmountOfNums - curAmountOfNums + 1
				curAmountOfNumsArr[amountOfBrackets] = 0
				amountOfBrackets--
				curAmountOfNumsArr[amountOfBrackets]++
				amountOfOpers--
				x = 0
				opers = opers[:amountOfOpers]
			} else if opers[amountOfOpers-1] == "*" {
				curAmountOfNums = curAmountOfNumsArr[amountOfBrackets]
				x = 1
				for i := AmountOfNums - 1; i >= AmountOfNums-curAmountOfNums; i-- {
					x *= numbers[i]
				}
				numbers = append(numbers[:len(numbers)-curAmountOfNums], x)
				AmountOfNums = AmountOfNums - curAmountOfNums + 1
				curAmountOfNumsArr[amountOfBrackets] = 0
				amountOfBrackets--
				curAmountOfNumsArr[amountOfBrackets]++
				amountOfOpers--
				x = 0
				opers = opers[:amountOfOpers]
			}
		}
		if string(sym) == "+" || string(sym) == "-" || string(sym) == "*" {
			opers = append(opers, string(sym))
			amountOfOpers++
		} else if string(sym) == "4" || string(sym) == "3" ||
			string(sym) == "5" || string(sym) == "6" ||
			string(sym) == "1" || string(sym) == "2" ||
			string(sym) == "7" || string(sym) == "8" ||
			string(sym) == "9" || string(sym) == "0" {
			AmountOfNums++
			curAmountOfNumsArr[amountOfBrackets]++
			numbers = append(numbers, int(sym-'0'))
		} else if string(sym) == "(" {
			amountOfBrackets++
		}
	}
	return numbers[0]
}

func main() {
	var expr string
	reader := bufio.NewReader(os.Stdin)
	expr, _ = reader.ReadString('\n')
	expr_ := strings.Replace(expr, " ", "", -1)
	if parenthesesChecker(expr_) {
		fmt.Println(polishDec(expr_))
	} else {
		fmt.Println("Error")
	}
}
