package main

import "fmt"

func isInDict(curExpr string, dict map[string]bool) bool {
	for i, _ := range dict {
		if i == curExpr {
			return true
		}
	}
	return false
}

func main() {
	var expression string
	fmt.Scan(&expression)

	calculated := make(map[string]bool)
	opers := make([]string, 0)
	vars := make([]string, 0)
	amountOfOpers := 0
	curAmount := 0
	allAmount := 0
	amountOfBrackets := 0
	curAmountArr := make([]int, len(expression))
	runes := []rune(expression)

	for _, rune := range runes {
		if rune == ')' {
			curAmount = curAmountArr[amountOfBrackets]
			curExpr := opers[amountOfOpers-1]
			for i := allAmount - curAmount; i < allAmount; i++ {
				curExpr += vars[i]
			}
			vars = append(vars[:len(vars)-curAmount], curExpr)
			if !isInDict(curExpr, calculated) {
				calculated[curExpr] = true
			}
			allAmount = (allAmount - curAmount) + 1
			amountOfOpers--
			curAmountArr[amountOfBrackets] = 0
			amountOfBrackets--
			curAmountArr[amountOfBrackets]++
			opers = opers[:amountOfOpers]
			curExpr = ""
		} else if rune == '(' {
			amountOfBrackets++
		} else if rune == '#' || rune == '$' || rune == '@' {
			opers = append(opers, string(rune))
			amountOfOpers++
		} else {
			vars = append(vars, string(rune))
			curAmountArr[amountOfBrackets]++
			allAmount++
		}
	}
	fmt.Println(len(calculated))
}
