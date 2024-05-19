package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

type ParsingTable map[string][]string

func (pt ParsingTable) PrintGenTable(genTable map[string][]string) {
	fmt.Println("Таблица парсинга:")
	for k, v := range genTable {
		fmt.Println(k, v)
	}
}

func (pt ParsingTable) ConvertToGoFile(filename string, gr Grammar) {
	file, err := os.OpenFile(filename, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 777)
	if err != nil {
		log.Fatal(err)
	}
	file.WriteString("package main\n\n")

	file.WriteString("var parsingTable = map[string][]string{\n")
	for key, val := range pt {
		if len(val) == 1 && val[0] == "" {
			file.WriteString(fmt.Sprintf("\t\"%s\":{},\n", key))
		} else {
			file.WriteString(fmt.Sprintf("\t\"%s\":{%s},\n", key, customJoin(val)))
		}
	}

	file.WriteString("}\n")
	file.WriteString("\nvar grammarAxiom = " + "\"" + gr.Axiom + "\"\n")
	genIsTerminalFunc(file, gr)
	file.Close()
}

func genIsTerminalFunc(file *os.File, gr Grammar) {
	file.WriteString("\nvar nonTerms = []string{\n")
	for nt := range gr.NonTerms {
		file.WriteString("\t\"" + nt + "\",\n")
	}
	file.WriteString("}\n")
	file.WriteString("\nfunc isTerminal(s string) bool {\n\treturn !slices.Contains(nonTerms, s)\n}")
}

func customJoin(arr []string) string {
	res := ""
	for _, elem := range arr {
		res += "\"" + elem + "\","
	}
	return strings.TrimSuffix(res, ",")
}

func NewParsingTable(gr Grammar) ParsingTable {
	firstByNonTerm := make(map[string]map[string]struct{})
	for n := range gr.NonTerms {
		findFirst(firstByNonTerm, n, gr)
	}

	followByNonTerm := make(map[string]map[string]struct{})
	for n := range gr.NonTerms {
		findFollow(firstByNonTerm, followByNonTerm, n, gr)
	}

	table := make(map[string][]string)

	for left, right := range gr.Rules {
		isEps := false
		for _, r := range right {
			r := strings.Split(r, " ")
			firstSet := make(map[string]struct{})
			for _, el := range r {
				isEps = false
				first := findFirst(firstByNonTerm, el, gr)
				for f := range first {
					if f == "" {
						isEps = true
					} else {
						firstSet[f] = struct{}{}
					}
				}

				if _, ok := first[""]; !ok {
					break
				}

			}

			for term := range firstSet {
				key := fmt.Sprintf("%s %s", left, term)
				if _, ok := table[key]; ok {
					log.Fatalf("rules is not LL1: conflict %s", key)
				}
				table[key] = r
			}

			if isEps {
				follow := followByNonTerm[left]
				for term := range follow {
					key := fmt.Sprintf("%s %s", left, term)
					if _, ok := table[key]; ok {
						log.Fatalf("rules is not LL1: conflict %s", key)
					}
					table[key] = r
				}
			}
		}
	}

	return table
}

func findFirst(
	firstByNonTerm map[string]map[string]struct{},
	symbol string,
	gr Grammar,
) map[string]struct{} {
	first := make(map[string]struct{})

	if _, ok := firstByNonTerm[symbol]; ok {
		return firstByNonTerm[symbol]
	}

	if symbol == "" {
		first[""] = struct{}{}
		return first
	}

	if _, ok := gr.Terms[symbol]; ok {
		first[symbol] = struct{}{}
		return first
	}

	for _, alt := range gr.Rules[symbol] {
		ruleSymbs := strings.Split(alt, " ")
		for _, rightSymbol := range ruleSymbs {
			firstOfRight := findFirst(firstByNonTerm, rightSymbol, gr)

			for k := range firstOfRight {
				if len(firstOfRight) == 1 && k == "" {
					first[k] = struct{}{}
				} else if k != "" {
					first[k] = struct{}{}
				}
			}

			if _, ok := firstOfRight[""]; !ok {
				break
			}
		}

	}

	firstByNonTerm[symbol] = first
	return first
}

func findFollow(
	firstByNonTerm map[string]map[string]struct{},
	followByNonTerm map[string]map[string]struct{},
	symbol string,
	gr Grammar,
) map[string]struct{} {
	followSet := make(map[string]struct{})

	if _, ok := followByNonTerm[symbol]; ok {
		return followByNonTerm[symbol]
	}

	if symbol == gr.Axiom {
		followSet["$"] = struct{}{}
	}

	visited := make(map[string]bool)
	visited[symbol] = true

	for key, rule := range gr.Rules {
		for _, alt := range rule {
			ruleSymbs := strings.Split(alt, " ")
			for i, rightSymbol := range ruleSymbs {
				if rightSymbol == symbol && i < len(ruleSymbs)-1 {
					next := ruleSymbs[i+1]

					for firstSymbol := range findFirst(firstByNonTerm, next, gr) {
						if firstSymbol != "" {
							followSet[firstSymbol] = struct{}{}
						}
					}

					if _, ok := firstByNonTerm[next][""]; ok {
						if !visited[key] {
							for followSymbol := range findFollow(firstByNonTerm, followByNonTerm, key, gr) {
								followSet[followSymbol] = struct{}{}
							}
						}
					}
				}

				if rightSymbol == symbol && i == len(ruleSymbs)-1 {
					if !visited[key] {
						for followSymbol := range findFollow(firstByNonTerm, followByNonTerm, key, gr) {
							followSet[followSymbol] = struct{}{}
						}
					}
				}
			}
		}
	}

	followByNonTerm[symbol] = followSet

	return followSet
}
