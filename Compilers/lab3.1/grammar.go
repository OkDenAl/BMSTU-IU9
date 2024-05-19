package main

import (
	"fmt"
	"log"
)

type Grammar struct {
	Rules    map[string][]string
	Axiom    string
	NonTerms map[string]struct{}
	Terms    map[string]struct{}
}

func GetGrammar(tree NodePrinter) Grammar {
	gr := Grammar{
		Rules:    make(map[string][]string),
		NonTerms: make(map[string]struct{}),
		Terms:    make(map[string]struct{}),
	}

	var traverse func(treeNode NodePrinter)
	traverse = func(treeNode NodePrinter) {
		if node, ok := treeNode.(*InnerNode); ok {
			switch node.nterm {
			case "PROG":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else {
					log.Fatal("Невалидная длина PROG")
				}
			case "DECLS":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина DECLS")
				}
			case "DECL":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина DECLS")
				}
			case "AXIOM":
				if len(node.children) == 2 {
					if val, ok := node.children[1].(*Leaf); ok {
						if gr.Axiom == "" {
							gr.Axiom = val.tok.val
							gr.NonTerms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Больше чем 1 аксиома")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде Аксиомы")
					}
				} else {
					log.Fatal("Невалидная длина AXIOM")
				}
			case "NTERM":
				if len(node.children) == 3 {
					if val, ok := node.children[1].(*Leaf); ok {
						if _, ok = gr.NonTerms[val.tok.val]; !ok {
							gr.NonTerms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный нетерминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде NTERM")
					}
					traverse(node.children[2])
				} else {
					log.Fatal("Невалидная длина NTERM")
				}
			case "TERM":
				if len(node.children) == 3 {
					if val, ok := node.children[1].(*Leaf); ok {
						if _, ok = gr.Terms[val.tok.val]; !ok {
							gr.Terms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный терминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде TERM")
					}
					traverse(node.children[2])
				} else {
					log.Fatal("Невалидная длина TERM")
				}
			case "NON_TERMS":
				if len(node.children) == 2 {
					if val, ok := node.children[0].(*Leaf); ok {
						if _, ok = gr.NonTerms[val.tok.val]; !ok {
							gr.NonTerms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный нетерминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде NON_TERMS")
					}
					traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина NON_TERMS")
				}
			case "TERMS":
				if len(node.children) == 2 {
					if val, ok := node.children[0].(*Leaf); ok {
						if _, ok = gr.Terms[val.tok.val]; !ok {
							gr.Terms[val.tok.val] = struct{}{}
						} else {
							log.Fatal("Повторный терминал")
						}
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде TERMS")
					}
					traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина TERMS")
				}
			case "RULES":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else {
					log.Fatal("Невалидная длина RULES")
				}
			case "INNER_RULES":
				if len(node.children) == 2 {
					traverse(node.children[0])
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					traverse(node.children[0])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина INNER_RULES")
				}
			case "RULE":
				if len(node.children) == 4 {
					var left string
					if val, ok := node.children[1].(*Leaf); ok {
						if _, ok := gr.NonTerms[val.tok.val]; !ok {
							log.Fatal("Необъявленный нетерминал в правой части правила: " + val.tok.String())
						}
						left = val.tok.val
					} else {
						log.Fatal("Невалидное дерево, ожидался лист в ноде RULE")
					}

					body := getBody(gr, node.children[3])
					gr.Rules[left] = append(gr.Rules[left], body...)
				} else {
					log.Fatal("Невалидная длина RULE")
				}
			}

		}
	}

	traverse(tree)
	if gr.Axiom == "" {
		log.Fatal("Не указано ни одной аксиомы")
	}

	for nterm := range gr.NonTerms {
		if _, ok := gr.Rules[nterm]; !ok {
			log.Fatal("Нетерминал " + nterm + " не присутствует ни в одном правиле в левой части")
		}
	}

	return gr
}

func getBody(gr Grammar, tree NodePrinter) []string {
	rules := make([]string, 0)
	curRule := ""
	var traverse func(treeNode NodePrinter)
	traverse = func(treeNode NodePrinter) {
		if node, ok := treeNode.(*InnerNode); ok {
			switch node.nterm {
			case "BODY":
				if len(node.children) == 2 {
					traverse(node.children[0])
					rules = append(rules, curRule)
					curRule = ""
					traverse(node.children[1])
				} else {
					log.Fatal("Невалидная длина BODY")
				}
			case "ALTS":
				if len(node.children) == 2 {
					traverse(node.children[0])
					rules = append(rules, curRule)
					curRule = ""
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					traverse(node.children[0])
					rules = append(rules, curRule)
					curRule = ""
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина ALTS")
				}
			case "ALT":
				if len(node.children) == 3 {
					if n, ok := node.children[0].(*Leaf); ok {
						_, termsOK := gr.Terms[n.tok.val]
						if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
							log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
						}
						if curRule != "" {
							curRule += " " + n.tok.val
						} else {
							curRule += n.tok.val
						}
					}
					traverse(node.children[1])
				} else if len(node.children) == 2 {
					if n, ok := node.children[0].(*Leaf); ok {
						if n.tok.Tag() != KW_EPSTag {
							_, termsOK := gr.Terms[n.tok.val]
							if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
								log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
							}
							if curRule != "" {
								curRule += " " + n.tok.val
							} else {
								curRule += n.tok.val
							}
							traverse(node.children[1])
						}
					}
				} else if len(node.children) != 1 {
					log.Fatal("Невалидная длина ALT")
				}
			case "SYMBOLS":
				if len(node.children) == 2 {
					if n, ok := node.children[0].(*Leaf); ok {
						_, termsOK := gr.Terms[n.tok.val]
						if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
							log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
						}
						if curRule != "" {
							curRule += " " + n.tok.val
						} else {
							curRule += n.tok.val
						}
					}
					traverse(node.children[1])
				} else if len(node.children) == 1 {
					if n, ok := node.children[0].(*Leaf); ok {
						_, termsOK := gr.Terms[n.tok.val]
						if _, ok := gr.NonTerms[n.tok.val]; !ok && !termsOK {
							log.Fatal("Необъявленный нетерминал в левой части правила: " + n.tok.String())
						}
						if curRule != "" {
							curRule += " " + n.tok.val
						} else {
							curRule += n.tok.val
						}
					}
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина SYMBOLS")
				}
			}
		}
	}

	traverse(tree)
	return rules
}

func (g Grammar) Print() {
	fmt.Println("Аксиома:", g.Axiom)
	fmt.Print("Нетерминалы: ")
	var str string
	for val := range g.NonTerms {
		str += val + " "
	}
	fmt.Println(str)

	fmt.Print("Терминалы: ")
	str = ""
	for val := range g.Terms {
		str += val + " "
	}
	fmt.Println(str)

	fmt.Println("Правила:")
	for key, val := range g.Rules {
		str = ""
		for _, r := range val {
			if str == "" {
				str += r
			} else {
				if r == "" {
					r = "ε"
				}
				str += " | " + r
			}
		}
		fmt.Println(key + " -> " + str)
	}
}
