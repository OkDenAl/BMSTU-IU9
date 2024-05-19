package main

import (
	"log"
	"strconv"
)

func Interpret(tree NodePrinter) int {
	var traverse func(treeNode NodePrinter) int
	traverse = func(treeNode NodePrinter) int {
		if node, ok := treeNode.(*InnerNode); ok {
			switch node.nterm {
			case "E":
				if len(node.children) == 2 {
					left := traverse(node.children[0])
					right := traverse(node.children[1])
					return left + right
				} else if len(node.children) == 1 {
					return traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина E")
				}
			case "E'":
				if len(node.children) == 3 {
					left := traverse(node.children[1])
					right := traverse(node.children[2])
					return left + right
				} else if len(node.children) == 2 {
					return traverse(node.children[1])
				} else if len(node.children) != 0 {
					log.Fatal("Невалидная длина E'")
				}
			case "T":
				if len(node.children) == 2 {
					left := traverse(node.children[0])
					right := traverse(node.children[1])
					return left * right
				} else if len(node.children) == 1 {
					return traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина T")
				}
			case "T'":
				if len(node.children) == 3 {
					left := traverse(node.children[1])
					right := traverse(node.children[2])
					return left * right
				} else if len(node.children) == 2 {
					return traverse(node.children[1])
				} else if len(node.children) == 0 {
					return 1
				} else {
					log.Fatal("Невалидная длина T'")
				}
			case "F":
				if len(node.children) == 3 {
					return traverse(node.children[1])
				} else if len(node.children) == 1 {
					return traverse(node.children[0])
				} else {
					log.Fatal("Невалидная длина F")
				}
			default:
				log.Fatal("Неизвестный нетерминал", node, len(node.children))
			}
		} else if node, ok := treeNode.(*Leaf); ok {
			value, _ := strconv.Atoi(node.tok.val)
			return value
		}

		return 0
	}

	return traverse(tree)
}
