package main

import (
	"fmt"
	"lab3.2/entity"
)

func printVote(vote entity.Vote) {
	fmt.Println(vote.Title)
	for variant := range vote.Variants {
		fmt.Printf("%s\t", variant)
	}
	fmt.Println()
}

func printRes(vote entity.Vote) {
	for variant := range vote.Variants {
		fmt.Printf("За вариант \"%s\" проголосовало %d человек(а) \n",
			variant, vote.Variants[variant])
	}
}

func printConnections(node *entity.Node) {
	for addr := range node.Connections {
		fmt.Println("|", addr)
	}
}
