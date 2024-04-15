package main

import "fmt"

type NodePrinter interface {
	Print(indent string)
}

type InnerNode struct {
	nterm    string
	children []NodePrinter
}

func NewInnerNode(nterm string) *InnerNode {
	return &InnerNode{nterm: nterm, children: make([]NodePrinter, 0)}
}

func (in *InnerNode) Print(indent string) {
	fmt.Println(indent+"Внутренний узел: ", in.nterm)
	for _, child := range in.children {
		child.Print(indent + "\t")
	}
}

type Leaf struct {
	tok Token
}

func NewLeaf(t Token) *Leaf {
	return &Leaf{tok: t}
}

func (l *Leaf) Print(indent string) {
	if l.tok.Tag() == TermTag || l.tok.Tag() == NonTermTag {
		fmt.Println(indent + fmt.Sprintf("Лист: %s - %s", tagToString[l.tok.Tag()], l.tok.val))
	} else {
		fmt.Println(indent + fmt.Sprintf("Лист: %s", tagToString[l.tok.Tag()]))
	}
}
