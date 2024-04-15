package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	defer func() {
		if r := recover(); r != nil {
			os.Exit(1)
		}
	}()

	if len(os.Args) < 2 {
		log.Fatal("usage must be: go run main.go <fileTag.txt>\n")
	}
	filePath := os.Args[1]

	file, err := os.Open(filePath)
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	reader := bufio.NewReader(file)

	compiler := NewCompiler()
	scn := NewScanner(reader, &compiler)
	//
	//t := scn.NextToken()
	//for t.Tag() != EOPTag {
	//	if t.Tag() != ErrTag {
	//		fmt.Println(t.String())
	//	}
	//	t = scn.NextToken()
	//}

	parser := NewParser()

	tree, err := parser.TopDownParse(&scn)
	if err != nil {
		log.Panic(err)
	}
	tree.Print("")

	//compiler.GetIdentsNames()
	//compiler.OutputMessages()

	fmt.Println("COMMENTS:")
	scn.printComments()
}
