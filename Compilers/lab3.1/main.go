package main

import (
	"bufio"
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

	parser := NewParser()

	tree, err := parser.TopDownParse(&scn)
	if err != nil {
		log.Panic(err)
	}

	gr := GetGrammar(tree)
	//gr.Print()

	table := NewParsingTable(gr)

	//fmt.Println(table)

	//table.ConvertToGoFile("./calculator/parsing_table_gen.go", gr)

	table.ConvertToGoFile("parsing_table_gen.go", gr)

	//tree.Print("")

	//compiler.GetIdentsNames()
	//compiler.OutputMessages()
}
