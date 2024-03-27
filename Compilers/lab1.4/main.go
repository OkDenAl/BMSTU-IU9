package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	if len(os.Args) < 2 {
		log.Fatal("usage must be: go run main.go <fileTag.txt>\n")
	}
	filePath := os.Args[1]

	file, err := os.Open(filePath)
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	//reader := bufio.NewReaderSize() // для ограничения размера буфера reader`a
	reader := bufio.NewReader(file)

	compiler := NewCompiler()
	scn := NewScanner(reader, &compiler)

	t := scn.NextToken()
	for t.Tag() != EOPTag {
		if t.Tag() != ErrTag {
			fmt.Println(t)
		}
		t = scn.NextToken()
	}
	fmt.Println(t)
	fmt.Println()

	compiler.GetIdentsNames()
	compiler.OutputMessages()
}
