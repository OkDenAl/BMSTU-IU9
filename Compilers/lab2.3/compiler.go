package main

import (
	"fmt"
	"reflect"
	"sort"
	"strings"
)

func SortedMapKeys(m map[Position]Message) (keyList []Position) {
	keys := reflect.ValueOf(m).MapKeys()

	for _, key := range keys {
		keyList = append(keyList, key.Interface().(Position))
	}
	sort.Slice(keyList, func(i, j int) bool {
		return keyList[i].line < keyList[j].line ||
			(keyList[i].line == keyList[j].line && keyList[i].pos < keyList[j].pos)
	})
	return
}

type Compiler struct {
	messages  map[Position]Message
	nameCodes map[string]int
	names     []string
}

func NewCompiler() Compiler {
	return Compiler{nameCodes: make(map[string]int), messages: make(map[Position]Message)}
}

func (c *Compiler) GetIdentsNames() {
	fmt.Println("IDENTs:")
	for i, name := range c.names {
		fmt.Printf("%d: %s\n", i, name)
	}
	fmt.Println()
}

func (c *Compiler) AddName(name string) int {
	name1 := strings.ToLower(name)
	if code, ok := c.nameCodes[name1]; ok {
		return code
	}
	code := len(c.names)
	c.names = append(c.names, name)
	c.nameCodes[name1] = code
	return code
}

func (c *Compiler) Name(code int) string {
	return c.names[code]
}

func (c *Compiler) AddMessage(isErr bool, p Position, text string) {
	c.messages[p] = NewMessage(isErr, text)
}

func (c *Compiler) OutputMessages() {
	list := SortedMapKeys(c.messages)
	for _, key := range list {
		val := c.messages[key]
		if val.isError {
			fmt.Print("Error")
		} else {
			fmt.Print("Warning")
		}
		fmt.Print(" ", key.String(), ": ")
		fmt.Println(val.text)
	}
}
