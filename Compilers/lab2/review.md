% Лабораторная работа № 2.1. Синтаксические деревья
% 15 февраля 2024 г.
% Окутин Денис, ИУ9-61Б

# Цель работы
Целью данной работы является изучение представления синтаксических деревьев
в памяти компилятора и приобретение навыков преобразования синтаксических деревьев.

# Индивидуальный вариант
Во всех вызовах функции fmt.Printf добавить в форматную строку префикс вида
LINE(FUNC_NAME):, где LINE — текущий номер строки, FUNC_NAME — имя функции,
из которой происходит вызов fmt.Printf.

# Реализация

Демонстрационная программа:

```go
package main

import "fmt"

type S struct {
}

func (s *S) test() {
	fmt.Printf("test\n")
}

func (s S) test1() {
	fmt.Printf("test\n")
}

func test() {
	fmt.Printf("test\n")
}

func Printf(string2 string) {

}

func test1() func() {
	return func() {
		Printf("test1")    // no change
		fmt.Print("test1") // no change
		fmt.Printf("test1")
	}
}

func main() {
	test()
	world := func() string {
		fmt.Printf("anonym func\n")
		func() {
			fmt.Printf("anonym1 func\n")
			fmt.Printf("anonym1 func\n")
		}()
		go func() {
			fmt.Printf("anonym1 func\n")
			var f func()
			f = func() {
				fmt.Printf("anonym1 func\n")
			}
			f()
			fmt.Printf("anonym1 func\n")
		}()

		fmt.Printf("anonym func\n")
		return "world"
	}
	fmt.Printf("Hello, %s", world())
	fmt.Printf("Hello, %s", world())
	fmt.Printf("Hello, %s", world())
}
```

Программа, осуществляющая преобразование синтаксического дерева:

```go
package main

import (
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"os"
	"strconv"
)

func getLine(fset *token.FileSet, node ast.Node) int {
	return fset.Position(node.Pos()).Line
}

func isFmtPackage(ce *ast.CallExpr) bool {
	if se, ok := ce.Fun.(*ast.SelectorExpr); ok {
		if ident, ok := se.X.(*ast.Ident); ok {
			return ident.Name == "fmt"
		}
	}

	return false
}

func isPrintfFunc(ce *ast.CallExpr) bool {
	if se, ok := ce.Fun.(*ast.SelectorExpr); ok {
		return se.Sel.Name == "Printf"
	}

	return false
}

func addCaller(name string, pos int, ce *ast.CallExpr) {
	if bl, ok := ce.Args[0].(*ast.BasicLit); ok {
		bl.Value = fmt.Sprintf("\"%d(%s): %s", pos, name, bl.Value[1:])
	}
}

func getReciever(fd *ast.FuncDecl) string {
	if fd.Recv == nil {
		return ""
	}

	recvType := fd.Recv.List[0].Type
	switch recvType.(type) {
	case *ast.Ident:
		return recvType.(*ast.Ident).Name
	case *ast.StarExpr:
		if ident, ok := recvType.(*ast.StarExpr).X.(*ast.Ident); ok {
			return "*" + ident.Name
		}
	}
	// never
	return ""
}

func addCallerToPrintf(file *ast.File, fset *token.FileSet) {
	const nameForAnonymousFunc = "func"
	var (
		name = ""
		i    = 1
	)

	ast.Inspect(file, func(node ast.Node) bool {
		switch node.(type) {
		case *ast.FuncDecl:
			i = 1
			reciever := getReciever(node.(*ast.FuncDecl))
			name = node.(*ast.FuncDecl).Name.String()

			if reciever != "" {
				if reciever[0] == '*' {
					name = fmt.Sprintf("(%s).%s", reciever, name)
				} else {
					name = fmt.Sprintf("%s.%s", reciever, name)
				}
			}
		case *ast.FuncLit:
			name = nameForAnonymousFunc + strconv.Itoa(i)
			i++
		case *ast.BlockStmt:
			for _, l := range node.(*ast.BlockStmt).List {
				if es, ok := l.(*ast.ExprStmt); ok {
					if ce, ok := es.X.(*ast.CallExpr); ok {
						if isFmtPackage(ce) && isPrintfFunc(ce) {
							addCaller(name, getLine(fset, ce), ce)
						}
					}
				}
			}
		}

		return true
	})
}

func main() {
	if len(os.Args) != 2 {
		return
	}

	resFile, err := os.OpenFile("./result/result.go", os.O_TRUNC|os.O_CREATE|os.O_WRONLY, os.ModePerm)
	if err != nil {
		fmt.Println(err)
	}

	fset := token.NewFileSet()
	if file, err := parser.ParseFile(fset, os.Args[1], nil, parser.ParseComments); err == nil {
		addCallerToPrintf(file, fset)

		if format.Node(resFile, fset, file) != nil {
			fmt.Printf("Formatter error: %v\n", err)
		}
	} else {
		fmt.Printf("%v", err)
		fmt.Printf("Errors in %s\n", os.Args[1])
	}
}
```

# Тестирование

Результат трансформации демонстрационной программы:

```go
package main

import "fmt"

type S struct {
}

func (s *S) test() {
	fmt.Printf("9((*S).test): test\n")
}

func (s S) test1() {
	fmt.Printf("13(S.test1): test\n")
}

func test() {
	fmt.Printf("17(test): test\n")
}

func Printf(string2 string) {

}

func test1() func() {
	return func() {
		Printf("test1")    // no change
		fmt.Print("test1") // no change
		fmt.Printf("28(func1): test1")
	}
}

func main() {
	test()
	world := func() string {
		fmt.Printf("35(func1): anonym func\n")
		func() {
			fmt.Printf("37(func2): anonym1 func\n")
			fmt.Printf("38(func2): anonym1 func\n")
		}()
		go func() {
			fmt.Printf("41(func3): anonym1 func\n")
			var f func()
			f = func() {
				fmt.Printf("44(func4): anonym1 func\n")
			}
			f()
			fmt.Printf("47(func3): anonym1 func\n")
		}()

		fmt.Printf("50(func1): anonym func\n")
		return "world"
	}
	fmt.Printf("53(main): Hello, %s", world())
	fmt.Printf("54(main): Hello, %s", world())
	fmt.Printf("55(main): Hello, %s", world())
}
```


# Вывод
В ходе данной лабораторной работы было изучено представление синтаксических деревьев
в памяти компилятора. Также были приобретены навыки преобразования 
синтаксических деревьев. 

Довольно активно использую язык Golang, поэтому было достаточно полезно
получить навык работы с библиотеками `go/ast`, `go/format`, `go/parser`, `go/token`,
ранее с ними не работал. Круто, что стандартная библиотека языка `Golang` предоставляет
возможности по работе с `AST`.

Интересно то, что как-то я уже делал похожую по смыслу задачу. Я также прокидывал
`caller name` (название функции, из которой была вызвана другая функция) в начало сообщений,
которые писались в логгере. Тогда я использовал пакет `runtime`. Было интересно узнать способ
получения по-сути того же `caller name` из синтаксического дерева. Хотя, я думаю, возможности работы
с `AST` в `Golang` можно использовать для более нетривиальных задач.