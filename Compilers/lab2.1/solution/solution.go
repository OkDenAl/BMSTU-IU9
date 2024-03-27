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
