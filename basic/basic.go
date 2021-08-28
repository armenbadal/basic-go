package main

import (
	"basic/interpreter"
	"basic/parser"
	"fmt"
)

func main() {
	fmt.Println("Բալ ալգորիթմական լեզվի իրականացումը Go լեզվով։")

	pars, err := parser.NewParser("../examples/ex00.bas")
	if nil == err {
		tree := pars.Parse()
		//println(fmt.Sprint(tree))
		interpreter.Execute(tree)
	}
}
