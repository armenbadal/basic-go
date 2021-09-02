package main

import (
	"basic/interpreter"
	"basic/parser"
	"fmt"
)

func main() {
	fmt.Println("Բալ ալգորիթմական լեզվի իրականացումը Go լեզվով։")
	fmt.Println()

	pars, err := parser.NewParser("../examples/ex01.bas")
	if nil == err {
		if tree := pars.Parse(); tree != nil {
			//println(fmt.Sprint(tree))
			interpreter.Execute(tree)
		}
	}
}
