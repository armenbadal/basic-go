package main

import (
	"basic/interpreter"
	"basic/parser"
	"fmt"
)

func main() {
	fmt.Println("Բալ ալգորիթմական լեզվի իրականացումը Go լեզվով։")
	fmt.Println("--------------------------------------------")

	pars, err := parser.New("../examples/ex01.bas")
	if nil == err {
		if tree := pars.Parse(); tree != nil {
			println(fmt.Sprint(tree))
			interp := interpreter.New()
			interp.Execute(tree)
		}
	}
}
