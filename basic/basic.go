package main

import (
	"basic/parser"
	"fmt"
)

func main() {
	fmt.Println("Բալ ալգորիթմական լեզվի իրականացումը Go լեզվով։")

	pars, err := parser.NewParser("../examples/ex00.bas")
	if nil == err {
		tree := pars.Parse()
		println(tree)
	}
}
