package main

import (
	"basic/parser"
	"fmt"
)

func main() {
	fmt.Println("Բեյսիկ-Փ լեզվի իրականացումը Go լեզվով։")

	pars, err := parser.NewParser("../examples/ex0.bas")
	if nil == err {
		tree := pars.Parse()
		print(tree)
	}
}
