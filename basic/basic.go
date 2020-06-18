package main

import (
	"basic/parser"
	"fmt"
)

func main() {
	fmt.Println("Բեյսիկ-Փ լեզվի իրականացումը Go լեզվով։")

	pars, err := parser.NewParser("c:/Projects/a0/test00.bas")
	if nil == err {
		pars.Parse()
	}
}
