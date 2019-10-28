package main

import (
	"fmt"
	"parser"
)

func main() {
	fmt.Println("Բեյսիկ-Փ լեզվի իրականացումը Go լեզվով։")

	pars := parser.NewParser("c:/Projects/a0/test00.bas")
	pars.Parse()
}
