package main

import (
	"basic/interpreter"
	"basic/parser"
	"fmt"
	"os"
)

func runOneFile(file string) int {
	pars, err := parser.New(file)
	if err != nil {
		fmt.Println(err)
		return 1
	}

	if tree := pars.Parse(); tree != nil {
		//println(fmt.Sprint(tree))
		interp := interpreter.New()
		interp.Execute(tree)
	}

	return 0
}

const version = "0.0.1"

func main() {
	if len(os.Args) == 1 {
		fmt.Printf("Բալ ալգորիթմական լեզու (%s)\n", version)
		os.Exit(0)
	}

	status := runOneFile(os.Args[1])
	os.Exit(status)
}
