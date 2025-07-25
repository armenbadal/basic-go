package main

import (
	"basic/interpreter"
	"basic/parser"
	"bufio"
	"fmt"
	"os"
)

func runOneFile(filename string) int {
	// բացել ֆայլային հոսքը
	file, er := os.Open(filename)
	if er != nil {
		fmt.Printf("ՍԽԱԼ։ ֆայլը բացելը ձախողվեց")
		return 1
	}
	defer file.Close()

	pars, err := parser.New(bufio.NewReader(file))
	if err != nil {
		fmt.Println(err)
		return 2
	}

	tree, err := pars.Parse()
	if err != nil {
		fmt.Println(err)
		return 3
	}
	//println(fmt.Sprint(tree))

	err = interpreter.Execute(tree)
	if err != nil {
		fmt.Println(err)
		return 4
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
