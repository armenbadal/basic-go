package parser

import (
	"bufio"
	"os"
	"testing"
)

func TestOne(t *testing.T) {
	file, er := os.Open("../../examples/ex99.bas")
	if er != nil {
		t.Error("ՍԽԱԼ։ ֆայլը բացելը ձախողվեց")
	}
	defer file.Close()

	pars := New(bufio.NewReader(file))

	tree, _ := pars.Parse()
	if nil == tree {
		t.Fatal("failed to parse the file")
	}

	if len(tree.Subroutines) != 3 {
		t.Error("failed to parse file")
	}
}
