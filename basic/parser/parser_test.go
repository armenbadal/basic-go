package parser

import "testing"

func TestOne(t *testing.T) {
	pars, err := New("../../examples/ex99.bas")
	if nil != err {
		t.Error("Failed to create parser")
	}

	tree, _ := pars.Parse()
	if nil == tree {
		t.Error("failed to parse the file")
	}

	if tree.Members != nil && len(tree.Members) != 3 {
		t.Error("failed to parse file")
	}
}
