package parser

import "testing"

func TestOne(t *testing.T) {
	pars, err := NewParser("../../examples/ex00.bas")
	if nil != err {
		t.Error("Failed to create parser")
	}

	tree := pars.Parse()
	if nil == tree {
		t.Error("failed to parse the file")
	}

}
