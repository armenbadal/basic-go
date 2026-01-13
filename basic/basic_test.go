package main

import "testing"

func TestRunOnFile(t *testing.T) {
	status := runOneFile("../examples/ex14.bas")
	if status != 0 {
		t.Fail()
	}
}
