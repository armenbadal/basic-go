package main

import "testing"

func TestRunOnFile(t *testing.T) {
	status := runOneFile("../examples/ex00.bas")
	if status != 0 {
		t.Fail()
	}
}
