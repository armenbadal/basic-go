package main

import "testing"

func TestRunOnFile(t *testing.T) {
	status := runOneFile("../examples/ex12.bas")
	if status != 0 {
		t.Fail()
	}
}
