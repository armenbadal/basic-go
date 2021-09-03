package interpreter

import "testing"

func TestValueToString(t *testing.T) {
	b0 := &value{kind: vBoolean, boolean: true}
	if b0.String() != "true" {
		t.Error("Failed to create string for boolean value")
	}
}

func TestCloneValue(t *testing.T) {
	v0 := &value{kind: vNumber, number: 3.1415}
	v1 := v0.clone()
	if v1.number != v0.number {
		t.Error("clonning failed")
	}
}
