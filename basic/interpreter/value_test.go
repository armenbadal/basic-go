package interpreter

import "testing"

func TestValueToString(t *testing.T) {
	b0 := &value{kind: vBoolean, boolean: true}
	if b0.toString() != "true" {
		t.Error("Failed to create string for boolean value")
	}
}
