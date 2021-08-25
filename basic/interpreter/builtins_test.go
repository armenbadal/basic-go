package interpreter

import "testing"

func TestLen(t *testing.T) {
	// տեքստերի համար
	const example = "I am a string"
	s0 := &value{kind: vText, text: example}
	r0 := builtins["LEN"](s0)
	if r0.kind != vNumber {
		t.Error("LEN should return a number")
	}
	if int(r0.number) != len(example) {
		t.Error("incorrect calculated value")
	}
}
