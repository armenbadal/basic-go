package interpreter

import (
	"testing"
)

func TestBoolean(t *testing.T) {
	b0 := NewBoolean(true)
	r0 := b0.evaluate(nil)
	println(r0)
}
