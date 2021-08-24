package ast

import (
	"testing"
)

func TestPrimitives(t *testing.T) {
	b0 := NewBoolean(true)

	b1 := NewBoolean(false)

	_ = b0
	_ = b1
}

func TestUnary(t *testing.T) {
	n0 := NewNumber(3.14)
	u0 := NewUnary("-", n0)
	_ = u0
}

func TestLet(t *testing.T) {

}
