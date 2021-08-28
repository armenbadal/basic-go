package ast

import (
	"strings"
	"testing"
)

func TestPrimitives(t *testing.T) {
	b0 := NewBoolean(true)
	if b0.String() != "TRUE" {
		t.Error("failed")
	}

	b1 := NewBoolean(false)
	if b1.String() != "FALSE" {
		t.Error("failed")
	}
}

func TestArrayLiteral(t *testing.T) {
	a0 := NewArray([]Node{NewBoolean(true), NewBoolean(false), NewBoolean(true)})
	if a0.String() != "[TRUE, FALSE, TRUE]" {
		t.Error("failed")
	}
}

func TestUnary(t *testing.T) {
	u0 := NewUnary("-", NewNumber(3.14))
	s0 := u0.String()
	if !strings.HasPrefix(s0, "- 3.14") {
		t.Error("failed")
	}

	u1 := NewUnary("NOT", NewBoolean(false))
	if u1.String() != "NOT FALSE" {
		t.Error("failed")
	}
}

func TestLet(t *testing.T) {

}
