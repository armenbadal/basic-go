package ast

import (
	"strings"
	"testing"
)

func TestPrimitives(t *testing.T) {
	b0 := &Boolean{true}
	if b0.String() != "TRUE" {
		t.Error("failed")
	}

	b1 := &Boolean{false}
	if b1.String() != "FALSE" {
		t.Error("failed")
	}
}

func TestArrayLiteral(t *testing.T) {
	a0 := &Array{[]Expression{&Boolean{true}, &Boolean{false}, &Boolean{true}}}
	if a0.String() != "[TRUE, FALSE, TRUE]" {
		t.Error("failed")
	}
}

func TestUnary(t *testing.T) {
	u0 := &Unary{Operation: "-", Right: &Number{Value: 3.14}}
	s0 := u0.String()
	if !strings.HasPrefix(s0, "- 3.14") {
		t.Error("failed")
	}

	u1 := &Unary{Operation: "NOT", Right: &Boolean{Value: false}}
	if u1.String() != "NOT FALSE" {
		t.Error("failed")
	}
}

func TestLet(t *testing.T) {

}
