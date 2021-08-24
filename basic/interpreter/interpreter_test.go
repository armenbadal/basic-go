package interpreter

import (
	"basic/ast"
	"testing"
)

func TestBoolean(t *testing.T) {
	b0 := &ast.Boolean{Value: true}
	v0 := evaluate(b0, nil)
	if v0.kind != vBoolean || !v0.boolean {
		t.Error("Failed to evaluate boolean object")
	}
}

func TestNumber(t *testing.T) {
	n0 := &ast.Number{Value: 3.1415}
	v0 := evaluate(n0, nil)
	if v0.kind != vNumber || v0.number != 3.1415 {
		t.Error("Failed to evaluate number object")
	}
}

func TestText(t *testing.T) {
	t0 := &ast.Text{Value: "Basic"}
	v0 := evaluate(t0, nil)
	if v0.kind != vText || v0.text != "Basic" {
		t.Error("Failed to evaluate text object")
	}
}

func TestArray(t *testing.T) {
	a0 := &ast.Array{Elements: make([]ast.Node, 3)}
	a0.Elements[0] = &ast.Boolean{Value: false}
	a0.Elements[1] = &ast.Number{Value: 3.1415}
	a0.Elements[2] = &ast.Text{Value: "Hello"}
	v0 := evaluate(a0, nil)
	if v0.kind != vArray {
		t.Error("Failed to evaluate array object")
	}
	if len(v0.array) != 3 {
		t.Error("Failed to evaluate array object")
	}
	if v0.array[0].kind != vBoolean || v0.boolean {
		t.Error("Failed to evaluate array object")
	}
}

func TestVariable(t *testing.T) {
	env := &environment{}
	env.openScope()
	env.set("x", &value{kind: vBoolean, boolean: true})

	v0 := &ast.Variable{Name: "x"}
	r0 := evaluate(v0, env)
	if r0.kind != vBoolean || !r0.boolean {
		t.Error("Failed to evaluate variable object")
	}
}
