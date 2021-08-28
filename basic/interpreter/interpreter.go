package interpreter

import (
	"basic/ast"
	"fmt"
)

// Կատարվող ծրագրի ցուցիչը
var program *ast.Program

//
func evaluateBoolean(b *ast.Boolean, env *environment) *value {
	return &value{kind: vBoolean, boolean: b.Value}
}

//
func evaluateNumber(n *ast.Number, env *environment) *value {
	return &value{kind: vNumber, number: n.Value}
}

//
func evaluateText(t *ast.Text, env *environment) *value {
	return &value{kind: vText, text: t.Value}
}

//
func evaluateArray(a *ast.Array, env *environment) *value {
	els := len(a.Elements)
	res := &value{kind: vArray, array: make([]*value, els)}
	for i, e := range a.Elements {
		res.array[i] = evaluate(e, env)
	}
	return res
}

//
func evaluateVariable(v *ast.Variable, env *environment) *value {
	return env.get(v.Name)
}

//
func evaluateUnary(u *ast.Unary, env *environment) *value {
	var result *value

	switch u.Operation {
	case "-":
		rv := evaluate(u.Right, env)
		if !rv.isNumber() {
			panic("Type error")
		}
		result = &value{kind: vNumber, number: -rv.number}
	case "NOT":
		rv := evaluate(u.Right, env)
		if !rv.isBoolean() {
			panic("Type error")
		}
		result = &value{kind: vBoolean, boolean: !rv.boolean}
	default:
		panic("Unknown unarty operation")
	}

	return result
}

//
func evaluateBinary(b *ast.Binary, env *environment) *value {
	var result *value

	rl := evaluate(b.Left, env)
	rr := evaluate(b.Right, env)

	switch b.Operation {
	case "+", "-", "*", "/", "\\", "^":
		if !rl.isNumber() || !rr.isNumber() {
			panic("Type error")
		}
		// TODO
	case "&":
		if !rl.isText() || !rr.isText() {
			panic("Type error")
		}
		result = &value{kind: vText, text: rl.text + rr.text}
	case "AND", "OR":
		if !rl.isBoolean() || !rr.isBoolean() {
			panic("Type error")
		}
		// TODO
	case "[]":
		if !rl.isArray() || !rr.isNumber() {
			panic("Type error")
		}
		// TODO check range
		result = rl.array[int(rr.number)]
	default:
		panic("Unknown binary operation")
	}

	return result
}

//
func evaluateApply(a *ast.Apply, env *environment) *value {
	avals := make([]*value, len(a.Arguments))
	for i, arg := range a.Arguments {
		avals[i] = evaluate(arg, env)
	}

	if bf, exists := builtins[a.Callee]; exists {
		return bf(avals...)
	}

	if uf, exists := program.Members[a.Callee]; exists {
		uds := uf.(*ast.Subroutine)
		if len(avals) != len(uds.Parameters) {
			panic("կիրառության արգումենտների և ենթածրագրի պարամետրերի քանակները հավասար չեն")
		}

		env.openScope()
		env.set(a.Callee, &value{kind: vUndefined})
		for i, p := range uds.Parameters {
			env.set(p, avals[i])
		}
		execute(uds.Body, env)
		result := env.get(a.Callee)
		env.closeScope()
		return result
	}

	panic("անծանոթ ենթածրագրի կիրառություն")
}

//
func evaluate(n ast.Node, env *environment) *value {
	var result *value

	switch e := n.(type) {
	case *ast.Boolean:
		result = evaluateBoolean(e, env)
	case *ast.Number:
		result = evaluateNumber(e, env)
	case *ast.Text:
		result = evaluateText(e, env)
	case *ast.Array:
		result = evaluateArray(e, env)
	case *ast.Variable:
		result = evaluateVariable(e, env)
	case *ast.Unary:
		result = evaluateUnary(e, env)
	case *ast.Binary:
		result = evaluateBinary(e, env)
	case *ast.Apply:
		result = evaluateApply(e, env)
	}

	return result
}

//
func executeDim(d *ast.Dim, env *environment) {
	sz := evaluate(d.Size, env)
	if !sz.isNumber() {
		panic("Type error")
	}
	arr := &value{kind: vArray, array: make([]*value, int(sz.number), int(sz.number))}
	env.set(d.Name, arr)
}

//
func executeLet(l *ast.Let, env *environment) {
	p := evaluate(l.Place, env)
	v := evaluate(l.Value, env)
	*p = *v
}

//
func executeInput(i *ast.Input, env *environment) {
}

//
func executePrint(p *ast.Print, env *environment) {
	e := evaluate(p.Value, env)
	fmt.Println(e.toString())
}

//
func executeIf(i *ast.If, env *environment) {
	c := evaluate(i.Condition, env)
	if !c.isBoolean() {
		panic("տիպի սխալ") // TODO review
	}

	if c.boolean {
		execute(i.Decision, env)
	} else {
		execute(i.Alternative, env)
	}
}

//
func executeWhile(w *ast.While, env *environment) {
	for {
		c := evaluate(w.Condition, env)
		if !c.isBoolean() {
			panic("execution error") // TODO review
		}

		if !c.boolean {
			break
		}

		execute(w.Body, env)
	}
}

//
func executeFor(f *ast.For, env *environment) {
}

//
func executeCall(c *ast.Call, env *environment) {
	ap := ast.Apply{Callee: c.Callee, Arguments: c.Arguments}
	_ = evaluateApply(&ap, env)
}

//
func executeSequence(s *ast.Sequence, env *environment) {
	env.openScope()
	for _, st := range s.Items {
		execute(st, env)
	}
	env.closeScope()
}

//
func execute(n ast.Node, env *environment) {
	switch s := n.(type) {
	case *ast.Dim:
		executeDim(s, env)
	case *ast.Let:
		executeLet(s, env)
	case *ast.Input:
		executeInput(s, env)
	case *ast.Print:
		executePrint(s, env)
	case *ast.If:
		executeIf(s, env)
	case *ast.While:
		executeWhile(s, env)
	case *ast.For:
		executeFor(s, env)
	case *ast.Call:
		executeCall(s, env)
	case *ast.Sequence:
		executeSequence(s, env)
	}
}

// Execute Կատարում է ամբողջ ծրագիրը՝ սկսելով Main անունով ենթածրագրից։
func Execute(p *ast.Program) {
	program = p

	cmain := ast.Apply{Callee: "Main", Arguments: make([]ast.Node, 0)}
	env := &environment{}
	env.openScope()
	_ = evaluate(&cmain, env)
	env.closeScope()
}
