package interpreter

import (
	"fmt"
)

// Պարզ տիպեր
const (
	vUndefined = '?'
	vBoolean   = 'B'
	vNumber    = 'N'
	vText      = 'T'
	vArray     = 'A'
)

// Value Ունիվերսալ արժեք
type value struct {
	kind    rune
	boolean bool
	number  float64
	text    string
	array   []*value
}

func (v *value) toString() string {
	res := "<undefined>"
	switch v.kind {
	case vBoolean:
		res = fmt.Sprint(v.boolean)
	case vNumber:
		res = fmt.Sprint(v.number)
	case vText:
		res = v.text
	case vArray:
		// TODO create string view of array
		res = "[]"
	case vUndefined:
	}
	return res
}

// Expression Արտահայտություններ
type expression interface {
	evaluate(*environment) *value
}

//
func (e *value) evaluate(env *environment) *value {
	return e
}

//
func (b *Boolean) evaluate(env *environment) *value {
	return &value{kind: vBoolean, boolean: b.Value}
}

//
func (n *Number) evaluate(env *environment) *value {
	return &value{kind: vNumber, number: n.Value}
}

//
func (t *Text) evaluate(env *environment) *value {
	return &value{kind: vText, text: t.Value}
}

//
func (a *Array) evaluate(env *environment) *value {
	els := len(a.Elements)
	res := &value{kind: vArray, array: make([]*value, els)}
	for i, e := range a.Elements {
		res.array[i] = e.(expression).evaluate(env)
	}
	return res
}

//
func (e *Variable) evaluate(env *environment) *value {
	return env.get(e.Name)
}

//
func (e *Unary) evaluate(env *environment) *value {
	return nil
}

//
func (e *Binary) evaluate(env *environment) *value {
	return nil
}

//
func (e *Apply) evaluate(env *environment) *value {
	return nil
}

// Statement Հրամանների ինտերֆեյսը
type Statement interface {
	execute(*environment)
}

//
func (s *Let) execute(env *environment) {
}

//
func (s *Input) execute(env *environment) {
}

//
func (s *Print) execute(env *environment) {
	e := s.Value.(expression).evaluate(env)
	fmt.Print(e.toString())
}

//
func (s *If) execute(env *environment) {
}

//
func (s *While) execute(env *environment) {
}

//
func (s *For) execute(env *environment) {
}

//
func (s *Call) execute(env *environment) {
}

//
func (s *Sequence) execute(env *environment) {
}

// Execute Կատարում է ամբողջ ծրագիրը՝ սկսելով Main անունով ենթածրագրից։
func (p *Program) Execute() {
}
