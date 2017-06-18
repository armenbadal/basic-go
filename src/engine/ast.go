
package engine

import (
	"container/list"
)

// Աբստրակտ քերականական ծառի հանգույց
type node struct {
	line int
}


// Ունիվերսալ արժեք
type Value struct {
	node
	kind rune
	number float64
	text string
}

// Նոր թվային օբյեկտ
func NewNumber(val float64) *Value {
	return &Value{kind: 'N', number: val}
}

// Նոր տեքստային օբյեկտ
func NewText(val string) *Value {
	return &Value{kind: 'T', text: val}
}


// Կատարման միջավայր
type Environment map[string]*Value


// Արտահայտություններ
type Expression interface {
	evaluate(Environment) *Value
}

//
type Variable struct {
	node
	name string
}

//
func NewVariable(nm string) *Variable {
	return &Variable{name: nm}
}

// Գործողություններ
const (
	NEG = iota
	ADD
	SUB
	CONC
	MUL
	DIV
	MOD
	POW

	EQ
	NE
	GT
	GE
	LT
	LE

	AND
	OR
	NOT
)


// Ունար գործողություն
type Unary struct {
	node
	opcode int
	expr Expression
}

//
func NewUnary(op int, eo Expression) *Unary {
	return &Unary{opcode: op, expr: eo}
}


// Բինար գործողություն
type Binary struct {
	node
	opcode int
	expro Expression
	expri Expression
}

//
func NewBinary(op int, exo, exi Expression) *Binary {
	return &Binary{opcode: op, expro: exo, expri: exi}
}


// Ֆունկցիայի կիրառում
type Apply struct {
	node
	callee *Subroutine
	arguments *list.List
}

//
func NewApply(cl *Subroutine, ags *list.List) *Apply {
	return &Apply{callee: cl, arguments: ags}
}


// Հրամաններ
type Statement interface {
	execute(Environment)
}

// Վերագրում
type Let struct {
	node
	varname string
	expr Expression
}

//
func NewLet(vn string, ex Expression) *Let {
	return &Let{varname: vn, expr: ex}
}


// Ներմուծում
type Input struct {
	node
	varname string
}

//
func NewInput(vn string) *Input {
	return &Input{varname: vn}
}


// Արտածում
type Print struct {
	node
	expr Expression
}

//
func NewPrint(ex Expression) *Print {
	return &Print{expr: ex}
}


// Ճյուղավորում
type If struct {
	node
	condition Expression
	decision Statement
	alternative Statement
}

//
func NewIf(co Expression, de Statement) *If {
	return &If{condition: co, decision: de}
}

//
func (s *If) SetElse(el Statement) {
	s.alternative = el
}



// Նախապայմանով ցիկլ
type While struct {
	node
	condition Expression
	body Statement
}

//
func NewWhile(co Expression, bo Statement) *While {
	return &While{condition: co, body: bo}
}


// Հաշվիչով ցիկլ
type For struct {
	node
	parameter string
	begin Expression
	end Expression
	step Expression
	body Statement
}

//
func NewFor(p string, b, e, s Expression, d Statement) *For {
	return &For{parameter: p, begin: b, end: e, step: s, body: d}
}


// Ենթածրագիր կանչ
type Call struct {
	callee *Subroutine
	arguments *list.List
}

//
func NewCall(cl *Subroutine, ags *list.List) *Call {
	return &Call{callee: cl, arguments: ags}
}


// Հրամանների հաջորդականություն
type Sequence struct {
	node
	items *list.List
}

//
func NewSequence() *Sequence {
	return &Sequence{items: list.New()}
}

//
func (s *Sequence) AddItem(e Statement) {
	s.items.PushBack(e)
}


// Ենթածրագիր
type Subroutine struct {
	node
	name string
	parameters *list.List
	body Statement
}

//
func NewSubroutine(nm string, pars *list.List, dy Statement) *Subroutine {
	return &Subroutine{name: nm, parameters: pars, body: dy}
}


// Ծրագիր
type Program struct {
	Members map[string]*Subroutine
}

//
func NewProgram() *Program {
	return &Program{Members: make(map[string]*Subroutine)}
}

//
func (p *Program) AddMember(su *Subroutine) {
	p.Members[su.name] = su
}


